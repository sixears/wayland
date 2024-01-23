{ pkgs }: pkgs.writers.writeBashBin "hostconfig" ''

# configure screens, typically of a multi-monitor display, to a layout
# uses /run/user/<id>/hostconfig.state to try to preserve prior layout
# defaults to a 4-screen layout on first use, e.g., on a freshly-booted host

set -eu -o pipefail

[[ -t 0 ]] || exec >& /tmp/hostconfig.log

swbin=/run/current-system/sw/bin

hostname=$swbin/hostname
id=$swbin/id
swaymsg=$swbin/swaymsg

id="$($id --user)"

: ''${XDG_RUNTIME_DIR:=/run/user/$id}

state_file="$XDG_RUNTIME_DIR"/hostconfig.state

declare -A outputs=( [left]=DP-1 [upper]=DP-2 [lower]=DP-3 [right]=DP-4 )
declare -A modes=( [upper]=2560x1440@59.951Hz [lower]=2560x1440@59.951Hz
                   [right]=2560x1440@59.951Hz [left]=1920x1080@60Hz )
declare -A transforms=( [upper]=180 [left]=270 [right]=90 )

current_screens=""

dry_run=false
verbose=false

warn() { echo "$@" >&2; }
usage() { warn "usage: $0 [-v|--verbose] [--dry-run] [1|2|2-2|4]"; exit 2; }

args=()
while [[ $# -ne 0 ]]; do
  case "$1" in
    -v | --verbose  ) verbose=true   ;;
    --dry-run       ) dry_run=true   ;;
    1 | 2 | 2-2 | 4 ) args+=( "$1" ) ;;
    -h              ) [[ -n $2 ]] && HOSTNAME="$2" || usage; shift ;;
    *               ) usage          ;;
  esac
  shift
done

: ''${HOSTNAME:="$($hostname --short)"}

swaymsg() {
  local cmd=( $swaymsg "$@")
  if $dry_run; then
    $verbose && warn "(CMD) ''${cmd[@]@Q}"
  else
    warn "CMD> ''${cmd[@]@Q}"
    "''${cmd[@]}"
  fi
}

case ''${#args[@]} in
  0) if [[ -e $state_file ]]; then
       current_screens="$(cat "$state_file")"
     else
       current_screens=""
     fi
     ;;

  1) current_screens="''${args[0]}" ;;
  *) usage                        ;;
esac

# mode_x 2560x1440@593951Hz => 2560
mode_x() { echo "''${1%x*}"; }

# mode_y 2560x1440@593951Hz
mode_y() {
  local mode="$1"
  local x_y="''${1%@*}"
  echo "''${x_y#*x}"
}


do_n () {
  local -n screens="$1"
  local i

  for i in "''${!outputs[@]}"; do
    if ''${screens[$i]}; then
      swaymsg output "''${outputs[$i]}" enable mode "''${modes[$i]}"
      local tx="''${transforms[$i]:-}"
      [[ -n $tx ]] && swaymsg output "''${outputs[$i]}" transform "$tx"
    else
      swaymsg output "''${outputs[$i]}" disable
    fi
  done

  left_width="$(mode_y "''${modes[left]}")"

  if ''${screens[upper]}; then # assumes upper => upper+lower
    left_y=$(( ($(mode_y "''${modes[upper]}") + $(mode_y "''${modes[lower]}")
                                            - $(mode_x "''${modes[left]}"))/2 ))
    lower_y="$(mode_y "''${modes[upper]}")"
  elif ''${screens[right]}; then
    left_y=$(( ($(mode_x "''${modes[right]}") - $(mode_x "''${modes[left]}"))/2 ))
  fi

  ''${screens[left]} && middle_x="$(mode_y "$left_width")"

  # assume upper => upper+lower
  if ''${screens[upper]}; then
    right_y=$(( ($(mode_y "''${modes[upper]}") + $(mode_y "''${modes[lower]}")
                                             - $(mode_x "''${modes[right]}"))/2 ))
    lower_y="$(mode_y "''${modes[upper]}")"
  fi

  if ''${screens[left]}; then
    if ''${screens[upper]} || ''${screens[lower]}; then
      right_x=$(( $(mode_y "''${modes[left]}") + $(mode_x "''${modes[upper]}") ))
    else
      right_x="$(mode_y "''${modes[left]}")"
    fi
  fi

  if ''${screens[left]}; then
    swaymsg output "''${outputs[left]}"  position 0                "''${left_y:-0}"
  fi

  ## !!! y-position left depends upon the use of upper/lower and/or right
  if ''${screens[upper]}; then
    swaymsg output "''${outputs[upper]}" position "''${middle_x:-0}" 0
  fi

  if ''${screens[lower]}; then
    swaymsg output "''${outputs[lower]}" position "''${middle_x:-0}" "''${lower_y:0}"
  fi

  if ''${screens[right]}; then
    swaymsg output "''${outputs[right]}" position "''${right_x:-0}"  "''${right_y:-0}"
  fi
}

do_4 () {
  local -A s=( [left]=true [upper]=true [lower]=true [right]=true )
  do_n s
}

do_2 () {
  local -A s=( [left]=false [upper]=true [lower]=true [right]=false )
  do_n s
}

do_2_2 () {
  local -A s=( [left]=true [upper]=false [lower]=false [right]=true )
  do_n s
}

case "$HOSTNAME" in
  dog )
        case "$current_screens" in
          2      ) do_2; current_screens=2 ;;
          2-2    ) do_2_2; current_screens=2-2 ;;
          "" | 4 ) do_4; current_screens=4 ;;

          1 )
            for i in HDMI-A-{2,3,4}; do $swaymsg output $i disable; done
            for i in HDMI-A-1; do swaymsg output $i mode 2560x1440@59.951Hz; done
            swaymsg output HDMI-A-1 position 0 0
            ;;

          *) warn "ERROR: bad current_screens: '$current_screens'"
             do_2; current_screens=2
             ;;
        esac

        echo "$current_screens" > $state_file
        ;;


    * ) ;;
esac

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# End:
