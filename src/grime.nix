{ pkgs, bash-header }: ''

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${bash-header}

Cmd[date]=${pkgs.coreutils}/bin/date
Cmd[grimshot]=${pkgs.sway-contrib.grimshot}/bin/grimshot
Cmd[wl-paste]=${pkgs.wl-clipboard}/bin/wl-paste

readonly DIR_DEFAULT=~/screenshots

# ------------------------------------------------------------------------------

main() {
  local dir="$1" args=( "''${@:2}" )
  local target=$dir/screenshot-"$( ''${Cmd[date]} +%Y-%m-%dT%H:%M:%S )".png
  ''${Cmd[grimshot]} copy "''${args[@]}" && \
    PATH=${pkgs.coreutils}/bin ''${Cmd[wl-paste]} --no-newline > "$target"
  echo "$target"
}

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname OPTION* GRIMSHOT-TARGET?

Example
  $Progname --dir /tmp screen

Options:
  --dir  Dir to write to.  Defaults to $DIR_DEFAULT

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
EOF
)"

orig_args=("$@")
getopt_args=( -o v
              --long verbose,debug,dry-run,help,dir:
              -n "$Progname" -- "$@" )
OPTS=$( ''${Cmd[getopt]} "''${getopt_args[@]}" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

args=()
dir="$DIR_DEFAULT"

while true; do
  case "$1" in
    # don't forget to update $Usage!!
    --dir           ) dir="$2"               ; shift 2 ;;

    # hidden option for testing
    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift ;;
    --help          ) usage                          ;;
    --dry-run       ) DryRun=true            ; shift ;;
    --debug         ) Debug=true             ; shift ;;
    --              ) shift; args+=( "$@" )  ; break ;;
    *               ) args+=( "$1" )         ; shift ;;
  esac
done

debug "CALLED AS: ''${0@Q} ''${orig_args[*]@Q}"
debug "ARG# (''${#args[@]})"
for i in ''${!args[@]}; do
  debug "ARG($i): ''${args[$i]@Q}"
done

if [[ ''${#args[@]} -eq 0 ]]; then
  args=( anything )
fi

main "$dir" "''${args[@]}"

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
