{ pkgs, bash-header, sway-rc }: ''

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${bash-header}

Cmd[nyx]=${pkgs.hix}/bin/nyx
Cmd[sway-rc]=${sway-rc}/bin/sway-rc
Cmd[swaymsg]=${pkgs.sway-unwrapped}/bin/swaymsg

# ------------------------------------------------------------------------------

main() {
  go 10 nyx install -c wayland sway-rc
  go 11 sway-rc
  go 12 swaymsg reload
}

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname OPTION*

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
EOF
)"

orig_args=("$@")
getopt_args=( -o v
              --long verbose,debug,dry-run,help
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

case ''${#args[@]} in
  0 ) main      ;;
  * ) die_usage ;;
esac

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
