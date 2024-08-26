{ pkgs, bash-header, hix }: ''

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${bash-header}

Cmd[nyx]=${hix}/bin/nyx
Cmd[readlink]=${pkgs.coreutils}/bin/readlink

Remote=false
Isolated=false

# ------------------------------------------------------------------------------

main() {
  local args=()
  $Remote && args+=( --remote )
  $Isolated && args+=( --isolated )

  gocmd 10 nyx "''${args[@]}" install -c gui XCompose
  local l; capture l gocmd 11 readlink "$HOME/.XCompose"
  local -r expect=".nix-profiles/gui/share/XCompose"
  [[ $l == "$expect" ]] || warn "~/.XCompose is not linked to $expect"
}

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname OPTION*

Re-install XCompose file from nix configuration

Available options:
  -r | --remote              disconnected from sixears network
  -R | --isolated            disconnected from all networks

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
EOF
)"

orig_args=("$@")
getopt_args=( -o vrR
              --long remote,isolated,verbose,debug,dry-run,help
              -n "$Progname" -- "$@" )
OPTS=$( ''${Cmd[getopt]} "''${getopt_args[@]}" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

args=()

while true; do
  case "$1" in
    # don't forget to update $Usage!!
    -r | --remote   ) Remote=false   ; shift ;;
    -R | --isolated ) Isolated=false ; shift ;;

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

$Remote && $Isolated && dieusage;

case ''${#args[@]} in
  0 ) main     ;;
  * ) dieusage ;;
esac

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
