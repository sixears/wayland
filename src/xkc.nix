{ pkgs, bash-header, hix }: ''

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${bash-header}

Cmd[column]=${pkgs.util-linux}/bin/column
Cmd[nyx]=${hix}/bin/nyx
Cmd[readlink]=${pkgs.coreutils}/bin/readlink

# globals ------------------------------

readonly XCOMPOSE="$HOME/.XCompose"

# ------------------------------------------------------------------------------

update() {
  local remote=false
  local isolated=false

  getopt_args=( -o rR --long remote,isolated,help
                -n "$Progname update" -- "$@" )
  local opts
  opts=$( ''${Cmd[getopt]} "''${getopt_args[@]}" )

  [ $? -eq 0 ] || dieusage "update: options parsing failed (--help for help)"

  eval set -- "$opts"

  args=()

  while true; do
    case "$1" in
      # don't forget to update $Usage!!
      -r | --remote   ) remote=true   ; shift ;;
      -R | --isolated ) isolated=true ; shift ;;

      --help          ) usage                          ;;
      --              ) shift; args+=( "$@" )  ; break ;;
      *               ) args+=( "$1" )         ; shift ;;
    esac
  done

  $remote && $isolated && dieusage "--remote && --isolated may not be combined";

  local args=()
  $remote && args+=( --remote )
  $isolated && args+=( --isolated )

  gocmd 10 nyx "''${args[@]}" install -c gui XCompose
  local l; capture l gocmd 11 readlink "$HOME/.XCompose"
  local -r expect=".nix-profiles/gui/share/XCompose"
  [[ $l == "$expect" ]] || warn "~/.XCompose is not linked to $expect"
}

# --------------------------------------

search() {
  getopt_args=( -o s: --long sort:,sort-by:,help
                -n "$Progname $FUNCNAME" -- "$@" )
  local opts
  opts=$( ''${Cmd[getopt]} "''${getopt_args[@]}" )

  [ $? -eq 0 ] || dieusage "$FUNCNAME: options parsing failed (--help for help)"

  eval set -- "$opts"

  args=()

  local sort_field

  while true; do
    case "$1" in
      # don't forget to update $Usage!!
      -s | --sort | --sort-by )
        if [[ -v sort_field ]]; then
          die "sort field already set to $sort_field"
        else
          case "$2" in
            k | key | keys  ) sort_field=1    ;;
            v | value       ) sort_field=2    ;;
            u | unicode-id  ) sort_field=3    ;;
            d | description ) sort_field=4    ;;
            1 | 2 | 3 | 4   ) sort_field="$2" ;;
            * ) die "unknown sort field '$2'" ;;
          esac
        fi
        shift 2
        ;;

      --help          ) usage                          ;;
      --              ) shift; args+=( "$@" )  ; break ;;
      *               ) args+=( "$1" )         ; shift ;;
    esac
  done

  case "''${#args[@]}" in
    0 ) dieusage "missing search term" ;;
    1 ) local search_term="''${args[0]}" ;;
    * ) dieusage "too many search terms (got ''${#args[@]})" ;;
  esac

  if [[ $search_term =~ ^[a-z]$ ]]; then
    search_term="lowercase $search_term"
  fi

  local column_args=( --table
                      --separator        $'\t'
                      --output-separator ' | '
                      --output-width     20
                    )
  local perl_script='s/ +/ /g;'
  perl_script+='s/ *: "([^"]+)"\s+(\w+)\s+#\s+(.*)$/\t$1\t$2\t$3/'
  # perl -CS turns on UTF-8 for stdin,stdout,stderr
  gocmd 12 grep -Ev '^ *(#.*)$' "$XCOMPOSE" | \
    gocmd 13 grep "$search_term"            | \
    gocmd 14 perl -pl -CS -E "$perl_script" | \
    gocmd 15 sort -t $'\t' -k ''${sort_field:-1} | \
    gocmd 16 column "''${column_args[@]}"
}

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname OPTION*

Manage XCompose file from nix configuration

Sub-commands:
  update ) update configuration from nix

           available options:
           -r | --remote      disconnected from sixears network
           -R | --isolated    disconnected from all networks

  search STRING ) search for some string in the (active) XCompose

                  available options:

                  -s | --sort | --sort-by  Sort the output by column:

                     1 | k | key | keys   input key (first column)
                     2 | v | value        unicode output (second column)
                     3 | u | unicode-id   unicode value (third column)
                     4 | d | description  description (fourth column)

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

case ''${#args[@]} in
  0 ) dieusage "$Usage";;
  * ) case "''${args[0]}" in
        update ) update "''${args[@]:1}" ;;
        search ) search "''${args[@]:1}" ;;
        *      ) dieusage "$Usage" ;;
      esac
      ;;
esac


# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
