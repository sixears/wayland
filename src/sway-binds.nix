{ pkgs, sway-sock, sway-show-bindings }: pkgs.writers.writeBashBin "sway-binds" ''

set -eu -o pipefail

readonly column="${pkgs.util-linux}"/bin/column
readonly sway_sock=${sway-sock}/bin/sway-sock
readonly sway_show_bindings=${sway-show-bindings}/bin/sway-show-bindings

export SWAYSOCK="$($sway_sock)"

column_args=( --table --separator $'\t' --output-separator ' | '
              --table-columns MODE,MODIFIERS,KEY,EFFECT )
$sway_show_bindings | $column "''${column_args[@]}"
echo
read -p 'hit return to exit'
''
# Local Variables:
# mode: sh
# sh-basic-offset: 2
# fill-column: 80
# End:
