{ pkgs, sway-float, sway-binds }: pkgs.writers.writeBashBin "sway-bindings" ''

set -eu -o pipefail
PATH=/dev/null

readonly alacritty=${pkgs.alacritty}/bin/alacritty
readonly sway_binds=${sway-binds}/bin/sway-binds
readonly sway_float=${sway-float}/bin/sway-float
exec $sway_float $alacritty --option font.size=9.0 --command $sway_binds

''
# Local Variables:
# mode: sh
# sh-basic-offset: 2
# fill-column: 80
# End:
