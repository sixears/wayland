{ pkgs, pidkill }: pkgs.writers.writeBashBin "sway-power-on" ''

set -eu -o pipefail
PATH=/dev/null

${pidkill}/bin/pidkill "$@"
exec {$pkgs.sway}/bin/swaymsg 'output * power on'

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# End:
''
