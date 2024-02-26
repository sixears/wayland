{ pkgs }: pkgs.writers.writeBashBin "xkb-file" ''

set -eu -o pipefail
PATH=/dev/null

xkbcomp=${pkgs.xorg.xkbcomp}/bin/xkbcomp

usage() { echo "usage $0 OUTPUT-XKB-FILE" >&2; exit 255; }

[[ $# -eq 1 ]]    || usage
[[ $1 =~ .xkb$ ]] || usage

exec $xkbcomp -xkb $DISPLAY "$1"

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# End:
''
