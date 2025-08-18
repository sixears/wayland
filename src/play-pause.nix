{ pkgs }: pkgs.writers.writeBash "play-pause" ''

set -eu -o pipefail
PATH=/dev/null

${pkgs.audacious}/bin/audtool --playback-pause
''
# Local Variables:
# mode: sh
# sh-basic-offset: 2
# fill-column: 80
# End:
