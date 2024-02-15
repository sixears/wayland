{ pkgs, sway-config, replace }: pkgs.writers.writeBashBin "sway-rc" ''

set -eu -o pipefail
PATH=/dev/null

uid="$(${pkgs.coreutils}/bin/id --user)"

replace=${replace}/bin/replace
if [[ -t 0 ]]; then
  # convenience hook for when run interactively
  output=/run/user/$uid/sway.rc
  echo "writing to $output" >&2
  exec >$output
fi
exec $replace __UID__ $uid < ${sway-config}/share/sway.rc

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# End:
''
