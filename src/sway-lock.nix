{ pkgs, flock-pid-run, vlc-lockfile, gammastep-lockfile }:
pkgs.writers.writeBashBin "sway-lock" ''

set -eu -o pipefail
PATH=/dev/null

flock=${pkgs.util-linux}/bin/flock
flock_pid_run=${flock-pid-run}/bin/flock-pid-run
id=${pkgs.coreutils}/bin/id
uid=$($id --user)
swaylock=${pkgs.swaylock}/bin/swaylock
true=${pkgs.coreutils}/bin/true

vlc_lockfile=${vlc-lockfile}

# add warning if vlc file is unavailable (when connected to terminal)
if [[ -t 0 ]]; then
  if $flock -n -x $vlc_lockfile $true; then
    :
  else
    echo "cannot flock $vlc_lockfile" >&2; exit 255
  fi
fi

exec $flock_pid_run $vlc_lockfile \
     $swaylock "$@"

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# End:
''
