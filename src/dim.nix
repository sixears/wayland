{ pkgs, flock-pid-run }: pkgs.writers.writeBashBin "dim" ''

PATH=/dev/null

daemonize=${pkgs.daemonize}/bin/daemonize
flock=${pkgs.util-linux}/bin/flock
flock_pid_run=${flock-pid-run}/bin/flock-pid-run
gammastep=${pkgs.gammastep}/bin/gammastep
id=${pkgs.coreutils}/bin/id
uid=$($id --user)
true=${pkgs.coreutils}/bin/true

vlc_lockfile=/run/user/$uid/vlc.pid
gamma_lockfile=/run/user/$uid/gammastep.pid

# add warning if vlc file is unavailable (when connected to terminal)
if [[ -t 0 ]]; then
  if $flock -n -x $vlc_lockfile $true; then
    :
  else
    echo "cannot flock $vlc_lockfile" >&2; exit 255
  fi
fi

exec $daemonize                        \
       $flock_pid_run $vlc_lockfile    \
       $flock_pid_run $gamma_lockfile  \
       $gammastep -l 0:0 -o -b 0.5:0.5

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# End:
''
