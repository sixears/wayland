{ pkgs, byobu }: pkgs.writers.writeBashBin "alac" ''

# use wofi to choose a tmux instance, launch an alacritty
# using that tmux

set -eu -o pipefail

alacritty=${pkgs.alacritty}/bin/alacritty
byobu=${byobu}/bin/byobu
env=${pkgs.coreutils}/bin/env
getent=${pkgs.glibc.bin}/bin/getent
id=${pkgs.coreutils}/bin/id
sort=${pkgs.coreutils}/bin/sort
tmux=${pkgs.tmux}/bin/tmux
tr=${pkgs.coreutils}/bin/tr
wc=${pkgs.coreutils}/bin/wc
wofi=${pkgs.wofi}/bin/wofi

: ''${USER:=$($id --user --name)}
: ''${HOME:="$(getent passwd "$USER" | $cut -d : -f 6 )"}
: ''${UID:=$($id --user)}
: ''${XDG_RUNTIME_DIR:=/run/user/$UID}

export XDG_RUNTIME_DIR

config_file=$HOME/rc/alacritty/config.yml

exec >& "$XDG_RUNTIME_DIR/alac-$$.log"
TZ=UTC ${pkgs.coreutils}/bin/date +%Y-%m-%dZ%H:%M:%S
echo "PID: $$"
echo
echo '-- ENV ---------------------------------'
$env --null | $sort --zero-terminated | $tr \\0 \\n
echo '----------------------------------------'
echo

sessions="$($tmux list-sessions -F '#{session_group}' | $sort -u)"
session_count=$($wc --lines <<< "$sessions")

echo '-- SESSIONS ----------------------------'
echo "$sessions"
echo '----------------------------------------'

wofi_args=( --sort-order=alphabetical --dmenu --gtk-dark
            --lines $((1+session_count)) )
term_name="$(echo "$sessions" | $wofi "''${wofi_args[@]}")"

alacritty_args=( --config-file $config_file --command $byobu new )
if [[ $term_name = "" ]]; then
  exec $alacritty "''${alacritty_args[@]}"
else
  # -A, -t are passed through to tmux
  exec $alacritty "''${alacritty_args[@]}" -A -t "$term_name"
fi
''

# Local Variables:
# mode: sh
# End:
