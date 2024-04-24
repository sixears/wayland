{ pkgs }: pkgs.writers.writeBashBin "sway-float" ''

# https://github.com/Synthetica9/sway-floating/blob/master/floating

set -eu -o pipefail
PATH=/dev/null

readonly jq=${pkgs.jq}/bin/jq
readonly swaymsg=${pkgs.sway}/bin/swaymsg
tail=${pkgs.coreutils}/bin/tail
xargs=${pkgs.findutils}/bin/xargs

$@ &
pid=$!

jqtxt='.container | select(.pid == $pid) | .id'

$swaymsg --type subscribe -m '[ "window" ]'            \
  | $jq --unbuffered --argjson pid "$pid" "$jqtxt" \
  | $xargs -I '@' -- $swaymsg '[ con_id=@ ] floating enable' &

# $! is the pid of the most recently-executed background process
subscription=$!

echo "waiting for pid $pid (subscription $subscription)"

# Wait for our process to close
$tail --pid=$pid --follow /dev/null

echo Killing subscription $subscription
kill $subscription

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# fill-column: 80
# End:
