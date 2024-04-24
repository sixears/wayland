{ pkgs }: pkgs.writers.writeBashBin "sway-sock" ''

set -eu -o pipefail

basename=${pkgs.coreutils}/bin/basename
id=${pkgs.coreutils}/bin/id
rm=${pkgs.coreutils}/bin/rm

readonly ID="$($id --user)"

declare -a socks=()

for i in /run/user/$ID/sway-ipc.$ID.*.sock; do
  b="$($basename "$i")"
  j="''${b#sway-ipc.$ID.}"
  pid="''${j%.sock}"

  if [[ -e /proc/$pid ]]; then
    socks+=( $i )
  else
    $rm -f "$i"
  fi
done

case ''${#socks[@]} in
  0 ) echo "no sway sockets found!" >&2;                       exit 1 ;;
  1 ) echo ''${socks[0]}                                              ;;
  * ) echo "multiple sway sockets found! (''${socks[*]})" >&2; exit 2 ;;
esac

# -- that's all, folks! --------------------------------------------------------
''
# Local Variables:
# mode: sh
# sh-basic-offset: 2
# fill-column: 80
# End:
