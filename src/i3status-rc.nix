{ pkgs, ... }: pkgs.writeTextDir "share/i3status.rc" ''
# i3status configuration file. See "man i3status" for documentation.

# It is important that this file is edited as UTF-8.  The following line should
# contain a sharp s: If this not correctly displayed, fix your editor first!
# ß

general {
  # separator is set by the bar itself, e.g., swaybar configured in the sway
  # config
  output_format = "i3bar"
  colors        = true
  interval      = 1
  markup        = pango
}

order += "battery 0"

order += "disk /home"
order += "disk /tmp"
order += "disk /var"
order += "disk /local"

order += "ethernet _first_"
order += "wireless _first_"
# order += "run_watch DHCP"
# order += "run_watch VPNC"
# order += "path_exists VPN"
order += "read_file swap"

order += load
order += cpu_usage
order += "cpu_temperature 0"
order += memory

order += "volume master"
order += "tztime gmt"

ethernet _first_ {
  # if you use %speed, i3status requires the cap_net_admin capability
  format_up = "🔌 %ip @ %speed"
  format_down = ""
}

wireless _first_ {
  format_up = "<span color='#73ABFF'>🛜 %essid @ %quality // %ip</span>"
  format_down = ""
}

battery 0 {
  format = "%status %percentage %remaining(%emptytime)"
  last_full_capacity = true
  hide_seconds   = false
  status_chr     = "⚡"
  status_bat     = "🔋"
  status_unk     = "⁈"
  status_full    = "🌝"
  path           = "/sys/class/power_supply/BAT%d/uevent"
  low_threshold  = 10
  threshold_type = percentage
}

tztime gmt {
  format   = "%a🕛%Y-%m-%dZ%H:%M:%S"
  timezone = "UTC"
}

load {
  format        = "🧠 %5min"
  max_threshold = 5
  separator     = false
}

cpu_usage {
  format = "%usage"
}

cpu_temperature 0 {
  format = "💻 %degrees °C"
  path  = "/sys/devices/platform/coretemp.0/hwmon/hwmon3/temp1_input"
}

memory {
  memory_used_method = classical
  format             = "🐏 %percentage_free (%free)"
  unit               = auto
  threshold_degraded = "20%"
  threshold_critical = "10%"
}

disk "/home" {
  format         = "/home %percentage_used"
  prefix_type    = binary
  low_threshold  = 10
  threshold_type = percentage_free
}

disk "/tmp" {
  format         = "/tmp %percentage_used"
  prefix_type    = binary
  low_threshold  = 10
  threshold_type = percentage_free
}

disk "/var" {
  format         = "/var %percentage_used"
  prefix_type    = binary
  low_threshold  = 10
  threshold_type = percentage_free
}
disk "/local" {
  format         = "/local %percentage_used"
  prefix_type    = binary
  low_threshold  = 10
  threshold_type = percentage_free
}

volume master {
  format       = "🔊 %volume"
  format_muted = "🔇 %volume"
  device       = "default"
  mixer        = "Master"
  mixer_idx    = 0
}

read_file swap {
  path   = "/tmp/martyn/swap-summary"
  format = "💿 %content"
}

# ------------------------------------------------------------------------------

## run_watch DHCP {
##   pidfile = "/var/run/dhclient*.pid"
## }

## run_watch VPNC {
##   # file containing the PID of a vpnc process
##   pidfile = "/var/run/vpnc/pid"
## }

## path_exists VPN {
##   # path exists when a VPN tunnel launched by nmcli/nm-applet is active
##   path = "/proc/sys/net/ipv4/conf/tun0"
## }
''
