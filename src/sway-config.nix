# see https://fontawesome.com/ for pretty icons

{ pkgs, dim, sway-lock, i3stat, hostconfig, alac, wallpaper, lock-wallpaper
, sway-bindings, swap-summary-fifo, gammastep-lockfile, sway-power-on, grime
, flock-pid-run, swap-summary, cpu-temperature, cpu-temp-fifo, xkb, wofi-config
, paths, pa-mic-toggle }:
pkgs.writeTextDir "share/sway.rc" ''

# Read `man 5 sway` for a complete reference.

# -- input configuration -------------------------------------------------------

# input "1:1:AT_Translated_Set_2_keyboard" {
input type:keyboard {
# xkb_layout us(dvorak-intl)
# xkb_options "caps:ctrl_modifier,compose:prsc,altwin:menu,eurosign:4"
  xkb_file ${xkb}
}

input type:touchpad {
  # https://wayland.freedesktop.org/libinput/doc/latest/configuration.html
  accel_profile adaptive
  # disable-while-typing; see
  # https://wayland.freedesktop.org/libinput/doc/latest/palm-detection.html
  dwt enabled
  # mouse-button with one- (L), two- (R) or three- (M) fingers
  # https://wayland.freedesktop.org/libinput/doc/latest/clickpad-softbuttons.html
  click_method clickfinger
  # tap-to-click, rather than having to press hard on the pad
  # https://wayland.freedesktop.org/libinput/doc/latest/tapping.html
# tap enabled
}

# -- basic appearance ----------------------------------------------------------

# used for window headers, for example
font pango:Monaco,9

# -- variables -----------------------------------------------------------------

# Sway activation key. Use Mod1 for Alt.
set $mod Mod4

# used in filenames in, e.g., /run/user/$uid/
set $uid __UID__

# -- application launchers -----------------------------------------------------

# Note: pass the final command to swaymsg so that the resulting window can be opened
# on the original workspace that the command was run on.

# menu-driven command launcher, in the middle of the screen
set $wofi ${pkgs.wofi}/bin/wofi

# another menu-driven selector/launcher, uses the top of the screen
set $dmenu      ${pkgs.dmenu}/bin/dmenu
# set $dmenu_path ${pkgs.dmenu}/bin/dmenu_path

# preferred terminal emulator
set $term ${alac}/bin/alac

# other utility programs
set $flock_pid_run ${flock-pid-run}/bin/flock-pid-run
set $xargs ${pkgs.findutils}/bin/xargs
set $grimshot ${pkgs.sway-contrib.grimshot}/bin/grimshot
set $grime ${grime}/bin/grime

set $swaymsg ${pkgs.sway}/bin/swaymsg
# set $menu $dmenu_path | $dmenu | $xargs $swaymsg exec --

# -- output configuration ------------------------------------------------------

# screen layout configuration
exec_always ${hostconfig}/bin/hostconfig
# you can get the names of your outputs by running: swaymsg -t get_outputs

# Default wallpaper (more resolutions are available in
# /run/current-system/sw/share/backgrounds/sway/)
output * bg ${wallpaper} center #131318

# see also https://github.com/NixOS/nixos-artwork

# -- idle/lock configuration ---------------------------------------------------

set $dim ${dim}/bin/dim
set $sway-lock ${sway-lock}/bin/sway-lock
set $lock $sway-lock --daemonize --inside-color 161616 --image ${lock-wallpaper}
set $power-off '$swaymsg "output * power off"'
set $power-on '${sway-power-on}/bin/sway-power-on ${gammastep-lockfile}'

set $swayidle_pid /run/user/$uid/swayidle

# This will dim the screen after 8 minutes of inactivity, lock it after another
# 2 minutes, then turn off the displays after further 10 minutes, and turn the
# screens back on when resumed.  It will also lock the screen before the
# computer goes to sleep.

# -w : Wait for command to finish executing before continuing, helpful for
#      ensuring that a before-sleep command has finished before the system goes
#      to sleep.

# add vlc detection; and halve the timeout times when running on a laptop
# put this in its own executable
exec_always $flock_pid_run $swayidle_pid    \
  swayidle -w                               \
  timeout  480 $dim       resume $power-on  \
  timeout  600 '$lock'    resume $power-on  \
  timeout 1200 $power-off resume $power-on  \
  before-sleep '$lock'

# -- key bindings --------------------------------------------------------------

# start a terminal
bindsym $mod+Return exec $term # new term

set $paths ${paths}/bin/paths

# start an executable
set $exec_path $paths $wofi --conf ${wofi-config} --show run | xargs $swaymsg exec --
bindsym $mod+Shift+Return exec $exec_path # exec menu

# kill focused window
bindsym $mod+Shift+Escape kill

# lock the screen
bindsym $mod+backslash exec $lock # lock screen

# -- floating windows --------------------------------------

# Drag floating windows by holding down $mod and left mouse button.
# Resize them with right mouse button + $mod.
# Despite the name, also works for non-floating windows.
# Change normal to inverse to use left mouse button for resizing and right
# mouse button for dragging.
floating_modifier $mod normal

# Reload the configuration file
bindsym $mod+Shift+Control+slash reload

set $sway-bindings ${sway-bindings}/bin/sway-bindings
bindsym $mod+Shift+slash exec $sway-bindings # show key bindings summary

# -- exit with Super+Sh+Ctrl+Esc ---------------------------

# set $exit_menu exec swaynag -t warning -m 'Exit sway?' -B 'Yes, exit sway' \
#                             '$swaymsg exit'

# exit sway (logs you out of your Wayland session)
set $exit_menu bash -c \
    'e="$(echo -e "no exit\nexit" | $dmenu)"; [[ $e == exit ]] && $swaymsg exit'

# set $exit_menu exec bash -c \
#     'e="$(echo -e "no exit\nexit" | $wofi --sort-order=default --show dmenu  \
#                                           --location=0 --width=30% --conf    \
#                                            <(echo hide_search=true)          \
#                                            --height=120 --cache=/dev/null)"; \
#     [[ $e == exit ]] && $swaymsg exit'

bindsym $mod+Shift+Ctrl+Escape exec $exit_menu  # TEST exit sway

# -- moving around -----------------------------------------


# cycle through focus with Super+Tab (plus modifiers)
bindsym $mod+Tab            focus next
bindsym $mod+Shift+Tab      focus prev
bindsym $mod+Mod1+Tab       focus parent
bindsym $mod+Mod1+Shift+Tab focus child

# {- $mod+cursor move focus (by window)
bindsym $mod+Left  focus left
bindsym $mod+Down  focus down
bindsym $mod+Up    focus up
bindsym $mod+Right focus right
# -}

# move position focused of window with the same, but add Shift
# {- $mod+Shift+cursor move window
bindsym $mod+Shift+Left  move left
bindsym $mod+Shift+Down  move down
bindsym $mod+Shift+Up    move up
bindsym $mod+Shift+Right move right
# -}

# {- $mod+Control+cursor move output focus
bindsym $mod+Control+left  focus output left
bindsym $mod+Control+right focus output right
bindsym $mod+Control+up    focus output up
bindsym $mod+Control+down  focus output down
# -}

# -- workspaces --------------------------------------------

# 📄 🦊 💬 🔑  📸 ⚛ 🚀 💾 💻
set $ws1 "1:"
set $ws2 "2:"
set $ws3 "3:"
set $ws4 "4:"
set $ws5 "5:♫"
set $ws6 "6:"
set $ws7 "7:"
set $ws8 "8:"
set $ws9 "9:"
set $ws0 "0:"

# Switch focus to workspace
# {- $mod+[0-9] select #WS
bindsym $mod+1 workspace $ws1
bindsym $mod+2 workspace $ws2
bindsym $mod+3 workspace $ws3
bindsym $mod+4 workspace $ws4
bindsym $mod+5 workspace $ws5
bindsym $mod+6 workspace $ws6
bindsym $mod+7 workspace $ws7
bindsym $mod+8 workspace $ws8
bindsym $mod+9 workspace $ws9
bindsym $mod+0 workspace $ws0
# -}

# Move focused container to workspace
# {- $mod+Shift+[0-9] move window to #WS
bindsym $mod+Shift+1 move container to workspace $ws1
bindsym $mod+Shift+2 move container to workspace $ws2
bindsym $mod+Shift+3 move container to workspace $ws3
bindsym $mod+Shift+4 move container to workspace $ws4
bindsym $mod+Shift+5 move container to workspace $ws5
bindsym $mod+Shift+6 move container to workspace $ws6
bindsym $mod+Shift+7 move container to workspace $ws7
bindsym $mod+Shift+8 move container to workspace $ws8
bindsym $mod+Shift+9 move container to workspace $ws9
bindsym $mod+Shift+0 move container to workspace $ws0
# -}

# to move this workspace to a particular display, use
# move workspace to OUTPUT
# find outputs with swaymsg -t get_outputs and/or swaymsg -t get_workspaces (probably easier)

# -- workspace structure -----------------------------------

bindsym $mod+plus  split none
bindsym $mod+bar   splith
bindsym $mod+minus splitv

bindsym $mod+space       layout toggle all
bindsym $mod+Shift+space fullscreen

# -- scratchpad --------------------------------------------

# Sway has a "scratchpad", which is a bag of holding for windows.
# You can send windows there and get them back later.

# F13 is the lower-LH 'pipe/slash' on the dell XPS9315 keyboard

# move the currently focused window to the scratchpad
bindsym $mod+F13 move scratchpad

# Show the next scratchpad window or hide the focused scratchpad window.
# If there are multiple scratchpad windows, this command cycles through them.
bindsym $mod+Shift+F13   scratchpad show

# this doesn't work so well, because if there is more than one window in the
# scratchpad; scratchpad show rotates through them (you have to hit it twice,
# immediately after the first hit); but if you follow that with 'floating
# toggle', the rotation doesn't work.

# bindsym $mod+Shift+F13   scratchpad show; floating toggle

# This isn't really about the scratchpad; but the only time I use floating
# windows is that bringing a window back from the scratchpad brings it as
# floater.  Once I've rotated through to the window I want, I then use this to
# lay it out.

bindsym $mod+Shift+Ctrl+Alt+Escape exec $exit_menu # exit
bindsym $mod+Control+F13 floating toggle

# -- resizing containers -----------------------------------

# enter resize mode with Super+=; exit with Return or Escape

# >> enter resize mode
bindsym $mod+equal mode resize

mode resize {
  # Return to default mode
  bindsym Return mode default
  bindsym Escape mode default

  # left/right will grow the containers width
  # Shift+left/right will shrink the containers width
  # up/down will grow the containers height
  # Shift+up/down will shrink the containers height
  # {- cursor grow
  bindsym Left        resize grow   width  10px
  bindsym Right       resize grow   width  10px
  bindsym Down        resize grow   height 10px
  bindsym Up          resize grow   height 10px
  # -}
  # {- Shift+cursor shrink
  bindsym Shift+Left  resize shrink width  10px
  bindsym Shift+Right resize shrink width  10px
  bindsym Shift+Down  resize shrink height 10px
  bindsym Shift+Up    resize shrink height 10px
  # -}
}

bindsym $mod+Shift+bracketleft bar mode hide
bindsym $mod+bracketleft       bar mode dock

set $pactl exec /run/current-system/sw/bin/pactl
set $mute  $pactl set-sink-mute @DEFAULT_SINK@ toggle
set $vol   $pactl set-sink-volume @DEFAULT_SINK@
set $pa-mic-toggle exec ${pa-mic-toggle}/bin/pa-mic-toggle

set $xbacklight ${pkgs.light}/bin/light

# >> audio mute
bindsym XF86AudioMute $mute
# >> mic mute
bindsym XF86AudioMicMute $pa-mic-toggle
# >> volume++
bindsym XF86AudioRaiseVolume $vol +1%
# >> volume--
bindsym XF86AudioLowerVolume $vol -1%
# >> brightness++
bindsym XF86MonBrightnessUp   exec $xbacklight -A 5
# >> brightness--
bindsym XF86MonBrightnessDown exec $xbacklight -U 5

# (F5)/AudioPlay on Dell_XPS 9315
bindsym XF86AudioPlay input type:touchpad events toggle enabled disabled

# (F10)/screenshot on Lenovo Thinkpad Carbon Gen12
# >> screenshot selection to clipboard
bindsym XF86Launch2 exec $grimshot copy anything
# >> screenshot to ~/screenshots/...
bindsym $mod+XF86Launch2       exec $grime anything
# >> screenshot screen to ~/screenshots/...
bindsym Shift+XF86Launch2      exec $grimshot copy screen
# >> screenshot screen to ~/screenshots/...
bindsym $mod+Shift+XF86Launch2 exec $grime screen

# (F12)/Star on Lenovo Thinkpad Carbon Gen12
bindsym XF86Favorites input type:touchpad events toggle enabled disabled

# -- swaybar -------------------------------------------------------------------

# read `man 5 sway-bar` for more information about this section.

# inputs to i3status
set $swap ${swap-summary-fifo}
set $cpu_temp ${cpu-temp-fifo}

exec_always ${swap-summary}/bin/swap-summary $swap
exec_always ${cpu-temperature}/bin/cpu-temperature $cpu_temp

bar {
  status_command ${i3stat}/bin/i3stat
  position top
  font  Monaco,Bold 13px
#  separator_symbol "   "
  mode dock

  colors {
    statusline  #78da59
    #statusline #62AEEE
    #background #585858
    background  #332f2e
    #separator  #ffff00
    separator   #fff32e
                        #border #bg     #txt
    focused_workspace   #332f2e #332f2e #ffba08
    #inactive_workspace #332f2e #585858 #cde4e6
    inactive_workspace  #332f2e #332f2e #cde4e6
    active_workspace    #777F8B #777F8B #cde4e6
    urgent_workspace    #DEDFE3 #DEDFE3 #767E8A
  }
}

include /etc/sway/config.d/*
''

# workspace $ws4 output HDMI-A-3

