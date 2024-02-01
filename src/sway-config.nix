# Read `man 5 sway` for a complete reference.

exec_always ~/bin/hostconfig

# -- input configuration -------------------------------------------------------

# input "1:1:AT_Translated_Set_2_keyboard" {
input type:keyboard {
  xkb_layout dvorak
  xkb_options "caps:ctrl_modifier,compose:prsc,altwin:menu,eurosign:4"
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
#  tap enabled
}

# -- basic appearance ----------------------------------------------------------

# used for window headers, for example
font pango:Monaco,9

# -- variables -----------------------------------------------------------------

# Sway activation key. Use Mod1 for Alt.
set $mod Mod4

# preferred terminal emulator
set $term /home/martyn/.nix-profiles/wayland/bin/alac

# Your preferred application launcher
# Note: pass the final command to swaymsg so that the resulting window can be opened
# on the original workspace that the command was run on.
set $menu dmenu_path | dmenu | xargs swaymsg exec --
set $lock 'swaylock --daemonize --inside-color 161616 --image /home/martyn/wallpapers/nixos3.jpg'

set $swap /run/user/1000/swap-summary

### Output configuration
#
# Default wallpaper (more resolutions are available in /run/current-system/sw/share/backgrounds/sway/)
output * bg /home/martyn/wallpapers/nixos1.jpg center #131318

# see also https://github.com/NixOS/nixos-artwork

#
# Example configuration:
#
#   output HDMI-A-1 resolution 1920x1080 position 1920,0
#
# You can get the names of your outputs by running: swaymsg -t get_outputs

# -- idle/lock configuration ---------------------------------------------------

# -w : Wait for command to finish executing before continuing, helpful for
#      ensuring that a before-sleep command has finished before the system goes
#      to sleep.

set $power-off 'swaymsg "output * power off"'
# set $power-on  'swaymsg "output * power on"'
set $power-on '/home/martyn/bin/sway-power-on /run/user/1000/gammastep.pid'

# This will dim the screen after 8 minutes of inactivity, lock it after another
# 2 minutes, then turn off the displays after further 10 minutes, and turn the
# screens back on when resumed.  It will also lock the screen before the
# computer goes to sleep.

# add vlc detection; and halve the timeout times when running on a laptop
# put this in its own executable
exec_always /home/martyn/bin/flock-pid-run /run/user/1000/swayidle \
  swayidle -w                              \
  timeout  480 __dim-exe__       resume $power-on \
  timeout  600 $lock      resume $power-on \
  timeout 1200 $power-off resume $power-on \
  before-sleep $lock


### Input configuration
#
# Example configuration:
#
#   input "2:14:SynPS/2_Synaptics_TouchPad" {
#       dwt enabled
#       tap enabled
#       natural_scroll enabled
#       middle_emulation enabled
#   }
#
# You can get the names of your inputs by running: swaymsg -t get_inputs
# Read `man 5 sway-input` for more information about this section.

### Key bindings
#
# Basics:
#
    # Start a terminal
    bindsym $mod+Return exec $term

    # Start an executable
    bindsym $mod+Shift+Return exec ~/bin/paths wofi --conf /home/martyn/rc/wayland/wofi/config --show run | xargs swaymsg exec --

    # Kill focused window
    bindsym $mod+Shift+q kill

    # Lock the screen
    bindsym $mod+backslash exec $lock

    # Drag floating windows by holding down $mod and left mouse button.
    # Resize them with right mouse button + $mod.
    # Despite the name, also works for non-floating windows.
    # Change normal to inverse to use left mouse button for resizing and right
    # mouse button for dragging.
    floating_modifier $mod normal

    # Reload the configuration file
    bindsym $mod+Shift+c reload

    # Exit sway (logs you out of your Wayland session)
#    bindsym $mod+Shift+e exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'


    bindsym $mod+Shift+Escape exec bash -c 'e="$(echo -e "no exit\nexit" | wofi --sort-order=default --show dmenu --location=0 --width=30% --conf <(echo hide_search=true) --height=120 --cache=/dev/null)"; [[ $e == exit ]] && swaymsg exit'
#
# Moving around:
#
    # Move your focus around
    bindsym $mod+Left focus left
    bindsym $mod+Down focus down
    bindsym $mod+Up focus up
    bindsym $mod+Right focus right

    bindsym $mod+Tab focus next
    bindsym $mod+Shift+Tab focus prev
    bindsym $mod+Mod1+Tab focus parent
    bindsym $mod+Mod1+Shift+Tab focus child

    # Move the focused window with the same, but add Shift
    bindsym $mod+Shift+Left move left
    bindsym $mod+Shift+Down move down
    bindsym $mod+Shift+Up move up
    bindsym $mod+Shift+Right move right

bindsym $mod+Control+left focus output left
bindsym $mod+Control+right focus output right
bindsym $mod+Control+up    focus output up
bindsym $mod+Control+down  focus output down
#
# Workspaces:
#

set $ws1 "1:📄"
set $ws2 "2:🦊"
set $ws3 "3:💬"
set $ws4 "4:🔑"
set $ws5 "5:♫"
set $ws6 "6:📸"
set $ws7 "7:⚛"
set $ws8 "8:🚀"
set $ws9 "9:💾"
# set $ws0 "0:💻"
set $ws0 "0:"

    # Switch to workspace
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
    # Move focused container to workspace
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
    # Note: workspaces can have any name you want, not just numbers.
    # We just use 1-10 as the default.

workspace $ws4 output HDMI-A-3

#
# Layout stuff:
#
    # You can "split" the current object of your focus with
    # $mod+b or $mod+v, for horizontal and vertical splits
    # respectively.
    bindsym $mod+d split none
    bindsym $mod+h splith
    bindsym $mod+v splitv

    # Switch the current container between different layout styles
    bindsym $mod+s layout stacking
    bindsym $mod+w layout tabbed
    bindsym $mod+e layout toggle split

    # Make the current focus fullscreen
    bindsym $mod+f fullscreen

    # Toggle the current focus between tiling and floating mode
    bindsym $mod+Shift+space floating toggle

    # Swap focus between the tiling area and the floating area
    bindsym $mod+space focus mode_toggle

    # Move focus to the parent container
    bindsym $mod+a focus parent
#
# Scratchpad:
#
    # Sway has a "scratchpad", which is a bag of holding for windows.
    # You can send windows there and get them back later.

    # Move the currently focused window to the scratchpad
    bindsym $mod+Shift+minus move scratchpad

    # Show the next scratchpad window or hide the focused scratchpad window.
    # If there are multiple scratchpad windows, this command cycles through them.
    bindsym $mod+minus scratchpad show
#
# Resizing containers:
#
mode "resize" {
    # left will shrink the containers width
    # right will grow the containers width
    # up will shrink the containers height
    # down will grow the containers height
    bindsym Left resize shrink width 10px
    bindsym Down resize grow height 10px
    bindsym Up resize shrink height 10px
    bindsym Right resize grow width 10px

    # Return to default mode
    bindsym Return mode "default"
    bindsym Escape mode "default"
}
bindsym $mod+r mode "resize"

#
# Status Bar:
#
# Read `man 5 sway-bar` for more information about this section.
##bar {
##    position top
##
##    # When the status_command prints a new line to stdout, swaybar updates.
##    # The default just shows the current date and time.
###    status_command while date +'%Y-%m-%d %I:%M:%S %p'; do sleep 1; done
##    status_command /home/martyn/bin/status
##
##    colors {
##        statusline #ffffff
##        background #323232
##        inactive_workspace #32323200 #32323200 #5c5c5c
##    }
##}

bindsym $mod+Shift+b bar mode hide
bindsym $mod+b       bar mode dock

# run swap summary service for swaybar
# exec_always /home/martyn/.nix-profiles/wayland/bin/swap-summary $swap
exec_always /nix/store/nv1xhj92p8h5ni9nfcsd0s8c049ay9hm-swap-summary/bin/swap-summary $swap

set $pactl exec /run/current-system/sw/bin/pactl
set $mute  $pactl set-sink-mute @DEFAULT_SINK@ toggle
set $vol   $pactl set-sink-volume @DEFAULT_SINK@

set $xbacklight exec /home/martyn/.nix-profiles/x/bin/xbacklight

bindsym XF86AudioMute $mute
bindsym XF86AudioRaiseVolume $vol +1%
bindsym XF86AudioLowerVolume $vol -1%
bindsym XF86MonBrightnessUp    $xbacklight -inc 5
bindsym XF86MonBrightnessDown  $xbacklight -dec 5
# (F5)/AudioPlay on Dell_XPS 9315
bindsym XF86AudioPlay input type:touchpad events toggle enabled disabled

bar {
#  status_command i3status -c ~/rc/i3status/i3status
  status_command /home/martyn/.nix-profiles/gui/bin/i3status -c ~/rc/i3status/i3status.rc
  position top
  font  Monaco,Bold 11px
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
