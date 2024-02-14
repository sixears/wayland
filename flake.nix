{
  description = "nix configuration for wayland things";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/354184a; # master 2023-12-13
    flake-utils.url = github:numtide/flake-utils/c0e246b9;
    myPkgs          = {
      url    = github:sixears/nix-pkgs/r0.0.7.0;
#      url    = path:/home/martyn/nix/pkgs/;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    gui-pkgs.url = path:/home/martyn/nix/gui;
  };

  outputs = { self, nixpkgs, flake-utils, gui-pkgs, myPkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs        = nixpkgs.legacyPackages.${system};
        my-pkgs     = myPkgs.packages.${system};
        my-settings = myPkgs.settings.${system};
        gui         = gui-pkgs.packages.${system};

        swap-summary-fifo = "/run/user/1000/swap-summary";
        vlc-lockfile       = my-settings.vlc-lockfile;
        gammastep-lockfile = "/run/user/$uid/gammastep";
        # flock-pid-run = my-pkgs.flock-pid-run;
        dim = import ./src/dim.nix
          { inherit pkgs vlc-lockfile gammastep-lockfile;
            inherit (my-pkgs) flock-pid-run; };
      in
        {
          packages = flake-utils.lib.flattenTree (with pkgs; rec {
            inherit (my-pkgs) swap-summary;
            # wdisplays is arandr for wayland; wev is xev for wayland
            inherit wdisplays wev;
            # wofi is a gui choser
            inherit wofi;
            # my personal wrapper around alacritty/byobu/tmux with a wofi selector
            alac = import ./src/alac.nix { inherit pkgs; };

            # clipboard management;
            inherit wl-clipboard; # wl-copy & wl-paste
            inherit wl-clipboard-x11; # xclip & xsel

            # multi-monitor setup
            hostconfig = import ./src/hostconfig.nix { inherit pkgs; };

##            i3status-rc =
##              import ./src/i3status-rc.nix { inherit pkgs swap-summary-fifo; };
##            inherit alacritty acpilight arandr i3status xmonad xscreensaver;
##            inherit (xorg) xdpyinfo xrandr;

##            urxvt = rxvt_unicode-with-plugins;

            sway-config =
              import ./src/sway-config.nix { inherit pkgs dim hostconfig alac;
                                             inherit (gui) i3stat;
                                             wallpaper = ./src/nixos3.jpg;
                                           };

##            i3status-rc =
##              let
##                src = nixpkgs.lib.strings.fileContents ./pkgs/i3status.nix;
##                replacements = {
##                  __alacritty_exe__  = "${pkgs.alacritty}/bin/alacritty";
##                  __byobu_exe__      = "${my-pkgs.byobu}/bin/byobu";
##                  __pactl_exe__      = "${pkgs.pulseaudio}/bin/pactl";
##                  __touchpad_exe__   = "${my-pkgs.touchpad}/bin/touchpad";
##                  __xbacklight_exe__ = "${pkgs.acpilight}/bin/xbacklight";
##                  __xmobar_exe__     = "${pkgs.xmobar}/bin/xmobar";
##                  __xmonad_exe__     = "${xmonad}/bin/xmonad";
##                  __xrandr_exe__     = "${pkgs.xorg.xrandr}/bin/xrandr";
##                  __xscreensaver_command_exe__ =
##                    "${pkgs.xscreensaver}/bin/xscreensaver-command";
##                };
##                repl_from = builtins.attrNames replacements;
##                repl_to   = map (x: replacements.${x}) repl_from;
##                repl_src = builtins.replaceStrings repl_from repl_to src;
##              in
##                pkgs.writeTextDir "share/xmonad-hs" repl_src;
          });
        }
    );
}

