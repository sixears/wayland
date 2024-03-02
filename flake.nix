{
  description = "nix configuration for wayland things";

  inputs = {
    nixpkgs.url     = github:NixOS/nixpkgs/354184a83; # master 2023-12-13
    flake-utils.url = github:numtide/flake-utils/c0e246b9;
    myPkgs          = {
      url    = github:sixears/nix-pkgs/r0.0.10.2;
#      url    = path:/home/martyn/nix/pkgs/;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    base-scripts-pkgs.url = path:/home/martyn/nix/base-scripts;
    gui-pkgs.url          = path:/home/martyn/nix/gui;
  };

  outputs = { self, nixpkgs, flake-utils, base-scripts-pkgs, gui-pkgs, myPkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs                  = nixpkgs.legacyPackages.${system};
        my-pkgs               = myPkgs.packages.${system};
        my-settings           = myPkgs.settings.${system};
        base-scripts          = base-scripts-pkgs.packages.${system};
        gui                   = gui-pkgs.packages.${system};

        vlc-lockfile       = my-settings.vlc-lockfile;
        gammastep-lockfile = "/run/user/1000/gammastep.pid";
        # flock-pid-run = my-pkgs.flock-pid-run;
        dim = import ./src/dim.nix
          { inherit pkgs vlc-lockfile gammastep-lockfile;
            inherit (my-pkgs) flock-pid-run; };

        sway-power-on =
          import ./src/sway-power-on.nix { inherit pkgs;
                                           inherit (my-pkgs) pidkill; };

        # my personal wrapper around alacritty/byobu/tmux with a wofi selector
        alac = import ./src/alac.nix { inherit pkgs; };

        # multi-monitor setup
        hostconfig = import ./src/hostconfig.nix { inherit pkgs; };

        sway-config =
          import ./src/sway-config.nix
            { inherit pkgs dim hostconfig alac gammastep-lockfile sway-power-on;
              inherit (gui)          i3stat;
              inherit (my-pkgs)      flock-pid-run swap-summary cpu-temperature;
              inherit (my-settings)  swap-summary-fifo cpu-temp-fifo;
              inherit (base-scripts) paths;
              wallpaper      = ./src/nixos1.jpg;
              lock-wallpaper = ./src/nixos3.jpg;
              xkb            = ./src/keyboard.xkb;
              wofi-config    = ./src/wofi.rc;
            };

      in
        {
          packages = flake-utils.lib.flattenTree (with pkgs; rec {
            inherit (my-pkgs) swap-summary;
            # wdisplays is arandr for wayland; wev is xev for wayland
            inherit wdisplays wev;
            # wofi is a gui chooser
            inherit wofi;

            # clipboard management;
            inherit wl-clipboard; # wl-copy & wl-paste
            inherit wl-clipboard-x11; # xclip & xsel

            inherit alac hostconfig;

            sway-rc  = import ./src/sway-rc.nix  { inherit pkgs sway-config;
                                                   inherit (my-pkgs) replace;
                                                 };

            xkb-file = import ./src/xkb-file.nix { inherit pkgs; };

          });
        }
    );
}
