{
  description = "nix configuration for wayland things";

  inputs = {
    nixpkgs.url     = github:NixOS/nixpkgs/938aa157; # nixos-24.05 2024-06-20
    flake-utils.url = github:numtide/flake-utils/c0e246b9;
    myPkgs          = {
      url    = github:sixears/nix-pkgs/r0.0.11.0;
#      url    = path:/home/martyn/nix/pkgs/;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    base-scripts-pkgs.url = path:/home/martyn/nix/base-scripts;
    gui-pkgs.url          = path:/home/martyn/nix/gui;
    hpkgs1          = {
      url    = github:sixears/hpkgs1/r0.0.24.0;
#      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    bashHeader      = {
      url    = github:sixears/bash-header/r0.0.3.0;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
  };

  outputs = { self, nixpkgs, flake-utils, base-scripts-pkgs, bashHeader
            , gui-pkgs, myPkgs, hpkgs1 }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs         = nixpkgs.legacyPackages.${system};
        my-pkgs      = myPkgs.packages.${system};
        my-settings  = myPkgs.settings.${system};
        base-scripts = base-scripts-pkgs.packages.${system};
        gui          = gui-pkgs.packages.${system};
        hpkgs        = hpkgs1.packages.${system};
        hlib         = hpkgs1.lib.${system};
        mkHBin       = hlib.mkHBin;
        bash-header  = bashHeader.packages.${system}.bash-header;

        vlc-lockfile       = my-settings.vlc-lockfile;
        gammastep-lockfile = "/run/user/1000/gammastep.pid";

        dim = import ./src/dim.nix
          { inherit pkgs vlc-lockfile gammastep-lockfile;
            inherit (my-pkgs) flock-pid-run; };

        sway-lock = import ./src/sway-lock.nix
          { inherit pkgs vlc-lockfile gammastep-lockfile;
            inherit (my-pkgs) flock-pid-run; };

        sway-power-on =
          import ./src/sway-power-on.nix { inherit pkgs;
                                           inherit (my-pkgs) pidkill; };

        # my personal wrapper around alacritty/byobu/tmux with a wofi selector
        alac = import ./src/alac.nix { inherit pkgs; };

        # multi-monitor setup
        hostconfig = import ./src/hostconfig.nix { inherit pkgs; };

        sway-show-bindings =
          (mkHBin "sway-show-bindings" ./src/sway-show-bindings.hs {
            libs = p: with p; with hlib.hpkgs; [
              base base1 hgettext parsers text-printer trifecta
            ];
          }).pkg;

        sway-float    = import ./src/sway-float.nix { inherit pkgs; };
        sway-sock     = import ./src/sway-sock.nix  { inherit pkgs; };
        sway-binds    = import ./src/sway-binds.nix
                          { inherit pkgs sway-sock sway-show-bindings; };
        sway-bindings = import ./src/sway-bindings.nix
                          { inherit pkgs sway-float sway-binds; };

        grime =
          let src = import ./src/grime.nix { inherit pkgs bash-header; };
          in  pkgs.writers.writeBashBin "grime" src;

        xkeyboard =
          let
            build = ''
              ${pkgs.coreutils}/bin/mkdir -p $out/share
              ${pkgs.coreutils}/bin/cp -av $src/ $out/share/xkeyboard/
            '';
          in
          derivation {
            inherit system;
            name = "xkeyboard";
            src  = ./src/xkeyboard;

  #          installPhase = ''
  #            mkdir -p $out
  #          '';

            inherit (pkgs) coreutils;
            builder = "${pkgs.bash}/bin/bash";
  #          args  = [ ./src/xkeyboard/builder.sh ];
  #          args  = [ "-c" "exit 99" ];
            args  = [ "-c" build ];
          };

        sway-config =
          import ./src/sway-config.nix
            { inherit pkgs dim hostconfig alac gammastep-lockfile sway-lock
                      sway-bindings sway-power-on grime;
              inherit (gui)          i3stat;
              inherit (my-pkgs)      flock-pid-run swap-summary cpu-temperature
                                     pa-mic-toggle;
              inherit (my-settings)  swap-summary-fifo cpu-temp-fifo;
              inherit (base-scripts) paths;
              wallpaper      = ./src/nixos1.jpg;
              lock-wallpaper = ./src/nixos3.jpg;
              wofi-config    = ./src/wofi.rc;
              xkb            =
                "${xkeyboard}/share/xkeyboard/keyboard.lenovo-thinkpad-x1-carbon-gen12.xkb";
#                ./src/keyboard.dvorak.xkb;
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

            ## FIXME
            inherit xkeyboard sway-config;
            xkb-file = import ./src/xkb-file.nix { inherit pkgs; };

            inherit sway-float sway-sock sway-binds sway-bindings
                    sway-show-bindings;
          });
        }
    );
}
