{
  config,
  config-vars,
  username,
  pkgs,
  ...
}:
{
  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = config-vars.stateVersion;

    pointerCursor = {
      name = "catppuccin-mocha-sapphire-cursors";
      package = pkgs.catppuccin-cursors.mochaSapphire;
      hyprcursor.enable = true;
      size = 24;
      gtk.enable = true;
    };
  };

  xdg.configFile."uwsm/env".source =
    "${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh";

  xdg.configFile.jj = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repo/dotfiles/jj";
  };

  xdg.configFile.nxim = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repo/nxim";
  };

  programs.home-manager.enable = true;

  imports = [
    ./cli
    ./discord.nix
    ./dropbox.nix
    ./emacs.nix
    ./hypr.nix
    ./rofi.nix
    ./telegram.nix
    ./waybar
    ./jetbrains.nix
  ];

  catppuccin = {
    enable = true;
    accent = "sapphire";
    flavor = "mocha";
    cursors = {
      enable = false;
    };
  };
}
