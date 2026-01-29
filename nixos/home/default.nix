{
  config,
  config-vars,
  username,
  ...
}:
{
  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = config-vars.stateVersion;

    pointerCursor = {
      hyprcursor.enable = true;
      size = 24;
    };
  };

  xdg.configFile."uwsm/env".source =
    "${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh";

  programs.home-manager.enable = true;

  imports = [
    ./cli
    ./discord.nix
    ./emacs.nix
    ./hypr.nix
    ./rofi.nix
    ./telegram.nix
    ./waybar
  ];

  catppuccin = {
    enable = true;
    accent = "sapphire";
    flavor = "macchiato";
    cursors = {
      enable = true;
    };
  };
}
