{config, lib, pkgs, ...}:
let
  cfg = config.dbConfig;
in
  {
    options = {
      dbConfig.niri = lib.mkEnableOption "Enable niri";
    };
    config = lib.mkIf cfg.niri {
      security.polkit.enable = true;
      services.gnome.gnome-keyring.enable = true;
      security.pam.services.swaylock = {};
      programs.niri.enable = true;

      environment.systemPackages = with pkgs; [
        alacritty fuzzel swaylock mako swayidle
      ];
    };
}
