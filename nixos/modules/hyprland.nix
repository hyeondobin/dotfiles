{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  hyprpkgs = inputs.hyprland.packages."${system}";
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.hyprland = lib.mkEnableOption "Enable hyprland";
  };
  config = lib.mkIf cfg.hyprland {

    programs.hyprland = {
      enable = true;
      withUWSM = true;
      xwayland.enable = true;
      package = hyprpkgs.hyprland;
      portalPackage = hyprpkgs.xdg-desktop-portal-hyprland;
    };
  };
}
