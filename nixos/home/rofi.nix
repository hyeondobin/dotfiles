{ config, lib, ... }:
let
  inherit (lib) mkIf;
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.rofi = lib.mkEnableOption "Enable rofi";

  };
  config = mkIf cfg.rofi {
    programs.rofi = {
      enable = true;
    };
  };
}
