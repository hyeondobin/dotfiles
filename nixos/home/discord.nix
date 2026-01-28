{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.discord = lib.mkEnableOption "Enable discord";
  };
  config = lib.mkIf cfg.discord {
    programs.discord = {
      enable = true;
    };
  };
}
