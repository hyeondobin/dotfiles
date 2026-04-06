{ config, lib, pkgs, ... }:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.discord = lib.mkEnableOption "Enable discord";
  };
  config = lib.mkIf cfg.discord {
    home.packages = [
      pkgs.vesktop
    ];
  };
}
