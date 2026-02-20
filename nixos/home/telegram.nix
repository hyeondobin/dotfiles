{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.telegram = lib.mkEnableOption "Enable discord";
  };
  config = lib.mkIf cfg.telegram {
    home.packages = [
      pkgs.telegram-desktop
    ];
  };
}
