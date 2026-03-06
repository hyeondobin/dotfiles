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
    dbConfig.qmk = lib.mkEnableOption "Enable qmk";
  };
  config = lib.mkIf cfg.qmk {
    environment.systemPackages = [
      pkgs.qmk
    ];
    hardware.keyboard.qmk = {
      enable = true;
    };
    hardware.keyboard.zsa = {
      enable = false;
    };
  };

}
