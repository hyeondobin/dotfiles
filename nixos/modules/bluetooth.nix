{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.bluetooth = lib.mkEnableOption "Enable bluetooth related options";
  };
  config = lib.mkIf cfg.bluetooth {
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
}
