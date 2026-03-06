{ config, lib, ... }:
{
  options = {
    dbConfig.internet = lib.mkEnableOption "Enable internet";
  };
  config = lib.mkIf config.dbConfig.internet {
    networking.networkmanager.enable = true;
  };
}
