{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.steam = lib.mkEnableOption "Enable steam";
  };
  config = lib.mkIf cfg.steam {
    programs.steam = {
      enable = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
      remotePlay.openFirewall = true;
    };
  };

}
