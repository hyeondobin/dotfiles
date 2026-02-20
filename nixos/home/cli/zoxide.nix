{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  config = lib.mkIf cfg.cli {
    programs.zoxide = {
      enable = true;
      enableFishIntegration = true;
      options = [ "--cmd cd" ];
    };
  };
}
