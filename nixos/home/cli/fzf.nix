{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  config = lib.mkIf cfg.cli {
    programs.fzf = {
      enable = true;
      enableFishIntegration = true;
    };
  };
}
