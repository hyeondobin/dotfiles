{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  config = lib.mkIf cfg.cli {
    programs.eza = {
      enable = true;
      colors = "always";
      enableFishIntegration = true;
      icons = "always";
      git = true;
      extraOptions = [
        "--long"
        "--all"
        "--header"
      ];
    };
  };
}
