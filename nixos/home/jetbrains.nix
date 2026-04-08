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
    dbConfig.jetbrains = lib.mkEnableOption "Enable jetbrains";
  };
  config = lib.mkIf cfg.jetbrains {
    home.packages = [
      pkgs.jetbrains-toolbox
    ];
  };
}
