{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jetbrains;
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
