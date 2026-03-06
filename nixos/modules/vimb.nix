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
    dbConfig.vimb = lib.mkEnableOption "Enable vimb";
  };
  config = lib.mkIf cfg.vimb {
    environment.systemPackages = [
      pkgs.vimb
    ];
  };
}
