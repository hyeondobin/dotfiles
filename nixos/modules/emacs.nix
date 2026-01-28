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
    dbConfig.emacs = lib.mkEnableOption "Enable emacs";
  };
  config = lib.mkIf cfg.emacs {
    environment.systemPackages = [
      pkgs.emacs
    ];
  };

}
