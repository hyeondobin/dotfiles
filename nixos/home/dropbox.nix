{config, lib, pkgs, ...}:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.dropbox = lib.mkEnableOption "Enable dropbox";
  };

  config = lib.mkIf cfg.dropbox {
    services.dropbox = {
      enable = true;
      path = "${config.home.homeDirectory}/data/dropbox";
      package = pkgs.dropbox;
    };
  };
}
