{config, lib, ...}:
let
  cfg = dbConfig.dropbox;
in
{
  options = {
    dbConfig.dropbox = lib.mkEnableOption "Enable dropbox";
  };

  config = lib.mkIf cfg.dropbox {
    services.dropbox = {
      enable = true;
      path = "${config.home.homeDirectory}/dropbox";
    };
  };
}
