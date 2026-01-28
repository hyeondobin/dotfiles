{
  config,
  lib,
  config-vars,
  ...
}:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.display = lib.mkEnableOption "Enable display";
  };
  config = lib.mkIf cfg.display {
    # login with dm
    # services.xserver.enable = true;
    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      # theme = "catppuccin-macchiato";
    };

  };
}
