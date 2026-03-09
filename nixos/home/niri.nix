{config, lib, ...}:
{
  options = {
    dbConfig.niri = lib.mkEnableOption "Enable Niri";
  };
  config = lib.mkIf config.dbConfig.niri {
    xdg.configFile = {
      niri = {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repo/dotfiles/Config/niri/.config/niri/config.kdl";
      };
    };
  };
}
