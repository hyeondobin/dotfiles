{ config, lib, pkgs, ... }:
{
  options = {
    dbConfig.sounds = lib.mkEnableOption "Enable sounds";
  };
  config = lib.mkIf config.dbConfig.sounds {
    # Sound
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    environment.systemPackages = [
      pkgs.pwvucontrol
    ];
  };
}
