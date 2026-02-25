{
  config-vars,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules
  ];

  dbConfig = {
    bluetooth = true;
    display = true;
    hyprland = true;
    ime = true;
    internet = true;
    qmk = true;
    sounds = true;
    steam = true;
    touch = true;
    niri = true;
  };

  # boot related configs
  boot.loader = {
    timeout = 1;
    grub = {
      enable = true;
      device = "nodev";
      efiSupport = true;
      useOSProber = true;
      configurationLimit = 10;
      timeoutStyle = "menu";
      # theme = pkgs.catppuccin-grub;
    };
    efi.canTouchEfiVariables = true;
  };

  # Enable graphic driver
  hardware = {
    graphics.enable = true;
  };

  networking.hostName = config-vars.hostname;
  system.stateVersion = config-vars.stateVersion;
}
