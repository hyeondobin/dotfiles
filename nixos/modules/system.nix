{
  pkgs,
  username,
  inputs,
  config-vars,
  ...
}:
{
  users.users.${username} = {
    isNormalUser = true;
    description = username;
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };
  nix.settings.trusted-users = [ username ];
  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    substituters = [
      # instead of building locally, download cached binary
      "https://cache.nixos.org"
      "https://hyprland.cachix.org"
      "https://nix-community.cachix.org"
      "https://catppuccin.cachix.org"
    ];
    trusted-substituters = [ "https://hyprland.cachix.org" ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "catppuccin.cachix.org-1:noG/4HkbhJb+lUAdKrph6LaozJvAeEEZj4N732IysmU="
    ];
    builders-use-substitutes = true;
  };
  nix.registry.nxim.flake = inputs.nxim;
  # nix.gc = {
  #   automatic = lib.mkDefault true;
  #   dates = lib.mkDefault "weekly";
  #   options = lib.mkDefault "--delete-older-than 7d";
  # };

  # nix cli wrapper
  programs.nh = {
    enable = true;
    flake = "/home/hyeondobin/repo/dotfiles";
    clean = {
      enable = true;
      extraArgs = "--keep 3 --keep-since 4d";
    };
  };

  # time zone
  time.timeZone = "Asia/Seoul";
  time.hardwareClockInLocalTime = true;

  # Internationalisation
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ko_KR.UTF-8";
    LC_IDENTIFICATION = "ko_KR.UTF-8";
    LC_MEASUREMENT = "ko_KR.UTF-8";
    LC_MONETARY = "ko_KR.UTF-8";
    LC_NAME = "ko_KR.UTF-8";
    LC_NUMERIC = "ko_KR.UTF-8";
    LC_PAPER = "ko_KR.UTF-8";
    LC_TELEPHONE = "ko_KR.UTF-8";
    LC_TIME = "ko_KR.UTF-8";
  };

  # input method
  programs.git = {
    enable = true;
    config = {
      user.name = "hyeondobin";
      user.email = "dobinhyeon@gmail.com";
      init = {
        defaultBranch = "main";
      };
      core.longpaths = true;
      pull.rebase = true;
    };
  };

  # init fish with bash
  programs.bash = {
    interactiveShellInit = ''
      if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]] then
        shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
        exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
      fi
    '';
    enable = true;
  };

  # font
  fonts.packages = [
    pkgs.nerd-fonts.jetbrains-mono
    pkgs.nerd-fonts.d2coding
    pkgs.noto-fonts-cjk-sans
  ];

  environment = {
    variables = {
      # EDITOR = "nvim";
      SSH_AUTH_SOCK = "/home/${username}/.bitwarden-ssh-agent.sock";
      NEWT_COLORS = ''
        root=#FFFFFF,#000000 border=#FF00FF,#000000 window=#000000,#000000 shadow=#000000,#000000 title=#FF00FF,#000000 button=#000000,#FF00FF button_active=#000000,#FFFF00 actbutton=#FFFF00,#000000 compactbutton=#FFFF00,#000000 checkbox=#FF0000,#000000 entry=#00FF00,#000000 disentry=#000000,#000000 textbox=#00FF00,#000000 acttextbox=#FF00FF,#000000 label=#00FFFF,#000000 listbox=#00FF00,#000000 actlistbox=#FF00FF,#000000 sellistbox=#FF0000,#000000 actsellistbox=#000000,#FFFF00
      '';
    };
    systemPackages = with pkgs; [
      # inputs.nxim.packages.${stdenv.hostPlatform.system}.nxim
      bat
      btop
      wget
      curl

      wl-clipboard
      kitty
      ghostty

      zoxide
      lazygit

      vivaldi
      bitwarden-desktop

      (catppuccin-sddm.override {
        flavor = "macchiato";
        fontSize = "12";
      })
    ];
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
  };
  catppuccin = {
    enable = true;
    accent = "sapphire";
    flavor = "macchiato";
    cursors = {
      enable = true;
    };
  };
  nxim =
    let
      nixdExtras = {
        nixpkgs = ''(builtins.getFlake "path:${toString inputs.self.outPath}").inputs.nixpkgs {}'';
        nixos_options = ''(builtins.getFlake "path:${toString inputs.self.outPath}").nixosConfigurations.${config-vars.hostname}.options'';
        home_manager_options = ''(builtins.getFlake "path:${toString inputs.self.outPath}").homeConfigurations."${username}@${config-vars.hostname}".options'';
      };
    in
    {
      enable = true;
      packageNames = [
        "nxim"
        "regularCats"
      ];
      packageDefinitions.merge = {
        nxim =
          { ... }:
          {
            extra = {
              inherit nixdExtras;
            };
          };
        regularCats =
          { ... }:
          {
            extra = {
              inherit nixdExtras;
            };
          };
      };
    };

}
