{
  description = "hyeondobin's Nix OS configuration";

  outputs =
    { self, ... }@inputs:
    let
      inherit (self) outputs;
      pkgs = import inputs.nixpkgs {
        inherit system overlays;
        # allow unfree packages
        config.allowUnfree = true;
      };
      overlays = [ inputs.rust-overlay.overlays.default ];
      system = "x86_64-linux";
      systems = [ "x86_64-linux" ];
      forAllSystems = inputs.nixpkgs.lib.getAttrs systems;
      username = "hyeondobin";
      configuration = config-vars: {
        nixosConfiguration = inputs.nixpkgs.lib.nixosSystem {
          inherit pkgs;
          specialArgs = {
            inherit
              self
              inputs
              outputs
              config-vars
              username
              ;
          };
          modules = [
            inputs.catppuccin.nixosModules.catppuccin
            inputs.nxim.nixosModules.default
            ./nixos/hosts/${config-vars.hostname}
          ];
        };
        homeConfiguration =
          system:
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit
                self
                inputs
                outputs
                config-vars
                username
                ;
            };
            modules = [
              inputs.catppuccin.homeModules.catppuccin
              ./nixos/home/${config-vars.hostname}
            ];
          };
      };
      Panruyal = configuration {
        stateVersion = "25.05";
        hostname = "Panruyal";
        inherit username system;
        userDesc = "Laptop";
      };
      Winix = configuration {
        stateVersion = "25.05";
        hostname = "Winix";
        inherit username system;
        userDesc = "WSL from VanLioum";
      };
    in
    {
      # packages = forAllSystems (system: import ./pkgs inputs.nixpkgs.legacyPackages.${system});
      # formatter = forAllSystems (system: inputs.nixpkgs.legacyPackages.${system}.nixfmt);
      # overlays = import ./overlays { inherit inputs; };

      nixosConfigurations = {
        Panruyal = Panruyal.nixosConfiguration;
        Winix = Winix.nixosConfiguration;
      };
      homeConfigurations = {
        "hyeondobin@Panruyal" = Panruyal.homeConfiguration "x86_64-linux";
        "hyeondobin@Winix" = Winix.homeConfiguration "x86_64-linux";
      };
    };

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nxim = {
      url = "github:hyeondobin/nxim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
    };

    ghostty = {
      url = "github:ghostty-org/ghostty";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
