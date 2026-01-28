{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.cli = lib.mkEnableOption "Enable CLI Tools";
  };
  imports = [
    ./fzf.nix
    ./eza.nix
    ./fish.nix
    ./ghostty.nix
    ./zoxide.nix
    ./tmux.nix
  ];
  config = lib.mkIf cfg.cli {

    programs.direnv = {
      enable = true;
      # enableFishIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
