{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  config = lib.mkIf cfg.cli {
    programs.tmux = {
      enable = true;
      clock24 = true;
      baseIndex = 1;
      keyMode = "vi";
      shortcut = "a";
      newSession = true;
      terminal = "tmux-256color";
      extraConfig = ''
        set -g mouse on
        set -g @catppuccin_flavor "macchiato"
        set -g @catppuccin_window_status_style "rounded"
      '';
    };
  };
}
