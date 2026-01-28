{ config, lib, ... }:
let
  cfg = config.dbConfig;
in
{
  config = lib.mkIf cfg.cli {
    programs.ghostty = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        font-feature = "-calt";
        mouse-hide-while-typing = true;
        scrollback-limit = 1000000;
        gtk-titlebar = true;
        keybind =
          let
            meh = "ctrl+alt+shift+";
            trg = "${meh}g>";
          in
          [
            "${trg}r=reload_config"
            # close split
            "${trg}s>c=close_surface"
            # creating splits
            "${trg}s>r=new_split:right"
            "${trg}s>d=new_split:down"

            # moving between splits
            "${meh}l=goto_split:right"
            "${meh}h=goto_split:left"
            "${meh}j=goto_split:down"
            "${meh}k=goto_split:up"

            # tab
            "${trg}t>n=new_tab"
            "${trg}t>c=close_tab"
            "${meh}n=next_tab"
            "${meh}p=previous_tab"

            # test
            "${trg}i=inspector:toggle"
          ];
      };
    };
  };
}
