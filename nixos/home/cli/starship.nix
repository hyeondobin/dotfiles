{config, lib,pkgs, ...}:
let
  cfg = config.dbConfig;
in
  {
    options = {
      dbConfig.starship = lib.mkEnableOption "Enable hm starship";
    };

    config = lib.mkIf cfg.cli {
      home.packages = lib.mkIf ( !cfg.starship ) [pkgs.starship];
      xdg.configFile."starship.toml" = lib.mkIf (!cfg.starship) {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repo/dotfiles/starship.toml";
      };
      programs.starship = lib.mkIf cfg.starship {
        enable = true;
        enableFishIntegration = true;
        settings = # lib.mkIf cfg.starship
          {
          add_newline = true;
          palette = "catppuccin_mocha";
          line_break = {
            disabled = true;
          };
          cmd_duration = {
            min_time = 10000;
            format = "took [$duration]($style)";
          };
          character = {
            success_symbol = ''[[󰄛](green) ❯](peach)'';
            error_symbol = ''[[󰄛](red) ❯](peach)'';
            vimcmd_symbol = ''[󰄛 ❮](subtext1)'';        # For use with zsh-vi-mode
          };
          hostname = {
            ssh_only = true;
            format = ''[$hostname]($style)'';
            trim_at = "-";
            style = "yellow";
          };
          memory_usage = {
            format = ''$symbol[$ram ( | swap)]($style)'';
            threshold = 70;
            style = "bold dimmed white";
            disabled = false;
          };
          time = {
            time_format = "%T";
            format = "🕙 $time($style) ";
            style = "bright-white";
            disabled = false;
          };
          git_branch = {
            disabled = true;
          };
          git_metrics = {
            disabled = true;
          };
          git_status = {
            disabled = true;
          };
          git_commit = {
            disabled = true;
          };

          username = {
            format = "[$user]($style_user) in ";
            style_user = "blue";
          };

          custom = {
            jj = {
              description = "The current jj status";
              when = "jj --ignore-working-copy root";
              symbol = "🥋 ";
              command = ''
                jj log --revisions @ --no-graph --ignore-working-copy --color always --limit 1 --template '
  separate(" ",
    change_id.shortest(4),
    bookmarks,
    "|",
    concat(
      if(conflict, "💥"),
      if(divergent, "🚧"),
      if(hidden, "👻"),
      if(immutable, "🔒"),
    ),
    raw_escape_sequence("\x1b[1;32m") ++ if(empty, "(empty)"),
    raw_escape_sequence("\x1b[1;32m") ++ coalesce(
      truncate_end(29, description.first_line(), "…"),
      "(no description set)",
    ) ++ raw_escape_sequence("\x1b[0m"),
  )
  '
              '';
            };
            jjstate = {
              when = "jj --ignore-working-copy root";
              command = ''
                jj log -r@ -n1 --ignore-working-copy --no-graph -T "" --stat | tail -n1 | sd "(\d+) files? changed, (\d+) insertions?\(\+\), (\d+) deletions?\(-\)" ' $\{1\}m $\{2\}+ $\{3\}' | sd " 0." ""
              '';
            };
            git_status = {
              when = "! jj --ignore-working-copy root";
              command = "starship module git_status";
              style = "";
              description = "Only show git_status if not in a jj repo";
            };
            git_commit = {
              when = "! jj --ignore-working-copy root";
              command = "starship module git_commit";
              style = "";
              description = "Only show git_commit if not in a jj repo";
            };
            git_metrics = {
              when = "! jj --ignore-working-copy root";
              command = "starship module git_metrics";
              style = "";
              description = "Only show git_metrics if not in a jj repo";
            };
            git_branch = {
              when = "! jj --ignore-working-copy root";
              command = "starship module git_branch";
              style = "";
              description = "Only show git_branch if not in a jj repo";
            };
          };

          palettes = {
            catppuccin_latte = {
              rosewater = "#dc8a78";
              flamingo = "#dd7878";
              pink = "#ea76cb";
              mauve = "#8839ef";
              red = "#d20f39";
              maroon = "#e64553";
              peach = "#fe640b";
              yellow = "#df8e1d";
              green = "#40a02b";
              teal = "#179299";
              sky = "#04a5e5";
              sapphire = "#209fb5";
              blue = "#1e66f5";
              lavender = "#7287fd";
              text = "#4c4f69";
              subtext1 = "#5c5f77";
              subtext0 = "#6c6f85";
              overlay2 = "#7c7f93";
              overlay1 = "#8c8fa1";
              overlay0 = "#9ca0b0";
              surface2 = "#acb0be";
              surface1 = "#bcc0cc";
              surface0 = "#ccd0da";
              base = "#eff1f5";
              mantle = "#e6e9ef";
              crust = "#dce0e8";
            };
            catppuccin_frappe = {
              rosewater = "#f2d5cf";
              flamingo = "#eebebe";
              pink = "#f4b8e4";
              mauve = "#ca9ee6";
              red = "#e78284";
              maroon = "#ea999c";
              peach = "#ef9f76";
              yellow = "#e5c890";
              green = "#a6d189";
              teal = "#81c8be";
              sky = "#99d1db";
              sapphire = "#85c1dc";
              blue = "#8caaee";
              lavender = "#babbf1";
              text = "#c6d0f5";
              subtext1 = "#b5bfe2";
              subtext0 = "#a5adce";
              overlay2 = "#949cbb";
              overlay1 = "#838ba7";
              overlay0 = "#737994";
              surface2 = "#626880";
              surface1 = "#51576d";
              surface0 = "#414559";
              base = "#303446";
              mantle = "#292c3c";
              crust = "#232634";
            };
            catppuccin_macchiato = {
              rosewater = "#f4dbd6";
              flamingo = "#f0c6c6";
              pink = "#f5bde6";
              mauve = "#c6a0f6";
              red = "#ed8796";
              maroon = "#ee99a0";
              peach = "#f5a97f";
              yellow = "#eed49f";
              green = "#a6da95";
              teal = "#8bd5ca";
              sky = "#91d7e3";
              sapphire = "#7dc4e4";
              blue = "#8aadf4";
              lavender = "#b7bdf8";
              text = "#cad3f5";
              subtext1 = "#b8c0e0";
              subtext0 = "#a5adcb";
              overlay2 = "#939ab7";
              overlay1 = "#8087a2";
              overlay0 = "#6e738d";
              surface2 = "#5b6078";
              surface1 = "#494d64";
              surface0 = "#363a4f";
              base = "#24273a";
              mantle = "#1e2030";
              crust = "#181926";
            };
            catppuccin_mocha = {


              rosewater = "#f5e0dc";
              flamingo = "#f2cdcd";
              pink = "#f5c2e7";
              mauve = "#cba6f7";
              red = "#f38ba8";
              maroon = "#eba0ac";
              peach = "#fab387";
              yellow = "#f9e2af";
              green = "#a6e3a1";
              teal = "#94e2d5";
              sky = "#89dceb";
              sapphire = "#74c7ec";
              blue = "#89b4fa";
              lavender = "#b4befe";
              text = "#cdd6f4";
              subtext1 = "#bac2de";
              subtext0 = "#a6adc8";
              overlay2 = "#9399b2";
              overlay1 = "#7f849c";
              overlay0 = "#6c7086";
              surface2 = "#585b70";
              surface1 = "#45475a";
              surface0 = "#313244";
              base = "#1e1e2e";
              mantle = "#181825";
              crust = "#11111b";
            };
          };
        };
      };
    };
  }
