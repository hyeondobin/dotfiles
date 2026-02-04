{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.dbConfig;
in
{
  config = lib.mkIf cfg.cli {
    programs.fish = {
      enable = true;
      preferAbbrs = true;
      shellAbbrs = {
        # need a ' ' at the end of abbrs as we use space as expand-abbr os self-insert
        cat = "bat ";
        cp = "cp -iv ";
        e = "$EDITOR ";
	ee = "$EDITOR ~/repo/dotfiles/emacs/init.el ";
        en = "$EDITOR ~/.config/nxim/flake.nix ";
        ex = "$EDITOR ~/repo/dotfiles/flake.nix ";
        l = "eza ";
        lt = "eza --tree";
        lz = "lazyjj ";
        mkdir = "mkdir -pv ";
        mv = "mv -iv ";
        nhs = "nh home switch --ask -b home-manager-backup ";
        nhsf = "nh home switch -b hm-backup ";
        nos = "nh os switch --ask ";
        nosf = "nh os switch ";
        noux = "nh os switch --update-input nxim";
        qfw = "qmk flash";
	ree = "systemctl --user restart emacs.service";
        sofi = "source ~/.config/fish/config.fish";
        vi = "nvim ";
        z = "cd ";
      };
      interactiveShellInit = ''
        set fish_greeting "Mao Myao"
        fish_vi_key_bindings
        bind -M insert \cp up-or-search
        bind -M insert \cn down-or-search
        bind -M insert " " expand-abbr or self-insert
        fastfetch
      '';
      shellInitLast = ''
	COMPLETE=fish jj | source
      '';
    };
    home.packages = with pkgs; [
      eza
      fastfetch
      zoxide
    ];
  };
}
