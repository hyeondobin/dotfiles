{
  self,
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.dbConfig;
  emacs = (
    (pkgs.emacsPackagesFor pkgs.emacs-pgtk).emacsWithPackages (epkgs: [
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
    ])
  );
in
{
  options = {
    dbConfig.emacs = lib.mkEnableOption "Enable emacs";
  };
  config = lib.mkIf cfg.emacs {
    home.packages = [
      # (pkgs.emacsWithPackagesFromUsePackage {
      #   package = pkgs.emacs-pgtk;
      #   config = "${inputs.self}" + "/emacs/config.el";
      #   extraEmacsPackages = epkgs: [
      #   ];
      # })
      emacs

      # language servers
      pkgs.nixd
    ];
    services.emacs = {
      enable = true;
      package = emacs;
    };
    home.sessionVariables = {
      EDITOR = "emacsclient -c -a emacs";
    };
    xdg.configFile = {
      emacs = {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repo/dotfiles/emacs";
      };
    };
  };
}
