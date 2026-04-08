{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.dbConfig;
  dbnemacs = (
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
      dbnemacs

      # language servers
      pkgs.nixd
      pkgs.clang-tools
      # pkgs.llvmPackages_latest.clang

      #deps
      pkgs.cmake
      pkgs.gnumake
    ];
    services.emacs = {
      enable = true;
      package = dbnemacs;
    };
    home.sessionVariables = {
      # EDITOR = "emacsclient -c -a emacs";
    };
    xdg.configFile = {
      emacs = {
        source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repo/dotfiles/emacs";
      };
    };
  };
}
