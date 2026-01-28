{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.dbConfig;
in
{
  options = {
    dbConfig.emacs = lib.mkEnableOption "Enable emacs";
  };
  config = lib.mkIf cfg.emacs {
    environment.systemPackages = [
      # (pkgs.emacsWithPackagesFromUsePackage {
      #   package = pkgs.emacs-pgtk;
      #   config = "${inputs.self}" + "/emacs/config.el";
      #   extraEmacsPackages = epkgs: [
      #   ];
      # })
      pkgs.emacs-pgtk
    ];
  };

}
