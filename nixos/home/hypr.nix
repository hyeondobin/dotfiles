{ config, lib, inputs, pkgs, ...}:

let
  inherit (pkgs.stdenv.hostPlatform) system;
  hyprpkgs = inputs.hyprland.packages."${system}";

  hypr-special-toggle = pkgs.writeShellApplication {
    name = "hypr-special-toggle";
    runtimeInputs = [ pkgs.jq pkgs.procps pkgs.fish ];
    text = ''
      fish -c '
      set prog $argv[1]
      set ws $argv[2]
      
      # PID를 통해 프로그램의 class 를 추출
      set pids (pgrep -if $prog)
      set class = ""
      if test -n "$pids"
      set class (hypr)
    '';
  };
in

{
options = {
  dbConfig.hyprland = lib.mkEnableOption "Enable Hyprland";
};

config = lib.mkIf config.dbConfig.hyprland {

home.sessionVariables = {
  ELECTRON_OZONE_PLATFORM_HINT = "wayland";
};

xdg.configFile.hypr = {
  source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/repo/dotfiles/Configs/hyprland/.config/hypr";
};
};
}
