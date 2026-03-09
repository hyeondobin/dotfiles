# home-manager,
{ ... }:
{
  imports = [
    ../.
  ];

  dbConfig = {
    cli = true;
    starship = true;
    discord = true;
    dropbox = true;
    emacs = false;
    hyprland = true;
    rofi = true;
    telegram = true;
    waybar = true;
  };
}
