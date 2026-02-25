# home-manager,
{ ... }:
{
  imports = [
    ../.
  ];

  dbConfig = {
    cli = true;
    starship = false;
    discord = true;
    emacs = true;
    hyprland = true;
    rofi = true;
    telegram = true;
    waybar = true;
  };
}
