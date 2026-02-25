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
    emacs = true;
    hyprland = true;
    rofi = true;
    telegram = true;
    waybar = true;
  };
}
