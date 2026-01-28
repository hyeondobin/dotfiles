# home-manager,
{ ... }:
{
  imports = [
    ../.
  ];

  dbConfig = {
    cli = true;
    discord = true;
    hyprland = true;
    rofi = true;
    telegram = true;
    waybar = true;
  };
}
