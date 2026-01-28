# home-manager,
{ ... }:
{
  imports = [
    ../.
  ];

  dbConfig = {
    discord = true;
    rofi = true;
    cli = true;
    hyprland = true;
    waybar = true;
    telegram = true;
  };
}
