{ home-manager, ... }:
{
  imports = [
    ../.
  ];
  dbConfig = {
    cli = true;
  };
}
