{ ... }:
{
  programs.git = {
    enable = true;
    config = {
      user.name = "hyeondobin";
      user.email = "dobinhyeon@gmail.com";
      init = {
        defaultBranch = "main";
      };
    };
  };
}
