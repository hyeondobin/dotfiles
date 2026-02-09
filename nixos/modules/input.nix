{
  config,
  lib,
  pkgs,
  ...
}:
{
  options = {
    dbConfig.ime = lib.mkEnableOption "Enable IME";
    dbConfig.touch = lib.mkEnableOption "Enable touch input";
  };
  config = (
    lib.mkMerge [
      (lib.mkIf config.dbConfig.touch {
        # touchpad
        services.libinput.enable = true;
      })
      (lib.mkIf config.dbConfig.ime {
        i18n.inputMethod = {
          enable = true;
          type = "fcitx5";
          fcitx5 = {
            addons = with pkgs; [
              fcitx5-hangul
              fcitx5-gtk
            ];
            settings = {
              addons = {
                clipboard.globalSection.TriggerKey = " ";
              };
              inputMethod = {
                GroupOrder."0" = "Default";
                "Groups/0" = {
                  Name = "Default";
                  "Default Layout" = "us";
                  DefaultIM = "hangul";
                };
                "Groups/0/Items/0".Name = "keyboard-us";
                "Groups/0/Items/1".Name = "hangul";
              };
              globalOptions = {
                Hotkey = {
                  EnumerateWithTriggerKeys = true;
                  EnumerateSkipFirst = false;
                  ModifierOnlyKeyTimeout = "250";
                };
                "Hotkey/TriggerKeys"."0" = "Alt+Alt_R";
                "Hotkey/TriggerKeys"."1" = "Hangul";
                "Hotkey/DeactivateKeys"."0" = "Escape";
                Behavior = {
                  ShareInputState = false;
                  PreeditEnabledByDefault = false;
                  ShowInputMethodInformation = true;
                  showInputMethodInformationWhenFocusIn = true;
                  CompactInputMethodInformation = true;
                };
              };
            };
          };
        };
      })
    ]
  );
}
