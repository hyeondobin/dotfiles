# Personal dotfiles 

## kanata

I use kanata to map keyboards 
kanata를 사용해서 키보드를 매핑한다. 
Using a layout based on miryoku layout. Only having 2 thumb keys, so I combined several layers.
miryoku 레이아웃을 기반으로 한 레이아웃을 사용. 엄지 키가 2개 밖에 없어서 몇 개의 레이어를 합쳐서 사용한다. 

* Layers
    - U_BASE
    - U_BASE_FAST
    - U_QWERTY (for korean input base)
    - U_QWERTY_FAST
    - U_NUM (number + symbol)
    - U_NAV
    - U_NAV_VI (not using for now maybe remove later)
    - U_BUTTON 
    - U_MOUSE (mouse + function)
    - U_MOUSE_ALT

## Languages

3 languages for input.

- English(colemak-dh)
- Korean(두벌식)
- Japanese(romaji)

Hotkeys for languages
- left alt+shift+1 = ENGILSH
- left alt+shift+2 = KOREAN
- left alt+shift+3 = JAPANESE

### Windows 

add languages KOREAN, ENGLISH(US) and JAPANESE
set hotkey as above
in Korean IME, use ralt to change input to Korean at first. 

### NixOS

Use fcitx as engine. Install fcitx5 and use config from this repo and it will be setup.
Use shift+space to change input method.
