function fish_user_key_bindings
    fish_vi_key_bindings
    bind -M insert -m default \cc force-repaint
    bind -M insert \el forward-word
    bind -M insert \cu accept-autosuggestion
    bind --preset \ev true
    bind -M insert \ev true
    bind -M visual \ev true
    bind -M normal \ev true
end
