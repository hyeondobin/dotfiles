fish_add_path ~/.emacs.d/bin/
set -gx SSH_AUTH_SOCK "/home/dobin/.bitwarden-ssh-agent.sock"

source ~/.config/fish/conf.d/abbrs.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
    set fish_greeting "Mao Myao"
    bind -M insert \cp up-or-search
    bind -M insert \cn down-or-search
    bind -M insert " " expand-abbr or self-insert
    bind -M insert \cf forward-word
    bind -M insert \cy accept-autosuggestion
    # https://stackoverflow.com/questions/61520166/how-to-create-a-key-binding-that-inserts-text-in-the-fish-shell
    bind -M insert \en "commandline -i '&| nom'"

    fastfetch
end

zoxide init fish --cmd cd | source
