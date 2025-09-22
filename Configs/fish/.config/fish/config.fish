fish_add_path ~/.emacs.d/bin/
set -gx SSH_AUTH_SOCK "/home/dobin/.bitwarden-ssh-agent.sock"

source ~/.config/fish/conf.d/abbrs.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
    set fish_greeting "Mao Myao"
    bind \cp up-or-search
    bind \cn down-or-search
    bind " " expand-abbr or self-insert
    bind " " expand-abbr or self-insert
    bind \cf forward-word
    bind \cy accept-autosuggestion
    # https://stackoverflow.com/questions/61520166/how-to-create-a-key-binding-that-inserts-text-in-the-fish-shell
    bind \en "commandline -i '&| nom'"

    fastfetch
end

zoxide init fish --cmd cd | source
