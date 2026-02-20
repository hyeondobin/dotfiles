if test -n "$WSLENV"
    source ~/.config/fish/conf.d/sshAgentBridge.fish
    echo $WSLENV
else
    if uname -v | grep Nix -q 
        echo "Skip exporting SSU_AUTH_SOCK on nixos"
    else
        set -gx SSH_AUTH_SOCK "/home/hyeondobin/.bitwarden-ssh-agent.sock"
    end
end
source ~/.config/fish/conf.d/abbrs.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
    set fish_greeting "Mao Myao"
    bind \cp up-or-search
    bind \cn down-or-search
    bind " " expand-abbr or self-insert
    bind " " expand-abbr or self-insert # expard-abbr with space by default 
    bind \cf forward-word
    bind \cy accept-autosuggestion
    # https://stackoverflow.com/questions/61520166/how-to-create-a-key-binding-that-inserts-text-in-the-fish-shell
    bind \en "commandline -i '&| nom'"

    if not set -q WSLENV
        fastfetch
    else
        echo "No fastfetch in WSL"
        echo "No fastfetch in WSL"
        echo "No fastfetch in WSL"
    end
end

zoxide init fish --cmd cd | source

# uv
fish_add_path "/home/dobin/.local/bin"
