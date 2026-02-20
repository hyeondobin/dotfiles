# set cmd ( setsid socat UNIX-LISTEN:$SSH_AUTH_SOCK,fork EXEC:"npiperelay.exe -ei -s //./pipe/openssh-ssh-agent",nofork & )
if set -q WSLENV
export SSH_AUTH_SOCK=$HOME/.ssh/agent.sock
ss -a | grep -q $SSH_AUTH_SOCK
if [ $status -ne 0 ]
    rm -f $SSH_AUTH_SOCK
cmd=( setsid socat UNIX-LISTEN:$SSH_AUTH_SOCK,fork EXEC:"npiperelay.exe -ei -s //./pipe/openssh-ssh-agent",nofork & ) $cmd >/dev/null 2>&1
end
end
