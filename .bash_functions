# .bash_functions
# ver 4.2 20190417 zot u1804
#   add name function for terminal names


# set the name of a gnome-terminal
function name () {
    echo -ne "\033]0;$1\007"
}


function e () {
    if [[ -v REM_HOST ]]; then
        emacs -name emacs-${REM_HOST}
    else
        emacs
    fi
}

function shssh () {
    ssh-add -l
    env | grep SSH
    echo "export SSH_AGENT_PID SSH_AUTH_SOCK"
    echo "gnome-session: $(pidof gnome-session)"
    echo "DBUS_SESSION: ${DBUS_SESSION_BUS_ADDRESS}"
}


