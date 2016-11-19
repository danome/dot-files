# .bash_functions
# Ver 1.1 20161016 zot u1404

function shssh () {
    ssh-add -l
    env | grep SSH
    echo "export SSH_AGENT_PID SSH_AUTH_SOCK"
    echo "gnome-session: $(pidof gnome-session)"
    echo "DBUS_SESSION: ${DBUS_SESSION_BUS_ADDRESS}"
}


function settos () {
    export TOSREL="$1"
    export TOSROOT="$HOME/$TOSREL/tinyos-2.x"
    export TOSDIR=$TOSROOT/tos
    export CLASSPATH=".:$TOSROOT/tools/java"
    shtos
}


function setmspgcc () {
    pushd ~/.bin > /dev/null
    /bin/rm -f mspgcc
    ln -s $1 mspgcc
    ls -ld mspgcc
    popd > /dev/null
}

function set-d () {
    /bin/rm -f ~/.bin/mspgcc
}

function set-z () {
    setmspgcc /opt/msp430-z1
}

function set-4 () {
    setmspgcc /opt/msp430-4${1}
}

function set-46 () {
    setmspgcc /opt/msp430-46
}

function set-47 () {
    setmspgcc /opt/msp430-47-expr
}

function shtos () {
#    echo "OSIAN_DEV:    $OSIAN_PPPD_DEVICE"
#    echo "OSIAN_ROOT:   $OSIAN_ROOT"
    echo "MOTECOM:       $MOTECOM"
    echo "TOSROOT:       $TOSROOT"
    echo "TOSDIR:        $TOSDIR"
    echo "TOSMAKE_PATH:  $TOSMAKE_PATH"
    echo "CLASSPATH:     $CLASSPATH"
    echo "MM_ROOT:       $MM_ROOT"
    echo "G4_ROOT:       $G4_ROOT"
    echo "LSRDEV_ROOT:   $LSRDEV_ROOT"
    echo "MAKERULES:     $MAKERULES"
    echo ""
    echo "ROOT_DIR:      $TINYOS_ROOT_DIR"
    echo "ADDITIONAL:    $TINYOS_ROOT_DIR_ADDITIONAL"
    echo "NO_COLOR:      $TOSMAKE_NO_COLOR"
    echo "" 
    echo "PACKAGING_DIR: $PACKAGING_DIR"
    echo "POST_VER:      $POST_VER"
    echo "REPO_DIR:      $REPO_DIR"
    echo -e ""
    echo -e "toolchain:"
    pushd ~/.bin > /dev/null
    if [ -e mspgcc ]; then
	ls -ld mspgcc
    else
	echo -e "\tdefault"
    fi
    hash -r
    echo
    msp430-gcc --version | head -1
    arm-none-eabi-gcc --version | head -1
    echo
    popd > /dev/null
    pushd ~/mm > /dev/null
#    ls -ld t2_cur t2_mm
    ls -ld t2_cur
    popd > /dev/null
    echo
}

function tmp_osian () {
    if [[ "$1" == "yes" ]] ; then
	TOSMAKE_PATH="$OSIAN_ROOT/tinyos/support/make $MM_ROOT/support/make"
    else
	TOSMAKE_PATH="$MM_ROOT/support/make"
    fi
    echo "TOSMAKE_PATH: $TOSMAKE_PATH"
}

function tcur () {
    if [ -z "$1" ] ; then
	echo "tcur (set t2_cur) needs 1 argument."
	return
    fi
    pushd ~/mm > /dev/null
    if [ ! -d "$1" ] ; then
	echo "*** arg 1 should be a directory in ~/mm"
	echo "    $1 doesn't exist"
	return
    fi
    if [ ! -h t2_cur ] ; then
	echo "*** ~/mm/t2_cur isn't a symbolic link, something is wrong"
	return
    fi
    /bin/rm -rf t2_cur
    ln -s $1 t2_cur
    pwd
    ls -l t2_cur
    popd > /dev/null
}


function set-osian () {
    tmp_osian yes
    tcur osian
}


function set-msp430 () {
    tmp_osian
    tcur msp430
}


function t_w () {
    pushd ~/mm > /dev/null
    if [[ ! -h t2_cur || ! -h t2_mm ]] ; then
	echo "*** ~mm/t2_cur or ~mm/t2_mm isn't a symbolic link, something is wrong"
	return
    fi
    return
    remove -rf t2_cur t2_mm
    ln -s t2_w t2_cur
    ln -s mm_w t2_mm
    pwd
    ls -l t2_cur t2_mm
    popd > /dev/null
}


function t_tip () {
    pushd ~/mm > /dev/null
    if [[ ! -h t2_cur || ! -h t2_mm ]] ; then
	echo "*** ~mm/t2_cur or ~mm/t2_mm isn't a symbolic link, something is wrong"
	return
    fi
    return
    remove -rf t2_cur t2_mm
    ln -s t2_tip t2_cur
    ln -s mm_master t2_mm
    pwd
    ls -l t2_cur t2_mm
    popd > /dev/null
}


function tande {
    gnome-terminal --geometry=80x40+0+0 --title=Main &
    emacs --geometry=98x50+0+0 &
}


# function nail () {
#    iptables -A INPUT -s $1 -j DROP
#    iptables -A FORWARD -s $1 -j DROP
#}
