#!/bin/bash
# copy dot-files and related configuration to home directory
SRC_DIR=$(pwd)/dot-files/
DST_DIR=~/
FILES=".bash_aliases .bash_functions .bash_login .bash_logout .bashrc .emacs.d \
.environment_bash .gdbinit .gitconfig .gitignore"
echo "*** link files from $SRC_DIR to $DST_DIR ***"
for i in $FILES
do
    echo $DST_DIR$i $SRC_DIR$i
    if test -s $DST_DIR$i;
    then
        rm -r --interactive=always $DST_DIR$i
        echo "$DST_DIR$i deleted"
    fi
    ln -s $SRC_DIR$i $DST_DIR$i
    echo "$SRC_DIR$i linked"
done
