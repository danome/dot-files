# dot-files

```
git clone https://github.com/danome/dot-files.git
SRC_DIR=./dot-files/
DST_DIR=~/
FILES=".bash_aliases .bash_functions .bash_login .bash_logout .bashrc .emacs.d \
.environment_bash .gdbinit .gitconfig .gitignore"
echo -e "\n*** dots from $SRC_DIR -> $DST_DIR ***"
(for i in $FILES; do echo $i; done) | rsync -aiuWr --files-from=- $SRC_DIR $DST_DIR
```
