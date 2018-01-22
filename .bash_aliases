# .bash_aliases
# Ver 3.2 20171224 zot u1404
# set up aliases for various machines

# external hosts
alias	tinyprod='ssh -AY -p 23456 tinyprod.net'

alias	a='alias'
alias	bye='exit'
alias	d='dirs'
alias	deep='echo $SHLVL shells deep'
alias	dircmp='/usr/5bin/dircmp'
alias	fndwr="gfind . \( -pm 200 -p 200 -o -pm 20 -p 20 -o -pm 2 -p 2 \) ! -name \*.o ! -name \*.a ! -name \*~ ! -name \*.sh ! -type d ! -name \*.cmd ! -name \*strip ! -name \*.symbols"
alias	h='history'
alias	h10='history 10'
alias	h20='history 20'
alias	h30='history 30'
alias	key='shift; ssh-add $@'
alias	ll='ls -lF'
alias	lsf='ls -FC'
alias	p='popd'
alias	pd='pushd'
alias	pd1='pushd +1'
alias	pd2='pushd +2'
alias	pd3='pushd +3'
alias	pd4='pushd +4'
alias	psall='ps -gaxu'

if [ "z$OSNAME" = "zDarwin" ] ; then
    alias	psh='ps -ajx | grep ssh'
else
    # Linux, ... , what else?
    alias	psh='ps -Af | grep ssh'
fi

alias	psme='ps -gxu'
alias	remove='/bin/rm'
alias	rm='rm -i'
alias	setlen='eval `resize`'

# turn off touch pad, see xinput
alias   tpon='xinput --enable 14'
alias   tpoff='xinput --disable 14'

alias	xdb='od -tx1z -A x'
alias	xdw='od -tx4z -A x'
alias	yow='exit'
