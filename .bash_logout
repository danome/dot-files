# ~/.bash_logout
# Ver 4.0 20161125 zot u1404 + vm
# echo "*** .bash_logout ***"

if [ -e /vagrant ] ; then
  export VM=true;
fi

if [ -z $VM ] ; then
  echo "$USER logged out at `date`."
fi
if [ -x /usr/bin/sudo ]; then
    sudo -k
fi
if [ -z $VM ] ; then
  sleep 1
fi
