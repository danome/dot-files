file file
file /usr/gcc-arm-none-eabi-4_9-2015q3/bin/arm-none-eabi-gdb-py
h
find /usr/lib* -iname "*libpython2.7*"
which python
file /usr/bin/python
ll /usr/bin/python*
dpkg --print-architecture
dpkg --print-foreign-architecture
dpkg --print-foreign-architectures
which python
dpkg -S /usr/bin/python
ll /usr/bin/python
dpkg -S /usr/bin/python2.7
sudo apt-get install python2.7:i386
sudo apt-get install python:i386
sudo apt-get install python:i386 python-minimal:i386
sudo apt-get install python2.7:i386 python-minimal:i386
sudo apt-get install python:i386 python2.7:i386 python-minimal:i386
sudo
cd /usr
ll
echo $PATH
pwd
tar xf /home/danome/toolchain/gcc-arm-none-eabi-4_9-2015q3-20150921/pkg/gcc-arm-none-eabi-4_9-2016q4-20161111-linux.tar.bz2 
ll
ssh-add
emacs
bye
apt-get purge -V ".*:i386"
apt-get update
dpkg --remove-architecture i386
apt-get update
exit
JLinkGDBServer -device msp432p401r
arm-none-eabi-gdb --version
which arm-none-eabi-gdb
arm-none-eabi-gdb-py --version
arm-none-eabi-gdb-py
q
dpkg --get-selections python
dpkg --get-selections python:i386
dpkg --get-selections ".*":i386
dpkg --get-selections "*":i386
sudo
echo $PATH
pwd
cd mm/tinyos-main/tinyos-2.x/apps/tests/msp432/TestGPIO/
JLinkExe 
which Ozone
file /usr/bin/Ozone 
file /opt/SEGGER/ozone/2.22.3/Ozone
Ozone
JLinkExe 
h
JLinkExe 
JLinkGDBServer -device msp432p401r
JLinkExe 
perstat
JLinkExe 
pwd
ll
od -tx4 build/exp_msp432/main.bin 
JLinkExe 
pwd
pd ~/mm/tp-master/tinyos-2.x/
find . -iname "*peripheralinit*"
pwd
d
pd
pd ~/mm/tinyos-main/tinyos-2.x/tos/platforms/exp_msp432/
pwd
ll
pwd
ll
pd
[d
pd
bye
JLinkGDBServer -device msp432p401r
cd mm/tinyos-main/tinyos-2.x/apps/tests/msp432/TestGPIO/
pwd
pd ~/mm/tinyos-main/tinyos-2.x/
pd ~/mm/tp-master/tinyos-2.x/
p
pd ~/mm/tp-master/tinyos-2.x/
p
pd
lsusb
pwd
d
ssh-add
emacs
xterm
emacs
bye
JLinkGDBServer -device msp432p401r
lsusb
cd mm/tinyos-main/tinyos-2.x/apps/tests/msp432/TestGPIO/
echo $PATH
ll /usr/
ll /usr/gcc-arm-none-eabi-4_9-2016q4/bin
pwd
ps -gaxuw
JLinkGDBServer -device msp432p401r
cd mm/tinyos-main/tinyos-2.x/apps/tests/msp432/TestGPIO/
python
pwd
dirs
pd ~/mm/tinyos-main/tinyos-2.x/
pd
pd ~/mm/tinyos-main/tinyos-2.x/tos/chips/msp432/driverlib/
grep -nHR DEVICE_PG1_1
pwd
d
pd
pd /opt/ti-arm
ll
find . -iname "*msp.h*"
d
p
pd
p
pd
pd tos/chips/msp432/include/
ll
ln -s msp432.h msp.h
ll
d
p
pd
pd tos/platforms/exp_msp432/
pd
p
pd
lsusb
pwd
cp ~/mm/tp-master/tinyos-2.x/tos/interfaces/Platform.nc ~/mm/tinyos-main/tinyos-2.x/tos/interfaces/
pwd
pd ~/mm/tp-master/tinyos-2.x/
find . -iname "*stack*"
pwd
pd
pd ~/mm/tinyos-main/tinyos-2.x/
pwd
pd /opt/ti-arm
pwd
p
pd
p
pd
/bin
pwd
ps -gaxuw
JLinkGDBServer -device msp432p401r
pwd
cd mm/tinyos-main/tinyos-2.x/
ll
pwd
pd apps/tests/msp432/TestGPIO/
pd
find . -iname "*cpu_stack*"
pd ~/mm/tp-master/tinyos-2.x/
find . -iname "*cpu_stack*"
p
git s
d
pd
d
pd
pd ~/mm/tp-master/tinyos-2.x/
pd
p
pd
ssh-add
emacs
bye
ll
ll /mnt/Dump
mkdir /mnt/sdd3
mkdir /mnt/sdd1
ll /mnt
mount -h
mount -l
mount /dev/sdd1 /mnt/sdd1
mount /dev/sdd2 /mnt/sdd2
mount /dev/sdd3 /mnt/sdd3
df -h
ll /mnt/sdd3
cd /mnt/sdd2
ll
cd u1404
ll
du -sh Docs/
ll Docs
ll
rm *.dump
df -h .
df -h
dump -h
dump -0uf zot_home_20161116_0.dump /dev/sdb10
umount /dev/sdd2
pwd
ll
ll -h
dump -0uf zot_home_20161116_0.dump /dev/sdb10
dump -0uf zot_root_20161116_0.dump /dev/sda2
df -h
pwd
ll -h
bye
umount /dev/sdd2
umount /dev/sdd3
ll /mnt/sdd1
umount /mnt/sdd1
bye
tail -f /var/log/syslog
gdisk -l /dev/sdd
ll /mnt
mkdir /mnt/sdd3
sudo
cd
umount /dev/sdd2
sudo
eject /dev/sdd
df -h
exit
pwd
cd
cd mm/tinyos-main/tinyos-2.x/
pd apps/tests/msp432/TestGPIO/
pwd
pd 
sudo eject /dev/sdd
sudo
ssh-add
emacs
bye
cd mm/tinyos-main/tinyos-2.x/
ll
pd tos/chips/msp432
ll
cd driverlib/
ll
mv rom.h.~2~ rom.h
git s .
diff rom.h.~1~ rom.h
pwd
pd
emacs
ll
pwd
cd mm/tinyos-main/tinyos-2.x/
pd apps/tests/msp432/TestGPIO/
arm-none-eabi-gcc -specs=/usr/lib/ncc/tdspecs -I/usr/lib/ncc -o build/exp_msp432/main.exe -Os -ggdb -v -D _BUILD=504 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/clock -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/leds -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/timer -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/usci -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/cortex -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/include -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/msp432p401/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib -I /home/cire/mm/t2_cur/tinyos-2.x/tos/lib/timer -nostartfiles -D PLATFORM_EXP_MSP432 -Wall -Wshadow -D __MSP432P401R__ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/system -I /home/cire/mm/t2_cur/tinyos-2.x/tos/types -I /home/cire/mm/t2_cur/tinyos-2.x/tos/interfaces -D IDENT_APPNAME="TestGPIOAppC" -D IDENT_USERNAME="cire" -D IDENT_HOSTNAME="zot" -D IDENT_USERHASH=0x11dce1bdL -D IDENT_TIMESTAMP=0x582d6cf7L -D IDENT_UIDHASH=0xd4c38324L --specs=nosys.specs -mcpu=cortex-m4 -march=armv7e-m -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -fno-strict-aliasing -fshort-enums TestGPIOAppC.nc -lm -L/home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432 -T /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/other/msp432p401r.lds -Wl,-Map=build/exp_msp432/main.map -Wl,--gc-sections /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/startup.c /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib/cs.c /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib/wdt_a.c /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib/sysctl.c /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib/interrupt.c /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib/cpu.c -DNESC=136
arm-none-eabi-gcc -specs=/usr/lib/ncc/tdspecs -I/usr/lib/ncc -E -o build/exp_msp432/main.out -Os -ggdb -D _BUILD=504 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/clock -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/leds -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/timer -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/usci -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/cortex -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/include -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/msp432p401/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib -I /home/cire/mm/t2_cur/tinyos-2.x/tos/lib/timer -nostartfiles -D PLATFORM_EXP_MSP432 -Wall -Wshadow -D __MSP432P401R__ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/system -I /home/cire/mm/t2_cur/tinyos-2.x/tos/types -I /home/cire/mm/t2_cur/tinyos-2.x/tos/interfaces -D IDENT_APPNAME="TestGPIOAppC" -D IDENT_USERNAME="cire" -D IDENT_HOSTNAME="zot" -D IDENT_USERHASH=0x11dce1bdL -D IDENT_TIMESTAMP=0x582d6cf7L -D IDENT_UIDHASH=0xd4c38324L --specs=nosys.specs -mcpu=cortex-m4 -march=armv7e-m -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -fno-strict-aliasing -fshort-enums /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/startup.c
arm-none-eabi-gcc -specs=/usr/lib/ncc/tdspecs -I/usr/lib/ncc -o build/exp_msp432/main.out -Os -ggdb -D _BUILD=504 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/clock -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/leds -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/timer -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/usci -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/cortex -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/include -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/msp432p401/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib -I /home/cire/mm/t2_cur/tinyos-2.x/tos/lib/timer -nostartfiles -D PLATFORM_EXP_MSP432 -Wall -Wshadow -D __MSP432P401R__ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/system -I /home/cire/mm/t2_cur/tinyos-2.x/tos/types -I /home/cire/mm/t2_cur/tinyos-2.x/tos/interfaces -D IDENT_APPNAME="TestGPIOAppC" -D IDENT_USERNAME="cire" -D IDENT_HOSTNAME="zot" -D IDENT_USERHASH=0x11dce1bdL -D IDENT_TIMESTAMP=0x582d6cf7L -D IDENT_UIDHASH=0xd4c38324L --specs=nosys.specs -mcpu=cortex-m4 -march=armv7e-m -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -fno-strict-aliasing -fshort-enums /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/startup.c
arm-none-eabi-gcc -specs=/usr/lib/ncc/tdspecs -I/usr/lib/ncc -E -o build/exp_msp432/main.out -Os -ggdb -D _BUILD=504 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/clock -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/leds -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/timer -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/usci -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/cortex -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/include -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/msp432p401/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib -I /home/cire/mm/t2_cur/tinyos-2.x/tos/lib/timer -nostartfiles -D PLATFORM_EXP_MSP432 -Wall -Wshadow -D __MSP432P401R__ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/system -I /home/cire/mm/t2_cur/tinyos-2.x/tos/types -I /home/cire/mm/t2_cur/tinyos-2.x/tos/interfaces -D IDENT_APPNAME="TestGPIOAppC" -D IDENT_USERNAME="cire" -D IDENT_HOSTNAME="zot" -D IDENT_USERHASH=0x11dce1bdL -D IDENT_TIMESTAMP=0x582d6cf7L -D IDENT_UIDHASH=0xd4c38324L --specs=nosys.specs -mcpu=cortex-m4 -march=armv7e-m -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -fno-strict-aliasing -fshort-enums /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/startup.c
arm-none-eabi-gcc -specs=/usr/lib/ncc/tdspecs -I/usr/lib/ncc -o build/exp_msp432/main.out -Os -ggdb -D _BUILD=504 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/clock -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/leds -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/timer -I /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/hardware/usci -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/cortex -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432 -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/include -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/msp432p401/pins -I /home/cire/mm/t2_cur/tinyos-2.x/tos/chips/msp432/driverlib -I /home/cire/mm/t2_cur/tinyos-2.x/tos/lib/timer -nostartfiles -D PLATFORM_EXP_MSP432 -Wall -Wshadow -D __MSP432P401R__ -I /home/cire/mm/t2_cur/tinyos-2.x/tos/system -I /home/cire/mm/t2_cur/tinyos-2.x/tos/types -I /home/cire/mm/t2_cur/tinyos-2.x/tos/interfaces -D IDENT_APPNAME="TestGPIOAppC" -D IDENT_USERNAME="cire" -D IDENT_HOSTNAME="zot" -D IDENT_USERHASH=0x11dce1bdL -D IDENT_TIMESTAMP=0x582d6cf7L -D IDENT_UIDHASH=0xd4c38324L --specs=nosys.specs -mcpu=cortex-m4 -march=armv7e-m -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -fno-strict-aliasing -fshort-enums /home/cire/mm/t2_cur/tinyos-2.x/tos/platforms/exp_msp432/startup.c
ll build/exp_msp432/
pwd
d
pd
git s
pwd
pd /opt/ti-arm/driverlib/MSP432P4xx/gcc
readelf -a msp432p4xx_driverlib.a 
pwd
ll
pwd
p
pd
pwd
git b
git fetch --all
git push cire
gitk --all&
git co msp432
git stash
git rebase master
git stash pop
pwd
p
git s
git co master
git s
diff -u tos/system/#RealMainP.nc\# tos/system/RealMainP.nc 
pwd
git s
git diff tos/chips/cortex/
git add -u tos/chips/cortex/
git s
git diff tos/system/
git b
git ci
git s
git diff tos/chips/
git add -u tos/chips/msp432/include/
git s
git ci
git add -u tos/chips/msp432/other/
git s
git ci
git s
git push
git co msp432
git stash list
git stash
git stash list
git rebase master
git stash pop
git co master
git push cire
git co msp432
ll
git s
pwd
git b
git co master
git s
git add tos/chips/msp430/Stack*
git add tos/chips/msp430/cpu_stack.h 
git add tos/interfaces/Stack.nc
git s
git b
git ci
git push
git push cire
git s
git co msp432
git stash list
git stash
git rebase master
git stash pop
git s
git b
pwd
pd tos/chips/msp432/
ll
diff -ru /opt/ti-arm/driverlib/MSP432P4xx driverlib
pwd
cd driverlib/
ll
pwd
cd ..
git s driverlib/
ll
mv driverlib/ driverlib_save
ll
pd /opt/ti-arm/driverlib/MSP432P4xx/
ll
cd ..
ll
tar cf - MSP432P4xx | (cd ~/mm/tinyos-main/tinyos-2.x/tos/chips/msp432; tar xf -)
p
ll
mv MSP432P4xx/ driverlib
cd driverlib
ll
pwd
ll
cd ..
ll
diff -ru driverlib driverlib_save
pwd
ll
pwd
ll
ll driverlib
git s
git add driverlib
git s
git b
git ci
diff -ru driverlib driverlib_save
cp driverlib_save/cs.c driverlib
git add -p 
git add -p .
pwd
git add -p driverlib
pwd
pd driverlib
git s .
git diff --cached
git mv debug.h msp432_dl_debug.h
cp ../driverlib_save/msp432_dl_debug.h .
git s .
git diff
git diff .
git add -u msp432_dl_debug.h 
git s .
git diff
git s
git s .
d
p
pd tos/chips/msp432
ll
diff -ru driverlib driverlib_save/
cp driverlib_save/00_README driverlib
cd driverlib
git s .
git ci
git b
pwd
diff -ru . ../driverlib_save/
ll gcc
git add gcc
git ci
git s .
ll gcc
git add gcc/msp432p4xx_driverlib.a 
pwd
remove -rf gcc
ll
git s .
diff -ru . ../driverlib_save/
pwd
cp ../driverlib_save/rom* .
diff -ru . ../driverlib_save/
git s.
git s .
git add cs.c
git diff --cached
git ci
git s
git add rom.h rom_map.h 
git ci
pwd
git s .
git add 00_README 
git ci
git push cire
pwd
git s
cd ..
ll
git s .
diff -ru driverlib driverlib_save/
remove -rf driverlib_save/
ll
pwd
p
git s
pwd
pd apps/tests/msp432/TestGPIO/
pd
pwd
cd ~/mm/tinyos-main/tinyos-2.x/
git s
git push cire
cd mm/tinyos-main/tinyos-2.x/
pd apps/tests/msp432/TestGPIO/
pd
ssh-add
emacs
JLinkGDBServer -device msp432p401r
cd mm/tinyos-main/tinyos-2.x/
pwd
pd apps/tests/msp432/TestGPIO/
pwd
si
pd
find . -iname "*moteclock*"
pwd
pd apps/Blink/
ll
ll build/
make exp_msp432
make sam3u_ek
pd
git s
stash list
git stash list
git stash
pd
make sam3u_ek
pwd
pd
p
pd
cd mm/tinyos-main/tinyos-2.x/
pd apps/tests/msp432/TestGPIO/
JLinkGDBServer -device msp432p401r
pwd
d
pd ~/mm/tinyos-main/tinyos-2.x/
pwd
pd
JLinkGDBServer -device msp432p401r
pd mm/tinyos-main/tinyos-2.x/
pd apps/tests/msp432/TestGPIO/
pwd
ps gaxuw | grep JLink
kill 15853
emacs
bye
apt-get update
apt-get upgrade -V
bye
