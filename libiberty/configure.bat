@echo off
if "%1" == "h8/300" goto h8300

echo Configuring libiberty for go32
copy Makefile.dos Makefile
echo #define NEED_sys_siglist 1 >> config.h
echo #define NEED_psignal 1 >> config.h
update alloca-normal.h alloca-conf.h
goto exit

:h8300
echo Configuring libiberty for H8/300
copy Makefile.dos Makefile

:exit
