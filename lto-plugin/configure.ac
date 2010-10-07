AC_PREREQ(2.64)
AC_INIT([LTO plugin for ld], 0.1,,[lto-plugin])
GCC_TOPLEV_SUBDIRS
AM_INIT_AUTOMAKE([foreign no-dist])
AM_MAINTAINER_MODE
AC_PROG_CC
AC_SYS_LARGEFILE
AC_ARG_VAR(LIBELFLIBS,[How to link libelf])
AC_ARG_VAR(LIBELFINC,[How to find libelf include files])
AM_PROG_LIBTOOL
AC_SUBST(target_noncanonical)
. ${srcdir}/../gcc/config.gcc
case ${lto_binary_reader} in
  *coff*) LTO_FORMAT=coff ;;
  *elf*)  LTO_FORMAT=elf ;;
  *) AC_MSG_ERROR([LTO plugin is not supported on this target.]) ;;
esac

AC_SUBST(LTO_FORMAT)
AC_TYPE_UINT64_T
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
