dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libgfortran.

sinclude(../libtool.m4)
dnl The lines below arrange for aclocal not to bring an installed
dnl libtool.m4 into aclocal.m4, while still arranging for automake to
dnl add a definition of LIBTOOL to Makefile.in.
ifelse(,,,[AC_SUBST(LIBTOOL)
AC_DEFUN([AM_PROG_LIBTOOL])
])



dnl
dnl Initialize the rest of the library configury.  At this point we have
dnl variables like $host.
dnl
dnl Substs:
dnl  libvtv_builddir     (absolute path)
dnl  libvtv_srcdir       (absolute path)
dnl  toplevel_builddir    (absolute path)
dnl  toplevel_srcdir      (absolute path)
dnl  with_cross_host
dnl  with_newlib
dnl  with_target_subdir
dnl plus
dnl  - the variables in LIBVTV_CHECK_HOST / configure.host
dnl  - default settings for all AM_CONDITIONAL test variables
dnl  - lots of tools, like CC and CXX
dnl
AC_DEFUN([LIBVTV_CONFIGURE], [

  # Use same top-level configure hooks in libgcc/libstdc++/libvtv.
  AC_ARG_ENABLE(vtable-verify,
  [  --enable-vtable-verify    Enable vtable verification feature ],
  [case "$enableval" in
   yes) enable_vtable_verify=yes ;;
   no)  enable_vtable_verify=no ;;
   *)   enable_vtable_verify=no;;
   esac],
  [enable_vtable_verify=no])
  AM_CONDITIONAL(ENABLE_VTABLE_VERIFY, test $enable_vtable_verify = yes)

  # These need to be absolute paths, yet at the same time need to
  # canonicalize only relative paths, because then amd will not unmount
  # drives. Thus the use of PWDCMD: set it to 'pawd' or 'amq -w' if using amd.
  libvtv_builddir=`${PWDCMD-pwd}`
  case $srcdir in
    [\\/$]* | ?:[\\/]*) libvtv_srcdir=${srcdir} ;;
    *) libvtv_srcdir=`cd "$srcdir" && ${PWDCMD-pwd} || echo "$srcdir"` ;;
  esac
  toplevel_builddir=${libvtv_builddir}/..
  toplevel_srcdir=${libvtv_srcdir}/..
  AC_SUBST(libvtv_builddir)
  AC_SUBST(libvtv_srcdir)
  AC_SUBST(toplevel_builddir)
  AC_SUBST(toplevel_srcdir)
])
