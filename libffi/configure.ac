dnl Process this with autoconf to create configure

AC_PREREQ(2.68)

AC_INIT([libffi], [3.4.2], [http://github.com/libffi/libffi/issues])
AC_CONFIG_HEADERS([fficonfig.h])

AM_ENABLE_MULTILIB(, ..)

AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}

AM_INIT_AUTOMAKE([no-dist])

# We would like our source tree to be readonly. However when releases or
# pre-releases are generated, the flex/bison generated files as well as the
# various formats of manuals need to be included along with the rest of the
# sources.  Therefore we have --enable-generated-files-in-srcdir to do
# just that.
AC_MSG_CHECKING(generated-files-in-srcdir)
AC_ARG_ENABLE(generated-files-in-srcdir,
AS_HELP_STRING([--enable-generated-files-in-srcdir],
 [put copies of generated files in source dir intended for creating source tarballs for users without texinfo bison or flex]),
[case "$enableval" in
 yes) enable_generated_files_in_srcdir=yes ;;
 no)  enable_generated_files_in_srcdir=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[enable_generated_files_in_srcdir=no])
AC_MSG_RESULT($enable_generated_files_in_srcdir)
AM_CONDITIONAL(GENINSRC, test "$enable_generated_files_in_srcdir" = yes)

# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.
# Also save and restore CFLAGS, since AC_PROG_CC will come up with
# defaults of its own if none are provided.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
save_CFLAGS=$CFLAGS
AC_PROG_CC
AC_PROG_CXX
CFLAGS=$save_CFLAGS
m4_undefine([_AC_ARG_VAR_PRECIOUS])
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

# Add CET specific flags if CET is enabled
GCC_CET_FLAGS(CET_FLAGS)
AC_SUBST(CET_FLAGS)

AM_PROG_AS
AM_PROG_CC_C_O
AC_PROG_LIBTOOL
AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])

AC_CHECK_TOOL(READELF, readelf)

# Test for 64-bit build.
AC_CHECK_SIZEOF([size_t])

AC_SUBST(SYSROOT_CFLAGS_FOR_TARGET)

cat > local.exp <<EOF
set SYSROOT_CFLAGS_FOR_TARGET "$SYSROOT_CFLAGS_FOR_TARGET"
set compiler_vendor "$ax_cv_c_compiler_vendor"
EOF

AM_MAINTAINER_MODE

AC_CHECK_HEADERS(sys/memfd.h)
AC_CHECK_FUNCS([memfd_create])

AC_CHECK_HEADERS(sys/mman.h)
AC_CHECK_FUNCS([mmap mkostemp mkstemp])
AC_FUNC_MMAP_BLACKLIST

dnl The -no-testsuite modules omit the test subdir.
AM_CONDITIONAL(TESTSUBDIR, test -d $srcdir/testsuite)

TARGETDIR="unknown"
HAVE_LONG_DOUBLE_VARIANT=0

. ${srcdir}/configure.host

if test -n "${UNSUPPORTED}"; then
  AC_MSG_ERROR(["libffi has not been ported to $host."])
fi

AC_SUBST(AM_RUNTESTFLAGS)
AC_SUBST(AM_LTLDFLAGS)

AC_HEADER_STDC
AC_CHECK_FUNCS(memcpy)
AC_FUNC_ALLOCA

AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)

# Also AC_SUBST this variable for ffi.h.
if test -z "$HAVE_LONG_DOUBLE"; then
  HAVE_LONG_DOUBLE=0
  if test $ac_cv_sizeof_long_double != 0; then
    if test $HAVE_LONG_DOUBLE_VARIANT != 0; then
      AC_DEFINE(HAVE_LONG_DOUBLE_VARIANT, 1, [Define if you support more than one size of the long double type])
      HAVE_LONG_DOUBLE=1
    else
      if test $ac_cv_sizeof_double != $ac_cv_sizeof_long_double; then
        HAVE_LONG_DOUBLE=1
        AC_DEFINE(HAVE_LONG_DOUBLE, 1, [Define if you have the long double type and it is bigger than a double])
      fi
    fi
  fi
fi
AC_SUBST(HAVE_LONG_DOUBLE)
AC_SUBST(HAVE_LONG_DOUBLE_VARIANT)

AC_C_BIGENDIAN

GCC_AS_CFI_PSEUDO_OP

case "$TARGET" in
  SPARC)
    AC_CACHE_CHECK([assembler and linker support unaligned pc related relocs],
	libffi_cv_as_sparc_ua_pcrel, [
	save_CFLAGS="$CFLAGS"
	save_LDFLAGS="$LDFLAGS"
	CFLAGS="$CFLAGS -fpic"
	LDFLAGS="$LDFLAGS -shared"
	AC_TRY_LINK([asm (".text; foo: nop; .data; .align 4; .byte 0; .uaword %r_disp32(foo); .text");],,
		    [libffi_cv_as_sparc_ua_pcrel=yes],
		    [libffi_cv_as_sparc_ua_pcrel=no])
	CFLAGS="$save_CFLAGS"
	LDFLAGS="$save_LDFLAGS"])
    if test "x$libffi_cv_as_sparc_ua_pcrel" = xyes; then
	AC_DEFINE(HAVE_AS_SPARC_UA_PCREL, 1,
		  [Define if your assembler and linker support unaligned PC relative relocs.])
    fi

    AC_CACHE_CHECK([assembler .register pseudo-op support],
       libffi_cv_as_register_pseudo_op, [
       libffi_cv_as_register_pseudo_op=unknown
       # Check if we have .register
       AC_TRY_COMPILE(,[asm (".register %g2, #scratch");],
		       [libffi_cv_as_register_pseudo_op=yes],
		       [libffi_cv_as_register_pseudo_op=no])
    ])
    if test "x$libffi_cv_as_register_pseudo_op" = xyes; then
       AC_DEFINE(HAVE_AS_REGISTER_PSEUDO_OP, 1,
	       [Define if your assembler supports .register.])
    fi
    ;;

  X86*)
    AC_CACHE_CHECK([assembler supports pc related relocs],
	libffi_cv_as_x86_pcrel, [
	libffi_cv_as_x86_pcrel=no
	echo '.text; foo: nop; .data; .long foo-.; .text' > conftest.s
	if $CC $CFLAGS -c conftest.s > /dev/null 2>&1; then
	    libffi_cv_as_x86_pcrel=yes
	fi
	])
    if test "x$libffi_cv_as_x86_pcrel" = xyes; then
	AC_DEFINE(HAVE_AS_X86_PCREL, 1,
		  [Define if your assembler supports PC relative relocs.])
    fi
    ;;

  S390)
    AC_CACHE_CHECK([compiler uses zarch features],
	libffi_cv_as_s390_zarch, [
	libffi_cv_as_s390_zarch=no
	echo 'void foo(void) { bar(); bar(); }' > conftest.c
	if $CC $CFLAGS -S conftest.c > /dev/null 2>&1; then
	    if grep -q brasl conftest.s; then
	        libffi_cv_as_s390_zarch=yes
	    fi
	fi
	])
    if test "x$libffi_cv_as_s390_zarch" = xyes; then
	AC_DEFINE(HAVE_AS_S390_ZARCH, 1,
		  [Define if the compiler uses zarch features.])
    fi
    ;;
esac

AC_CACHE_CHECK([whether compiler supports pointer authentication],
   libffi_cv_as_ptrauth, [
   libffi_cv_as_ptrauth=unknown
   AC_TRY_COMPILE(,[
#ifdef __clang__
# if __has_feature(ptrauth_calls)
#  define HAVE_PTRAUTH 1
# endif
#endif

#ifndef HAVE_PTRAUTH
# error Pointer authentication not supported
#endif
		   ],
		   [libffi_cv_as_ptrauth=yes],
		   [libffi_cv_as_ptrauth=no])
])
if test "x$libffi_cv_as_ptrauth" = xyes; then
    AC_DEFINE(HAVE_PTRAUTH, 1,
	      [Define if your compiler supports pointer authentication.])
fi

# On PaX enable kernels that have MPROTECT enable we can't use PROT_EXEC.
AC_ARG_ENABLE(pax_emutramp,
  [  --enable-pax_emutramp       enable pax emulated trampolines, for we can't use PROT_EXEC],
  if test "$enable_pax_emutramp" = "yes"; then
    AC_DEFINE(FFI_MMAP_EXEC_EMUTRAMP_PAX, 1,
      [Define this if you want to enable pax emulated trampolines])
  fi)

FFI_EXEC_TRAMPOLINE_TABLE=0
case "$target" in
     *arm*-apple-* | aarch64-apple-*)
       FFI_EXEC_TRAMPOLINE_TABLE=1
       AC_DEFINE(FFI_EXEC_TRAMPOLINE_TABLE, 1,
                 [Cannot use PROT_EXEC on this target, so, we revert to
                   alternative means])
     ;;
     *-apple-* | *-*-freebsd* | *-*-kfreebsd* | *-*-openbsd* | *-pc-solaris* | *-linux-android*)
       AC_DEFINE(FFI_MMAP_EXEC_WRIT, 1,
                 [Cannot use malloc on this target, so, we revert to
                   alternative means])
     ;;
esac
AM_CONDITIONAL(FFI_EXEC_TRAMPOLINE_TABLE, test x$FFI_EXEC_TRAMPOLINE_TABLE = x1)
AC_SUBST(FFI_EXEC_TRAMPOLINE_TABLE)

if test x$TARGET = xX86_64; then
    AC_CACHE_CHECK([toolchain supports unwind section type],
	libffi_cv_as_x86_64_unwind_section_type, [
        cat  > conftest1.s << EOF
.text
.globl foo
foo:
jmp bar
.section .eh_frame,"a",@unwind
bar:
EOF

        cat > conftest2.c  << EOF
extern void foo();
int main(){foo();}
EOF

	libffi_cv_as_x86_64_unwind_section_type=no
	# we ensure that we can compile _and_ link an assembly file containing an @unwind section
	# since the compiler can support it and not the linker (ie old binutils)
	if $CC -Wa,--fatal-warnings $CFLAGS -c conftest1.s > /dev/null 2>&1 && \
           $CC conftest2.c conftest1.o > /dev/null 2>&1 ; then
	    libffi_cv_as_x86_64_unwind_section_type=yes
	fi
	])
    if test "x$libffi_cv_as_x86_64_unwind_section_type" = xyes; then
	AC_DEFINE(HAVE_AS_X86_64_UNWIND_SECTION_TYPE, 1,
		  [Define if your assembler supports unwind section type.])
    fi
fi

if test "x$GCC" = "xyes"; then
  AC_CACHE_CHECK([whether .eh_frame section should be read-only],
      libffi_cv_ro_eh_frame, [
  	libffi_cv_ro_eh_frame=yes
  	echo 'extern void foo (void); void bar (void) { foo (); foo (); }' > conftest.c
  	if $CC $CFLAGS -c -fpic -fexceptions -fno-lto -o conftest.o conftest.c > /dev/null 2>&1; then
	    if $READELF -WS conftest.o | grep -q -n 'eh_frame .* WA'; then
	        libffi_cv_ro_eh_frame=no
	    fi
  	fi
  	rm -f conftest.*
      ])
  if test "x$libffi_cv_ro_eh_frame" = xyes; then
      AC_DEFINE(HAVE_RO_EH_FRAME, 1,
  	      [Define if .eh_frame sections should be read-only.])
      AC_DEFINE(EH_FRAME_FLAGS, "a",
  	      [Define to the flags needed for the .section .eh_frame directive.  ])
  else
      AC_DEFINE(EH_FRAME_FLAGS, "aw",
  	      [Define to the flags needed for the .section .eh_frame directive.  ])
  fi

  AC_CACHE_CHECK([for __attribute__((visibility("hidden")))],
      libffi_cv_hidden_visibility_attribute, [
  	echo 'int __attribute__ ((visibility ("hidden"))) foo (void) { return 1  ; }' > conftest.c
  	libffi_cv_hidden_visibility_attribute=no
  	if AC_TRY_COMMAND(${CC-cc} -Werror -S conftest.c -o conftest.s 1>&AS_MESSAGE_LOG_FD); then
  	    if egrep '(\.hidden|\.private_extern).*foo' conftest.s >/dev/null; then
  		libffi_cv_hidden_visibility_attribute=yes
  	    fi
  	fi
  	rm -f conftest.*
      ])
  if test $libffi_cv_hidden_visibility_attribute = yes; then
      AC_DEFINE(HAVE_HIDDEN_VISIBILITY_ATTRIBUTE, 1,
  	      [Define if __attribute__((visibility("hidden"))) is supported.])
  fi
fi

# See if makeinfo has been installed and is modern enough
# that we can use it.
ACX_CHECK_PROG_VER([MAKEINFO], [makeinfo], [--version],
                   [GNU texinfo.* \([0-9][0-9.]*\)],
                   [4.[4-9]*|4.[1-9][0-9]*|[5-9]*|[1-9][0-9]*])
AM_CONDITIONAL(BUILD_DOCS, test $gcc_cv_prog_makeinfo_modern = "yes")

AH_BOTTOM([
#ifdef HAVE_HIDDEN_VISIBILITY_ATTRIBUTE
#ifdef LIBFFI_ASM
#ifdef __APPLE__
#define FFI_HIDDEN(name) .private_extern name
#else
#define FFI_HIDDEN(name) .hidden name
#endif
#else
#define FFI_HIDDEN __attribute__ ((visibility ("hidden")))
#endif
#else
#ifdef LIBFFI_ASM
#define FFI_HIDDEN(name)
#else
#define FFI_HIDDEN
#endif
#endif
])

AC_SUBST(TARGET)
AC_SUBST(TARGETDIR)

changequote(<,>)
TARGET_OBJ=
for i in $SOURCES; do
  TARGET_OBJ="${TARGET_OBJ} src/${TARGETDIR}/"`echo $i | sed 's/[cS]$/lo/'`
done
changequote([,])
AC_SUBST(TARGET_OBJ)

AC_SUBST(SHELL)

AC_ARG_ENABLE(debug,
[  --enable-debug          debugging mode],
  if test "$enable_debug" = "yes"; then
    AC_DEFINE(FFI_DEBUG, 1, [Define this if you want extra debugging.])
  fi)
AM_CONDITIONAL(FFI_DEBUG, test "$enable_debug" = "yes")

AC_ARG_ENABLE(structs,
[  --disable-structs       omit code for struct support],
  if test "$enable_structs" = "no"; then
    AC_DEFINE(FFI_NO_STRUCTS, 1, [Define this if you do not want support for aggregate types.])
  fi)
AM_CONDITIONAL(FFI_DEBUG, test "$enable_debug" = "yes")

AC_ARG_ENABLE(raw-api,
[  --disable-raw-api       make the raw api unavailable],
  if test "$enable_raw_api" = "no"; then
    AC_DEFINE(FFI_NO_RAW_API, 1, [Define this if you do not want support for the raw API.])
  fi)

AC_ARG_ENABLE(exec-static-tramp,
[  --enable-exec-static-tramp  enable use of static exec trampolines])

if test "$enable_exec_static_tramp" = yes; then
case "$target" in
     *-cygwin*)
     ;;
     *arm*-*-linux-* | aarch64*-*-linux-* | i*86-*-linux-* | x86_64-*-linux-*)
       AC_DEFINE(FFI_EXEC_STATIC_TRAMP, 1,
                 [Define this if you want statically defined trampolines])
     ;;
esac
fi

AC_ARG_ENABLE(purify-safety,
[  --enable-purify-safety  purify-safe mode],
  if test "$enable_purify_safety" = "yes"; then
    AC_DEFINE(USING_PURIFY, 1, [Define this if you are using Purify and want to suppress spurious messages.])
  fi)

GCC_WITH_TOOLEXECLIBDIR

if test -n "$with_cross_host" &&
   test x"$with_cross_host" != x"no"; then
  toolexecdir='$(exec_prefix)/$(target_alias)'
  case ${with_toolexeclibdir} in
    no)
      toolexeclibdir='$(toolexecdir)/lib'
      ;;
    *)
      toolexeclibdir=${with_toolexeclibdir}
      ;;
  esac
else
  toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
  toolexeclibdir='$(libdir)'
fi
multi_os_directory=`$CC -print-multi-os-directory`
case $multi_os_directory in
  .) ;; # Avoid trailing /.
  *) toolexeclibdir=$toolexeclibdir/$multi_os_directory ;;
esac
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Check linker support.
LIBFFI_ENABLE_SYMVERS

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

AC_CONFIG_COMMANDS(include, [test -d include || mkdir include])
AC_CONFIG_COMMANDS(src, [
test -d src || mkdir src
test -d src/$TARGETDIR || mkdir src/$TARGETDIR
], [TARGETDIR="$TARGETDIR"])

AC_CONFIG_FILES(include/Makefile include/ffi.h Makefile testsuite/Makefile man/Makefile libffi.pc)

AC_CONFIG_LINKS(include/ffitarget.h:src/$TARGETDIR/ffitarget.h)

AC_OUTPUT
