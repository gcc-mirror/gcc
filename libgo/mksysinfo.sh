#!/bin/sh

# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Create sysinfo.go.

# This shell script creates the sysinfo.go file which holds types and
# constants extracted from the system header files.  This relies on a
# hook in gcc: the -fdump-go-spec option will generate debugging
# information in Go syntax.

# We currently #include all the files at once, which works, but leads
# to exposing some names which ideally should not be exposed, as they
# match grep patterns.  E.g., WCHAR_MIN gets exposed because it starts
# with W, like the wait flags.

CC=${CC:-gcc}
OUT=tmp-sysinfo.go

set -e

rm -f sysinfo.c
cat > sysinfo.c <<EOF
#include "config.h"

#define _GNU_SOURCE
#define _LARGEFILE_SOURCE
#define _FILE_OFFSET_BITS 64

#if defined(__sun__) && defined(__svr4__)
/* Needed by Solaris header files.  */
#define _XOPEN_SOURCE 600
#define __EXTENSIONS__
#endif

#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <signal.h>
#if defined(HAVE_SYSCALL_H)
#include <syscall.h>
#endif
#if defined(HAVE_SYS_SYSCALL_H)
#include <sys/syscall.h>
#endif
#if defined(HAVE_SYS_EPOLL_H)
#include <sys/epoll.h>
#endif
#if defined(HAVE_SYS_PTRACE_H)
#include <sys/ptrace.h>
#endif
#include <sys/resource.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/un.h>
#if defined(HAVE_SYS_USER_H)
#include <sys/user.h>
#endif
#if defined(HAVE_SYS_UTSNAME_H)
#include <sys/utsname.h>
#endif
#include <unistd.h>
EOF

${CC} -fdump-go-spec=gen-sysinfo.go -std=gnu99 -S -o sysinfo.s sysinfo.c

echo 'package syscall' > ${OUT}

# Get all the consts and types, skipping ones which could not be
# represented in Go and ones which we need to rewrite.  We also skip
# function declarations, as we don't need them here.  All the symbols
# will all have a leading underscore.
grep -v '^// ' gen-sysinfo.go | \
  grep -v '^func' | \
  grep -v '^type _timeval ' | \
  grep -v '^type _timespec ' | \
  grep -v '^type _timestruc_t ' | \
  grep -v '^type _epoll_' | \
  grep -v 'in6_addr' | \
  grep -v 'sockaddr_in6' | \
  sed -e 's/\([^a-zA-Z0-9_]\)_timeval\([^a-zA-Z0-9_]\)/\1Timeval\2/g' \
      -e 's/\([^a-zA-Z0-9_]\)_timespec\([^a-zA-Z0-9_]\)/\1Timespec\2/g' \
      -e 's/\([^a-zA-Z0-9_]\)_timestruc_t\([^a-zA-Z0-9_]\)/\1Timestruc\2/g' \
    >> ${OUT}

# The errno constants.
grep '^const _E' gen-sysinfo.go | \
  sed -e 's/^\(const \)_\(E[^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}

# The O_xxx flags.
grep '^const _\(O\|F\|FD\)_' gen-sysinfo.go | \
  sed -e 's/^\(const \)_\([^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}
if ! grep '^const O_ASYNC' ${OUT} >/dev/null 2>&1; then
  echo "const O_ASYNC = 0" >> ${OUT}
fi
if ! grep '^const O_CLOEXEC' ${OUT} >/dev/null 2>&1; then
  echo "const O_CLOEXEC = 0" >> ${OUT}
fi

# The signal numbers.
grep '^const _SIG[^_]' gen-sysinfo.go | \
  grep -v '^const _SIGEV_' | \
  sed -e 's/^\(const \)_\(SIG[^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}

# The syscall numbers.  We force the names to upper case.
grep '^const _SYS_' gen-sysinfo.go | \
  sed -e 's/const _\(SYS_[^= ]*\).*$/\1/' | \
  while read sys; do
    sup=`echo $sys | tr abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ`
    echo "const $sup = _$sys" >> ${OUT}
  done

# Stat constants.
grep '^const _S_' gen-sysinfo.go | \
  sed -e 's/^\(const \)_\(S_[^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}

# Process status constants.
grep '^const _W' gen-sysinfo.go |
  sed -e 's/^\(const \)_\(W[^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}
# WSTOPPED was introduced in glibc 2.3.4.
if ! grep '^const _WSTOPPED = ' gen-sysinfo.go >/dev/null 2>&1; then
  if grep '^const _WUNTRACED = ' gen-sysinfo.go > /dev/null 2>&1; then
    echo 'const WSTOPPED = _WUNTRACED' >> ${OUT}
  else
    echo 'const WSTOPPED = 2' >> ${OUT}
  fi
fi
if grep '^const ___WALL = ' gen-sysinfo.go >/dev/null 2>&1 \
   && ! grep '^const _WALL = ' gen-sysinfo.go >/dev/null 2>&1; then
  echo 'const WALL = ___WALL' >> ${OUT}
fi

# Networking constants.
grep '^const _\(AF\|SOCK\|SOL\|SO\|IPPROTO\|TCP\|IP\|IPV6\)_' gen-sysinfo.go |
  sed -e 's/^\(const \)_\([^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}
grep '^const _SOMAXCONN' gen-sysinfo.go |
  sed -e 's/^\(const \)_\(SOMAXCONN[^= ]*\)\(.*\)$/\1\2 = _\2/' \
    >> ${OUT}
grep '^const _SHUT_' gen-sysinfo.go |
  sed -e 's/^\(const \)_\(SHUT[^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}

# The net package requires a definition for IPV6ONLY.
if ! grep '^const IPV6_V6ONLY ' ${OUT} >/dev/null 2>&1; then
  echo "const IPV6_V6ONLY = 0" >> ${OUT}
fi

# pathconf constants.
grep '^const __PC' gen-sysinfo.go |
  sed -e 's/^\(const \)__\(PC[^= ]*\)\(.*\)$/\1\2 = __\2/' >> ${OUT}

# The epoll constants were picked up by the errno constants, but we
# need to be sure the EPOLLRDHUP is defined.
if ! grep '^const EPOLLRDHUP' ${OUT} >/dev/null 2>&1; then
  echo "const EPOLLRDHUP = 0x2000" >> ${OUT}
fi

# Ptrace constants.  We don't expose all the PTRACE flags, just the
# PTRACE_O_xxx and PTRACE_EVENT_xxx ones.
grep '^const _PTRACE_O' gen-sysinfo.go |
  sed -e 's/^\(const \)_\(PTRACE_O[^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}
grep '^const _PTRACE_EVENT' gen-sysinfo.go |
  sed -e 's/^\(const \)_\(PTRACE_EVENT[^= ]*\)\(.*\)$/\1\2 = _\2/' >> ${OUT}
# We need PTRACE_SETOPTIONS and PTRACE_GETEVENTMSG, but they are not
# defined in older versions of glibc.
if ! grep '^const _PTRACE_SETOPTIONS' ${OUT} > /dev/null 2>&1; then
  echo "const _PTRACE_SETOPTIONS = 0x4200" >> ${OUT}
fi
if ! grep '^const PTRACE_O_TRACESYSGOOD' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_TRACESYSGOOD = 0x1" >> ${OUT}
fi
if ! grep '^const PTRACE_O_TRACEFORK' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_TRACEFORK = 0x2" >> ${OUT}
fi
if ! grep '^const PTRACE_O_TRACEVFORK' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_TRACEVFORK = 0x4" >> ${OUT}
fi
if ! grep '^const PTRACE_O_TRACECLONE' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_TRACECLONE = 0x8" >> ${OUT}
fi
if ! grep '^const PTRACE_O_TRACEEXEC' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_TRACEEXEC = 0x10" >> ${OUT}
fi
if ! grep '^const PTRACE_O_TRACEVFORKDONE' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_TRACEVFORKDONE = 0x20" >> ${OUT}
fi
if ! grep '^const PTRACE_O_TRACEEXIT' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_TRACEEXIT = 0x40" >> ${OUT}
fi
if ! grep '^const PTRACE_O_MASK' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_O_MASK = 0x7f" >> ${OUT}
fi
if ! grep '^const _PTRACE_GETEVENTMSG' ${OUT} > /dev/null 2>&1; then
  echo "const _PTRACE_GETEVENTMSG = 0x4201" >> ${OUT}
fi
if ! grep '^const PTRACE_EVENT_FORK' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_EVENT_FORK = 1" >> ${OUT}
fi
if ! grep '^const PTRACE_EVENT_VFORK' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_EVENT_VFORK = 2" >> ${OUT}
fi
if ! grep '^const PTRACE_EVENT_CLONE' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_EVENT_CLONE = 3" >> ${OUT}
fi
if ! grep '^const PTRACE_EVENT_EXEC' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_EVENT_EXEC = 4" >> ${OUT}
fi
if ! grep '^const PTRACE_EVENT_VFORK_DONE' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_EVENT_VFORK_DONE = 5" >> ${OUT}
fi
if ! grep '^const PTRACE_EVENT_EXIT' ${OUT} > /dev/null 2>&1; then
  echo "const PTRACE_EVENT_EXIT = 6" >> ${OUT}
fi
if ! grep '^const _PTRACE_TRACEME' ${OUT} > /dev/null 2>&1; then
  echo "const _PTRACE_TRACEME = 0" >> ${OUT}
fi

# The registers returned by PTRACE_GETREGS.  This is probably
# GNU/Linux specific; it should do no harm if there is no
# _user_regs_struct.
regs=`grep '^type _user_regs_struct struct' gen-sysinfo.go || true`
if test "$regs" != ""; then
  regs=`echo $regs | sed -e 's/type _user_regs_struct struct //' -e 's/[{}]//g'`
  regs=`echo $regs | sed -e s'/^ *//'`
  nregs=
  while test -n "$regs"; do
    field=`echo $regs | sed -e 's/^\([^;]*\);.*$/\1/'`
    regs=`echo $regs | sed -e 's/^[^;]*; *\(.*\)$/\1/'`
    # Capitalize the first character of the field.
    f=`echo $field | sed -e 's/^\(.\).*$/\1/'`
    r=`echo $field | sed -e 's/^.\(.*\)$/\1/'`
    f=`echo $f | tr abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ`
    field="$f$r"
    nregs="$nregs $field;"
  done
  echo "type PtraceRegs struct {$nregs }" >> ${OUT}
fi

# Some basic types.
echo 'type Size_t _size_t' >> ${OUT}
echo "type Ssize_t _ssize_t" >> ${OUT}
if grep '^const _HAVE_OFF64_T = ' gen-sysinfo.go > /dev/null 2>&1; then
  echo "type Offset_t _off64_t" >> ${OUT}
else
  echo "type Offset_t _off_t" >> ${OUT}
fi
echo "type Mode_t _mode_t" >> ${OUT}
echo "type Pid_t _pid_t" >> ${OUT}
echo "type Uid_t _uid_t" >> ${OUT}
echo "type Gid_t _gid_t" >> ${OUT}
echo "type Socklen_t _socklen_t" >> ${OUT}

# The long type, needed because that is the type that ptrace returns.
sizeof_long=`grep '^const ___SIZEOF_LONG__ = ' gen-sysinfo.go | sed -e 's/.*= //'`
if test "$sizeof_long" = "4"; then
  echo "type _C_long int32" >> ${OUT}
elif test "$sizeof_long" = "8"; then
  echo "type _C_long int64" >> ${OUT}
else
  echo 1>&2 "mksysinfo.sh: could not determine size of long (got $sizeof_long)"
  exit 1
fi

# Solaris 2 needs _u?pad128_t, but its default definition in terms of long
# double is commented by -fdump-go-spec.
if grep "^// type _pad128_t" gen-sysinfo.go > /dev/null 2>&1; then
  echo "type _pad128_t struct { _l [4]int32; }" >> ${OUT}
fi
if grep "^// type _upad128_t" gen-sysinfo.go > /dev/null 2>&1; then
  echo "type _upad128_t struct { _l [4]uint32; }" >> ${OUT}
fi

# The time structures need special handling: we need to name the
# types, so that we can cast integers to the right types when
# assigning to the structures.
timeval=`grep '^type _timeval ' gen-sysinfo.go`
timeval_sec=`echo $timeval | sed -n -e 's/^.*tv_sec \([^ ]*\);.*$/\1/p'`
timeval_usec=`echo $timeval | sed -n -e 's/^.*tv_usec \([^ ]*\);.*$/\1/p'`
echo "type Timeval_sec_t $timeval_sec" >> ${OUT}
echo "type Timeval_usec_t $timeval_usec" >> ${OUT}
echo $timeval | \
  sed -e 's/type _timeval /type Timeval /' \
      -e 's/tv_sec *[a-zA-Z0-9_]*/Sec Timeval_sec_t/' \
      -e 's/tv_usec *[a-zA-Z0-9_]*/Usec Timeval_usec_t/' >> ${OUT}
timespec=`grep '^type _timespec ' gen-sysinfo.go`
timespec_sec=`echo $timespec | sed -n -e 's/^.*tv_sec \([^ ]*\);.*$/\1/p'`
timespec_nsec=`echo $timespec | sed -n -e 's/^.*tv_nsec \([^ ]*\);.*$/\1/p'`
echo "type Timespec_sec_t $timespec_sec" >> ${OUT}
echo "type Timespec_nsec_t $timespec_nsec" >> ${OUT}
echo $timespec | \
  sed -e 's/^type _timespec /type Timespec /' \
      -e 's/tv_sec *[a-zA-Z0-9_]*/Sec Timespec_sec_t/' \
      -e 's/tv_nsec *[a-zA-Z0-9_]*/Nsec Timespec_nsec_t/' >> ${OUT}

timestruc=`grep '^type _timestruc_t ' gen-sysinfo.go || true`
if test "$timestruc" != ""; then
  timestruc_sec=`echo $timestruc | sed -n -e 's/^.*tv_sec \([^ ]*\);.*$/\1/p'`
  timestruc_nsec=`echo $timestruc | sed -n -e 's/^.*tv_nsec \([^ ]*\);.*$/\1/p'`
  echo "type Timestruc_sec_t $timestruc_sec" >> ${OUT}
  echo "type Timestruc_nsec_t $timestruc_nsec" >> ${OUT}
  echo $timestruc | \
    sed -e 's/^type _timestruc_t /type Timestruc /' \
        -e 's/tv_sec *[a-zA-Z0-9_]*/Sec Timestruc_sec_t/' \
        -e 's/tv_nsec *[a-zA-Z0-9_]*/Nsec Timestruc_nsec_t/' >> ${OUT}
fi

# The stat type.
# Prefer largefile variant if available.
stat=`grep '^type _stat64 ' gen-sysinfo.go || true`
if test "$stat" != ""; then
  grep '^type _stat64 ' gen-sysinfo.go
else
  grep '^type _stat ' gen-sysinfo.go
fi | sed -e 's/type _stat\(64\)\?/type Stat_t/' \
         -e 's/st_dev/Dev/' \
         -e 's/st_ino/Ino/g' \
         -e 's/st_nlink/Nlink/' \
         -e 's/st_mode/Mode/' \
         -e 's/st_uid/Uid/' \
         -e 's/st_gid/Gid/' \
         -e 's/st_rdev/Rdev/' \
         -e 's/st_size/Size/' \
         -e 's/st_blksize/Blksize/' \
         -e 's/st_blocks/Blocks/' \
         -e 's/st_atim/Atime/' \
         -e 's/st_mtim/Mtime/' \
         -e 's/st_ctim/Ctime/' \
         -e 's/\([^a-zA-Z0-9_]\)_timeval\([^a-zA-Z0-9_]\)/\1Timeval\2/g' \
         -e 's/\([^a-zA-Z0-9_]\)_timespec\([^a-zA-Z0-9_]\)/\1Timespec\2/g' \
         -e 's/\([^a-zA-Z0-9_]\)_timestruc_t\([^a-zA-Z0-9_]\)/\1Timestruc\2/g' \
       >> ${OUT}

# The directory searching types.
# Prefer largefile variant if available.
dirent=`grep '^type _dirent64 ' gen-sysinfo.go || true`
if test "$dirent" != ""; then
  grep '^type _dirent64 ' gen-sysinfo.go
else
  grep '^type _dirent ' gen-sysinfo.go
fi | sed -e 's/type _dirent\(64\)\?/type Dirent/' \
         -e 's/d_name \[0+1\]/d_name [0+256]/' \
         -e 's/d_name/Name/' \
         -e 's/]int8/]byte/' \
         -e 's/d_ino/Ino/' \
         -e 's/d_off/Off/' \
         -e 's/d_reclen/Reclen/' \
         -e 's/d_type/Type/' \
      >> ${OUT}
echo "type DIR _DIR" >> ${OUT}

# The rusage struct.
rusage=`grep '^type _rusage struct' gen-sysinfo.go`
if test "$rusage" != ""; then
  rusage=`echo $rusage | sed -e 's/type _rusage struct //' -e 's/[{}]//g'`
  rusage=`echo $rusage | sed -e 's/^ *//'`
  nrusage=
  while test -n "$rusage"; do
    field=`echo $rusage | sed -e 's/^\([^;]*\);.*$/\1/'`
    rusage=`echo $rusage | sed -e 's/^[^;]*; *\(.*\)$/\1/'`
    # Drop the leading ru_, capitalize the next character.
    field=`echo $field | sed -e 's/^ru_//'`
    f=`echo $field | sed -e 's/^\(.\).*$/\1/'`
    r=`echo $field | sed -e 's/^.\(.*\)$/\1/'`
    f=`echo $f | tr abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ`
    # Fix _timeval _timespec, and _timestruc_t.
    r=`echo $r | sed -e s'/ _timeval$/ Timeval/'`
    r=`echo $r | sed -e s'/ _timespec$/ Timespec/'`
    r=`echo $r | sed -e s'/ _timestruc_t$/ Timestruc/'`
    field="$f$r"
    nrusage="$nrusage $field;"
  done
  echo "type Rusage struct {$nrusage }" >> ${OUT}
fi

# The utsname struct.
grep '^type _utsname ' gen-sysinfo.go | \
    sed -e 's/_utsname/Utsname/' \
      -e 's/sysname/Sysname/' \
      -e 's/nodename/Nodename/' \
      -e 's/release/Release/' \
      -e 's/version/Version/' \
      -e 's/machine/Machine/' \
      -e 's/domainname/Domainname/' \
    >> ${OUT}

# The iovec struct.
iovec=`grep '^type _iovec ' gen-sysinfo.go`
iovec_len=`echo $iovec | sed -n -e 's/^.*iov_len \([^ ]*\);.*$/\1/p'`
echo "type Iovec_len_t $iovec_len" >> ${OUT}
echo $iovec | \
    sed -e 's/_iovec/Iovec/' \
      -e 's/iov_base/Base/' \
      -e 's/iov_len *[a-zA-Z0-9_]*/Len Iovec_len_t/' \
    >> ${OUT}

# The msghdr struct.
msghdr=`grep '^type _msghdr ' gen-sysinfo.go`
msghdr_controllen=`echo $msghdr | sed -n -e 's/^.*msg_controllen \([^ ]*\);.*$/\1/p'`
echo "type Msghdr_controllen_t $msghdr_controllen" >> ${OUT}
echo $msghdr | \
    sed -e 's/_msghdr/Msghdr/' \
      -e 's/msg_name/Name/' \
      -e 's/msg_namelen/Namelen/' \
      -e 's/msg_iov/Iov/' \
      -e 's/msg_iovlen/Iovlen/' \
      -e 's/_iovec/Iovec/' \
      -e 's/msg_control/Control/' \
      -e 's/msg_controllen *[a-zA-Z0-9_]*/Controllen Msghdr_controllen_t/' \
      -e 's/msg_flags/Flags/' \
    >> ${OUT}

exit $?
