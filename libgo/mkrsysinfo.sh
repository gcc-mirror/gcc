#!/bin/sh

# Copyright 2016 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Create runtime_sysinfo.go from gen-sysinfo.go and errno.i.

OUT=tmp-runtime_sysinfo.go

set -e

echo 'package runtime' > ${OUT}

# Get all the consts and types, skipping ones which could not be
# represented in Go and ones which we need to rewrite.  We also skip
# function declarations, as we don't need them here.  All the symbols
# will all have a leading underscore.
grep -v '^// ' gen-sysinfo.go | \
  grep -v '^func' | \
  grep -v '^var ' | \
  grep -v '^type _timeval ' | \
  grep -v '^type _timespec_t ' | \
  grep -v '^type _timespec ' | \
  grep -v '^type _epoll_' | \
  grep -v 'in6_addr' | \
  grep -v 'sockaddr_in6' | \
  sed -e 's/\([^a-zA-Z0-9_]\)_timeval\([^a-zA-Z0-9_]\)/\1timeval\2/g' \
      -e 's/\([^a-zA-Z0-9_]\)_timespec_t\([^a-zA-Z0-9_]\)/\1timespec\2/g' \
      -e 's/\([^a-zA-Z0-9_]\)_timespec\([^a-zA-Z0-9_]\)/\1timespec\2/g' \
    >> ${OUT}

# On AIX, the _arpcom struct, is filtered by the above grep sequence, as it as
# a field of type _in6_addr, but other types depend on _arpcom, so we need to
# put it back.
grep '^type _arpcom ' gen-sysinfo.go | \
  sed -e 's/_in6_addr/[16]byte/' >> ${OUT}

# Same on Solaris for _mld_hdr_t.
grep '^type _mld_hdr_t ' gen-sysinfo.go | \
  sed -e 's/_in6_addr/[16]byte/' >> ${OUT}

# The time structures need special handling: we need to name the
# types, so that we can cast integers to the right types when
# assigning to the structures.
timeval=`grep '^type _timeval ' gen-sysinfo.go`
timeval_sec=`echo $timeval | sed -n -e 's/^.*tv_sec \([^ ]*\);.*$/\1/p'`
timeval_usec=`echo $timeval | sed -n -e 's/^.*tv_usec \([^ ]*\);.*$/\1/p'`
echo "type timeval_sec_t $timeval_sec" >> ${OUT}
echo "type timeval_usec_t $timeval_usec" >> ${OUT}
echo $timeval | \
  sed -e 's/type _timeval /type timeval /' \
      -e 's/tv_sec *[a-zA-Z0-9_]*/tv_sec timeval_sec_t/' \
      -e 's/tv_usec *[a-zA-Z0-9_]*/tv_usec timeval_usec_t/' >> ${OUT}
echo >> ${OUT}
echo "func (tv *timeval) set_usec(x int32) {" >> ${OUT}
echo "	tv.tv_usec = timeval_usec_t(x)" >> ${OUT}
echo "}" >> ${OUT}

timespec=`grep '^type _timespec ' gen-sysinfo.go || true`
if test "$timespec" = ""; then
  # IRIX 6.5 has __timespec instead.
  timespec=`grep '^type ___timespec ' gen-sysinfo.go || true`
fi
timespec_sec=`echo $timespec | sed -n -e 's/^.*tv_sec \([^ ]*\);.*$/\1/p'`
timespec_nsec=`echo $timespec | sed -n -e 's/^.*tv_nsec \([^ ]*\);.*$/\1/p'`
echo "type timespec_sec_t $timespec_sec" >> ${OUT}
echo "type timespec_nsec_t $timespec_nsec" >> ${OUT}
echo $timespec | \
  sed -e 's/^type ___timespec /type timespec /' \
      -e 's/^type _timespec /type timespec /' \
      -e 's/tv_sec *[a-zA-Z0-9_]*/tv_sec timespec_sec_t/' \
      -e 's/tv_nsec *[a-zA-Z0-9_]*/tv_nsec timespec_nsec_t/' >> ${OUT}
echo >> ${OUT}
echo "func (ts *timespec) set_sec(x int64) {" >> ${OUT}
echo "	ts.tv_sec = timespec_sec_t(x)" >> ${OUT}
echo "}" >> ${OUT}
echo >> ${OUT}
echo "func (ts *timespec) set_nsec(x int32) {" >> ${OUT}
echo "	ts.tv_nsec = timespec_nsec_t(x)" >> ${OUT}
echo "}" >> ${OUT}

# Define the epollevent struct.  This needs special attention because
# the C definition uses a union and is sometimes packed.
if grep '^const _epoll_data_offset ' ${OUT} >/dev/null 2>&1; then
  val=`grep '^const _epoll_data_offset ' ${OUT} | sed -e 's/const _epoll_data_offset = \(.*\)$/\1/'`
  if test "$val" = "4"; then
      echo 'type epollevent struct { events uint32; data [8]byte }' >> ${OUT}
  elif test "$val" = "8"; then
      if test "$GOARCH" = "sparc64" -a "$GOOS" = "linux"; then
          echo 'type epollevent struct { events uint32; pad [4]byte; data [8]byte; _align [0]int64 }' >> ${OUT}
      else
          echo 'type epollevent struct { events uint32; pad [4]byte; data [8]byte }' >> ${OUT}
      fi
  else
      echo 1>&2 "unknown epoll data offset value ${val}"
      exit 1
  fi
fi
# Make sure EPOLLET is positive.
if grep '^const _EPOLLET = [0-9]' gen-sysinfo.go > /dev/null 2>&1; then
  echo "const _EPOLLETpos = _EPOLLET" >> ${OUT}
else
  echo "const _EPOLLETpos = 0x80000000" >> ${OUT}
fi
# Make sure EPOLLRDHUP and EPOLL_CLOEXEC are defined.
if ! grep '^const _EPOLLRDHUP' ${OUT} >/dev/null 2>&1; then
  echo "const _EPOLLRDHUP = 0x2000" >> ${OUT}
fi
if ! grep '^const _EPOLL_CLOEXEC' ${OUT} >/dev/null 2>&1; then
  echo "const _EPOLL_CLOEXEC = 02000000" >> ${OUT}
fi

# AIX 7.1 is a 64 bits value for _FCLOEXEC (referenced by O_CLOEXEC)
# which leads to a constant overflow when using O_CLOEXEC in some
# go code. Issue wan not present in 6.1 (no O_CLOEXEC) and is no
# more present in 7.2 (_FCLOEXEC is a 32 bit value).
if test "${GOOS}" = "aix" && `oslevel | grep -q "^7.1"`; then
    sed -e 's/const __FCLOEXEC = .*/const __FCLOEXEC = 0/' ${OUT} > ${OUT}-2
    mv ${OUT}-2 ${OUT}
fi

# Make sure _MAP_FAILED is defined.
if ! grep '^const _MAP_FAILED =' gen-sysinfo.go > /dev/null 2>&1; then
  echo "const _MAP_FAILED = ^uintptr(0)" >> ${OUT}
fi
# Make sure _MAP_ANON is defined.
if ! grep '^const _MAP_ANON =' gen-sysinfo.go > /dev/null 2>&1; then
  if grep '^const _MAP_ANONYMOUS ' gen-sysinfo.go > /dev/null 2>&1; then
    echo "const _MAP_ANON = _MAP_ANONYMOUS" >> ${OUT}
  else
    echo "const _MAP_ANON = 0" >> ${OUT}
  fi
fi
# Make sure _MADV_DONTNEED is defined.
if ! grep '^const _MADV_DONTNEED =' gen-sysinfo.go > /dev/null 2>&1; then
  echo "const _MADV_DONTNEED = 0" >> ${OUT}
fi
# Make sure _MADV_FREE is defined.
if ! grep '^const _MADV_FREE =' gen-sysinfo.go > /dev/null 2>&1; then
  echo "const _MADV_FREE = 0" >> ${OUT}
fi
# Make sure _MADV_HUGEPAGE is defined.
if ! grep '^const _MADV_HUGEPAGE =' gen-sysinfo.go > /dev/null 2>&1; then
  echo "const _MADV_HUGEPAGE = 0" >> ${OUT}
fi
# Make sure _MADV_NOHUGEPAGE is defined.
if ! grep '^const _MADV_NOHUGEPAGE =' gen-sysinfo.go > /dev/null 2>&1; then
  echo "const _MADV_NOHUGEPAGE = 0" >> ${OUT}
fi

# The semt structure, for Solaris.
grep '^type _sem_t ' gen-sysinfo.go | \
    sed -e 's/_sem_t/semt/' >> ${OUT}

# Solaris 2 needs _u?pad128_t, but its default definition in terms of long
# double is commented by -fdump-go-spec.
if grep "^// type _pad128_t" gen-sysinfo.go > /dev/null 2>&1; then
  echo "type _pad128_t struct { _l [4]int32; }" >> ${OUT}
fi
if grep "^// type _upad128_t" gen-sysinfo.go > /dev/null 2>&1; then
  echo "type _upad128_t struct { _l [4]uint32; }" >> ${OUT}
fi

# The Solaris 11 Update 1 _zone_net_addr_t struct.
grep '^type _zone_net_addr_t ' gen-sysinfo.go | \
    sed -e 's/_in6_addr/[16]byte/' \
    >> ${OUT}

# The Solaris 12 _flow_arp_desc_t struct.
grep '^type _flow_arp_desc_t ' gen-sysinfo.go | \
    sed -e 's/_in6_addr_t/[16]byte/g' \
    >> ${OUT}

# The Solaris 12 _flow_l3_desc_t struct.
grep '^type _flow_l3_desc_t ' gen-sysinfo.go | \
    sed -e 's/_in6_addr_t/[16]byte/g' \
    >> ${OUT}

# The Solaris 12 _mac_ipaddr_t struct.
grep '^type _mac_ipaddr_t ' gen-sysinfo.go | \
    sed -e 's/_in6_addr_t/[16]byte/g' \
    >> ${OUT}

# The Solaris 12 _mactun_info_t struct.
grep '^type _mactun_info_t ' gen-sysinfo.go | \
    sed -e 's/_in6_addr_t/[16]byte/g' \
    >> ${OUT}

# The Solaris port_event_t struct.
grep '^type _port_event_t ' gen-sysinfo.go | \
    sed -e s'/_port_event_t/portevent/' \
    >> ${OUT}

# The *BSD kevent struct.
grep '^type _kevent ' gen-sysinfo.go | \
    sed -e s'/_kevent/keventt/' \
      -e 's/ udata [^;}]*/ udata *byte/' \
    >> ${OUT}
