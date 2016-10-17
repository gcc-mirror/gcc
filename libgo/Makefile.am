# Makefile.am -- Go library Makefile.

# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Process this file with autoreconf to produce Makefile.in.

# Go support.
SUFFIXES = .c .go .gox .o .obj .lo .a

if LIBGO_IS_RTEMS
subdirs = testsuite
endif

SUBDIRS = ${subdirs}

gcc_version := $(shell $(GOC) -dumpversion)

MAINT_CHARSET = latin1

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs
PWD_COMMAND = $${PWDCMD-pwd}
STAMP = echo timestamp >

toolexecdir = $(glibgo_toolexecdir)
toolexeclibdir = $(glibgo_toolexeclibdir)
toolexeclibgodir = $(nover_glibgo_toolexeclibdir)/go/$(gcc_version)/$(target_alias)
libexecsubdir = $(libexecdir)/gcc/$(target_alias)/$(gcc_version)

LIBFFI = @LIBFFI@
LIBFFIINCS = @LIBFFIINCS@

LIBATOMIC = @LIBATOMIC@

WARN_CFLAGS = $(WARN_FLAGS) $(WERROR)

# -I/-D flags to pass when compiling.
AM_CPPFLAGS = -I $(srcdir)/runtime $(LIBFFIINCS) $(PTHREAD_CFLAGS)

ACLOCAL_AMFLAGS = -I ./config -I ../config

AM_CFLAGS = -fexceptions -fnon-call-exceptions -fplan9-extensions \
	$(SPLIT_STACK) $(WARN_CFLAGS) \
	$(STRINGOPS_FLAG) $(OSCFLAGS) \
	-I $(srcdir)/../libgcc -I $(srcdir)/../libbacktrace \
	-I $(MULTIBUILDTOP)../../gcc/include

if USING_SPLIT_STACK
AM_LDFLAGS = -XCClinker $(SPLIT_STACK)
endif

# Multilib support.
MAKEOVERRIDES=

# Work around what appears to be a GNU make  handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CC_FOR_TARGET=$(CC_FOR_TARGET)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"GOC_FOR_TARGET=$(GOC_FOR_TARGET)" \
	"GOC=$(GOC)" \
	"GOCFLAGS=$(GOCFLAGS)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
	"SHELL=$(SHELL)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"includedir=$(includedir)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)" \
	"gxx_include_dir=$(gxx_include_dir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"LD=$(LD)" \
	"RANLIB=$(RANLIB)" \
	"NM=$(NM)" \
	"NM_FOR_BUILD=$(NM_FOR_BUILD)" \
	"NM_FOR_TARGET=$(NM_FOR_TARGET)" \
	"DESTDIR=$(DESTDIR)" \
	"WERROR=$(WERROR)"

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)

if GOC_IS_LLGO
toolexeclib_LTLIBRARIES = libgo-llgo.la
toolexeclib_LIBRARIES = libgobegin-llgo.a
else
toolexeclib_LTLIBRARIES = libgo.la
toolexeclib_LIBRARIES = libgobegin.a libgolibbegin.a libnetgo.a
endif

toolexeclibgo_DATA = \
	bufio.gox \
	bytes.gox \
	context.gox \
	crypto.gox \
	encoding.gox \
	errors.gox \
	expvar.gox \
	flag.gox \
	fmt.gox \
	hash.gox \
	html.gox \
	image.gox \
	io.gox \
	log.gox \
	math.gox \
	mime.gox \
	net.gox \
	os.gox \
	path.gox \
	reflect.gox \
	regexp.gox \
	runtime.gox \
	sort.gox \
	strconv.gox \
	strings.gox \
	sync.gox \
	syscall.gox \
	testing.gox \
	time.gox \
	unicode.gox

toolexeclibgoarchivedir = $(toolexeclibgodir)/archive

toolexeclibgoarchive_DATA = \
	archive/tar.gox \
	archive/zip.gox

toolexeclibgocompressdir = $(toolexeclibgodir)/compress

toolexeclibgocompress_DATA = \
	compress/bzip2.gox \
	compress/flate.gox \
	compress/gzip.gox \
	compress/lzw.gox \
	compress/zlib.gox

toolexeclibgocontainerdir = $(toolexeclibgodir)/container

toolexeclibgocontainer_DATA = \
	container/heap.gox \
	container/list.gox \
	container/ring.gox

toolexeclibgocryptodir = $(toolexeclibgodir)/crypto

toolexeclibgocrypto_DATA = \
	crypto/aes.gox \
	crypto/cipher.gox \
	crypto/des.gox \
	crypto/dsa.gox \
	crypto/ecdsa.gox \
	crypto/elliptic.gox \
	crypto/hmac.gox \
	crypto/md5.gox \
	crypto/rand.gox \
	crypto/rc4.gox \
	crypto/rsa.gox \
	crypto/sha1.gox \
	crypto/sha256.gox \
	crypto/sha512.gox \
	crypto/subtle.gox \
	crypto/tls.gox \
	crypto/x509.gox

toolexeclibgocryptox509dir = $(toolexeclibgocryptodir)/x509

toolexeclibgocryptox509_DATA = \
	crypto/x509/pkix.gox

toolexeclibgodatabasedir = $(toolexeclibgodir)/database

toolexeclibgodatabase_DATA = \
	database/sql.gox

toolexeclibgodatabasesqldir = $(toolexeclibgodatabasedir)/sql

toolexeclibgodatabasesql_DATA = \
	database/sql/driver.gox

toolexeclibgodebugdir = $(toolexeclibgodir)/debug

toolexeclibgodebug_DATA = \
	debug/dwarf.gox \
	debug/elf.gox \
	debug/gosym.gox \
	debug/macho.gox \
	debug/pe.gox \
	debug/plan9obj.gox

toolexeclibgoencodingdir = $(toolexeclibgodir)/encoding

toolexeclibgoencoding_DATA = \
	encoding/ascii85.gox \
	encoding/asn1.gox \
	encoding/base32.gox \
	encoding/base64.gox \
	encoding/binary.gox \
	encoding/csv.gox \
	encoding/gob.gox \
	encoding/hex.gox \
	encoding/json.gox \
	encoding/pem.gox \
	encoding/xml.gox

toolexeclibgoexpdir = $(toolexeclibgodir)/exp

toolexeclibgoexp_DATA = \
	exp/proxy.gox \
	exp/terminal.gox

toolexeclibgogodir = $(toolexeclibgodir)/go

toolexeclibgogo_DATA = \
	go/ast.gox \
	go/build.gox \
	go/constant.gox \
	go/doc.gox \
	go/format.gox \
	go/importer.gox \
	go/parser.gox \
	go/printer.gox \
	go/scanner.gox \
	go/token.gox \
	go/types.gox

toolexeclibgohashdir = $(toolexeclibgodir)/hash

toolexeclibgohash_DATA = \
	hash/adler32.gox \
	hash/crc32.gox \
	hash/crc64.gox \
	hash/fnv.gox

toolexeclibgohtmldir = $(toolexeclibgodir)/html

toolexeclibgohtml_DATA = \
	html/template.gox

toolexeclibgoimagedir = $(toolexeclibgodir)/image

toolexeclibgoimage_DATA = \
	image/color.gox \
	image/draw.gox \
	image/gif.gox \
	image/jpeg.gox \
	image/png.gox

toolexeclibgoimagecolordir = $(toolexeclibgoimagedir)/color

toolexeclibgoimagecolor_DATA = \
	image/color/palette.gox

toolexeclibgoindexdir = $(toolexeclibgodir)/index

toolexeclibgoindex_DATA = \
	index/suffixarray.gox

toolexeclibgoiodir = $(toolexeclibgodir)/io

toolexeclibgoio_DATA = \
	io/ioutil.gox

toolexeclibgologdir = $(toolexeclibgodir)/log

toolexeclibgolog_DATA = \
	log/syslog.gox

toolexeclibgomathdir = $(toolexeclibgodir)/math

toolexeclibgomath_DATA = \
	math/big.gox \
	math/cmplx.gox \
	math/rand.gox

toolexeclibgomimedir = $(toolexeclibgodir)/mime

toolexeclibgomime_DATA = \
	mime/multipart.gox \
	mime/quotedprintable.gox

toolexeclibgonetdir = $(toolexeclibgodir)/net

toolexeclibgonet_DATA = \
	net/http.gox \
	net/mail.gox \
	net/rpc.gox \
	net/smtp.gox \
	net/textproto.gox \
	net/url.gox

toolexeclibgonethttpdir = $(toolexeclibgonetdir)/http

toolexeclibgonethttp_DATA = \
	net/http/cgi.gox \
	net/http/cookiejar.gox \
	net/http/fcgi.gox \
	net/http/httptest.gox \
	net/http/httptrace.gox \
	net/http/httputil.gox \
	net/http/pprof.gox

toolexeclibgonetrpcdir = $(toolexeclibgonetdir)/rpc

toolexeclibgonetrpc_DATA = \
	net/rpc/jsonrpc.gox

toolexeclibgoolddir = $(toolexeclibgodir)/old

toolexeclibgoold_DATA = \
	old/regexp.gox \
	old/template.gox

toolexeclibgoosdir = $(toolexeclibgodir)/os

toolexeclibgoos_DATA = \
	os/exec.gox \
	os/signal.gox \
	os/user.gox

toolexeclibgopathdir = $(toolexeclibgodir)/path

toolexeclibgopath_DATA = \
	path/filepath.gox

toolexeclibgoregexpdir = $(toolexeclibgodir)/regexp

toolexeclibgoregexp_DATA = \
	regexp/syntax.gox

toolexeclibgoruntimedir = $(toolexeclibgodir)/runtime

toolexeclibgoruntime_DATA = \
	runtime/debug.gox \
	runtime/pprof.gox

toolexeclibgosyncdir = $(toolexeclibgodir)/sync

toolexeclibgosync_DATA = \
	sync/atomic.gox

toolexeclibgotestingdir = $(toolexeclibgodir)/testing

toolexeclibgotesting_DATA = \
	testing/iotest.gox \
	testing/quick.gox

toolexeclibgotextdir = $(toolexeclibgodir)/text

toolexeclibgotext_DATA = \
	text/scanner.gox \
	text/tabwriter.gox \
	text/template.gox

toolexeclibgotexttemplatedir = $(toolexeclibgotextdir)/template

toolexeclibgotexttemplate_DATA = \
	text/template/parse.gox

toolexeclibgounicodedir = $(toolexeclibgodir)/unicode

toolexeclibgounicode_DATA = \
	unicode/utf16.gox \
	unicode/utf8.gox

if HAVE_SYS_MMAN_H
runtime_mem_file = runtime/mem.c
else
runtime_mem_file = runtime/mem_posix_memalign.c
endif

if LIBGO_IS_RTEMS
rtems_task_variable_add_file = runtime/rtems-task-variable-add.c
else
rtems_task_variable_add_file =
endif

if LIBGO_IS_LINUX
runtime_thread_files = runtime/thread-linux.c
else
runtime_thread_files = runtime/thread-sema.c
endif

if LIBGO_IS_LINUX
runtime_getncpu_file = runtime/getncpu-linux.c
else
if LIBGO_IS_DARWIN
runtime_getncpu_file = runtime/getncpu-bsd.c
else
if LIBGO_IS_IRIX
runtime_getncpu_file = runtime/getncpu-irix.c
else
if LIBGO_IS_SOLARIS
runtime_getncpu_file = runtime/getncpu-solaris.c
else
if LIBGO_IS_FREEBSD
runtime_getncpu_file = runtime/getncpu-bsd.c
else
if LIBGO_IS_NETBSD
runtime_getncpu_file = runtime/getncpu-bsd.c
else
runtime_getncpu_file = runtime/getncpu-none.c
endif
endif
endif
endif
endif
endif

if LIBGO_IS_LINUX
runtime_netpoll_files = runtime/netpoll_epoll.c
else
if LIBGO_IS_SOLARIS
runtime_netpoll_files = runtime/netpoll_select.c
else
runtime_netpoll_files = runtime/netpoll_kqueue.c
endif
endif

runtime_files = \
	runtime/go-append.c \
	runtime/go-assert.c \
	runtime/go-assert-interface.c \
	runtime/go-breakpoint.c \
	runtime/go-caller.c \
	runtime/go-callers.c \
	runtime/go-can-convert-interface.c \
	runtime/go-cdiv.c \
	runtime/go-cgo.c \
	runtime/go-check-interface.c \
	runtime/go-construct-map.c \
	runtime/go-convert-interface.c \
	runtime/go-copy.c \
	runtime/go-defer.c \
	runtime/go-deferred-recover.c \
	runtime/go-eface-compare.c \
	runtime/go-eface-val-compare.c \
	runtime/go-ffi.c \
	runtime/go-fieldtrack.c \
	runtime/go-interface-compare.c \
	runtime/go-interface-eface-compare.c \
	runtime/go-interface-val-compare.c \
	runtime/go-make-slice.c \
	runtime/go-matherr.c \
	runtime/go-memclr.c \
	runtime/go-memcmp.c \
	runtime/go-memequal.c \
	runtime/go-memmove.c \
	runtime/go-nanotime.c \
	runtime/go-now.c \
	runtime/go-new.c \
	runtime/go-nosys.c \
	runtime/go-panic.c \
	runtime/go-recover.c \
	runtime/go-reflect-call.c \
	runtime/go-runtime-error.c \
	runtime/go-setenv.c \
	runtime/go-signal.c \
	runtime/go-strcmp.c \
	runtime/go-strslice.c \
	runtime/go-type-complex.c \
	runtime/go-type-eface.c \
	runtime/go-type-float.c \
	runtime/go-type-identity.c \
	runtime/go-type-interface.c \
	runtime/go-type-string.c \
	runtime/go-typedesc-equal.c \
	runtime/go-unsafe-new.c \
	runtime/go-unsafe-newarray.c \
	runtime/go-unsafe-pointer.c \
	runtime/go-unsetenv.c \
	runtime/go-unwind.c \
	runtime/go-varargs.c \
	runtime/env_posix.c \
	runtime/heapdump.c \
	runtime/mcache.c \
	runtime/mcentral.c \
	$(runtime_mem_file) \
	runtime/mfixalloc.c \
	runtime/mgc0.c \
	runtime/mheap.c \
	runtime/msize.c \
	$(runtime_netpoll_files) \
	runtime/panic.c \
	runtime/parfor.c \
	runtime/print.c \
	runtime/proc.c \
	runtime/runtime.c \
	runtime/signal_unix.c \
	runtime/thread.c \
	$(runtime_thread_files) \
	runtime/yield.c \
	$(rtems_task_variable_add_file) \
	go-iface.c \
	lfstack.c \
	malloc.c \
	netpoll.c \
	reflect.c \
	runtime1.c \
	sigqueue.c \
	$(runtime_getncpu_file)

goc2c.$(OBJEXT): runtime/goc2c.c
	$(CC_FOR_BUILD) -c $(CFLAGS_FOR_BUILD) $<

goc2c: goc2c.$(OBJEXT)
	$(CC_FOR_BUILD) $(CFLAGS_FOR_BUILD) $(LDFLAGS_FOR_BUILD) -o $@ $<

malloc.c: $(srcdir)/runtime/malloc.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

mprof.c: $(srcdir)/runtime/mprof.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

netpoll.c: $(srcdir)/runtime/netpoll.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

reflect.c: $(srcdir)/runtime/reflect.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

runtime1.c: $(srcdir)/runtime/runtime1.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

sema.c: $(srcdir)/runtime/sema.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

sigqueue.c: $(srcdir)/runtime/sigqueue.goc goc2c
	./goc2c --go-pkgpath os_signal $< > $@.tmp
	mv -f $@.tmp $@

time.c: $(srcdir)/runtime/time.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

%.c: $(srcdir)/runtime/%.goc goc2c
	./goc2c $< > $@.tmp
	mv -f $@.tmp $@

version.go: s-version; @true
s-version: Makefile
	rm -f version.go.tmp
	echo "package sys" > version.go.tmp
	echo 'const DefaultGoroot = "$(prefix)"' >> version.go.tmp
	echo 'const TheVersion = "'`cat $(srcdir)/VERSION | sed 1q`' '`$(GOC) --version | sed 1q`'"' >> version.go.tmp
	echo 'const GOARCH = "'$(GOARCH)'"' >> version.go.tmp
	echo 'const GOOS = "'$(GOOS)'"' >> version.go.tmp
	echo 'const GccgoToolDir = "$(libexecsubdir)"' >> version.go.tmp
	echo >> version.go.tmp
	echo "type ArchFamilyType int" >> version.go.tmp
	echo >> version.go.tmp
	echo "const (" >> version.go.tmp
	echo "	UNKNOWN ArchFamilyType = iota" >> version.go.tmp
	for a in $(ALLGOARCHFAMILY); do \
	  echo "	$${a}" >> version.go.tmp; \
	done
	echo ")" >> version.go.tmp
	echo >> version.go.tmp
	for a in $(ALLGOARCH); do \
	  f=`echo $${a} | sed -e 's/\(.\).*/\1/' -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`; \
	  n="$${f}`echo $${a} | sed -e 's/.//'`"; \
	  if test "$${a}" = "$(GOARCH)"; then \
	    echo "const Goarch$${n} = 1" >> version.go.tmp; \
	  else \
	    echo "const Goarch$${n} = 0" >> version.go.tmp; \
	  fi; \
	done
	echo >> version.go.tmp
	echo "const (" >> version.go.tmp
	echo "	ArchFamily = $(GOARCH_FAMILY)" >> version.go.tmp
	echo "	BigEndian = $(GOARCH_BIGENDIAN)" >> version.go.tmp
	echo "	CacheLineSize = $(GOARCH_CACHELINESIZE)" >> version.go.tmp
	echo "	PhysPageSize = $(GOARCH_PHYSPAGESIZE)" >> version.go.tmp
	echo "	PCQuantum = $(GOARCH_PCQUANTUM)" >> version.go.tmp
	echo "	Int64Align = $(GOARCH_INT64ALIGN)" >> version.go.tmp
	echo "	HugePageSize = $(GOARCH_HUGEPAGESIZE)" >> version.go.tmp
	echo "	MinFrameSize = $(GOARCH_MINFRAMESIZE)" >> version.go.tmp
	echo ")" >> version.go.tmp
	echo >> version.go.tmp
	for a in $(ALLGOOS); do \
	  f=`echo $${a} | sed -e 's/\(.\).*/\1/' -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`; \
	  n="$${f}`echo $${a} | sed -e 's/.//'`"; \
	  if test "$${a}" = "$(GOOS)"; then \
	    echo "const Goos$${n} = 1" >> version.go.tmp; \
	  else \
	    echo "const Goos$${n} = 0" >> version.go.tmp; \
	  fi; \
	done
	echo >> version.go.tmp
	echo "type Uintreg uintptr" >> version.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh version.go.tmp version.go
	$(STAMP) $@

runtime_sysinfo.go: s-runtime_sysinfo; @true
s-runtime_sysinfo: $(srcdir)/mkrsysinfo.sh gen-sysinfo.go
	$(SHELL) $(srcdir)/mkrsysinfo.sh
	$(SHELL) $(srcdir)/mvifdiff.sh tmp-runtime_sysinfo.go runtime_sysinfo.go
	$(STAMP) $@

noinst_DATA = zstdpkglist.go

# Generate the list of go std packages that were included in libgo
zstdpkglist.go: s-zstdpkglist; @true
s-zstdpkglist: Makefile
	rm -f zstdpkglist.go.tmp
	echo 'package main' > zstdpkglist.go.tmp
	echo "" >> zstdpkglist.go.tmp
	echo 'var stdpkg = map[string]bool{' >> zstdpkglist.go.tmp
	echo $(libgo_go_objs) 'unsafe.lo' 'runtime/cgo.lo' | sed 's/\.lo /\": true,\n/g' | sed 's/\.lo/\": true,/' | sed 's/-go//' | grep -v _c | sed 's/^/\t\"/' | sort | uniq >> zstdpkglist.go.tmp
	echo '}' >> zstdpkglist.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh zstdpkglist.go.tmp zstdpkglist.go
	$(STAMP) $@

if LIBGO_IS_LINUX
syscall_epoll_file = epoll.go
else
syscall_epoll_file =
endif

extra_go_files_syscall = \
	libcalls.go \
	sysinfo.go \
	syscall_arch.go \
	$(syscall_epoll_file)

libcalls.go: s-libcalls; @true
s-libcalls: libcalls-list go/syscall/mksyscall.awk $(srcdir)/go/syscall/*.go
	rm -f libcalls.go.tmp
	$(AWK) -f $(srcdir)/go/syscall/mksyscall.awk `cat libcalls-list` > libcalls.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh libcalls.go.tmp libcalls.go
	$(STAMP) $@

libcalls-list: s-libcalls-list; @true
s-libcalls-list: Makefile $(srcdir)/go/syscall/*.go
	rm -f libcalls-list.tmp
	$(SHELL) $(srcdir)/match.sh --goarch=$(GOARCH) --goos=$(GOOS) --srcdir=$(srcdir)/go/syscall $(matchargs_syscall) > libcalls-list.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh libcalls-list.tmp libcalls-list
	$(STAMP) $@

syscall_arch.go: s-syscall_arch; @true
s-syscall_arch: Makefile
	rm -f syscall_arch.go.tmp
	echo "package syscall" > syscall_arch.go.tmp
	echo 'const ARCH = "'$(GOARCH)'"' >> syscall_arch.go.tmp
	echo 'const OS = "'$(GOOS)'"' >> syscall_arch.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh syscall_arch.go.tmp syscall_arch.go
	$(STAMP) $@

SYSINFO_FLAGS = \
	$(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(CPPFLAGS) $(OSCFLAGS) -O

gen-sysinfo.go: s-gen-sysinfo; @true
s-gen-sysinfo: $(srcdir)/sysinfo.c config.h
	$(CC) $(SYSINFO_FLAGS) -fdump-go-spec=tmp-gen-sysinfo.go -std=gnu99 -S -o sysinfo.s $(srcdir)/sysinfo.c
	rm -f sysinfo.s
	$(SHELL) $(srcdir)/mvifdiff.sh tmp-gen-sysinfo.go gen-sysinfo.go
	$(STAMP) $@

errno.i: s-errno; @true
s-errno:
	echo '#include <errno.h>' | $(CC) $(SYSINFO_FLAGS) -x c - -E -dM > tmp-errno.i
	$(SHELL) $(srcdir)/mvifdiff.sh tmp-errno.i errno.i
	$(STAMP) $@

sysinfo.go: s-sysinfo; @true
s-sysinfo: $(srcdir)/mksysinfo.sh gen-sysinfo.go errno.i
	$(SHELL) $(srcdir)/mksysinfo.sh
	$(SHELL) $(srcdir)/mvifdiff.sh tmp-sysinfo.go sysinfo.go
	$(STAMP) $@

# The epoll struct has an embedded union and is packed on x86_64,
# which is too complicated for mksysinfo.sh.  We find the offset of
# the only field we care about in configure.ac, and generate the
# struct here.
epoll.go: s-epoll; @true
s-epoll: Makefile
	rm -f epoll.go.tmp
	echo 'package syscall' > epoll.go.tmp
	echo 'type EpollEvent struct {' >> epoll.go.tmp
	echo '	Events uint32' >> epoll.go.tmp
	case "$(SIZEOF_STRUCT_EPOLL_EVENT),$(STRUCT_EPOLL_EVENT_FD_OFFSET)" in \
	0,0) echo 1>&2 "*** struct epoll_event data.fd offset unknown"; \
	   exit 1; ;; \
	8,4) echo '	Fd int32' >> epoll.go.tmp; ;; \
	12,4) echo '	Fd int32' >> epoll.go.tmp; \
	   echo '	Pad [4]byte' >> epoll.go.tmp; ;; \
	12,8) echo '	Pad [4]byte' >> epoll.go.tmp; \
	   echo '	Fd int32' >> epoll.go.tmp; ;; \
	16,8) echo '	Pad [4]byte' >> epoll.go.tmp; \
	   echo '	Fd int32' >> epoll.go.tmp; \
	   echo '	Pad2 [4]byte' >> epoll.go.tmp; ;; \
	*) echo 1>&2 "*** struct epoll_event unsupported"; \
	   exit 1; ;; \
	esac
	echo '}' >> epoll.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh epoll.go.tmp epoll.go
	$(STAMP) $@

if LIBGO_IS_LINUX
syscall_lib_clone_lo = syscall/clone_linux.lo
else
syscall_lib_clone_lo =
endif

libgo_go_objs = \
	bufio.lo \
	bytes.lo \
	bytes/index.lo \
	context.lo \
	crypto.lo \
	encoding.lo \
	errors.lo \
	expvar.lo \
	flag.lo \
	fmt.lo \
	hash.lo \
	html.lo \
	image.lo \
	io.lo \
	log.lo \
	math.lo \
	mime.lo \
	net.lo \
	os.lo \
	path.lo \
	reflect-go.lo \
	reflect/makefunc_ffi_c.lo \
	regexp.lo \
	runtime-go.lo \
	sort.lo \
	strconv.lo \
	strings.lo \
	strings/index.lo \
	sync.lo \
	syscall.lo \
	$(syscall_lib_clone_lo) \
	syscall/errno.lo \
	syscall/signame.lo \
	syscall/wait.lo \
	testing.lo \
	time-go.lo \
	unicode.lo \
	archive/tar.lo \
	archive/zip.lo \
	compress/bzip2.lo \
	compress/flate.lo \
	compress/gzip.lo \
	compress/lzw.lo \
	compress/zlib.lo \
	container/heap.lo \
	container/list.lo \
	container/ring.lo \
	crypto/aes.lo \
	crypto/cipher.lo \
	crypto/des.lo \
	crypto/dsa.lo \
	crypto/ecdsa.lo \
	crypto/elliptic.lo \
	crypto/hmac.lo \
	crypto/md5.lo \
	crypto/rand.lo \
	crypto/rc4.lo \
	crypto/rsa.lo \
	crypto/sha1.lo \
	crypto/sha256.lo \
	crypto/sha512.lo \
	crypto/subtle.lo \
	crypto/tls.lo \
	crypto/x509.lo \
	crypto/x509/pkix.lo \
	database/sql.lo \
	database/sql/driver.lo \
	debug/dwarf.lo \
	debug/elf.lo \
	debug/gosym.lo \
	debug/macho.lo \
	debug/pe.lo \
	debug/plan9obj.lo \
	encoding/ascii85.lo \
	encoding/asn1.lo \
	encoding/base32.lo \
	encoding/base64.lo \
	encoding/binary.lo \
	encoding/csv.lo \
	encoding/gob.lo \
	encoding/hex.lo \
	encoding/json.lo \
	encoding/pem.lo \
	encoding/xml.lo \
	exp/proxy.lo \
	exp/terminal.lo \
	html/template.lo \
	go/ast.lo \
	go/build.lo \
	go/constant.lo \
	go/doc.lo \
	go/format.lo \
	go/importer.lo \
	go/internal/gcimporter.lo \
	go/internal/gccgoimporter.lo \
	go/parser.lo \
	go/printer.lo \
	go/scanner.lo \
	go/token.lo \
	go/types.lo \
	golang_org/x/net/http2/hpack.lo \
	golang_org/x/net/lex/httplex.lo \
	$(golang_org_x_net_route_lo) \
	hash/adler32.lo \
	hash/crc32.lo \
	hash/crc64.lo \
	hash/fnv.lo \
	net/http/cgi.lo \
	net/http/cookiejar.lo \
	net/http/fcgi.lo \
	net/http/httptest.lo \
	net/http/httptrace.lo \
	net/http/httputil.lo \
	net/http/internal.lo \
	net/http/pprof.lo \
	image/color.lo \
	image/color/palette.lo \
	image/draw.lo \
	image/gif.lo \
	image/internal/imageutil.lo \
	image/jpeg.lo \
	image/png.lo \
	index/suffixarray.lo \
	internal/nettrace.lo \
	internal/race.lo \
	internal/singleflight.lo \
	internal/syscall/unix.lo \
	internal/testenv.lo \
	internal/trace.lo \
	io/ioutil.lo \
	log/syslog.lo \
	log/syslog/syslog_c.lo \
	math/big.lo \
	math/cmplx.lo \
	math/rand.lo \
	mime/multipart.lo \
	mime/quotedprintable.lo \
	net/http.lo \
	net/internal/socktest.lo \
	net/mail.lo \
	net/rpc.lo \
	net/smtp.lo \
	net/textproto.lo \
	net/url.lo \
	old/regexp.lo \
	old/template.lo \
	os/exec.lo \
	$(os_lib_inotify_lo) \
	os/signal.lo \
	os/user.lo \
	path/filepath.lo \
	regexp/syntax.lo \
	net/rpc/jsonrpc.lo \
	runtime/debug.lo \
	runtime/pprof.lo \
	runtime/internal/atomic.lo \
	runtime/internal/atomic_c.lo \
	runtime/internal/sys.lo \
	sync/atomic.lo \
	sync/atomic_c.lo \
	text/scanner.lo \
	text/tabwriter.lo \
	text/template.lo \
	text/template/parse.lo \
	testing/iotest.lo \
	testing/quick.lo \
	unicode/utf16.lo \
	unicode/utf8.lo

libgo_ldflags = \
	-version-info $(libtool_VERSION) $(PTHREAD_CFLAGS) $(AM_LDFLAGS)

libgo_libadd = \
	$(libgo_go_objs) ../libbacktrace/libbacktrace.la \
	$(LIBATOMIC) $(LIBFFI) $(PTHREAD_LIBS) $(MATH_LIBS) $(NET_LIBS)

libgo_la_SOURCES = $(runtime_files)
libgo_la_LDFLAGS = $(libgo_ldflags)
libgo_la_LIBADD = $(libgo_libadd)

libgo_llgo_la_SOURCES = $(runtime_files)
libgo_llgo_la_LDFLAGS = $(libgo_ldflags)
libgo_llgo_la_LIBADD = $(libgo_libadd)

libgobegin_a_SOURCES = \
	runtime/go-main.c

libgobegin_llgo_a_SOURCES = \
	runtime/go-main.c

# Use -fPIC for libgobegin so that it can be put in a PIE.
libgobegin_a_CFLAGS = $(AM_CFLAGS) -fPIC
libgobegin_llgo_a_CFLAGS = $(AM_CFLAGS) -fPIC

libgolibbegin_a_SOURCES = \
	runtime/go-libmain.c

libgolibbegin_a_CFLAGS = $(AM_CFLAGS) -fPIC

libnetgo_a_SOURCES =
libnetgo_a_LIBADD = netgo.o

# Make sure runtime.inc is built before compiling any .c file.
$(libgo_la_OBJECTS): runtime.inc
$(libgo_llgo_la_OBJECTS): runtime.inc
$(libgobegin_a_OBJECTS): runtime.inc
$(libgobegin_llgo_a_OBJECTS): runtime.inc
$(libgolibbegin_a_OBJECTS): runtime.inc

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

GOCFLAGS = $(CFLAGS)
AM_GOCFLAGS = $(STRINGOPS_FLAG) $(GO_SPLIT_STACK)
GOCOMPILE = $(GOC) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_GOCFLAGS) $(GOCFLAGS)

LTGOCOMPILE = $(LIBTOOL) --tag GO --mode=compile $(GOC) $(INCLUDES) \
	$(AM_GOCFLAGS) $(GOCFLAGS)

GOLINK = $(LIBTOOL) --tag GO --mode-link $(GOC) \
	$(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_GOCFLAGS) $(LTLDFLAGS) -o $@

# Build the dependencies for a Go package.
BUILDDEPS = \
	$(MKDIR_P) $(@D); \
	dir=`echo $@ | sed -e 's/.lo.dep$$//' -e 's/-go//'`; \
	files=`$(SHELL) $(srcdir)/match.sh --goarch=$(GOARCH) --goos=$(GOOS) --srcdir=$(srcdir)/go/$$dir --extrafiles="$(extra_go_files_$(subst /,_,$(subst -go,,$(subst .lo.dep,,$@))))" $(matchargs_$(subst /,_,$(subst -go,,$(subst .lo.dep,,$@))))`; \
	$(SHELL) $(srcdir)/godeps.sh `echo $@ | sed -e 's/.dep$$//'` $$files > $@.tmp; \
	if ! cmp $@.tmp $@ >/dev/null 2>/dev/null; then \
	  rm -f `echo $@ | sed -e 's/\.dep$$//'`; \
	fi; \
	mv -f $@.tmp $@

# Build the .go files for a package, generating a .lo file.
BUILDPACKAGE = \
	$(MKDIR_P) $(@D); \
	files=`echo $^ | sed -e 's/[^ ]*\.gox//g' -e 's/[^ ]*\.dep//'`; \
	$(LTGOCOMPILE) -I . -c -fgo-pkgpath=`echo $@ | sed -e 's/.lo$$//' -e 's/-go$$//'` $($(subst -,_,$(subst .,_,$(subst /,_,$@)))_GOCFLAGS) -o $@ $$files

# Build deps for netgo.o.
BUILDNETGODEPS = \
	$(MKDIR_P) $(@D); \
	files=`$(SHELL) $(srcdir)/match.sh --nocgo --tag=netgo --goarch=$(GOARCH) --goos=$(GOOS) --srcdir=$(srcdir)/go/net`; \
	$(SHELL) $(srcdir)/godeps.sh netgo.o $$files > $@.tmp; \
	mv -f $@.tmp $@

# Build netgo.o.
BUILDNETGO = \
	$(MKDIR_P) $(@D); \
	files=`echo $^ | sed -e 's/[^ ]*\.gox//g' -e 's/[^ ]*\.dep//'`; \
	$(GOCOMPILE) -I . -c -fPIC -fgo-pkgpath=net -o $@ $$files

GOTESTFLAGS =
GOBENCH = 

# Check a package.
CHECK = \
	GC="$(GOC) $(GOCFLAGS) $($(subst /,_,$@)_GOCFLAGS) -L `${PWD_COMMAND}` -L `${PWD_COMMAND}`/.libs"; \
	export GC; \
	GOLIBS="$(MATH_LIBS) $(NET_LIBS) $(LIBS)"; \
	export GOLIBS; \
	RUNTESTFLAGS="$(RUNTESTFLAGS)"; \
	export RUNTESTFLAGS; \
	MAKE="$(MAKE)"; \
	export MAKE; \
	libgccdir=`${GOC} -print-libgcc-file-name | sed -e 's|/[^/]*$$||'`; \
	LD_LIBRARY_PATH="`${PWD_COMMAND}`/.libs:$${libgccdir}:${LD_LIBRARY_PATH}"; \
	LD_LIBRARY_PATH=`echo $${LD_LIBRARY_PATH} | sed 's,::*,:,g;s,^:*,,;s,:*$$,,'`; \
	export LD_LIBRARY_PATH; \
	$(MKDIR_P) $(@D); \
	rm -f $@-testsum $@-testlog; \
	files=`$(SHELL) $(srcdir)/match.sh --goarch=$(GOARCH) --goos=$(GOOS) --srcdir=$(srcdir)/go/$(@D) --extrafiles="$(extra_go_files_$(subst /,_,$(@D)))" $(matchargs_$(subst /,_,$(@D)))`; \
	if test "$(USE_DEJAGNU)" = "yes"; then \
	  $(SHELL) $(srcdir)/testsuite/gotest --goarch=$(GOARCH) --goos=$(GOOS) --dejagnu=yes --basedir=$(srcdir) --srcdir=$(srcdir)/go/$(@D) --pkgpath="$(@D)" --pkgfiles="$$files" --testname="$(@D)" $(GOTESTFLAGS); \
	elif test "$(GOBENCH)" != ""; then \
	  $(SHELL) $(srcdir)/testsuite/gotest --goarch=$(GOARCH) --goos=$(GOOS) --basedir=$(srcdir) --srcdir=$(srcdir)/go/$(@D) --pkgpath="$(@D)" --pkgfiles="$$files" --bench="$(GOBENCH)" $(GOTESTFLAGS); \
	else \
	  if $(SHELL) $(srcdir)/testsuite/gotest --goarch=$(GOARCH) --goos=$(GOOS) --basedir=$(srcdir) --srcdir=$(srcdir)/go/$(@D) --pkgpath="$(@D)" --pkgfiles="$$files" $(GOTESTFLAGS) >>$@-testlog 2>&1; then \
	    echo "PASS: $(@D)" >> $@-testlog; \
	    echo "PASS: $(@D)"; \
	    echo "PASS: $(@D)" > $@-testsum; \
	  else \
	    echo "FAIL: $(@D)" >> $@-testlog; \
	    cat $@-testlog; \
	    echo "FAIL: $(@D)" > $@-testsum; \
	    exit 1; \
	  fi; \
	fi

# Build all packages before checking any.
CHECK_DEPS = \
	$(toolexeclibgo_DATA) \
	$(toolexeclibgoarchive_DATA) \
	$(toolexeclibgocompress_DATA) \
	$(toolexeclibgocontainer_DATA) \
	$(toolexeclibgocrypto_DATA) \
	$(toolexeclibgodebug_DATA) \
	$(toolexeclibgoencoding_DATA) \
	$(toolexeclibgoexp_DATA) \
	$(toolexeclibgogo_DATA) \
	$(toolexeclibgohash_DATA) \
	$(toolexeclibgoimage_DATA) \
	$(toolexeclibgoindex_DATA) \
	$(toolexeclibgoio_DATA) \
	$(toolexeclibgolog_DATA) \
	$(toolexeclibgomath_DATA) \
	$(toolexeclibgomime_DATA) \
	$(toolexeclibgonet_DATA) \
	$(toolexeclibgonethttp_DATA) \
	$(toolexeclibgoos_DATA) \
	$(toolexeclibgopath_DATA) \
	$(toolexeclibgorpc_DATA) \
	$(toolexeclibgoruntime_DATA) \
	$(toolexeclibgosync_DATA) \
	$(toolexeclibgotesting_DATA) \
	$(toolexeclibgotext_DATA) \
	$(toolexeclibgotexttemplate_DATA) \
	$(toolexeclibgounicode_DATA)

if GOC_IS_LLGO
CHECK_DEPS += libgo-llgo.la libgobegin-llgo.a
else
CHECK_DEPS += libgo.la libgobegin.a
endif

@go_include@ bufio.lo.dep
bufio.lo.dep: $(srcdir)/go/bufio/*.go
	$(BUILDDEPS)
bufio.lo:
	$(BUILDPACKAGE)
bufio/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: bufio/check

@go_include@ bytes.lo.dep
bytes.lo.dep: $(srcdir)/go/bytes/*.go
	$(BUILDDEPS)
bytes.lo:
	$(BUILDPACKAGE)
bytes/index.lo: go/bytes/indexbyte.c runtime.inc
	@$(MKDIR_P) bytes
	$(LTCOMPILE) -c -o bytes/index.lo $(srcdir)/go/bytes/indexbyte.c
bytes/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: bytes/check

@go_include@ context.lo.dep
context.lo.dep: $(srcdir)/go/context/*.go
	$(BUILDDEPS)
context.lo:
	$(BUILDPACKAGE)
context/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: context/check

@go_include@ crypto.lo.dep
crypto.lo.dep: $(srcdir)/go/crypto/*.go
	$(BUILDDEPS)
crypto.lo:
	$(BUILDPACKAGE)
crypto/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/check

@go_include@ encoding.lo.dep
encoding.lo.dep: $(srcdir)/go/encoding/*.go
	$(BUILDDEPS)
encoding.lo:
	$(BUILDPACKAGE)
encoding/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/check

@go_include@ errors.lo.dep
errors.lo.dep: $(srcdir)/go/errors/*.go
	$(BUILDDEPS)
errors.lo:
	$(BUILDPACKAGE)
errors/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: errors/check

@go_include@ expvar.lo.dep
expvar.lo.dep: $(srcdir)/go/expvar/*.go
	$(BUILDDEPS)
expvar.lo:
	$(BUILDPACKAGE)
expvar/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: expvar/check

@go_include@ flag.lo.dep
flag.lo.dep: $(srcdir)/go/flag/*.go
	$(BUILDDEPS)
flag.lo:
	$(BUILDPACKAGE)
flag/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: flag/check

@go_include@ fmt.lo.dep
fmt.lo.dep: $(srcdir)/go/fmt/*.go
	$(BUILDDEPS)
fmt.lo:
	$(BUILDPACKAGE)
fmt/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: fmt/check

@go_include@ hash.lo.dep
hash.lo.dep: $(srcdir)/go/hash/*.go
	$(BUILDDEPS)
hash.lo:
	$(BUILDPACKAGE)
hash/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: hash/check

@go_include@ html.lo.dep
html.lo.dep: $(srcdir)/go/html/*.go
	$(BUILDDEPS)
html.lo:
	$(BUILDPACKAGE)
html/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: html/check

@go_include@ image.lo.dep
image.lo.dep: $(srcdir)/go/image/*.go
	$(BUILDDEPS)
image.lo:
	$(BUILDPACKAGE)
image/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/check

@go_include@ io.lo.dep
io.lo.dep: $(srcdir)/go/io/*.go
	$(BUILDDEPS)
io.lo:
	$(BUILDPACKAGE)
io/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: io/check

@go_include@ log.lo.dep
log.lo.dep: $(srcdir)/go/log/*.go
	$(BUILDDEPS)
log.lo:
	$(BUILDPACKAGE)
log/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: log/check

@go_include@ math.lo.dep
math.lo.dep: $(srcdir)/go/math/*.go
	$(BUILDDEPS)
math.lo:
	$(MKDIR_P) $(@D)
	files=`echo $^ | sed -e 's/[^ ]*\.gox//g'`; \
	$(LTGOCOMPILE) $(MATH_FLAG) -I . -c -fgo-pkgpath=math -o $@ $$files
math/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: math/check

@go_include@ mime.lo.dep
mime.lo.dep: $(srcdir)/go/mime/*.go
	$(BUILDDEPS)
mime.lo:
	$(BUILDPACKAGE)
mime/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: mime/check

@go_include@ net.lo.dep
net.lo.dep: $(srcdir)/go/net/*.go
	$(BUILDDEPS)
net.lo:
	$(BUILDPACKAGE)
net/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/check

@go_include@ netgo.o.dep
netgo.o.dep: $(srcdir)/go/net/*.go
	$(BUILDNETGODEPS)
netgo.o: netgo.o.dep
	$(BUILDNETGO)

if LIBGO_IS_SOLARIS
if HAVE_STAT_TIMESPEC
matchargs_os = --tag=solaristag
else
matchargs_os =
endif
else
matchargs_os =
endif

@go_include@ os.lo.dep
os.lo.dep: $(srcdir)/go/os/*.go
	$(BUILDDEPS)
os.lo:
	$(BUILDPACKAGE)
os/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: os/check

@go_include@ path.lo.dep
path.lo.dep: $(srcdir)/go/path/*.go
	$(BUILDDEPS)
path.lo:
	$(BUILDPACKAGE)
path/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: path/check

@go_include@ reflect-go.lo.dep
reflect-go.lo.dep: $(srcdir)/go/reflect/*.go
	$(BUILDDEPS)
reflect-go.lo:
	$(BUILDPACKAGE)
reflect/check: $(CHECK_DEPS)
	@$(CHECK)
reflect/makefunc_ffi_c.lo: go/reflect/makefunc_ffi_c.c runtime.inc
	@$(MKDIR_P) reflect
	$(LTCOMPILE) -c -o $@ $<
.PHONY: reflect/check

@go_include@ regexp.lo.dep
regexp.lo.dep: $(srcdir)/go/regexp/*.go
	$(BUILDDEPS)
regexp.lo:
	$(BUILDPACKAGE)
regexp/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: regexp/check

extra_go_files_runtime = runtime_sysinfo.go

@go_include@ runtime-go.lo.dep
runtime-go.lo.dep: $(srcdir)/go/runtime/*.go $(extra_go_files_runtime)
	$(BUILDDEPS)
runtime_go_lo_GOCFLAGS = -fgo-c-header=runtime.inc.tmp -fgo-compiling-runtime
runtime-go.lo:
	$(BUILDPACKAGE)
runtime.inc: s-runtime-inc; @true
s-runtime-inc: runtime-go.lo Makefile
	rm -f runtime.inc.tmp2
	grep -v "#define _" runtime.inc.tmp > runtime.inc.tmp2
	for pattern in '_G[a-z]' '_P[a-z]' _Max _Lock _Sig _Trace _MHeap _Num; do \
	  grep "#define $$pattern" runtime.inc.tmp >> runtime.inc.tmp2; \
	done
	$(SHELL) $(srcdir)/mvifdiff.sh runtime.inc.tmp2 runtime.inc
	$(STAMP) $@
runtime_check_GOCFLAGS = -fgo-compiling-runtime
runtime/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: runtime/check

@go_include@ sort.lo.dep
sort.lo.dep: $(srcdir)/go/sort/*.go
	$(BUILDDEPS)
sort.lo:
	$(BUILDPACKAGE)
sort/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: sort/check

@go_include@ strconv.lo.dep
strconv.lo.dep: $(srcdir)/go/strconv/*.go
	$(BUILDDEPS)
strconv.lo:
	$(BUILDPACKAGE)
strconv/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: strconv/check

@go_include@ strings.lo.dep
strings.lo.dep: $(srcdir)/go/strings/*.go
	$(BUILDDEPS)
strings.lo:
	$(BUILDPACKAGE)
strings/index.lo: go/strings/indexbyte.c runtime.inc
	@$(MKDIR_P) strings
	$(LTCOMPILE) -c -o strings/index.lo $(srcdir)/go/strings/indexbyte.c
strings/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: strings/check

@go_include@ sync.lo.dep
sync.lo.dep: $(srcdir)/go/sync/*.go
	$(BUILDDEPS)
sync.lo:
	$(BUILDPACKAGE)
sync/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: sync/check

@go_include@ testing.lo.dep
testing.lo.dep: $(srcdir)/go/testing/*.go
	$(BUILDDEPS)
testing.lo:
	$(BUILDPACKAGE)
testing/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: testing/check

@go_include@ time-go.lo.dep
time-go.lo.dep: $(srcdir)/go/time/*.go
	$(BUILDDEPS)
time-go.lo:
	$(BUILDPACKAGE)
time/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: time/check

@go_include@ unicode.lo.dep
unicode.lo.dep: $(srcdir)/go/unicode/*.go
	$(BUILDDEPS)
unicode.lo:
	$(BUILDPACKAGE)
unicode/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: unicode/check

@go_include@ archive/tar.lo.dep
archive/tar.lo.dep: $(srcdir)/go/archive/tar/*.go
	$(BUILDDEPS)
archive/tar.lo:
	$(BUILDPACKAGE)
archive/tar/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: archive/tar/check

@go_include@ archive/zip.lo.dep
archive/zip.lo.dep: $(srcdir)/go/archive/zip/*.go
	$(BUILDDEPS)
archive/zip.lo:
	$(BUILDPACKAGE)
archive/zip/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: archive/zip/check

@go_include@ compress/bzip2.lo.dep
compress/bzip2.lo.dep: $(srcdir)/go/compress/bzip2/*.go
	$(BUILDDEPS)
compress/bzip2.lo:
	$(BUILDPACKAGE)
compress/bzip2/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: compress/bzip2/check

@go_include@ compress/flate.lo.dep
compress/flate.lo.dep: $(srcdir)/go/compress/flate/*.go
	$(BUILDDEPS)
compress/flate.lo:
	$(BUILDPACKAGE)
compress/flate/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: compress/flate/check

@go_include@ compress/gzip.lo.dep
compress/gzip.lo.dep: $(srcdir)/go/compress/gzip/*.go
	$(BUILDDEPS)
compress/gzip.lo:
	$(BUILDPACKAGE)
compress/gzip/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: compress/gzip/check

@go_include@ compress/lzw.lo.dep
compress/lzw.lo.dep: $(srcdir)/go/compress/lzw/*.go
	$(BUILDDEPS)
compress/lzw.lo:
	$(BUILDPACKAGE)
compress/lzw/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: compress/lzw/check

@go_include@ compress/zlib.lo.dep
compress/zlib.lo.dep: $(srcdir)/go/compress/zlib/*.go
	$(BUILDDEPS)
compress/zlib.lo:
	$(BUILDPACKAGE)
compress/zlib/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: compress/zlib/check

@go_include@ container/heap.lo.dep
container/heap.lo.dep: $(srcdir)/go/container/heap/*.go
	$(BUILDDEPS)
container/heap.lo:
	$(BUILDPACKAGE)
container/heap/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: container/heap/check

@go_include@ container/list.lo.dep
container/list.lo.dep: $(srcdir)/go/container/list/*.go
	$(BUILDDEPS)
container/list.lo:
	$(BUILDPACKAGE)
container/list/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: container/list/check

@go_include@ container/ring.lo.dep
container/ring.lo.dep: $(srcdir)/go/container/ring/*.go
	$(BUILDDEPS)
container/ring.lo:
	$(BUILDPACKAGE)
container/ring/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: container/ring/check

@go_include@ crypto/aes.lo.dep
crypto/aes.lo.dep: $(srcdir)/go/crypto/aes/*.go
	$(BUILDDEPS)
crypto/aes.lo:
	$(BUILDPACKAGE)
crypto/aes/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/aes/check

@go_include@ crypto/cipher.lo.dep
crypto/cipher.lo.dep: $(srcdir)/go/crypto/cipher/*.go
	$(BUILDDEPS)
crypto/cipher.lo:
	$(BUILDPACKAGE)
crypto/cipher/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/cipher/check

@go_include@ crypto/des.lo.dep
crypto/des.lo.dep: $(srcdir)/go/crypto/des/*.go
	$(BUILDDEPS)
crypto/des.lo:
	$(BUILDPACKAGE)
crypto/des/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/des/check

@go_include@ crypto/dsa.lo.dep
crypto/dsa.lo.dep: $(srcdir)/go/crypto/dsa/*.go
	$(BUILDDEPS)
crypto/dsa.lo:
	$(BUILDPACKAGE)
crypto/dsa/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/dsa/check

@go_include@ crypto/ecdsa.lo.dep
crypto/ecdsa.lo.dep: $(srcdir)/go/crypto/ecdsa/*.go
	$(BUILDDEPS)
crypto/ecdsa.lo:
	$(BUILDPACKAGE)
crypto/ecdsa/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/ecdsa/check

@go_include@ crypto/elliptic.lo.dep
crypto/elliptic.lo.dep: $(srcdir)/go/crypto/elliptic/*.go
	$(BUILDDEPS)
crypto/elliptic.lo:
	$(BUILDPACKAGE)
crypto/elliptic/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/elliptic/check

@go_include@ crypto/hmac.lo.dep
crypto/hmac.lo.dep: $(srcdir)/go/crypto/hmac/*.go
	$(BUILDDEPS)
crypto/hmac.lo:
	$(BUILDPACKAGE)
crypto/hmac/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/hmac/check

@go_include@ crypto/md5.lo.dep
crypto/md5.lo.dep: $(srcdir)/go/crypto/md5/*.go
	$(BUILDDEPS)
crypto/md5.lo:
	$(BUILDPACKAGE)
crypto/md5/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/md5/check

@go_include@ crypto/rand.lo.dep
crypto/rand.lo.dep: $(srcdir)/go/crypto/rand/*.go
	$(BUILDDEPS)
crypto/rand.lo:
	$(BUILDPACKAGE)
crypto/rand/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/rand/check

@go_include@ crypto/rc4.lo.dep
crypto/rc4.lo.dep: $(srcdir)/go/crypto/rc4/*.go
	$(BUILDDEPS)
crypto/rc4.lo:
	$(BUILDPACKAGE)
crypto/rc4/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/rc4/check

@go_include@ crypto/rsa.lo.dep
crypto/rsa.lo.dep: $(srcdir)/go/crypto/rsa/*.go
	$(BUILDDEPS)
crypto/rsa.lo:
	$(BUILDPACKAGE)
crypto/rsa/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/rsa/check

@go_include@ crypto/sha1.lo.dep
crypto/sha1.lo.dep: $(srcdir)/go/crypto/sha1/*.go
	$(BUILDDEPS)
crypto/sha1.lo:
	$(BUILDPACKAGE)
crypto/sha1/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/sha1/check

@go_include@ crypto/sha256.lo.dep
crypto/sha256.lo.dep: $(srcdir)/go/crypto/sha256/*.go
	$(BUILDDEPS)
crypto/sha256.lo:
	$(BUILDPACKAGE)
crypto/sha256/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/sha256/check

@go_include@ crypto/sha512.lo.dep
crypto/sha512.lo.dep: $(srcdir)/go/crypto/sha512/*.go
	$(BUILDDEPS)
crypto/sha512.lo:
	$(BUILDPACKAGE)
crypto/sha512/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/sha512/check

@go_include@ crypto/subtle.lo.dep
crypto/subtle.lo.dep: $(srcdir)/go/crypto/subtle/*.go
	$(BUILDDEPS)
crypto/subtle.lo:
	$(BUILDPACKAGE)
crypto/subtle/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/subtle/check

@go_include@ crypto/tls.lo.dep
crypto/tls.lo.dep: $(srcdir)/go/crypto/tls/*.go
	$(BUILDDEPS)
crypto/tls.lo:
	$(BUILDPACKAGE)
crypto/tls/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/tls/check

@go_include@ crypto/x509.lo.dep
crypto/x509.lo.dep: $(srcdir)/go/crypto/x509/*.go
	$(BUILDDEPS)
crypto/x509.lo:
	$(BUILDPACKAGE)
crypto/x509/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/x509/check

@go_include@ crypto/x509/pkix.lo.dep
crypto/x509/pkix.lo.dep: $(srcdir)/go/crypto/x509/pkix/*.go
	$(BUILDDEPS)
crypto/x509/pkix.lo:
	$(BUILDPACKAGE)
crypto/x509/pkix/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/x509/pkix/check

@go_include@ database/sql.lo.dep
database/sql.lo.dep: $(srcdir)/go/database/sql/*.go
	$(BUILDDEPS)
database/sql.lo:
	$(BUILDPACKAGE)
database/sql/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: database/sql/check

@go_include@ database/sql/driver.lo.dep
database/sql/driver.lo.dep: $(srcdir)/go/database/sql/driver/*.go
	$(BUILDDEPS)
database/sql/driver.lo:
	$(BUILDPACKAGE)
database/sql/driver/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: database/sql/driver/check

@go_include@ debug/dwarf.lo.dep
debug/dwarf.lo.dep: $(srcdir)/go/debug/dwarf/*.go
	$(BUILDDEPS)
debug/dwarf.lo:
	$(BUILDPACKAGE)
debug/dwarf/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: debug/dwarf/check

@go_include@ debug/elf.lo.dep
debug/elf.lo.dep: $(srcdir)/go/debug/elf/*.go
	$(BUILDDEPS)
debug/elf.lo:
	$(BUILDPACKAGE)
debug/elf/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: debug/elf/check

@go_include@ debug/gosym.lo.dep
debug/gosym.lo.dep: $(srcdir)/go/debug/gosym/*.go
	$(BUILDDEPS)
debug/gosym.lo:
	$(BUILDPACKAGE)
debug/gosym/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: debug/gosym/check

@go_include@ debug/macho.lo.dep
debug/macho.lo.dep: $(srcdir)/go/debug/macho/*.go
	$(BUILDDEPS)
debug/macho.lo:
	$(BUILDPACKAGE)
debug/macho/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: debug/macho/check

@go_include@ debug/pe.lo.dep
debug/pe.lo.dep: $(srcdir)/go/debug/pe/*.go
	$(BUILDDEPS)
debug/pe.lo:
	$(BUILDPACKAGE)
debug/pe/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: debug/pe/check

@go_include@ debug/plan9obj.lo.dep
debug/plan9obj.lo.dep: $(srcdir)/go/debug/plan9obj/*.go
	$(BUILDDEPS)
debug/plan9obj.lo:
	$(BUILDPACKAGE)
debug/plan9obj/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: debug/plan9obj/check

@go_include@ encoding/asn1.lo.dep
encoding/asn1.lo.dep: $(srcdir)/go/encoding/asn1/*.go
	$(BUILDDEPS)
encoding/asn1.lo:
	$(BUILDPACKAGE)
encoding/asn1/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/asn1/check

@go_include@ encoding/ascii85.lo.dep
encoding/ascii85.lo.dep: $(srcdir)/go/encoding/ascii85/*.go
	$(BUILDDEPS)
encoding/ascii85.lo:
	$(BUILDPACKAGE)
encoding/ascii85/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/ascii85/check

@go_include@ encoding/base32.lo.dep
encoding/base32.lo.dep: $(srcdir)/go/encoding/base32/*.go
	$(BUILDDEPS)
encoding/base32.lo:
	$(BUILDPACKAGE)
encoding/base32/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/base32/check

@go_include@ encoding/base64.lo.dep
encoding/base64.lo.dep: $(srcdir)/go/encoding/base64/*.go
	$(BUILDDEPS)
encoding/base64.lo:
	$(BUILDPACKAGE)
encoding/base64/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/base64/check

@go_include@ encoding/binary.lo.dep
encoding/binary.lo.dep: $(srcdir)/go/encoding/binary/*.go
	$(BUILDDEPS)
encoding/binary.lo:
	$(BUILDPACKAGE)
encoding/binary/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/binary/check

@go_include@ encoding/csv.lo.dep
encoding/csv.lo.dep: $(srcdir)/go/encoding/csv/*.go
	$(BUILDDEPS)
encoding/csv.lo:
	$(BUILDPACKAGE)
encoding/csv/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/csv/check

@go_include@ encoding/gob.lo.dep
encoding/gob.lo.dep: $(srcdir)/go/encoding/gob/*.go
	$(BUILDDEPS)
encoding/gob.lo:
	$(BUILDPACKAGE)
encoding/gob/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/gob/check

@go_include@ encoding/hex.lo.dep
encoding/hex.lo.dep: $(srcdir)/go/encoding/hex/*.go
	$(BUILDDEPS)
encoding/hex.lo:
	$(BUILDPACKAGE)
encoding/hex/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/hex/check

@go_include@ encoding/json.lo.dep
encoding/json.lo.dep: $(srcdir)/go/encoding/json/*.go
	$(BUILDDEPS)
encoding/json.lo:
	$(BUILDPACKAGE)
encoding/json/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/json/check

@go_include@ encoding/pem.lo.dep
encoding/pem.lo.dep: $(srcdir)/go/encoding/pem/*.go
	$(BUILDDEPS)
encoding/pem.lo:
	$(BUILDPACKAGE)
encoding/pem/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/pem/check

@go_include@ encoding/xml.lo.dep
encoding/xml.lo.dep: $(srcdir)/go/encoding/xml/*.go
	$(BUILDDEPS)
encoding/xml.lo:
	$(BUILDPACKAGE)
encoding/xml/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: encoding/xml/check

@go_include@ exp/proxy.lo.dep
exp/proxy.lo.dep: $(srcdir)/go/exp/proxy/*.go
	$(BUILDDEPS)
exp/proxy.lo:
	$(BUILDPACKAGE)
exp/proxy/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: exp/proxy/check

@go_include@ exp/terminal.lo.dep
exp/terminal.lo.dep: $(srcdir)/go/exp/terminal/*.go
	$(BUILDDEPS)
exp/terminal.lo:
	$(BUILDPACKAGE)
exp/terminal/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: exp/terminal/check

@go_include@ html/template.lo.dep
html/template.lo.dep: $(srcdir)/go/html/template/*.go
	$(BUILDDEPS)
html/template.lo:
	$(BUILDPACKAGE)
html/template/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: html/template/check

@go_include@ go/ast.lo.dep
go/ast.lo.dep: $(srcdir)/go/go/ast/*.go
	$(BUILDDEPS)
go/ast.lo:
	$(BUILDPACKAGE)
go/ast/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/ast/check

@go_include@ go/build.lo.dep
go/build.lo.dep: $(srcdir)/go/go/build/*.go
	$(BUILDDEPS)
go/build.lo:
	$(BUILDPACKAGE)
go/build/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/build/check

@go_include@ go/constant.lo.dep
go/constant.lo.dep: $(srcdir)/go/go/constant/*.go
	$(BUILDDEPS)
go/constant.lo:
	$(BUILDPACKAGE)
go/constant/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/constant/check

@go_include@ go/doc.lo.dep
go/doc.lo.dep: $(srcdir)/go/go/doc/*.go
	$(BUILDDEPS)
go/doc.lo:
	$(BUILDPACKAGE)
go/doc/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/doc/check

@go_include@ go/format.lo.dep
go/format.lo.dep: $(srcdir)/go/go/format/*.go
	$(BUILDDEPS)
go/format.lo:
	$(BUILDPACKAGE)
go/format/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/format/check

@go_include@ go/importer.lo.dep
go/importer.lo.dep: $(srcdir)/go/go/importer/*.go
	$(BUILDDEPS)
go/importer.lo:
	$(BUILDPACKAGE)
go/importer/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/importer/check

@go_include@ go/parser.lo.dep
go/parser.lo.dep: $(srcdir)/go/go/parser/*.go
	$(BUILDDEPS)
go/parser.lo:
	$(BUILDPACKAGE)
go/parser/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/parser/check

@go_include@ go/printer.lo.dep
go/printer.lo.dep: $(srcdir)/go/go/printer/*.go
	$(BUILDDEPS)
go/printer.lo:
	$(BUILDPACKAGE)
go/printer/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/printer/check

@go_include@ go/scanner.lo.dep
go/scanner.lo.dep: $(srcdir)/go/go/scanner/*.go
	$(BUILDDEPS)
go/scanner.lo:
	$(BUILDPACKAGE)
go/scanner/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/scanner/check

@go_include@ go/token.lo.dep
go/token.lo.dep: $(srcdir)/go/go/token/*.go
	$(BUILDDEPS)
go/token.lo:
	$(BUILDPACKAGE)
go/token/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/token/check

@go_include@ go/types.lo.dep
go/types.lo.dep: $(srcdir)/go/go/types/*.go
	$(BUILDDEPS)
go/types.lo:
	$(BUILDPACKAGE)
go/types/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/types/check

@go_include@ go/internal/gcimporter.lo.dep
go/internal/gcimporter.lo.dep: $(srcdir)/go/go/internal/gcimporter/*.go
	$(BUILDDEPS)
go/internal/gcimporter.lo:
	$(BUILDPACKAGE)
go/internal/gcimporter/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/internal/gcimporter/check

@go_include@ go/internal/gccgoimporter.lo.dep
go/internal/gccgoimporter.lo.dep: $(srcdir)/go/go/internal/gccgoimporter/*.go
	$(BUILDDEPS)
go/internal/gccgoimporter.lo:
	$(BUILDPACKAGE)
go/internal/gccgoimporter/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: go/internal/gccgoimporter/check

@go_include@ golang_org/x/net/http2/hpack.lo.dep
golang_org/x/net/http2/hpack.lo.dep: $(srcdir)/go/golang_org/x/net/http2/hpack/*.go
	$(BUILDDEPS)
golang_org/x/net/http2/hpack.lo:
	$(BUILDPACKAGE)
golang_org/x/net/http2/hpack/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: golang_org/x/net/http2/hpack/check

@go_include@ golang_org/x/net/lex/httplex.lo.dep
golang_org/x/net/lex/httplex.lo.dep: $(srcdir)/go/golang_org/x/net/lex/httplex/*.go
	$(BUILDDEPS)
golang_org/x/net/lex/httplex.lo:
	$(BUILDPACKAGE)
golang_org/x/net/lex/httplex/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: golang_org/x/net/lex/httplex/check

if LIBGO_IS_BSD

golang_org_x_net_route_lo = \
	golang_org/x/net/route/route.lo
golang_org_x_net_route_check = \
	golang_org/x/net/route/check

@go_include@ golang_org/x/net/route.lo.dep
golang_org/x/net/route.lo.dep: $(srcdir)/go/golang_org/x/net/route/*.go
	$(BUILDDEPS)
golang_org/x/net/route.lo:
	$(BUILDPACKAGE)
golang_org/x/net/route/check: $(CHECK_DEPS)
	@$(CHECK)

endif

.PHONY: golang_org/x/net/route/check

@go_include@ hash/adler32.lo.dep
hash/adler32.lo.dep: $(srcdir)/go/hash/adler32/*.go
	$(BUILDDEPS)
hash/adler32.lo:
	$(BUILDPACKAGE)
hash/adler32/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: hash/adler32/check

@go_include@ hash/crc32.lo.dep
hash/crc32.lo.dep: $(srcdir)/go/hash/crc32/*.go
	$(BUILDDEPS)
hash/crc32.lo:
	$(BUILDPACKAGE)
hash/crc32/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: hash/crc32/check

@go_include@ hash/crc64.lo.dep
hash/crc64.lo.dep: $(srcdir)/go/hash/crc64/*.go
	$(BUILDDEPS)
hash/crc64.lo:
	$(BUILDPACKAGE)
hash/crc64/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: hash/crc64/check

@go_include@ hash/fnv.lo.dep
hash/fnv.lo.dep: $(srcdir)/go/hash/fnv/*.go
	$(BUILDDEPS)
hash/fnv.lo:
	$(BUILDPACKAGE)
hash/fnv/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: hash/fnv/check

@go_include@ image/color.lo.dep
image/color.lo.dep: $(srcdir)/go/image/color/*.go
	$(BUILDDEPS)
image/color.lo:
	$(BUILDPACKAGE)
image/color/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/color/check

@go_include@ image/color/palette.lo.dep
image/color/palette.lo.dep: $(srcdir)/go/image/color/palette/*.go
	$(BUILDDEPS)
image/color/palette.lo:
	$(BUILDPACKAGE)
image/color/palette/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/color/palette/check

@go_include@ image/draw.lo.dep
image/draw.lo.dep: $(srcdir)/go/image/draw/*.go
	$(BUILDDEPS)
image/draw.lo:
	$(BUILDPACKAGE)
image/draw/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/draw/check

@go_include@ image/gif.lo.dep
image/gif.lo.dep: $(srcdir)/go/image/gif/*.go
	$(BUILDDEPS)
image/gif.lo:
	$(BUILDPACKAGE)
image/gif/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/gif/check

@go_include@ image/internal/imageutil.lo.dep
image/internal/imageutil.lo.dep: $(srcdir)/go/image/internal/imageutil/*.go
	$(BUILDDEPS)
image/internal/imageutil.lo:
	$(BUILDPACKAGE)
image/internal/imageutil/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/internal/imageutil/check

@go_include@ image/jpeg.lo.dep
image/jpeg.lo.dep: $(srcdir)/go/image/jpeg/*.go
	$(BUILDDEPS)
image/jpeg.lo:
	$(BUILDPACKAGE)
image/jpeg/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/jpeg/check

@go_include@ image/png.lo.dep
image/png.lo.dep: $(srcdir)/go/image/png/*.go
	$(BUILDDEPS)
image/png.lo:
	$(BUILDPACKAGE)
image/png/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/png/check

@go_include@ index/suffixarray.lo.dep
index/suffixarray.lo.dep: $(srcdir)/go/index/suffixarray/*.go
	$(BUILDDEPS)
index/suffixarray.lo:
	$(BUILDPACKAGE)
index/suffixarray/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: index/suffixarray/check

@go_include@ internal/nettrace.lo.dep
internal/nettrace.lo.dep: $(srcdir)/go/internal/nettrace/*.go
	$(BUILDDEPS)
internal/nettrace.lo:
	$(BUILDPACKAGE)
internal/nettrace/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: internal/nettrace/check

@go_include@ internal/race.lo.dep
internal/race.lo.dep: $(srcdir)/go/internal/race/*.go
	$(BUILDDEPS)
internal/race.lo:
	$(BUILDPACKAGE)
internal/race/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: internal/race/check

@go_include@ internal/singleflight.lo.dep
internal/singleflight.lo.dep: $(srcdir)/go/internal/singleflight/*.go
	$(BUILDDEPS)
internal/singleflight.lo:
	$(BUILDPACKAGE)
internal/singleflight/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: internal/singleflight/check

@go_include@ internal/syscall/unix.lo.dep
internal/syscall/unix.lo.dep: $(srcdir)/go/internal/syscall/unix/*.go
	$(BUILDDEPS)
internal/syscall/unix.lo:
	$(BUILDPACKAGE)
internal/syscall/unix/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: internal/syscall/unix/check

@go_include@ internal/testenv.lo.dep
internal/testenv.lo.dep: $(srcdir)/go/internal/testenv/*.go
	$(BUILDDEPS)
internal/testenv.lo:
	$(BUILDPACKAGE)
internal/testenv/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: internal/testenv/check

@go_include@ internal/trace.lo.dep
internal/trace.lo.dep: $(srcdir)/go/internal/trace/*.go
	$(BUILDDEPS)
internal/trace.lo:
	$(BUILDPACKAGE)
internal/trace/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: internal/trace/check

@go_include@ io/ioutil.lo.dep
io/ioutil.lo.dep: $(srcdir)/go/io/ioutil/*.go
	$(BUILDDEPS)
io/ioutil.lo:
	$(BUILDPACKAGE)
io/ioutil/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: io/ioutil/check

@go_include@ log/syslog.lo.dep
log/syslog.lo.dep: $(srcdir)/go/log/syslog/*.go
	$(BUILDDEPS)
log/syslog.lo:
	$(BUILDPACKAGE)
log/syslog/syslog_c.lo: go/log/syslog/syslog_c.c runtime.inc log/syslog.lo
	@$(MKDIR_P) log/syslog
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/log/syslog/syslog_c.c
log/syslog/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: log/syslog/check

@go_include@ math/big.lo.dep
math/big.lo.dep: $(srcdir)/go/math/big/*.go
	$(BUILDDEPS)
math/big.lo:
	$(BUILDPACKAGE)
math/big/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: math/big/check

@go_include@ math/cmplx.lo.dep
math/cmplx.lo.dep: $(srcdir)/go/math/cmplx/*.go
	$(BUILDDEPS)
math/cmplx.lo:
	$(BUILDPACKAGE)
math/cmplx/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: math/cmplx/check

@go_include@ math/rand.lo.dep
math/rand.lo.dep: $(srcdir)/go/math/rand/*.go
	$(BUILDDEPS)
math/rand.lo:
	$(BUILDPACKAGE)
math/rand/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: math/rand/check

@go_include@ mime/multipart.lo.dep
mime/multipart.lo.dep: $(srcdir)/go/mime/multipart/*.go
	$(BUILDDEPS)
mime/multipart.lo:
	$(BUILDPACKAGE)
mime/multipart/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: mime/multipart/check

@go_include@ mime/quotedprintable.lo.dep
mime/quotedprintable.lo.dep: $(srcdir)/go/mime/quotedprintable/*.go
	$(BUILDDEPS)
mime/quotedprintable.lo:
	$(BUILDPACKAGE)
mime/quotedprintable/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: mime/quotedprintable/check

@go_include@ net/http.lo.dep
net/http.lo.dep: $(srcdir)/go/net/http/*.go
	$(BUILDDEPS)
net/http.lo:
	$(BUILDPACKAGE)
net/http/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/http/check

@go_include@ net/mail.lo.dep
net/mail.lo.dep: $(srcdir)/go/net/mail/*.go
	$(BUILDDEPS)
net/mail.lo:
	$(BUILDPACKAGE)
net/mail/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/mail/check

@go_include@ net/rpc.lo.dep
net/rpc.lo.dep: $(srcdir)/go/net/rpc/*.go
	$(BUILDDEPS)
net/rpc.lo:
	$(BUILDPACKAGE)
net/rpc/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/rpc/check

@go_include@ net/smtp.lo.dep
net/smtp.lo.dep: $(srcdir)/go/net/smtp/*.go
	$(BUILDDEPS)
net/smtp.lo:
	$(BUILDPACKAGE)
net/smtp/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/smtp/check

@go_include@ net/url.lo.dep
net/url.lo.dep: $(srcdir)/go/net/url/*.go
	$(BUILDDEPS)
net/url.lo:
	$(BUILDPACKAGE)
net/url/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/url/check

@go_include@ net/textproto.lo.dep
net/textproto.lo.dep: $(srcdir)/go/net/textproto/*.go
	$(BUILDDEPS)
net/textproto.lo:
	$(BUILDPACKAGE)
net/textproto/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/textproto/check

@go_include@ net/http/cgi.lo.dep
net/http/cgi.lo.dep: $(srcdir)/go/net/http/cgi/*.go
	$(BUILDDEPS)
net/http/cgi.lo:
	$(BUILDPACKAGE)
net/http/cgi/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/http/cgi/check

@go_include@ net/http/cookiejar.lo.dep
net/http/cookiejar.lo.dep: $(srcdir)/go/net/http/cookiejar/*.go
	$(BUILDDEPS)
net/http/cookiejar.lo:
	$(BUILDPACKAGE)
net/http/cookiejar/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/http/cookiejar/check

@go_include@ net/http/fcgi.lo.dep
net/http/fcgi.lo.dep: $(srcdir)/go/net/http/fcgi/*.go
	$(BUILDDEPS)
net/http/fcgi.lo:
	$(BUILDPACKAGE)
net/http/fcgi/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/http/fcgi/check

@go_include@ net/http/httptest.lo.dep
net/http/httptest.lo.dep: $(srcdir)/go/net/http/httptest/*.go
	$(BUILDDEPS)
net/http/httptest.lo:
	$(BUILDPACKAGE)
net/http/httptest/check: $(check_deps)
	@$(CHECK)
.PHONY: net/http/httptest/check

@go_include@ net/http/httptrace.lo.dep
net/http/httptrace.lo.dep: $(srcdir)/go/net/http/httptrace/*.go
	$(BUILDDEPS)
net/http/httptrace.lo:
	$(BUILDPACKAGE)
net/http/httptrace/check: $(check_deps)
	@$(CHECK)
.PHONY: net/http/httptrace/check

@go_include@ net/http/httputil.lo.dep
net/http/httputil.lo.dep: $(srcdir)/go/net/http/httputil/*.go
	$(BUILDDEPS)
net/http/httputil.lo:
	$(BUILDPACKAGE)
net/http/httputil/check: $(check_deps)
	@$(CHECK)
.PHONY: net/http/httputil/check

@go_include@ net/http/internal.lo.dep
net/http/internal.lo.dep: $(srcdir)/go/net/http/internal/*.go
	$(BUILDDEPS)
net/http/internal.lo:
	$(BUILDPACKAGE)
net/http/internal/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/http/internal/check

@go_include@ net/http/pprof.lo.dep
net/http/pprof.lo.dep: $(srcdir)/go/net/http/pprof/*.go
	$(BUILDDEPS)
net/http/pprof.lo:
	$(BUILDPACKAGE)
net/http/pprof/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/http/pprof/check

@go_include@ net/internal/socktest.lo.dep
net/internal/socktest.lo.dep: $(srcdir)/go/net/internal/socktest/*.go
	$(BUILDDEPS)
net/internal/socktest.lo:
	$(BUILDPACKAGE)
net/internal/socktest/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/internal/socktest/check

@go_include@ net/rpc/jsonrpc.lo.dep
net/rpc/jsonrpc.lo.dep: $(srcdir)/go/net/rpc/jsonrpc/*.go
	$(BUILDDEPS)
net/rpc/jsonrpc.lo:
	$(BUILDPACKAGE)
net/rpc/jsonrpc/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/rpc/jsonrpc/check

@go_include@ old/regexp.lo.dep
old/regexp.lo.dep: $(srcdir)/go/old/regexp/*.go
	$(BUILDDEPS)
old/regexp.lo:
	$(BUILDPACKAGE)
old/regexp/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: old/regexp/check

@go_include@ old/template.lo.dep
old/template.lo.dep: $(srcdir)/go/old/template/*.go
	$(BUILDDEPS)
old/template.lo:
	$(BUILDPACKAGE)
old/template/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: old/template/check

@go_include@ os/exec.lo.dep
os/exec.lo.dep: $(srcdir)/go/os/exec/*.go
	$(BUILDDEPS)
os/exec.lo:
	$(BUILDPACKAGE)
os/exec/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: os/exec/check

@go_include@ os/signal.lo.dep
os/signal.lo.dep: $(srcdir)/go/os/signal/*.go
	$(BUILDDEPS)
os/signal.lo:
	$(BUILDPACKAGE)
os/signal/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: os/signal/check

@go_include@ os/user.lo.dep
os/user.lo.dep: $(srcdir)/go/os/user/*.go
	$(BUILDDEPS)
os/user.lo:
	$(BUILDPACKAGE)
os/user/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: os/user/check

@go_include@ path/filepath.lo.dep
path/filepath.lo.dep: $(srcdir)/go/path/filepath/*.go
	$(BUILDDEPS)
path/filepath.lo:
	$(BUILDPACKAGE)
path/filepath/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: path/filepath/check

@go_include@ regexp/syntax.lo.dep
regexp/syntax.lo.dep: $(srcdir)/go/regexp/syntax/*.go
	$(BUILDDEPS)
regexp/syntax.lo:
	$(BUILDPACKAGE)
regexp/syntax/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: regexp/syntax/check

@go_include@ runtime/debug.lo.dep
runtime/debug.lo.dep: $(srcdir)/go/runtime/debug/*.go
	$(BUILDDEPS)
runtime/debug.lo:
	$(BUILDPACKAGE)
runtime/debug/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: runtime/debug/check

@go_include@ runtime/pprof.lo.dep
runtime/pprof.lo.dep: $(srcdir)/go/runtime/pprof/*.go
	$(BUILDDEPS)
runtime/pprof.lo:
	$(BUILDPACKAGE)
runtime/pprof/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: runtime/pprof/check
# At least for now, we need -static-libgo for this test, because
# otherwise we can't get the line numbers.
# Also use -fno-inline to get better results from the memory profiler.
runtime_pprof_check_GOCFLAGS = -static-libgo -fno-inline

@go_include@ runtime/internal/atomic.lo.dep
runtime/internal/atomic.lo.dep: $(srcdir)/go/runtime/internal/atomic/*.go
	$(BUILDDEPS)
runtime_internal_atomic_lo_GOCFLAGS = -fgo-compiling-runtime
runtime/internal/atomic.lo:
	$(BUILDPACKAGE)
runtime/internal/atomic_c.lo: go/runtime/internal/atomic/atomic.c runtime.inc runtime/internal/atomic.lo
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/runtime/internal/atomic/atomic.c
runtime/internal/atomic/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: runtime/internal/atomic/check

extra_go_files_runtime_internal_sys = version.go

@go_include@ runtime/internal/sys.lo.dep
runtime/internal/sys.lo.dep: $(srcdir)/go/runtime/internal/sys/*.go
	$(BUILDDEPS)
runtime_internal_sys_lo_GOCFLAGS = -fgo-compiling-runtime
runtime/internal/sys.lo:
	$(BUILDPACKAGE)
runtime/internal/sys/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: runtime/internal/sys/check

@go_include@ sync/atomic.lo.dep
sync/atomic.lo.dep: $(srcdir)/go/sync/atomic/*.go
	$(BUILDDEPS)
sync/atomic.lo:
	$(BUILDPACKAGE)
sync/atomic_c.lo: go/sync/atomic/atomic.c runtime.inc sync/atomic.lo
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/sync/atomic/atomic.c
sync/atomic/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: sync/atomic/check

@go_include@ text/scanner.lo.dep
text/scanner.lo.dep: $(srcdir)/go/text/scanner/*.go
	$(BUILDDEPS)
text/scanner.lo:
	$(BUILDPACKAGE)
text/scanner/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: text/scanner/check

@go_include@ text/tabwriter.lo.dep
text/tabwriter.lo.dep: $(srcdir)/go/text/tabwriter/*.go
	$(BUILDDEPS)
text/tabwriter.lo:
	$(BUILDPACKAGE)
text/tabwriter/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: text/tabwriter/check

@go_include@ text/template.lo.dep
text/template.lo.dep: $(srcdir)/go/text/template/*.go
	$(BUILDDEPS)
text/template.lo:
	$(BUILDPACKAGE)
text/template/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: text/template/check

@go_include@ text/template/parse.lo.dep
text/template/parse.lo.dep: $(srcdir)/go/text/template/parse/*.go
	$(BUILDDEPS)
text/template/parse.lo:
	$(BUILDPACKAGE)
text/template/parse/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: text/template/parse/check

@go_include@ testing/iotest.lo.dep
testing/iotest.lo.dep: $(srcdir)/go/testing/iotest/*.go
	$(BUILDDEPS)
testing/iotest.lo:
	$(BUILDPACKAGE)
testing/iotest/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: testing/iotest/check

@go_include@ testing/quick.lo.dep
testing/quick.lo.dep: $(srcdir)/go/testing/quick/*.go
	$(BUILDDEPS)
testing/quick.lo:
	$(BUILDPACKAGE)
testing/quick/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: testing/quick/check

@go_include@ unicode/utf16.lo.dep
unicode/utf16.lo.dep: $(srcdir)/go/unicode/utf16/*.go
	$(BUILDDEPS)
unicode/utf16.lo:
	$(BUILDPACKAGE)
unicode/utf16/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: unicode/utf16/check

@go_include@ unicode/utf8.lo.dep
unicode/utf8.lo.dep: $(srcdir)/go/unicode/utf8/*.go
	$(BUILDDEPS)
unicode/utf8.lo:
	$(BUILDPACKAGE)
unicode/utf8/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: unicode/utf8/check

@go_include@ syscall.lo.dep
syscall.lo.dep: $(srcdir)/go/syscall/*.go $(extra_go_files_syscall)
	$(BUILDDEPS)
syscall.lo:
	$(BUILDPACKAGE)
syscall/clone_linux.lo: go/syscall/clone_linux.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $<
syscall/errno.lo: go/syscall/errno.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $<
syscall/signame.lo: go/syscall/signame.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $<
syscall/wait.lo: go/syscall/wait.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $<
syscall/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: syscall/check

# How to build a .gox file from a .lo file.
BUILDGOX = \
	f=`echo $< | sed -e 's/.lo$$/.o/'`; \
	$(OBJCOPY) -j .go_export $$f $@.tmp && mv -f $@.tmp $@

bufio.gox: bufio.lo
	$(BUILDGOX)
bytes.gox: bytes.lo
	$(BUILDGOX)
context.gox: context.lo
	$(BUILDGOX)
crypto.gox: crypto.lo
	$(BUILDGOX)
encoding.gox: encoding.lo
	$(BUILDGOX)
errors.gox: errors.lo
	$(BUILDGOX)
expvar.gox: expvar.lo
	$(BUILDGOX)
flag.gox: flag.lo
	$(BUILDGOX)
fmt.gox: fmt.lo
	$(BUILDGOX)
hash.gox: hash.lo
	$(BUILDGOX)
html.gox: html.lo
	$(BUILDGOX)
image.gox: image.lo
	$(BUILDGOX)
io.gox: io.lo
	$(BUILDGOX)
log.gox: log.lo
	$(BUILDGOX)
math.gox: math.lo
	$(BUILDGOX)
mime.gox: mime.lo
	$(BUILDGOX)
net.gox: net.lo
	$(BUILDGOX)
os.gox: os.lo
	$(BUILDGOX)
path.gox: path.lo
	$(BUILDGOX)
reflect.gox: reflect-go.lo
	$(BUILDGOX)
regexp.gox: regexp.lo
	$(BUILDGOX)
runtime.gox: runtime-go.lo
	$(BUILDGOX)
sort.gox: sort.lo
	$(BUILDGOX)
strconv.gox: strconv.lo
	$(BUILDGOX)
strings.gox: strings.lo
	$(BUILDGOX)
sync.gox: sync.lo
	$(BUILDGOX)
syscall.gox: syscall.lo
	$(BUILDGOX)
testing.gox: testing.lo
	$(BUILDGOX)
time.gox: time-go.lo
	$(BUILDGOX)
unicode.gox: unicode.lo
	$(BUILDGOX)

archive/tar.gox: archive/tar.lo
	$(BUILDGOX)
archive/zip.gox: archive/zip.lo
	$(BUILDGOX)

compress/bzip2.gox: compress/bzip2.lo
	$(BUILDGOX)
compress/flate.gox: compress/flate.lo
	$(BUILDGOX)
compress/gzip.gox: compress/gzip.lo
	$(BUILDGOX)
compress/lzw.gox: compress/lzw.lo
	$(BUILDGOX)
compress/zlib.gox: compress/zlib.lo
	$(BUILDGOX)

container/heap.gox: container/heap.lo
	$(BUILDGOX)
container/list.gox: container/list.lo
	$(BUILDGOX)
container/ring.gox: container/ring.lo
	$(BUILDGOX)

crypto/aes.gox: crypto/aes.lo
	$(BUILDGOX)
crypto/cipher.gox: crypto/cipher.lo
	$(BUILDGOX)
crypto/des.gox: crypto/des.lo
	$(BUILDGOX)
crypto/dsa.gox: crypto/dsa.lo
	$(BUILDGOX)
crypto/ecdsa.gox: crypto/ecdsa.lo	
	$(BUILDGOX)
crypto/elliptic.gox: crypto/elliptic.lo
	$(BUILDGOX)
crypto/hmac.gox: crypto/hmac.lo
	$(BUILDGOX)
crypto/md5.gox: crypto/md5.lo
	$(BUILDGOX)
crypto/rand.gox: crypto/rand.lo
	$(BUILDGOX)
crypto/rc4.gox: crypto/rc4.lo
	$(BUILDGOX)
crypto/rsa.gox: crypto/rsa.lo
	$(BUILDGOX)
crypto/sha1.gox: crypto/sha1.lo
	$(BUILDGOX)
crypto/sha256.gox: crypto/sha256.lo
	$(BUILDGOX)
crypto/sha512.gox: crypto/sha512.lo
	$(BUILDGOX)
crypto/subtle.gox: crypto/subtle.lo
	$(BUILDGOX)
crypto/tls.gox: crypto/tls.lo
	$(BUILDGOX)
crypto/x509.gox: crypto/x509.lo
	$(BUILDGOX)

crypto/x509/pkix.gox: crypto/x509/pkix.lo
	$(BUILDGOX)

database/sql.gox: database/sql.lo
	$(BUILDGOX)

database/sql/driver.gox: database/sql/driver.lo
	$(BUILDGOX)

debug/dwarf.gox: debug/dwarf.lo
	$(BUILDGOX)
debug/elf.gox: debug/elf.lo
	$(BUILDGOX)
debug/gosym.gox: debug/gosym.lo
	$(BUILDGOX)
debug/macho.gox: debug/macho.lo
	$(BUILDGOX)
debug/pe.gox: debug/pe.lo
	$(BUILDGOX)
debug/plan9obj.gox: debug/plan9obj.lo
	$(BUILDGOX)

encoding/ascii85.gox: encoding/ascii85.lo
	$(BUILDGOX)
encoding/asn1.gox: encoding/asn1.lo
	$(BUILDGOX)
encoding/base32.gox: encoding/base32.lo
	$(BUILDGOX)
encoding/base64.gox: encoding/base64.lo
	$(BUILDGOX)
encoding/binary.gox: encoding/binary.lo
	$(BUILDGOX)
encoding/csv.gox: encoding/csv.lo
	$(BUILDGOX)
encoding/gob.gox: encoding/gob.lo
	$(BUILDGOX)
encoding/hex.gox: encoding/hex.lo
	$(BUILDGOX)
encoding/json.gox: encoding/json.lo
	$(BUILDGOX)
encoding/pem.gox: encoding/pem.lo
	$(BUILDGOX)
encoding/xml.gox: encoding/xml.lo
	$(BUILDGOX)

exp/proxy.gox: exp/proxy.lo
	$(BUILDGOX)
exp/terminal.gox: exp/terminal.lo
	$(BUILDGOX)

html/template.gox: html/template.lo
	$(BUILDGOX)

go/ast.gox: go/ast.lo
	$(BUILDGOX)
go/build.gox: go/build.lo
	$(BUILDGOX)
go/constant.gox: go/constant.lo
	$(BUILDGOX)
go/doc.gox: go/doc.lo
	$(BUILDGOX)
go/format.gox: go/format.lo
	$(BUILDGOX)
go/importer.gox: go/importer.lo
	$(BUILDGOX)
go/parser.gox: go/parser.lo
	$(BUILDGOX)
go/printer.gox: go/printer.lo
	$(BUILDGOX)
go/scanner.gox: go/scanner.lo
	$(BUILDGOX)
go/token.gox: go/token.lo
	$(BUILDGOX)
go/types.gox: go/types.lo
	$(BUILDGOX)

go/internal/gcimporter.gox: go/internal/gcimporter.lo
	$(BUILDGOX)
go/internal/gccgoimporter.gox: go/internal/gccgoimporter.lo
	$(BUILDGOX)

golang_org/x/net/http2/hpack.gox: golang_org/x/net/http2/hpack.lo
	$(BUILDGOX)

golang_org/x/net/lex/httplex.gox: golang_org/x/net/lex/httplex.lo
	$(BUILDGOX)

if LIBGO_IS_BSD
golang_org/x/net/route.gox: golang_org/x/net/route.lo
	$(BUILDGOX)
endif

hash/adler32.gox: hash/adler32.lo
	$(BUILDGOX)
hash/crc32.gox: hash/crc32.lo
	$(BUILDGOX)
hash/crc64.gox: hash/crc64.lo
	$(BUILDGOX)
hash/fnv.gox: hash/fnv.lo
	$(BUILDGOX)

image/color.gox: image/color.lo
	$(BUILDGOX)
image/draw.gox: image/draw.lo
	$(BUILDGOX)
image/gif.gox: image/gif.lo
	$(BUILDGOX)
image/internal/imageutil.gox: image/internal/imageutil.lo
	$(BUILDGOX)
image/jpeg.gox: image/jpeg.lo
	$(BUILDGOX)
image/png.gox: image/png.lo
	$(BUILDGOX)

image/color/palette.gox: image/color/palette.lo
	$(BUILDGOX)

index/suffixarray.gox: index/suffixarray.lo
	$(BUILDGOX)

internal/nettrace.gox: internal/nettrace.lo
	$(BUILDGOX)
internal/race.gox: internal/race.lo
	$(BUILDGOX)
internal/singleflight.gox: internal/singleflight.lo
	$(BUILDGOX)
internal/syscall/unix.gox: internal/syscall/unix.lo
	$(BUILDGOX)
internal/testenv.gox: internal/testenv.lo
	$(BUILDGOX)
internal/trace.gox: internal/trace.lo
	$(BUILDGOX)

io/ioutil.gox: io/ioutil.lo
	$(BUILDGOX)

log/syslog.gox: log/syslog.lo
	$(BUILDGOX)

math/big.gox: math/big.lo
	$(BUILDGOX)
math/cmplx.gox: math/cmplx.lo
	$(BUILDGOX)
math/rand.gox: math/rand.lo
	$(BUILDGOX)

mime/multipart.gox: mime/multipart.lo
	$(BUILDGOX)
mime/quotedprintable.gox: mime/quotedprintable.lo
	$(BUILDGOX)

net/http.gox: net/http.lo
	$(BUILDGOX)
net/mail.gox: net/mail.lo
	$(BUILDGOX)
net/rpc.gox: net/rpc.lo
	$(BUILDGOX)
net/smtp.gox: net/smtp.lo
	$(BUILDGOX)
net/textproto.gox: net/textproto.lo
	$(BUILDGOX)
net/url.gox: net/url.lo
	$(BUILDGOX)

net/http/cgi.gox: net/http/cgi.lo
	$(BUILDGOX)
net/http/cookiejar.gox: net/http/cookiejar.lo
	$(BUILDGOX)
net/http/fcgi.gox: net/http/fcgi.lo
	$(BUILDGOX)
net/http/httptest.gox: net/http/httptest.lo
	$(BUILDGOX)
net/http/httptrace.gox: net/http/httptrace.lo
	$(BUILDGOX)
net/http/httputil.gox: net/http/httputil.lo
	$(BUILDGOX)
net/http/pprof.gox: net/http/pprof.lo
	$(BUILDGOX)

net/http/internal.gox: net/http/internal.lo
	$(BUILDGOX)

net/internal/socktest.gox: net/internal/socktest.lo
	$(BUILDGOX)

net/rpc/jsonrpc.gox: net/rpc/jsonrpc.lo
	$(BUILDGOX)

old/regexp.gox: old/regexp.lo
	$(BUILDGOX)
old/template.gox: old/template.lo
	$(BUILDGOX)

os/exec.gox: os/exec.lo
	$(BUILDGOX)
os/signal.gox: os/signal.lo
	$(BUILDGOX)
os/user.gox: os/user.lo
	$(BUILDGOX)

path/filepath.gox: path/filepath.lo
	$(BUILDGOX)

regexp/syntax.gox: regexp/syntax.lo
	$(BUILDGOX)

runtime/debug.gox: runtime/debug.lo
	$(BUILDGOX)
runtime/pprof.gox: runtime/pprof.lo
	$(BUILDGOX)
runtime/internal/atomic.gox: runtime/internal/atomic.lo
	$(BUILDGOX)
runtime/internal/sys.gox: runtime/internal/sys.lo
	$(BUILDGOX)

sync/atomic.gox: sync/atomic.lo
	$(BUILDGOX)

text/scanner.gox: text/scanner.lo
	$(BUILDGOX)
text/tabwriter.gox: text/tabwriter.lo
	$(BUILDGOX)
text/template.gox: text/template.lo
	$(BUILDGOX)
text/template/parse.gox: text/template/parse.lo
	$(BUILDGOX)

testing/iotest.gox: testing/iotest.lo
	$(BUILDGOX)
testing/quick.gox: testing/quick.lo
	$(BUILDGOX)

unicode/utf16.gox: unicode/utf16.lo
	$(BUILDGOX)
unicode/utf8.gox: unicode/utf8.lo
	$(BUILDGOX)

TEST_PACKAGES = \
	bufio/check \
	bytes/check \
	context/check \
	errors/check \
	expvar/check \
	flag/check \
	fmt/check \
	html/check \
	image/check \
	io/check \
	log/check \
	math/check \
	mime/check \
	net/check \
	os/check \
	path/check \
	reflect/check \
	regexp/check \
	runtime/check \
	sort/check \
	strconv/check \
	strings/check \
	sync/check \
	syscall/check \
	time/check \
	unicode/check \
	archive/tar/check \
	archive/zip/check \
	compress/bzip2/check \
	compress/flate/check \
	compress/gzip/check \
	compress/lzw/check \
	compress/zlib/check \
	container/heap/check \
	container/list/check \
	container/ring/check \
	crypto/aes/check \
	crypto/cipher/check \
	crypto/des/check \
	crypto/dsa/check \
	crypto/ecdsa/check \
	crypto/elliptic/check \
	crypto/hmac/check \
	crypto/md5/check \
	crypto/rand/check \
	crypto/rc4/check \
	crypto/rsa/check \
	crypto/sha1/check \
	crypto/sha256/check \
	crypto/sha512/check \
	crypto/subtle/check \
	crypto/tls/check \
	crypto/x509/check \
	database/sql/check \
	database/sql/driver/check \
	debug/dwarf/check \
	debug/elf/check \
	debug/macho/check \
	debug/pe/check \
	debug/plan9obj/check \
	encoding/ascii85/check \
	encoding/asn1/check \
	encoding/base32/check \
	encoding/base64/check \
	encoding/binary/check \
	encoding/csv/check \
	encoding/gob/check \
	encoding/hex/check \
	encoding/json/check \
	encoding/pem/check \
	encoding/xml/check \
	exp/proxy/check \
	exp/terminal/check \
	html/template/check \
	go/ast/check \
	go/build/check \
	go/constant/check \
	go/doc/check \
	go/format/check \
	go/internal/gcimporter/check \
	go/internal/gccgoimporter/check \
	go/parser/check \
	go/printer/check \
	go/scanner/check \
	go/token/check \
	go/types/check \
	golang_org/x/net/http2/hpack/check \
	golang_org/x/net/lex/httplex/check \
	$(golang_org_x_net_route_check) \
	hash/adler32/check \
	hash/crc32/check \
	hash/crc64/check \
	hash/fnv/check \
	image/color/check \
	image/draw/check \
	image/jpeg/check \
	image/png/check \
	index/suffixarray/check \
	internal/singleflight/check \
	internal/trace/check \
	io/ioutil/check \
	log/syslog/check \
	math/big/check \
	math/cmplx/check \
	math/rand/check \
	mime/multipart/check \
	mime/quotedprintable/check \
	net/http/check \
	net/http/cgi/check \
	net/http/cookiejar/check \
	net/http/fcgi/check \
	net/http/httptest/check \
	net/http/httptrace/check \
	net/http/httputil/check \
	net/http/internal/check \
	net/internal/socktest/check \
	net/mail/check \
	net/rpc/check \
	net/smtp/check \
	net/textproto/check \
	net/url/check \
	net/rpc/jsonrpc/check \
	old/regexp/check \
	old/template/check \
	os/exec/check \
	os/signal/check \
	os/user/check \
	path/filepath/check \
	regexp/syntax/check \
	runtime/debug/check \
	runtime/pprof/check \
	runtime/internal/atomic/check \
	runtime/internal/sys/check \
	sync/atomic/check \
	text/scanner/check \
	text/tabwriter/check \
	text/template/check \
	text/template/parse/check \
	testing/quick/check \
	unicode/utf16/check \
	unicode/utf8/check

check: check-tail
check-recursive: check-head

check-head:
	@echo "Test Run By $${USER} on `date`" > libgo.head
	@echo "Native configuration is $(host_triplet)" >> libgo.head
	@echo >> libgo.head
	@echo "		=== libgo tests ===" >> libgo.head
	@echo >> libgo.head

check-tail: check-recursive check-multi
	@if test "$(USE_DEJAGNU)" = "yes"; then \
	  exit 0; \
	fi; \
	lib=`${PWD_COMMAND} | sed -e 's,^.*/\([^/][^/]*\)$$,\1,'`; \
	for dir in . $(MULTIDIRS); do \
	  mv ../$${dir}/$${lib}/libgo.sum ../$${dir}/$${lib}/libgo.sum.sep; \
	  mv ../$${dir}/$${lib}/libgo.log ../$${dir}/$${lib}/libgo.log.sep; \
	done; \
	mv libgo.head libgo.sum; \
	cp libgo.sum libgo.log; \
	echo "Schedule of variations:" >> libgo.sum; \
	for dir in . $(MULTIDIRS); do \
	  multidir=../$${dir}/$${lib}; \
	  multivar=`cat $${multidir}/libgo.var`; \
	  echo "    $${multivar}" >> libgo.sum; \
	done; \
	echo >> libgo.sum; \
	pass=0; fail=0; untested=0; \
	for dir in . $(MULTIDIRS); do \
	  multidir=../$${dir}/$${lib}; \
	  multivar=`cat $${multidir}/libgo.var`; \
	  echo "Running target $${multivar}" >> libgo.sum; \
	  echo "Running $(srcdir)/libgo.exp ..." >> libgo.sum; \
	  cat $${multidir}/libgo.sum.sep >> libgo.sum; \
	  cat $${multidir}/libgo.log.sep >> libgo.log; \
	  if test -n "${MULTIDIRS}"; then \
	    echo "		=== libgo Summary for $${multivar} ===" >> libgo.sum; \
	    echo >> libgo.sum; \
	  fi; \
	  p=`grep -c PASS $${multidir}/libgo.sum.sep`; \
	  pass=`expr $$pass + $$p`; \
	  if test "$$p" -ne "0" && test -n "${MULTIDIRS}"; then \
	    echo "# of expected passes		$$p" >> libgo.sum; \
	  fi; \
	  p=`grep -c FAIL $${multidir}/libgo.sum.sep`; \
	  fail=`expr $$fail + $$p`; \
	  if test "$$p" -ne "0" && test -n "${MULTIDIRS}"; then \
	    echo "# of unexpected failures	$$p" >> libgo.sum; \
	  fi; \
	  p=`grep -c UNTESTED $${multidir}/libgo.sum.sep`; \
	  untested=`expr $$untested + $$p`; \
	  if test "$$p" -ne "0" && test -n "${MULTIDIRS}"; then \
	    echo "# of untested testcases		$$p" >> libgo.sum; \
	  fi; \
	done; \
	echo >> libgo.sum; \
	echo "		=== libgo Summary ===" >> libgo.sum; \
	echo >> libgo.sum; \
	if test "$$pass" -ne "0"; then \
	  echo "# of expected passes		$$pass" >> libgo.sum; \
	fi; \
	if test "$$fail" -ne "0"; then \
	  echo "# of unexpected failures	$$fail" >> libgo.sum; \
	fi; \
	if test "$$untested" -ne "0"; then \
	  echo "# of untested testcases		$$untested" >> libgo.sum; \
	fi; \
	echo `echo $(GOC) | sed -e 's/ .*//'`  `$(GOC) -v 2>&1 | grep " version" | sed -n -e 's/.* \(version.*$$\)/\1/p'` >> libgo.sum; \
	echo >> libgo.log; \
	echo "runtest completed at `date`" >> libgo.log; \
	if test "$$fail" -ne "0"; then \
	  status=1; \
	else \
	  status=0; \
	fi; \
	exit $$status

check-am:
	@rm -f libgo.sum libgo.log libgo.tail
	@multivar="unix"; \
	[ -z "$(MULTIFLAGS)" ] || multivar="$${multivar}/$(MULTIFLAGS)"; \
	echo "$${multivar}" > libgo.var
	@for f in $(TEST_PACKAGES); do \
	   rm -f $$f-testsum $$f-testlog; \
	 done
	-@$(MAKE) -k $(TEST_PACKAGES)
	@for f in $(TEST_PACKAGES); do \
	  if test -f $$f-testsum; then \
	    cat $$f-testsum >> libgo.sum; \
	  fi; \
	  if test -f $$f-testlog; then \
	    cat $$f-testlog >> libgo.log; \
	  fi; \
	done

check-multi:
	$(MULTIDO) $(AM_MAKEFLAGS) DO=check-am multi-do # $(MAKE)

bench:
	-@$(MAKE) -k $(TEST_PACKAGES) GOBENCH=.

MOSTLYCLEAN_FILES = libgo.head libgo.sum.sep libgo.log.sep

mostlyclean-local:
	find . -name '*.lo' -print | xargs $(LIBTOOL) --mode=clean rm -f
	find . -name '*.$(OBJEXT)' -print | xargs rm -f
	find . -name '*-testsum' -print | xargs rm -f
	find . -name '*-testlog' -print | xargs rm -f

CLEANFILES = *.go *.gox goc2c *.c s-version libgo.sum libgo.log

clean-local:
	find . -name '*.la' -print | xargs $(LIBTOOL) --mode=clean rm -f
	find . -name '*.a' -print | xargs rm -f
