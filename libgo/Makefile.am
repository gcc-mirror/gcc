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

AM_CFLAGS = -fexceptions -fnon-call-exceptions \
	$(SPLIT_STACK) $(WARN_CFLAGS) \
	$(STRINGOPS_FLAG) $(HWCAP_CFLAGS) $(OSCFLAGS) \
	-I $(srcdir)/../libgcc -I $(srcdir)/../libbacktrace \
	-I $(MULTIBUILDTOP)../../gcc/include

AM_LDFLAGS =

if USING_SPLIT_STACK
AM_LDFLAGS += -XCClinker $(SPLIT_STACK)
endif

if LIBGO_IS_AIX
# Using an import file for libgo avoid requiring to use the -brtl flag
# when builing a go program
AM_LDFLAGS += -Wl,-bbigtoc -Wl,-bI:$(srcdir)/libgo.imp
EXTRA_libgo_la_DEPENDENCIES = libgo.imp
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
toolexeclib_LIBRARIES = libgobegin.a libgolibbegin.a
endif

noinst_LIBRARIES = libgotool.a

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
	debug/plan9obj.gox \
	debug/xcoff.gox

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
	math/bits.gox \
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
	runtime/pprof.gox \
	runtime/trace.gox

toolexeclibgosyncdir = $(toolexeclibgodir)/sync

toolexeclibgosync_DATA = \
	sync/atomic.gox

toolexeclibgotestingdir = $(toolexeclibgodir)/testing

toolexeclibgotesting_DATA = \
	testing/iotest.gox \
	testing/quick.gox

toolexeclibgotestinginternaldir = $(toolexeclibgotestingdir)/internal

toolexeclibgotestinginternal_DATA = \
	testing/internal/testdeps.gox

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

# Some packages are only needed for tests, so unlike the other
# internal packages nothing will explicitly depend on them.
# Force them to be built.
noinst_DATA = \
	golang_org/x/net/internal/nettest.gox \
	golang_org/x/net/nettest.gox \
	internal/testenv.gox \
	internal/trace.gox \
	net/internal/socktest.gox \
	os/signal/internal/pty.gox \
	runtime/pprof/internal/profile.gox

if LIBGO_IS_RTEMS
rtems_task_variable_add_file = runtime/rtems-task-variable-add.c
else
rtems_task_variable_add_file =
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
if LIBGO_IS_AIX
runtime_getncpu_file = runtime/getncpu-aix.c
else
runtime_getncpu_file = runtime/getncpu-none.c
endif
endif
endif
endif
endif
endif
endif

runtime_files = \
	runtime/aeshash.c \
	runtime/go-assert.c \
	runtime/go-caller.c \
	runtime/go-callers.c \
	runtime/go-cdiv.c \
	runtime/go-cgo.c \
	runtime/go-construct-map.c \
	runtime/go-ffi.c \
	runtime/go-fieldtrack.c \
	runtime/go-matherr.c \
	runtime/go-memclr.c \
	runtime/go-memcmp.c \
	runtime/go-memequal.c \
	runtime/go-memmove.c \
	runtime/go-nanotime.c \
	runtime/go-now.c \
	runtime/go-nosys.c \
	runtime/go-reflect-call.c \
	runtime/go-runtime-error.c \
	runtime/go-setenv.c \
	runtime/go-signal.c \
	runtime/go-strslice.c \
	runtime/go-typedesc-equal.c \
	runtime/go-unsafe-pointer.c \
	runtime/go-unsetenv.c \
	runtime/go-unwind.c \
	runtime/go-varargs.c \
	runtime/env_posix.c \
	runtime/panic.c \
	runtime/print.c \
	runtime/proc.c \
	runtime/runtime_c.c \
	runtime/stack.c \
	runtime/thread.c \
	runtime/yield.c \
	$(rtems_task_variable_add_file) \
	$(runtime_getncpu_file)

version.go: s-version; @true
s-version: Makefile
	rm -f version.go.tmp
	echo "package sys" > version.go.tmp
	echo 'func init() { DefaultGoroot = "$(prefix)" }' >> version.go.tmp
	echo 'const TheVersion = "'`cat $(srcdir)/VERSION | sed 1q`' '`$(GOC) --version | sed 1q`'"' >> version.go.tmp
	echo 'const Goexperiment = ``' >> version.go.tmp
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
	echo "  ArchFamily = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) family`" >> version.go.tmp
	echo "  BigEndian = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) bigendian`" >> version.go.tmp
	echo "  CacheLineSize = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) cachelinesize`" >> version.go.tmp
	echo "  DefaultPhysPageSize = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) defaultphyspagesize`" >> version.go.tmp
	echo "  HugePageSize = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) hugepagesize`" >> version.go.tmp
	echo "  Int64Align = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) int64align`" >> version.go.tmp
	echo "  MinFrameSize = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) minframesize`" >> version.go.tmp
	echo "  PCQuantum = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) pcquantum`" >> version.go.tmp
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

cpugen.go: s-cpu; @true
s-cpu: Makefile
	rm -f cpugen.go.tmp
	echo "package cpu" > cpugen.go.tmp
	echo "const CacheLineSize = `$(SHELL) $(srcdir)/goarch.sh $(GOARCH) cachelinesize`" >> cpugen.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh cpugen.go.tmp cpugen.go
	$(STAMP) $@

objabi.go: s-objabi; @true
s-objabi: Makefile
	rm -f objabi.go.tmp
	echo "package objabi" > objabi.go.tmp
	echo "import \"runtime\"" >> objabi.go.tmp
	echo 'func init() { defaultGOROOT = `$(prefix)` }' >> objabi.go.tmp
	echo 'const defaultGO386 = `sse2`' >> objabi.go.tmp
	echo 'const defaultGOARM = `5`' >> objabi.go.tmp
	echo 'const defaultGOMIPS = `hardfloat`' >> objabi.go.tmp
	echo 'const defaultGOMIPS64 = `hardfloat`' >> objabi.go.tmp
	echo 'const defaultGOOS = runtime.GOOS' >> objabi.go.tmp
	echo 'const defaultGOARCH = runtime.GOARCH' >> objabi.go.tmp
	echo 'const defaultGO_EXTLINK_ENABLED = ``' >> objabi.go.tmp
	echo 'const version = `'`cat $(srcdir)/VERSION | sed 1q`' '`$(GOC) --version | sed 1q`'`' >> objabi.go.tmp
	echo 'const stackGuardMultiplier = 1' >> objabi.go.tmp
	echo 'const goexperiment = ``' >> objabi.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh objabi.go.tmp objabi.go
	$(STAMP) $@

gccgosizes.go: s-gccgosizes; @true
s-gccgosizes: Makefile goarch.sh
	rm -f gccgosizes.go.tmp
	echo "package types" > gccgosizes.go.tmp
	echo >> gccgosizes.go.tmp
	echo "var gccgoArchSizes = map[string]*StdSizes{" >> gccgosizes.go.tmp
	for a in $(ALLGOARCH); do \
	  ptrsize=`$(SHELL) $(srcdir)/goarch.sh $$a ptrsize`; \
	  maxalign=`$(SHELL) $(srcdir)/goarch.sh $$a maxalign`; \
	  echo "	\"$$a\": {$${ptrsize}, $${maxalign}}," >> gccgosizes.go.tmp; \
	done
	echo "}" >> gccgosizes.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh gccgosizes.go.tmp gccgosizes.go
	$(STAMP) $@

runtime_sysinfo.go: s-runtime_sysinfo; @true
s-runtime_sysinfo: $(srcdir)/mkrsysinfo.sh gen-sysinfo.go
	GOARCH=$(GOARCH) GOOS=$(GOOS) $(SHELL) $(srcdir)/mkrsysinfo.sh
	$(SHELL) $(srcdir)/mvifdiff.sh tmp-runtime_sysinfo.go runtime_sysinfo.go
	$(STAMP) $@

sigtab.go: s-sigtab; @true
s-sigtab: $(srcdir)/mksigtab.sh gen-sysinfo.go
	GOOS=$(GOOS) $(SHELL) $(srcdir)/mksigtab.sh > tmp-sigtab.go
	$(SHELL) $(srcdir)/mvifdiff.sh tmp-sigtab.go sigtab.go
	$(STAMP) $@

GCCGO_INSTALL_NAME := $(shell echo gccgo|sed '$(program_transform_name)')
GCC_INSTALL_NAME := $(shell echo gcc|sed '$(program_transform_name)')
GXX_INSTALL_NAME := $(shell echo g++|sed '$(program_transform_name)')

zdefaultcc.go: s-zdefaultcc; @true
s-zdefaultcc: Makefile
	echo 'package cfg' > zdefaultcc.go.tmp
	echo >> zdefaultcc.go.tmp
	echo 'func DefaultGCCGO(goos, goarch string) string { return "$(bindir)/$(GCCGO_INSTALL_NAME)" }' >> zdefaultcc.go.tmp
	echo 'func DefaultCC(goos, goarch string) string { return "$(GCC_INSTALL_NAME)" }' >> zdefaultcc.go.tmp
	echo 'func DefaultCXX(goos, goarch string) string { return "$(GXX_INSTALL_NAME)" }' >> zdefaultcc.go.tmp
	echo 'const DefaultPkgConfig = "pkg-config"' >> zdefaultcc.go.tmp
	echo 'var OSArchSupportsCgo = map[string]bool{}' >> zdefaultcc.go.tmp
	$(SHELL) $(srcdir)/../move-if-change zdefaultcc.go.tmp zdefaultcc.go
	$(STAMP) $@ 

# Post-process runtime.inc.raw (raw output of -fgo-c-header option when
# compiling runtime) to prune out certain types that should not be
# exported back to C. See comments in mkruntimeinc.sh for more details.
runtime.inc: s-runtime-inc; @true
s-runtime-inc: runtime.lo Makefile
	$(SHELL) $(srcdir)/mkruntimeinc.sh
	$(SHELL) $(srcdir)/mvifdiff.sh tmp-runtime.inc runtime.inc
	$(STAMP) $@

noinst_DATA += zdefaultcc.go

# Generate the list of go std packages that were included in libgo
zstdpkglist.go: s-zstdpkglist; @true
s-zstdpkglist: Makefile
	rm -f zstdpkglist.go.tmp
	echo 'package goroot' > zstdpkglist.go.tmp
	echo "" >> zstdpkglist.go.tmp
	echo 'var stdpkg = map[string]bool{' >> zstdpkglist.go.tmp
	echo $(libgo_go_objs) 'unsafe.lo' 'runtime/cgo.lo' | sed 's|[a-z0-9_/]*_c\.lo||g' | sed 's|\([a-z0-9_/]*\)\.lo|"\1": true,|g' >> zstdpkglist.go.tmp
	echo '}' >> zstdpkglist.go.tmp
	$(SHELL) $(srcdir)/mvifdiff.sh zstdpkglist.go.tmp zstdpkglist.go
	$(STAMP) $@

if LIBGO_IS_LINUX
syscall_epoll_file = epoll.go
else
syscall_epoll_file =
endif

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
	GOARCH=$(GOARCH) GOOS=$(GOOS) $(SHELL) $(srcdir)/mksysinfo.sh
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

PACKAGES = $(shell cat $(srcdir)/libgo-packages.txt)

libgo_go_objs = \
	$(addsuffix .lo,$(PACKAGES)) \
	bytes/index.lo \
	internal/bytealg/bytealg.lo \
	reflect/makefunc_ffi_c.lo \
	strings/index.lo \
	$(syscall_lib_clone_lo) \
	syscall/errno.lo \
	syscall/signame.lo \
	syscall/wait.lo \
	$(golang_org_x_net_lif_lo) \
	$(golang_org_x_net_route_lo) \
	log/syslog/syslog_c.lo \
	$(os_lib_inotify_lo) \
	runtime/internal/atomic_c.lo \
	sync/atomic_c.lo \
	internal/cpu/cpu_gccgo.lo

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

GOTOOL_PACKAGES = $(shell cat $(srcdir)/gotool-packages.txt)

libgotool_a_SOURCES =
libgotool_a_DEPENDENCIES = $(addsuffix .lo,$(GOTOOL_PACKAGES))
libgotool_a_LIBADD = $(addsuffix .o,$(GOTOOL_PACKAGES))

define STATIC_template
$(subst -,_,$(subst .,_,$(subst /,_,$(1))))_GOCFLAGS = -static
endef

$(foreach package,$(GOTOOL_PACKAGES),$(eval $(call STATIC_template,$(package).lo)))

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
	dir=`echo $@ | sed -e 's/.lo.dep$$//'`; \
	files=`$(SHELL) $(srcdir)/match.sh --goarch=$(GOARCH) --goos=$(GOOS) --srcdir=$(srcdir)/go/$$dir --extrafiles="$(extra_go_files_$(subst /,_,$(subst .lo.dep,,$@)))" $(matchargs_$(subst /,_,$(subst .lo.dep,,$@)))`; \
	$(SHELL) $(srcdir)/godeps.sh `echo $@ | sed -e 's/.dep$$//'` $$files > $@.tmp; \
	if ! cmp $@.tmp $@ >/dev/null 2>/dev/null; then \
	  rm -f `echo $@ | sed -e 's/\.dep$$//'`; \
	fi; \
	mv -f $@.tmp $@

# Build the .go files for a package, generating a .lo file.
BUILDPACKAGE = \
	$(MKDIR_P) $(@D); \
	files=`echo $^ | sed -e 's/[^ ]*\.gox//g' -e 's/[^ ]*\.dep//'`; \
	$(LTGOCOMPILE) -I . -c -fgo-pkgpath=`echo $@ | sed -e 's/.lo$$//' -e 's|golang_org|vendor/golang_org|'` $($(subst -,_,$(subst .,_,$(subst /,_,$@)))_GOCFLAGS) -o $@ $$files

# How to build a .gox file from a .lo file.
# Matching .o file can either be in the same directory as the .lo (non-PIC
# object) or in the .libs directory (PIC object).
BUILDGOX = \
	f="$(basename $<).o"; \
	if test ! -f $$f; then \
	  f="$(basename $(<D)/.libs/$(<F)).o"; \
	fi; \
	$(OBJCOPY) -j .go_export $$f $@.tmp; \
	$(SHELL) $(srcdir)/mvifdiff.sh $@.tmp `echo $@ | sed -e 's/s-gox/gox/'`

GOTESTFLAGS =
GOBENCH = 

# Check a package.
CHECK = \
	GC="$(GOC) $(GOCFLAGS) $($(subst /,_,$@)_GOCFLAGS) -L `${PWD_COMMAND}` -L `${PWD_COMMAND}`/.libs"; \
	export GC; \
	GOLIBS="$(extra_check_libs_$(subst /,_,$(@D))) $(MATH_LIBS) $(NET_LIBS) $(LIBS)"; \
	export GOLIBS; \
	RUNTESTFLAGS="$(RUNTESTFLAGS)"; \
	export RUNTESTFLAGS; \
	MAKE="$(MAKE)"; \
	export MAKE; \
	NM="$(NM)"; \
	export NM; \
	libgccdir=`${GOC} ${GOCFLAGS} -print-libgcc-file-name | sed -e 's|/[^/]*$$||'`; \
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
	$(toolexeclibgounicode_DATA) \
	$(noinst_DATA) \
	$(noinst_LIBRARIES)

if GOC_IS_LLGO
CHECK_DEPS += libgo-llgo.la libgobegin-llgo.a
else
CHECK_DEPS += libgo.la libgobegin.a
endif

# PACKAGE_template defines the rules for each package.
# For example, for the package bufio, it produces:
#
# @go_include@ bufio.lo.dep
# bufio.lo.dep: $(srcdir)/go/bufio/*.go
# 	$(BUILDDEPS)
# bufio.lo:
# 	$(BUILDPACKAGE)
# bufio/check: $(CHECK_DEPS)
# 	@$(CHECK)
# .PHONY: bufio/check
#
# This is invoked with $(1) set to a package, which is a directory name,
# such as "bufio" or "archive/tar".
define PACKAGE_template
@go_include@ $(1).lo.dep
$(1).lo.dep: $(srcdir)/go/$(1)/*.go
	$$(BUILDDEPS)
$(1).lo:
	$$(BUILDPACKAGE)
$(1)/check: $$(CHECK_DEPS)
	@$$(CHECK)
.PHONY: $(1)/check
$(1).gox: $(1).s-gox; @true
$(1).s-gox: $(1).lo
	$$(BUILDGOX)
	$$(STAMP) $$@
endef

# This line expands PACKAGE_template once for each package name listed
# in $(PACKAGES).
$(foreach package,$(PACKAGES),$(eval $(call PACKAGE_template,$(package))))
$(foreach package,$(GOTOOL_PACKAGES),$(eval $(call PACKAGE_template,$(package))))

# Pass -ffp-contract=off, or 386-specific options, when building the
# math package.  MATH_FLAG is defined in configure.ac.
math_lo_GOCFLAGS = $(MATH_FLAG)
math_check_GOCFLAGS = $(MATH_FLAG)

# Add the generated file runtime_sysinfo.go to the runtime package.
extra_go_files_runtime = runtime_sysinfo.go sigtab.go
runtime.lo.dep: $(extra_go_files_runtime)

# Add generated files to the syscall package.
extra_go_files_syscall = \
	libcalls.go \
	sysinfo.go \
	syscall_arch.go \
	$(syscall_epoll_file)
syscall.lo.dep: $(extra_go_files_syscall)

# Pass -fgo-compiling-runtime when compiling the runtime package.
runtime_lo_GOCFLAGS = -fgo-c-header=runtime.inc.raw -fgo-compiling-runtime
runtime_check_GOCFLAGS = -fgo-compiling-runtime
runtime_internal_atomic_lo_GOCFLAGS = -fgo-compiling-runtime
runtime_internal_atomic_lo_check_GOCFLAGS = -fgo-compiling-runtime
runtime_internal_sys_lo_GOCFLAGS = -fgo-compiling-runtime
runtime_internal_sys_lo_check_GOCFLAGS = -fgo-compiling-runtime

# If libffi is supported (the normal case) use the ffi build tag for
# the runtime package.
if USE_LIBFFI
matchargs_runtime = --tag=libffi
else
matchargs_runtime =
endif

# At least for now, we need -static-libgo for this test, because
# otherwise we can't get the line numbers.
# Also use -fno-inline to get better results from the memory profiler.
runtime_pprof_check_GOCFLAGS = -static-libgo -fno-inline

extra_go_files_runtime_internal_sys = version.go
runtime/internal/sys.lo.dep: $(extra_go_files_runtime_internal_sys)

extra_go_files_internal_cpu = cpugen.go
internal/cpu.lo.dep: $(extra_go_files_internal_cpu)

extra_go_files_internal_goroot = zstdpkglist.go
internal/goroot.lo.dep: $(extra_go_files_internal_goroot)

extra_go_files_go_types = gccgosizes.go
go/types.lo.dep: $(extra_go_files_go_types)

extra_go_files_cmd_internal_objabi = objabi.go
cmd/internal/objabi.lo.dep: $(extra_go_files_cmd_internal_objabi)

extra_go_files_cmd_go_internal_cfg = zdefaultcc.go
cmd/go/internal/cfg.lo.dep: $(extra_go_files_cmd_go_internal_cfg)

extra_check_libs_cmd_go_internal_cache = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_generate = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_get = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_load = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_imports = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_modconv = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_modfetch = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_modfetch_codehost = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_modfile = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_modload = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_module = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_mvs = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_search = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_web2 = $(abs_builddir)/libgotool.a
extra_check_libs_cmd_go_internal_work = $(abs_builddir)/libgotool.a

extra_check_libs_cmd_vet_internal_cfg = $(abs_builddir)/libgotool.a

# FIXME: The following C files may as well move to the runtime
# directory and be treated like other C files.

# Use C code to speed up {bytes,strings}.IndexByte and friends.
bytes/index.lo: go/bytes/indexbyte.c runtime.inc
	@$(MKDIR_P) bytes
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/bytes/indexbyte.c
strings/index.lo: go/strings/indexbyte.c runtime.inc
	@$(MKDIR_P) strings
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/strings/indexbyte.c
internal/bytealg/bytealg.lo: go/internal/bytealg/bytealg.c runtime.inc
	@$(MKDIR_P) internal/bytealg
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/internal/bytealg/bytealg.c

# Use a C function with a fixed number of arguments to call a C
# varargs function.
log/syslog/syslog_c.lo: go/log/syslog/syslog_c.c runtime.inc
	@$(MKDIR_P) log/syslog
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/log/syslog/syslog_c.c

# The interface to libffi from the reflect package is written in C.
reflect/makefunc_ffi_c.lo: go/reflect/makefunc_ffi_c.c runtime.inc
	@$(MKDIR_P) reflect
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/reflect/makefunc_ffi_c.c

# The atomic functions are written in C.
runtime/internal/atomic_c.lo: go/runtime/internal/atomic/atomic.c runtime.inc
	@$(MKDIR_P) runtime/internal
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/runtime/internal/atomic/atomic.c
sync/atomic_c.lo: go/sync/atomic/atomic.c runtime.inc
	@$(MKDIR_P) sync
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/sync/atomic/atomic.c

# A few syscall functions are written in C.
syscall/clone_linux.lo: go/syscall/clone_linux.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/syscall/clone_linux.c
syscall/errno.lo: go/syscall/errno.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/syscall/errno.c
syscall/signame.lo: go/syscall/signame.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/syscall/signame.c
syscall/wait.lo: go/syscall/wait.c runtime.inc
	@$(MKDIR_P) syscall
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/syscall/wait.c

# internal/cpu needs some C code.
internal/cpu/cpu_gccgo.lo: go/internal/cpu/cpu_gccgo.c runtime.inc
	@$(MKDIR_P) internal/cpu
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/internal/cpu/cpu_gccgo.c

# Solaris 11.4 changed the type of fields in struct stat.
# Use a build tag, based on a configure check, to cope.
if LIBGO_IS_SOLARIS
if HAVE_STAT_TIMESPEC
matchargs_os = --tag=solaristag
else
matchargs_os =
endif
else
matchargs_os =
endif

if LIBGO_IS_BSD

# Build golang_org/x/net/route only on BSD systems.

$(eval $(call PACKAGE_template,golang_org/x/net/route))

golang_org_x_net_route_lo = \
	golang_org/x/net/route.lo
golang_org_x_net_route_check = \
	golang_org/x/net/route/check

endif

if LIBGO_IS_SOLARIS

# Build golang_org/x/net/lif only on Solaris systems.

$(eval $(call PACKAGE_template,golang_org/x/net/lif))

golang_org_x_net_lif_lo = \
	golang_org/x/net/lif.lo
golang_org_x_net_lif_check = \
	golang_org/x/net/lif/check

endif

TPACKAGES = $(shell cat $(srcdir)/check-packages.txt)
TEST_PACKAGES = $(addsuffix /check,$(TPACKAGES)) \
	$(golang_org_x_net_lif_check) \
	$(golang_org_x_net_route_check)

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
	-@$(MAKE) $(AM_MAKEFLAGS) -k $(TEST_PACKAGES)
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
	-@$(MAKE) $(AM_MAKEFLAGS) -k $(TEST_PACKAGES) GOBENCH=.

MOSTLYCLEANFILES = \
	s-runtime_sysinfo s-sigtab s-runtime-inc s-zstdpkglist \
	s-libcalls s-libcalls-list s-syscall_arch s-gen-sysinfo s-sysinfo \
	s-errno s-epoll \
	libgo.head libgo.sum.sep libgo.log.sep libgo.var \
	libcalls-list runtime.inc runtime.inc.tmp2 runtime.inc.tmp3

mostlyclean-local:
	find . -name '*.lo' -print | xargs $(LIBTOOL) --mode=clean rm -f
	find . -name '*.$(OBJEXT)' -print | xargs rm -f
	find . -name '*-testsum' -print | xargs rm -f
	find . -name '*-testlog' -print | xargs rm -f

CLEANFILES = *.go *.c s-* libgo.sum libgo.log runtime.inc

clean-local:
	find . -name '*.la' -print | xargs $(LIBTOOL) --mode=clean rm -f
	find . -name '*.a' -print | xargs rm -f
	find . -name '*.gox' -print | xargs rm -f
	find . -name '*.s-gox' -print | xargs rm -f

distclean-local:
	find . -name '*.lo.dep' -print | xargs rm -f
