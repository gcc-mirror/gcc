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

gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

MAINT_CHARSET = latin1

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs
PWD_COMMAND = $${PWDCMD-pwd}
STAMP = echo timestamp >

toolexecdir = $(glibgo_toolexecdir)
toolexeclibdir = $(glibgo_toolexeclibdir)

LIBFFI = @LIBFFI@
LIBFFIINCS = @LIBFFIINCS@

WARN_CFLAGS = $(WARN_FLAGS) $(WERROR)

# -I/-D flags to pass when compiling.
AM_CPPFLAGS = -I $(srcdir)/runtime $(LIBFFIINCS) $(PTHREAD_CFLAGS)

ACLOCAL_AMFLAGS = -I ./config -I ../config

AM_CFLAGS = -fexceptions -fplan9-extensions $(SPLIT_STACK) $(WARN_CFLAGS) \
	$(STRINGOPS_FLAG) \
	-I $(srcdir)/../gcc -I $(MULTIBUILDTOP)../../gcc/include

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

toolexeclib_LTLIBRARIES = libgo.la
toolexeclib_LIBRARIES = libgobegin.a

toolexeclibgodir = $(toolexeclibdir)/go/$(gcc_version)/$(target_alias)

toolexeclibgo_DATA = \
	asn1.gox \
	big.gox \
	bufio.gox \
	bytes.gox \
	cmath.gox \
	ebnf.gox \
	exec.gox \
	expvar.gox \
	flag.gox \
	fmt.gox \
	gob.gox \
	hash.gox \
	html.gox \
	http.gox \
	image.gox \
	io.gox \
	json.gox \
	log.gox \
	math.gox \
	mime.gox \
	net.gox \
	netchan.gox \
	os.gox \
	patch.gox \
	path.gox \
	rand.gox \
	reflect.gox \
	regexp.gox \
	rpc.gox \
	runtime.gox \
	scanner.gox \
	smtp.gox \
	sort.gox \
	strconv.gox \
	strings.gox \
	sync.gox \
	syscall.gox \
	syslog.gox \
	tabwriter.gox \
	template.gox \
	testing.gox \
	time.gox \
	try.gox \
	unicode.gox \
	utf16.gox \
	utf8.gox \
	websocket.gox \
	xml.gox

toolexeclibgoarchivedir = $(toolexeclibgodir)/archive

toolexeclibgoarchive_DATA = \
	archive/tar.gox \
	archive/zip.gox

toolexeclibgocompressdir = $(toolexeclibgodir)/compress

toolexeclibgocompress_DATA = \
	compress/flate.gox \
	compress/gzip.gox \
	compress/zlib.gox

toolexeclibgocontainerdir = $(toolexeclibgodir)/container

toolexeclibgocontainer_DATA = \
	container/heap.gox \
	container/list.gox \
	container/ring.gox \
	container/vector.gox

toolexeclibgocryptodir = $(toolexeclibgodir)/crypto

toolexeclibgocrypto_DATA = \
	crypto/aes.gox \
	crypto/block.gox \
	crypto/blowfish.gox \
	crypto/cast5.gox \
	crypto/cipher.gox \
	crypto/elliptic.gox \
	crypto/hmac.gox \
	crypto/md4.gox \
	crypto/md5.gox \
	crypto/ocsp.gox \
	crypto/rand.gox \
	crypto/rc4.gox \
	crypto/ripemd160.gox \
	crypto/rsa.gox \
	crypto/sha1.gox \
	crypto/sha256.gox \
	crypto/sha512.gox \
	crypto/subtle.gox \
	crypto/tls.gox \
	crypto/twofish.gox \
	crypto/x509.gox \
	crypto/xtea.gox

toolexeclibgocryptoopenpgpdir = $(toolexeclibgocryptodir)/openpgp

toolexeclibgocryptoopenpgp_DATA = \
	crypto/openpgp/armor.gox \
	crypto/openpgp/error.gox \
	crypto/openpgp/s2k.gox

toolexeclibgodebugdir = $(toolexeclibgodir)/debug

toolexeclibgodebug_DATA = \
	debug/dwarf.gox \
	debug/elf.gox \
	debug/gosym.gox \
	debug/macho.gox \
	debug/pe.gox \
	debug/proc.gox

toolexeclibgoencodingdir = $(toolexeclibgodir)/encoding

toolexeclibgoencoding_DATA = \
	encoding/ascii85.gox \
	encoding/base32.gox \
	encoding/base64.gox \
	encoding/binary.gox \
	encoding/line.gox \
	encoding/git85.gox \
	encoding/hex.gox \
	encoding/pem.gox

toolexeclibgoexpdir = $(toolexeclibgodir)/exp

toolexeclibgoexp_DATA = \
	exp/datafmt.gox \
	exp/draw.gox \
	exp/eval.gox

toolexeclibgogodir = $(toolexeclibgodir)/go

toolexeclibgogo_DATA = \
	go/ast.gox \
	go/doc.gox \
	go/parser.gox \
	go/printer.gox \
	go/scanner.gox \
	go/token.gox \
	go/typechecker.gox

toolexeclibgohashdir = $(toolexeclibgodir)/hash

toolexeclibgohash_DATA = \
	hash/adler32.gox \
	hash/crc32.gox \
	hash/crc64.gox

toolexeclibgohttpdir = $(toolexeclibgodir)/http

toolexeclibgohttp_DATA = \
	http/pprof.gox

toolexeclibgoimagedir = $(toolexeclibgodir)/image

toolexeclibgoimage_DATA = \
	image/jpeg.gox \
	image/png.gox

toolexeclibgoindexdir = $(toolexeclibgodir)/index

toolexeclibgoindex_DATA = \
	index/suffixarray.gox

toolexeclibgoiodir = $(toolexeclibgodir)/io

toolexeclibgoio_DATA = \
	io/ioutil.gox

toolexeclibgomimedir = $(toolexeclibgodir)/mime

toolexeclibgomime_DATA = \
	mime/multipart.gox

toolexeclibgonetdir = $(toolexeclibgodir)/net

toolexeclibgonet_DATA = \
	net/dict.gox \
	net/textproto.gox

toolexeclibgoosdir = $(toolexeclibgodir)/os

if LIBGO_IS_LINUX
# os_inotify_gox = os/inotify.gox
os_inotify_gox =
else
os_inotify_gox =
endif

toolexeclibgoos_DATA = \
	$(os_inotify_gox) \
	os/signal.gox

toolexeclibgorpcdir = $(toolexeclibgodir)/rpc

toolexeclibgorpc_DATA = \
	rpc/jsonrpc.gox

toolexeclibgoruntimedir = $(toolexeclibgodir)/runtime

toolexeclibgoruntime_DATA = \
	runtime/debug.gox \
	runtime/pprof.gox

toolexeclibgotestingdir = $(toolexeclibgodir)/testing

toolexeclibgotesting_DATA = \
	testing/iotest.gox \
	testing/quick.gox \
	testing/script.gox

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

runtime_files = \
	runtime/go-append.c \
	runtime/go-assert.c \
	runtime/go-assert-interface.c \
	runtime/go-byte-array-to-string.c \
	runtime/go-breakpoint.c \
	runtime/go-caller.c \
	runtime/go-can-convert-interface.c \
	runtime/go-cgo.c \
	runtime/go-chan-cap.c \
	runtime/go-chan-len.c \
	runtime/go-check-interface.c \
	runtime/go-close.c \
	runtime/go-closed.c \
	runtime/go-construct-map.c \
	runtime/go-convert-interface.c \
	runtime/go-copy.c \
	runtime/go-defer.c \
	runtime/go-deferred-recover.c \
	runtime/go-eface-compare.c \
	runtime/go-eface-val-compare.c \
	runtime/go-getgoroot.c \
	runtime/go-go.c \
	runtime/go-gomaxprocs.c \
	runtime/go-int-array-to-string.c \
	runtime/go-int-to-string.c \
	runtime/go-interface-compare.c \
	runtime/go-interface-eface-compare.c \
	runtime/go-interface-val-compare.c \
	runtime/go-lock-os-thread.c \
	runtime/go-map-delete.c \
	runtime/go-map-index.c \
	runtime/go-map-len.c \
	runtime/go-map-range.c \
	runtime/go-nanotime.c \
	runtime/go-new-channel.c \
	runtime/go-new-map.c \
	runtime/go-new.c \
	runtime/go-note.c \
	runtime/go-panic.c \
	runtime/go-panic-defer.c \
	runtime/go-print.c \
	runtime/go-rec-big.c \
	runtime/go-rec-nb-big.c \
	runtime/go-rec-nb-small.c \
	runtime/go-rec-small.c \
	runtime/go-recover.c \
	runtime/go-reflect.c \
	runtime/go-reflect-call.c \
	runtime/go-reflect-chan.c \
	runtime/go-reflect-map.c \
	runtime/go-rune.c \
	runtime/go-runtime-error.c \
	runtime/go-sched.c \
	runtime/go-select.c \
	runtime/go-semacquire.c \
	runtime/go-send-big.c \
	runtime/go-send-nb-big.c \
	runtime/go-send-nb-small.c \
	runtime/go-send-small.c \
	runtime/go-signal.c \
	runtime/go-strcmp.c \
	runtime/go-string-to-byte-array.c \
	runtime/go-string-to-int-array.c \
	runtime/go-strplus.c \
	runtime/go-strslice.c \
	runtime/go-trampoline.c \
	runtime/go-type-eface.c \
	runtime/go-type-error.c \
	runtime/go-type-identity.c \
	runtime/go-type-interface.c \
	runtime/go-type-string.c \
	runtime/go-typedesc-equal.c \
	runtime/go-typestring.c \
	runtime/go-unreflect.c \
	runtime/go-unsafe-new.c \
	runtime/go-unsafe-newarray.c \
	runtime/go-unsafe-pointer.c \
	runtime/go-unwind.c \
	runtime/mcache.c \
	runtime/mcentral.c \
	$(runtime_mem_file) \
	runtime/mfinal.c \
	runtime/mfixalloc.c \
	runtime/mgc0.c \
	runtime/mheap.c \
	runtime/mheapmap32.c \
	runtime/mheapmap64.c \
	runtime/msize.c \
	runtime/proc.c \
	runtime/thread.c \
	$(rtems_task_variable_add_file) \
	chan.c \
	iface.c \
	malloc.c \
	map.c \
	mprof.c \
	reflect.c \
	sigqueue.c \
	string.c

goc2c.$(OBJEXT): runtime/goc2c.c
	$(CC_FOR_BUILD) -c $(CFLAGS_FOR_BUILD) $<

goc2c: goc2c.$(OBJEXT)
	$(CC_FOR_BUILD) $(CFLAGS_FOR_BUILD) $(LDFLAGS_FOR_BUILD) -o $@ $<

malloc.c: $(srcdir)/runtime/malloc.goc goc2c
	./goc2c --gcc --go-prefix libgo_runtime $< > $@.tmp
	mv -f $@.tmp $@

mprof.c: $(srcdir)/runtime/mprof.goc goc2c
	./goc2c --gcc --go-prefix libgo_runtime $< > $@.tmp
	mv -f $@.tmp $@

reflect.c: $(srcdir)/runtime/reflect.goc goc2c
	./goc2c --gcc --go-prefix libgo_reflect $< > $@.tmp
	mv -f $@.tmp $@

sigqueue.c: $(srcdir)/runtime/sigqueue.goc goc2c
	./goc2c --gcc --go-prefix libgo_runtime $< > $@.tmp
	mv -f $@.tmp $@

%.c: $(srcdir)/runtime/%.goc goc2c
	./goc2c --gcc $< > $@.tmp
	mv -f $@.tmp $@

go_asn1_files = \
	go/asn1/asn1.go \
	go/asn1/common.go \
	go/asn1/marshal.go

go_big_files = \
	go/big/arith.go \
	go/big/int.go \
	go/big/nat.go \
	go/big/rat.go

go_bufio_files = \
	go/bufio/bufio.go

go_bytes_files = \
	go/bytes/buffer.go \
	go/bytes/bytes.go \
	go/bytes/bytes_decl.go
go_bytes_c_files = \
	go/bytes/indexbyte.c

go_cmath_files = \
	go/cmath/abs.go \
	go/cmath/asin.go \
	go/cmath/conj.go \
	go/cmath/exp.go \
	go/cmath/isinf.go \
	go/cmath/isnan.go \
	go/cmath/log.go \
	go/cmath/phase.go \
	go/cmath/polar.go \
	go/cmath/pow.go \
	go/cmath/rect.go \
	go/cmath/sin.go \
	go/cmath/sqrt.go \
	go/cmath/tan.go

go_ebnf_files = \
	go/ebnf/ebnf.go \
	go/ebnf/parser.go

go_exec_files = \
	go/exec/exec.go \
	go/exec/lp_unix.go

go_expvar_files = \
	go/expvar/expvar.go

go_flag_files = \
	go/flag/flag.go

go_fmt_files = \
	go/fmt/doc.go \
	go/fmt/format.go \
	go/fmt/print.go \
	go/fmt/scan.go

go_gob_files = \
	go/gob/decode.go \
	go/gob/decoder.go \
	go/gob/doc.go \
	go/gob/encode.go \
	go/gob/encoder.go \
	go/gob/error.go \
	go/gob/type.go

go_hash_files = \
	go/hash/hash.go

go_html_files = \
	go/html/doc.go \
	go/html/entity.go \
	go/html/escape.go \
	go/html/parse.go \
	go/html/token.go

go_http_files = \
	go/http/chunked.go \
	go/http/client.go \
	go/http/dump.go \
	go/http/fs.go \
	go/http/lex.go \
	go/http/persist.go \
	go/http/request.go \
	go/http/response.go \
	go/http/server.go \
	go/http/status.go \
	go/http/transfer.go \
	go/http/url.go

go_image_files = \
	go/image/color.go \
	go/image/format.go \
	go/image/geom.go \
	go/image/image.go \
	go/image/names.go

go_io_files = \
	go/io/multi.go \
	go/io/io.go \
	go/io/pipe.go

go_json_files = \
	go/json/decode.go \
	go/json/encode.go \
	go/json/indent.go \
	go/json/scanner.go \
	go/json/stream.go

go_log_files = \
	go/log/log.go

go_math_files = \
	go/math/acosh.go \
	go/math/asin.go \
	go/math/asinh.go \
	go/math/atan.go \
	go/math/atanh.go \
	go/math/atan2.go \
	go/math/bits.go \
	go/math/cbrt.go \
	go/math/const.go \
	go/math/copysign.go \
	go/math/erf.go \
	go/math/exp.go \
	go/math/exp_port.go \
	go/math/exp2.go \
	go/math/expm1.go \
	go/math/fabs.go \
	go/math/fdim.go \
	go/math/floor.go \
	go/math/fmod.go \
	go/math/frexp.go \
	go/math/gamma.go \
	go/math/hypot.go \
	go/math/hypot_port.go \
	go/math/j0.go \
	go/math/j1.go \
	go/math/jn.go \
	go/math/ldexp.go \
	go/math/lgamma.go \
	go/math/log.go \
	go/math/log1p.go \
	go/math/log10.go \
	go/math/logb.go \
	go/math/modf.go \
	go/math/nextafter.go \
	go/math/pow.go \
	go/math/pow10.go \
	go/math/remainder.go \
	go/math/signbit.go \
	go/math/sin.go \
	go/math/sincos.go \
	go/math/sinh.go \
	go/math/sqrt.go \
	go/math/sqrt_port.go \
	go/math/tan.go \
	go/math/tanh.go \
	go/math/unsafe.go

go_mime_files = \
	go/mime/grammar.go \
	go/mime/mediatype.go \
	go/mime/type.go

if LIBGO_IS_RTEMS
go_net_fd_os_file = go/net/fd_rtems.go
go_net_newpollserver_file = go/net/newpollserver_rtems.go
else # !LIBGO_IS_RTEMS
if LIBGO_IS_LINUX
go_net_fd_os_file = go/net/fd_linux.go
go_net_newpollserver_file = go/net/newpollserver.go
else # !LIBGO_IS_LINUX && !LIBGO_IS_RTEMS
# By default use select with pipes.  Most systems should have
# something better.
go_net_fd_os_file = go/net/fd_rtems.go
go_net_newpollserver_file = go/net/newpollserver.go
endif # !LIBGO_IS_LINUX
endif # !LIBGO_IS_RTEMS

go_net_files = \
	go/net/dial.go \
	go/net/dnsclient.go \
	go/net/dnsconfig.go \
	go/net/dnsmsg.go \
	$(go_net_newpollserver_file) \
	go/net/fd.go \
	$(go_net_fd_os_file) \
	go/net/hosts.go \
	go/net/ip.go \
	go/net/iprawsock.go \
	go/net/ipsock.go \
	go/net/net.go \
	go/net/parse.go \
	go/net/pipe.go \
	go/net/port.go \
	go/net/sock.go \
	go/net/tcpsock.go \
	go/net/udpsock.go \
	go/net/unixsock.go

go_netchan_files = \
	go/netchan/common.go \
	go/netchan/export.go \
	go/netchan/import.go

if LIBGO_IS_SOLARIS
if LIBGO_IS_386
go_os_dir_file = go/os/dir_largefile.go
else
go_os_dir_file = go/os/dir_regfile.go
endif
else
if LIBGO_IS_LINUX
go_os_dir_file = go/os/dir_largefile.go
else
go_os_dir_file = go/os/dir_regfile.go
endif
endif

if LIBGO_IS_LINUX
go_os_sys_file = go/os/sys_linux.go
else
if LIBGO_IS_SOLARIS
go_os_sys_file = go/os/sys_uname.go
else
if LIBGO_IS_RTEMS
go_os_sys_file = go/os/sys_uname.go
else
go_os_sys_file = go/os/sys_bsd.go
endif
endif
endif

go_os_files = \
	$(go_os_dir_file) \
	go/os/dir.go \
	go/os/env.go \
	go/os/env_unix.go \
	go/os/error.go \
	go/os/exec.go \
	go/os/file.go \
	go/os/file_unix.go \
	go/os/getwd.go \
	go/os/path.go \
	go/os/proc.go \
	go/os/stat.go \
	$(go_os_sys_file) \
	go/os/time.go \
	go/os/types.go

go_patch_files = \
	go/patch/apply.go \
	go/patch/git.go \
	go/patch/patch.go \
	go/patch/textdiff.go

go_path_files = \
	go/path/match.go \
	go/path/path.go \
	go/path/path_unix.go

go_rand_files = \
	go/rand/exp.go \
	go/rand/normal.go \
	go/rand/rand.go \
	go/rand/rng.go \
	go/rand/zipf.go

go_reflect_files = \
	go/reflect/deepequal.go \
	go/reflect/type.go \
	go/reflect/value.go

go_regexp_files = \
	go/regexp/regexp.go

go_rpc_files = \
	go/rpc/client.go \
	go/rpc/debug.go \
	go/rpc/server.go

go_runtime_files = \
	go/runtime/debug.go \
	go/runtime/error.go \
	go/runtime/extern.go \
	go/runtime/malloc_defs.go \
	go/runtime/runtime_defs.go \
	go/runtime/sig.go \
	go/runtime/softfloat64.go \
	go/runtime/type.go \
	version.go

version.go: s-version; @true
s-version: Makefile
	rm -f version.go.tmp
	echo "package runtime" > version.go.tmp
	echo 'const defaultGoroot = "$(prefix)"' >> version.go.tmp
	echo 'const theVersion = "'`$(CC) --version | sed 1q`'"' >> version.go.tmp
	echo 'const theGoarch = "'$(GOARCH)'"' >> version.go.tmp
	echo 'const theGoos = "'$(GOOS)'"' >> version.go.tmp
	$(SHELL) $(srcdir)/../move-if-change version.go.tmp version.go
	$(STAMP) $@

go_scanner_files = \
	go/scanner/scanner.go

go_smtp_files = \
	go/smtp/auth.go \
	go/smtp/smtp.go

go_sort_files = \
	go/sort/search.go \
	go/sort/sort.go

go_strconv_files = \
	go/strconv/atob.go \
	go/strconv/atof.go \
	go/strconv/atoi.go \
	go/strconv/decimal.go \
	go/strconv/ftoa.go \
	go/strconv/itoa.go \
	go/strconv/quote.go

go_strings_files = \
	go/strings/reader.go \
	go/strings/strings.go

go_sync_files = \
	go/sync/mutex.go \
	go/sync/once.go \
	go/sync/rwmutex.go
go_sync_c_files = \
	go/sync/cas.c

if LIBGO_IS_SOLARIS
go_syslog_file = go/syslog/syslog_solaris.go
else
go_syslog_file = go/syslog/syslog_unix.go
endif

go_syslog_files = \
	go/syslog/syslog.go \
	$(go_syslog_file)
go_syslog_c_files = \
	go/syslog/syslog_c.c

go_tabwriter_files = \
	go/tabwriter/tabwriter.go

go_template_files = \
	go/template/format.go \
	go/template/template.go

go_testing_files = \
	go/testing/benchmark.go \
	go/testing/testing.go

go_time_files = \
	go/time/format.go \
	go/time/sleep.go \
	go/time/tick.go \
	go/time/time.go \
	go/time/zoneinfo_unix.go

go_try_files = \
	go/try/try.go

go_unicode_files = \
	go/unicode/casetables.go \
	go/unicode/digit.go \
	go/unicode/letter.go \
	go/unicode/tables.go

go_utf16_files = \
	go/utf16/utf16.go

go_utf8_files = \
	go/utf8/string.go \
	go/utf8/utf8.go

go_websocket_files = \
	go/websocket/client.go \
	go/websocket/server.go \
	go/websocket/websocket.go

go_xml_files = \
	go/xml/read.go \
	go/xml/xml.go

go_archive_tar_files = \
	go/archive/tar/common.go \
	go/archive/tar/reader.go \
	go/archive/tar/writer.go

go_archive_zip_files = \
	go/archive/zip/reader.go \
	go/archive/zip/struct.go

go_compress_flate_files = \
	go/compress/flate/deflate.go \
	go/compress/flate/huffman_bit_writer.go \
	go/compress/flate/huffman_code.go \
	go/compress/flate/inflate.go \
	go/compress/flate/reverse_bits.go \
	go/compress/flate/token.go \
	go/compress/flate/util.go

go_compress_gzip_files = \
	go/compress/gzip/gzip.go \
	go/compress/gzip/gunzip.go

go_compress_zlib_files = \
	go/compress/zlib/reader.go \
	go/compress/zlib/writer.go

go_container_heap_files = \
	go/container/heap/heap.go

go_container_list_files = \
	go/container/list/list.go

go_container_ring_files = \
	go/container/ring/ring.go

go_container_vector_files = \
	go/container/vector/defs.go \
	go/container/vector/intvector.go \
	go/container/vector/stringvector.go \
	go/container/vector/vector.go

go_crypto_aes_files = \
	go/crypto/aes/block.go \
	go/crypto/aes/cipher.go \
	go/crypto/aes/const.go
go_crypto_block_files = \
	go/crypto/block/cbc.go \
	go/crypto/block/cfb.go \
	go/crypto/block/cmac.go \
	go/crypto/block/cipher.go \
	go/crypto/block/ctr.go \
	go/crypto/block/eax.go \
	go/crypto/block/ecb.go \
	go/crypto/block/ofb.go \
	go/crypto/block/xor.go
go_crypto_blowfish_files = \
	go/crypto/blowfish/block.go \
	go/crypto/blowfish/const.go \
	go/crypto/blowfish/cipher.go
go_crypto_cast5_files = \
	go/crypto/cast5/cast5.go
go_crypto_cipher_files = \
	go/crypto/cipher/cbc.go \
	go/crypto/cipher/cfb.go \
	go/crypto/cipher/cipher.go \
	go/crypto/cipher/ctr.go \
	go/crypto/cipher/io.go \
	go/crypto/cipher/ocfb.go \
	go/crypto/cipher/ofb.go
go_crypto_elliptic_files = \
	go/crypto/elliptic/elliptic.go
go_crypto_hmac_files = \
	go/crypto/hmac/hmac.go
go_crypto_md4_files = \
	go/crypto/md4/md4.go \
	go/crypto/md4/md4block.go
go_crypto_md5_files = \
	go/crypto/md5/md5.go \
	go/crypto/md5/md5block.go
go_crypto_ocsp_files = \
	go/crypto/ocsp/ocsp.go
go_crypto_rand_files = \
	go/crypto/rand/rand.go \
	go/crypto/rand/rand_unix.go
go_crypto_rc4_files = \
	go/crypto/rc4/rc4.go
go_crypto_ripemd160_files = \
	go/crypto/ripemd160/ripemd160.go \
	go/crypto/ripemd160/ripemd160block.go
go_crypto_rsa_files = \
	go/crypto/rsa/pkcs1v15.go \
	go/crypto/rsa/rsa.go
go_crypto_sha1_files = \
	go/crypto/sha1/sha1.go \
	go/crypto/sha1/sha1block.go
go_crypto_sha256_files = \
	go/crypto/sha256/sha256.go \
	go/crypto/sha256/sha256block.go
go_crypto_sha512_files = \
	go/crypto/sha512/sha512.go \
	go/crypto/sha512/sha512block.go
go_crypto_subtle_files = \
	go/crypto/subtle/constant_time.go
go_crypto_tls_files = \
	go/crypto/tls/alert.go \
	go/crypto/tls/ca_set.go \
	go/crypto/tls/cipher_suites.go \
	go/crypto/tls/common.go \
	go/crypto/tls/conn.go \
	go/crypto/tls/handshake_client.go \
	go/crypto/tls/handshake_messages.go \
	go/crypto/tls/handshake_server.go \
	go/crypto/tls/key_agreement.go \
	go/crypto/tls/prf.go \
	go/crypto/tls/tls.go
go_crypto_twofish_files = \
	go/crypto/twofish/twofish.go
go_crypto_x509_files = \
	go/crypto/x509/x509.go
go_crypto_xtea_files = \
	go/crypto/xtea/block.go \
	go/crypto/xtea/cipher.go

go_crypto_openpgp_armor_files = \
	go/crypto/openpgp/armor/armor.go \
	go/crypto/openpgp/armor/encode.go
go_crypto_openpgp_error_files = \
	go/crypto/openpgp/error/error.go
go_crypto_openpgp_s2k_files = \
	go/crypto/openpgp/s2k/s2k.go

go_debug_dwarf_files = \
	go/debug/dwarf/buf.go \
	go/debug/dwarf/const.go \
	go/debug/dwarf/entry.go \
	go/debug/dwarf/open.go \
	go/debug/dwarf/type.go \
	go/debug/dwarf/unit.go
go_debug_elf_files = \
	go/debug/elf/elf.go \
	go/debug/elf/file.go
go_debug_gosym_files = \
	go/debug/gosym/pclntab.go \
	go/debug/gosym/symtab.go
go_debug_macho_files = \
	go/debug/macho/file.go \
	go/debug/macho/macho.go
go_debug_pe_files = \
	go/debug/pe/file.go \
	go/debug/pe/pe.go

go_debug_proc_files = \
	go/debug/proc/proc.go \
	go/debug/proc/proc_$(GOOS).go \
	$(GO_DEBUG_PROC_REGS_OS_ARCH_FILE)

go_encoding_ascii85_files = \
	go/encoding/ascii85/ascii85.go
go_encoding_base32_files = \
	go/encoding/base32/base32.go
go_encoding_base64_files = \
	go/encoding/base64/base64.go
go_encoding_binary_files = \
	go/encoding/binary/binary.go
go_encoding_git85_files = \
	go/encoding/git85/git.go
go_encoding_hex_files = \
	go/encoding/hex/hex.go
go_encoding_line_files = \
	go/encoding/line/line.go
go_encoding_pem_files = \
	go/encoding/pem/pem.go

go_exp_datafmt_files = \
	go/exp/datafmt/datafmt.go \
	go/exp/datafmt/parser.go
go_exp_draw_files = \
	go/exp/draw/draw.go \
	go/exp/draw/event.go
go_exp_eval_files = \
	go/exp/eval/abort.go \
	go/exp/eval/bridge.go \
	go/exp/eval/compiler.go \
	go/exp/eval/expr.go \
	go/exp/eval/expr1.go \
	go/exp/eval/func.go \
	go/exp/eval/scope.go \
	go/exp/eval/stmt.go \
	go/exp/eval/type.go \
	go/exp/eval/typec.go \
	go/exp/eval/value.go \
	go/exp/eval/world.go

go_go_ast_files = \
	go/go/ast/ast.go \
	go/go/ast/filter.go \
	go/go/ast/print.go \
	go/go/ast/scope.go \
	go/go/ast/walk.go
go_go_doc_files = \
	go/go/doc/comment.go \
	go/go/doc/doc.go
go_go_parser_files = \
	go/go/parser/interface.go \
	go/go/parser/parser.go
go_go_printer_files = \
	go/go/printer/nodes.go \
	go/go/printer/printer.go
go_go_scanner_files = \
	go/go/scanner/errors.go \
	go/go/scanner/scanner.go
go_go_token_files = \
	go/go/token/position.go \
	go/go/token/token.go
go_go_typechecker_files = \
	go/go/typechecker/scope.go \
	go/go/typechecker/typechecker.go \
	go/go/typechecker/universe.go

go_hash_adler32_files = \
	go/hash/adler32/adler32.go
go_hash_crc32_files = \
	go/hash/crc32/crc32.go
go_hash_crc64_files = \
	go/hash/crc64/crc64.go

go_http_pprof_files = \
	go/http/pprof/pprof.go

go_image_jpeg_files = \
	go/image/jpeg/huffman.go \
	go/image/jpeg/idct.go \
	go/image/jpeg/reader.go

go_image_png_files = \
	go/image/png/reader.go \
	go/image/png/writer.go

go_index_suffixarray_files = \
	go/index/suffixarray/qsufsort.go \
	go/index/suffixarray/suffixarray.go

go_io_ioutil_files = \
	go/io/ioutil/ioutil.go \
	go/io/ioutil/tempfile.go

go_mime_multipart_files = \
	go/mime/multipart/multipart.go

go_net_dict_files = \
	go/net/dict/dict.go

go_net_textproto_files = \
	go/net/textproto/pipeline.go \
	go/net/textproto/reader.go \
	go/net/textproto/textproto.go \
	go/net/textproto/writer.go

go_os_inotify_files = \
	go/os/inotify/inotify_linux.go

go_os_signal_files = \
	go/os/signal/signal.go \
	unix.go

go_rpc_jsonrpc_files = \
	go/rpc/jsonrpc/client.go \
	go/rpc/jsonrpc/server.go

go_runtime_debug_files = \
	go/runtime/debug/stack.go
go_runtime_pprof_files = \
	go/runtime/pprof/pprof.go

go_testing_iotest_files = \
	go/testing/iotest/logger.go \
	go/testing/iotest/reader.go \
	go/testing/iotest/writer.go
go_testing_quick_files = \
	go/testing/quick/quick.go
go_testing_script_files = \
	go/testing/script/script.go

# Define Syscall and Syscall6.
if LIBGO_IS_RTEMS
syscall_syscall_file = syscalls/syscall_stubs.go
else
syscall_syscall_file = syscalls/syscall.go
endif

# Declare libc functions that vary for largefile systems.
if LIBGO_IS_LINUX
# Always use lseek64 on GNU/Linux.
syscall_filesize_file = syscalls/sysfile_largefile.go
syscall_stat_file = syscalls/sysfile_stat_largefile.go
else # !LIBGO_IS_LINUX
if LIBGO_IS_SOLARIS
# FIXME: Same for sparc vs. sparc64.  Introduce new/additional conditional?
if LIBGO_IS_386
# Use lseek64 on 386 Solaris.
syscall_filesize_file = syscalls/sysfile_largefile.go
syscall_stat_file = syscalls/sysfile_stat_largefile.go
else # !LIBGO_IS_LINUX && LIBGO_IS_SOLARIS && !LIBGO_IS_386
# Use lseek on amd64 Solaris.
syscall_filesize_file = syscalls/sysfile_regfile.go
syscall_stat_file = syscalls/sysfile_stat_regfile.go
endif # !LIBGO_IS_386
else # !LIBGO_IS_LINUX && !LIBGO_IS_SOLARIS
# Use lseek by default.
syscall_filesize_file = syscalls/sysfile_regfile.go
syscall_stat_file = syscalls/sysfile_stat_regfile.go
endif # !LIBGO_IS_SOLARIS
endif # !LIBGO_IS_LINUX


# Define ForkExec, PtraceForkExec, Exec, and Wait4.
if LIBGO_IS_RTEMS
syscall_exec_os_file = syscalls/exec_stubs.go
else
syscall_exec_os_file = syscalls/exec.go
endif

# Define Sleep.
if LIBGO_IS_RTEMS
syscall_sleep_file = syscalls/sleep_rtems.go
else
syscall_sleep_file = syscalls/sleep_select.go
endif

# Define Errstr.
if LIBGO_IS_RTEMS
syscall_errstr_file = syscalls/errstr_rtems.go
else
syscall_errstr_file = syscalls/errstr.go
endif

# Declare libc_strerror_r which is the Go name for strerror_r.
if LIBGO_IS_RTEMS
# RTEMS uses newlib in which strerror_r returns char *.
syscall_errstr_decl_file = syscalls/errstr_decl_rtems.go
else
if LIBGO_IS_LINUX
# In Linux the POSIX strerror_r is called __xpg_strerror_r.
syscall_errstr_decl_file = syscalls/errstr_decl_linux.go
else
# On other systems we hope strerror_r is just strerror_r.
syscall_errstr_decl_file = syscalls/errstr_decl.go
endif
endif

# Define socket sizes and types.
if LIBGO_IS_LINUX
syscall_socket_os_file = syscalls/socket_linux.go
else
if LIBGO_IS_SOLARIS
syscall_socket_os_file = syscalls/socket_solaris.go
else
syscall_socket_os_file = syscalls/socket_bsd.go
endif
endif

# Support for epoll.
if LIBGO_IS_LINUX
syscall_socket_epoll_file = syscalls/socket_epoll.go
else
syscall_socket_epoll_file =
endif

# Support for uname.
if LIBGO_IS_SOLARIS
if LIBGO_IS_386
# 32-bit Solaris 2/x86 needs _nuname, handled in syscall_solaris_386.go.
syscall_uname_file =
else # !LIBGO_IS_386 && LIBGO_IS_SOLARIS
syscall_uname_file = syscalls/syscall_uname.go
endif
else # !LIBGO_IS_SOLARIS
syscall_uname_file = syscalls/syscall_uname.go
endif

syscall_arch.go: s-syscall_arch; @true
s-syscall_arch: Makefile
	rm -f syscall_arch.go.tmp
	echo "package syscall" > syscall_arch.go.tmp
	echo 'const ARCH = "'$(GOARCH)'"' >> syscall_arch.go.tmp
	echo 'const OS = "'$(GOOS)'"' >> syscall_arch.go.tmp
	$(SHELL) $(srcdir)/../move-if-change syscall_arch.go.tmp syscall_arch.go
	$(STAMP) $@

go_syscall_files = \
	$(syscall_errstr_file) \
	$(syscall_errstr_decl_file) \
	syscalls/exec_helpers.go \
	$(syscall_exec_os_file) \
	$(syscall_filesize_file) \
	$(syscall_stat_file) \
	$(syscall_sleep_file) \
	syscalls/socket.go \
	$(syscall_socket_os_file) \
	$(syscall_socket_epoll_file) \
	$(syscall_syscall_file) \
	$(syscall_uname_file) \
	syscalls/syscall_unix.go \
	syscalls/stringbyte.go \
	syscalls/syscall_$(GOOS).go \
	$(GO_SYSCALLS_SYSCALL_OS_ARCH_FILE) \
	syscalls/sysfile_posix.go \
	sysinfo.go \
	syscall_arch.go
go_syscall_c_files = \
	syscalls/errno.c

if LIBGO_IS_LINUX
# os_lib_inotify_lo = os/inotify.lo
os_lib_inotify_lo =
else
os_lib_inotify_lo =
endif

libgo_go_objs = \
	asn1/asn1.lo \
	big/big.lo \
	bufio/bufio.lo \
	bytes/bytes.lo \
	bytes/index.lo \
	cmath/cmath.lo \
	ebnf/ebnf.lo \
	exec/exec.lo \
	expvar/expvar.lo \
	flag/flag.lo \
	fmt/fmt.lo \
	gob/gob.lo \
	hash/hash.lo \
	html/html.lo \
	http/http.lo \
	image/image.lo \
	io/io.lo \
	json/json.lo \
	log/log.lo \
	math/math.lo \
	mime/mime.lo \
	net/net.lo \
	netchan/netchan.lo \
	os/os.lo \
	patch/patch.lo \
	path/path.lo \
	rand/rand.lo \
	reflect/reflect.lo \
	regexp/regexp.lo \
	rpc/rpc.lo \
	runtime/runtime.lo \
	scanner/scanner.lo \
	smtp/smtp.lo \
	sort/sort.lo \
	strconv/strconv.lo \
	strings/strings.lo \
	sync/mutex.lo \
	sync/cas.lo \
	syslog/syslog.lo \
	syslog/syslog_c.lo \
	tabwriter/tabwriter.lo \
	template/template.lo \
	time/time.lo \
	try/try.lo \
	unicode/unicode.lo \
	utf16/utf16.lo \
	utf8/utf8.lo \
	websocket/websocket.lo \
	xml/xml.lo \
	archive/tar.lo \
	archive/zip.lo \
	compress/flate.lo \
	compress/gzip.lo \
	compress/zlib.lo \
	container/heap.lo \
	container/list.lo \
	container/ring.lo \
	container/vector.lo \
	crypto/aes.lo \
	crypto/block.lo \
	crypto/blowfish.lo \
	crypto/cast5.lo \
	crypto/cipher.lo \
	crypto/elliptic.lo \
	crypto/hmac.lo \
	crypto/md4.lo \
	crypto/md5.lo \
	crypto/ocsp.lo \
	crypto/rand.lo \
	crypto/rc4.lo \
	crypto/ripemd160.lo \
	crypto/rsa.lo \
	crypto/sha1.lo \
	crypto/sha256.lo \
	crypto/sha512.lo \
	crypto/subtle.lo \
	crypto/tls.lo \
	crypto/twofish.lo \
	crypto/x509.lo \
	crypto/xtea.lo \
	crypto/openpgp/armor.lo \
	crypto/openpgp/error.lo \
	crypto/openpgp/s2k.lo \
	debug/dwarf.lo \
	debug/elf.lo \
	debug/gosym.lo \
	debug/macho.lo \
	debug/pe.lo \
	debug/proc.lo \
	encoding/ascii85.lo \
	encoding/base32.lo \
	encoding/base64.lo \
	encoding/binary.lo \
	encoding/git85.lo \
	encoding/hex.lo \
	encoding/line.lo \
	encoding/pem.lo \
	exp/datafmt.lo \
	exp/draw.lo \
	exp/eval.lo \
	go/ast.lo \
	go/doc.lo \
	go/parser.lo \
	go/printer.lo \
	go/scanner.lo \
	go/token.lo \
	go/typechecker.lo \
	hash/adler32.lo \
	hash/crc32.lo \
	hash/crc64.lo \
	http/pprof.lo \
	image/jpeg.lo \
	image/png.lo \
	index/suffixarray.lo \
	io/ioutil.lo \
	mime/multipart.lo \
	net/dict.lo \
	net/textproto.lo \
	$(os_lib_inotify_lo) \
	os/signal.lo \
	rpc/jsonrpc.lo \
	runtime/debug.lo \
	runtime/pprof.lo \
	syscalls/syscall.lo \
	syscalls/errno.lo \
	testing/testing.lo \
	testing/iotest.lo \
	testing/quick.lo \
	testing/script.lo

libgo_la_SOURCES = $(runtime_files)

libgo_la_LIBADD = \
	$(libgo_go_objs) $(LIBFFI) $(PTHREAD_LIBS) $(MATH_LIBS) $(NET_LIBS)

libgobegin_a_SOURCES = \
	runtime/go-main.c

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

GOCFLAGS = $(CFLAGS)
AM_GOCFLAGS = $(STRINGOPS_FLAG)
GOCOMPILE = $(GOC) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_GOCFLAGS) $(GOCFLAGS)

LTGOCOMPILE = $(LIBTOOL) --tag GO --mode=compile $(GOC) $(INCLUDES) \
	$(AM_GOCFLAGS) $(GOCFLAGS)

GOLINK = $(LIBTOOL) --tag GO --mode-link $(GOC) \
	$(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_GOCFLAGS) $(LTLDFLAGS) -o $@

# Build the .go files for a package, generating a .lo file.
BUILDPACKAGE = \
	$(MKDIR_P) $(@D); \
	files=`echo $^ | sed -e 's/[^ ]*\.gox//g'`; \
	$(LTGOCOMPILE) -I . -c -fgo-prefix="libgo_$(@D)" -o $@ $$files

if LIBGO_IS_RTEMS
use_dejagnu = yes
else
use_dejagnu = no
endif

# Check a package.
CHECK = \
	@GC="$(GOC) -L `${PWD_COMMAND}` -L `${PWD_COMMAND}`/.libs -Wl,-R,`${PWD_COMMAND}`/.libs"; \
	export GC; \
	RUNTESTFLAGS="$(RUNTESTFLAGS)"; \
	export RUNTESTFLAGS; \
	MAKE="$(MAKE)"; \
	export MAKE; \
	rm -f $@-log; \
	prefix=`if test "$(@D)" = "regexp"; then echo regexp-test; else dirname $(@D); fi`; \
	test "$${prefix}" != "." || prefix="$(@D)"; \
	if test "$(use_dejagnu)" = "yes"; then \
	  $(SHELL) $(srcdir)/testsuite/gotest --dejagnu=yes --basedir=$(srcdir) --srcdir=$(srcdir)/go/$(@D) --prefix="libgo_$${prefix}" --pkgfiles="$(go_$(subst /,_,$(@D))_files)"; \
	else \
	  if $(SHELL) $(srcdir)/testsuite/gotest --basedir=$(srcdir) --srcdir=$(srcdir)/go/$(@D) --prefix="libgo_$${prefix}" --pkgfiles="$(go_$(subst /,_,$(@D))_files)" >>$@-log 2>&1; then \
	    echo "PASS: $(@D)"; \
	  else \
	    echo "FAIL: $(@D)"; \
	    cat $@-log; \
	    exit 1; \
	  fi; \
	fi

# Build all packages before checking any.
CHECK_DEPS = libgo.la libgobegin.a \
	$(toolexeclib_DATA) \
	$(toolexeclibarchive_DATA) \
	$(toolexeclibcompress_DATA) \
	$(toolexeclibcontainer_DATA) \
	$(toolexeclibcrypto_DATA) \
	$(toolexeclibdebug_DATA) \
	$(toolexeclibencoding_DATA) \
	$(toolexeclibexp_DATA) \
	$(toolexeclibgo_DATA) \
	$(toolexeclibhash_DATA) \
	$(toolexeclibhttp_DATA) \
	$(toolexeclibimage_DATA) \
	$(toolexeclibio_DATA) \
	$(toolexeclibos_DATA) \
	$(toolexeclibrpc_DATA) \
	$(toolexeclibruntime_DATA) \
	$(toolexeclibtesting_DATA)

asn1/asn1.lo: $(go_asn1_files) bytes.gox fmt.gox io.gox os.gox reflect.gox \
		strconv.gox strings.gox time.gox
	$(BUILDPACKAGE)
asn1/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: asn1/check

big/big.lo: $(go_big_files) fmt.gox rand.gox strings.gox
	$(BUILDPACKAGE)
big/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: big/check

bufio/bufio.lo: $(go_bufio_files) bytes.gox io.gox os.gox strconv.gox utf8.gox
	$(BUILDPACKAGE)
bufio/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: bufio/check

bytes/bytes.lo: $(go_bytes_files) io.gox os.gox unicode.gox utf8.gox
	$(BUILDPACKAGE)
bytes/index.lo: $(go_bytes_c_files) bytes/bytes.lo
	$(LTCOMPILE) -c -o bytes/index.lo $(srcdir)/go/bytes/indexbyte.c
bytes/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: bytes/check

cmath/cmath.lo: $(go_cmath_files) math.gox
	$(BUILDPACKAGE)
cmath/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: cmath/check

ebnf/ebnf.lo: $(go_ebnf_files) container/vector.gox go/scanner.gox \
		go/token.gox os.gox strconv.gox unicode.gox utf8.gox
	$(BUILDPACKAGE)
ebnf/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: ebnf/check

exec/exec.lo: $(go_exec_files) os.gox strings.gox
	$(BUILDPACKAGE)
exec/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: exec/check

expvar/expvar.lo: $(go_expvar_files) bytes.gox fmt.gox http.gox json.gox \
		log.gox os.gox runtime.gox strconv.gox sync.gox
	$(BUILDPACKAGE)
expvar/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: expvar/check

flag/flag.lo: $(go_flag_files) fmt.gox os.gox strconv.gox
	$(BUILDPACKAGE)
flag/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: flag/check

fmt/fmt.lo: $(go_fmt_files) bytes.gox io.gox os.gox reflect.gox strconv.gox \
		strings.gox unicode.gox utf8.gox
	$(BUILDPACKAGE)
fmt/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: fmt/check

gob/gob.lo: $(go_gob_files) bytes.gox fmt.gox io.gox math.gox os.gox \
		reflect.gox runtime.gox strings.gox sync.gox unicode.gox \
		utf8.gox
	$(BUILDPACKAGE)
gob/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: gob/check

hash/hash.lo: $(go_hash_files) io.gox
	$(BUILDPACKAGE)
hash/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: hash/check

html/html.lo: $(go_html_files) bytes.gox io.gox os.gox strconv.gox strings.gox \
		utf8.gox
	$(BUILDPACKAGE)
html/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: html/check

http/http.lo: $(go_http_files) bufio.gox bytes.gox container/list.gox \
		container/vector.gox crypto/rand.gox crypto/tls.gox \
		encoding/base64.gox fmt.gox io.gox io/ioutil.gox log.gox \
		mime.gox mime/multipart.gox net.gox os.gox path.gox sort.gox \
		strconv.gox strings.gox sync.gox time.gox utf8.gox
	$(BUILDPACKAGE)
http/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: http/check

image/image.lo: $(go_image_files) bufio.gox io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
image/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: image/check

io/io.lo: $(go_io_files) os.gox runtime.gox sync.gox
	$(BUILDPACKAGE)
io/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: io/check

json/json.lo: $(go_json_files) bytes.gox container/vector.gox fmt.gox io.gox \
		math.gox os.gox reflect.gox runtime.gox strconv.gox \
		strings.gox unicode.gox utf16.gox utf8.gox
	$(BUILDPACKAGE)
json/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: json/check

log/log.lo: $(go_log_files) bytes.gox fmt.gox io.gox runtime.gox os.gox \
		sync.gox time.gox
	$(BUILDPACKAGE)
log/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: log/check

math/math.lo: $(go_math_files)
	$(BUILDPACKAGE)
math/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: math/check

mime/mime.lo: $(go_mime_files) bufio.gox bytes.gox os.gox strings.gox \
		sync.gox unicode.gox
	$(BUILDPACKAGE)
mime/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: mime/check

net/net.lo: $(go_net_files) bytes.gox fmt.gox io.gox os.gox reflect.gox \
		strconv.gox strings.gox sync.gox syscall.gox
	$(BUILDPACKAGE)
net/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: net/check

netchan/netchan.lo: $(go_netchan_files) gob.gox log.gox net.gox os.gox \
		reflect.gox strconv.gox sync.gox time.gox
	$(BUILDPACKAGE)
netchan/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: netchan/check

os/os.lo: $(go_os_files) sync.gox syscall.gox
	$(BUILDPACKAGE)
os/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: os/check

patch/patch.lo: $(go_patch_files) bytes.gox compress/zlib.gox \
		crypto/sha1.gox encoding/git85.gox fmt.gox io.gox os.gox \
		path.gox strings.gox
	$(BUILDPACKAGE)
patch/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: patch/check

path/path.lo: $(go_path_files) io/ioutil.gox os.gox sort.gox strings.gox \
		utf8.gox
	$(BUILDPACKAGE)
path/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: path/check

rand/rand.lo: $(go_rand_files) math.gox sync.gox
	$(BUILDPACKAGE)
rand/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: rand/check

reflect/reflect.lo: $(go_reflect_files) math.gox runtime.gox strconv.gox \
		sync.gox
	$(BUILDPACKAGE)
reflect/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: reflect/check

regexp/regexp.lo: $(go_regexp_files) bytes.gox io.gox os.gox strings.gox \
		utf8.gox
	$(BUILDPACKAGE)
regexp/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: regexp/check

rpc/rpc.lo: $(go_rpc_files) bufio.gox fmt.gox gob.gox http.gox io.gox log.gox \
		net.gox os.gox reflect.gox sort.gox strings.gox strconv.gox \
		sync.gox template.gox unicode.gox utf8.gox
	$(BUILDPACKAGE)
rpc/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: rpc/check

runtime/runtime.lo: $(go_runtime_files)
	$(BUILDPACKAGE)
runtime/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: runtime/check

scanner/scanner.lo: $(go_scanner_files) bytes.gox fmt.gox io.gox os.gox \
		unicode.gox utf8.gox
	$(BUILDPACKAGE)
scanner/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: scanner/check

smtp/smtp.lo: $(go_smtp_files) crypto/tls.gox encoding/base64.gox io.gox \
		net.gox net/textproto.gox os.gox strings.gox
	$(BUILDPACKAGE)
smtp/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: smtp/check

sort/sort.lo: $(go_sort_files)
	$(BUILDPACKAGE)
sort/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: sort/check

strconv/strconv.lo: $(go_strconv_files) bytes.gox math.gox os.gox strings.gox \
		unicode.gox utf8.gox
	$(BUILDPACKAGE)
strconv/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: strconv/check

strings/strings.lo: $(go_strings_files) os.gox unicode.gox utf8.gox
	$(BUILDPACKAGE)
strings/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: strings/check

sync/mutex.lo: $(go_sync_files) runtime.gox
	$(BUILDPACKAGE)
sync/cas.lo: $(go_sync_c_files) sync/mutex.lo
	$(LTCOMPILE) -c -o sync/cas.lo $(srcdir)/go/sync/cas.c
sync/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: sync/check

syslog/syslog.lo: $(go_syslog_files) fmt.gox log.gox net.gox os.gox syscall.gox
	$(BUILDPACKAGE)
syslog/syslog_c.lo: $(go_syslog_c_files) syslog/syslog.lo
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/syslog/syslog_c.c
syslog/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: syslog/check

tabwriter/tabwriter.lo: $(go_tabwriter_files) bytes.gox io.gox os.gox utf8.gox
	$(BUILDPACKAGE)
tabwriter/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: tabwriter/check

template/template.lo: $(go_template_files) bytes.gox fmt.gox io.gox os.gox \
		reflect.gox runtime.gox strings.gox container/vector.gox
	$(BUILDPACKAGE)
template/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: template/check

testing/testing.lo: $(go_testing_files) flag.gox fmt.gox os.gox regexp.gox \
		runtime.gox time.gox
	$(BUILDPACKAGE)
testing/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: testing/check

time/time.lo: $(go_time_files) bytes.gox container/heap.gox io/ioutil.gox \
		os.gox strconv.gox sync.gox syscall.gox
	$(BUILDPACKAGE)
time/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: time/check

try/try.lo: $(go_try_files) fmt.gox io.gox os.gox reflect.gox unicode.gox
	$(BUILDPACKAGE)
try/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: try/check

unicode/unicode.lo: $(go_unicode_files)
	$(BUILDPACKAGE)
unicode/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: unicode/check

utf16/utf16.lo: $(go_utf16_files) unicode.gox
	$(BUILDPACKAGE)
utf16/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: utf16/check

utf8/utf8.lo: $(go_utf8_files) unicode.gox
	$(BUILDPACKAGE)
utf8/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: utf8/check

websocket/websocket.lo: $(go_websocket_files) bufio.gox bytes.gox \
		container/vector.gox crypto/md5.gox crypto/tls.gox \
		encoding/binary.gox fmt.gox http.gox io.gox net.gox os.gox \
		rand.gox strings.gox
	$(BUILDPACKAGE)
websocket/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: websocket/check

xml/xml.lo: $(go_xml_files) bufio.gox bytes.gox fmt.gox io.gox os.gox \
		reflect.gox strconv.gox strings.gox unicode.gox utf8.gox
	$(BUILDPACKAGE)
xml/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: xml/check

archive/tar.lo: $(go_archive_tar_files) bytes.gox io.gox os.gox strconv.gox \
		strings.gox
	$(BUILDPACKAGE)
archive/tar/check: $(CHECK_DEPS)
	@$(MKDIR_P) archive/tar
	$(CHECK)
.PHONY: archive/tar/check

archive/zip.lo: $(go_archive_zip_files) bufio.gox bytes.gox \
		compress/flate.gox hash.gox hash/crc32.gox \
		encoding/binary.gox io.gox os.gox
	$(BUILDPACKAGE)
archive/zip/check: $(CHECK_DEPS)
	@$(MKDIR_P) archive/zip
	$(CHECK)
.PHONY: archive/zip/check

compress/flate.lo: $(go_compress_flate_files) bufio.gox io.gox math.gox \
		os.gox sort.gox strconv.gox
	$(BUILDPACKAGE)
compress/flate/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/flate
	$(CHECK)
.PHONY: compress/flate/check

compress/gzip.lo: $(go_compress_gzip_files) bufio.gox compress/flate.gox \
		hash.gox hash/crc32.gox io.gox os.gox
	$(BUILDPACKAGE)
compress/gzip/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/gzip
	$(CHECK)
.PHONY: compress/gzip/check

compress/zlib.lo: $(go_compress_zlib_files) bufio.gox compress/flate.gox \
		hash.gox hash/adler32.gox io.gox os.gox
	$(BUILDPACKAGE)
compress/zlib/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/zlib
	$(CHECK)
.PHONY: compress/zlib/check

container/heap.lo: $(go_container_heap_files) sort.gox
	$(BUILDPACKAGE)
container/heap/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/heap
	$(CHECK)
.PHONY: container/heap/check

container/list.lo: $(go_container_list_files)
	$(BUILDPACKAGE)
container/list/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/list
	$(CHECK)
.PHONY: container/list/check

container/ring.lo: $(go_container_ring_files)
	$(BUILDPACKAGE)
container/ring/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/ring
	$(CHECK)
.PHONY: container/ring/check

container/vector.lo: $(go_container_vector_files)
	$(BUILDPACKAGE)
container/vector/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/vector
	$(CHECK)
.PHONY: container/vector/check

crypto/aes.lo: $(go_crypto_aes_files) os.gox strconv.gox
	$(BUILDPACKAGE)
crypto/aes/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/aes
	$(CHECK)
.PHONY: crypto/aes/check

crypto/block.lo: $(go_crypto_block_files) fmt.gox io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
crypto/block/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/block
	$(CHECK)
.PHONY: crypto/block/check

crypto/blowfish.lo: $(go_crypto_blowfish_files) os.gox strconv.gox
	$(BUILDPACKAGE)
crypto/blowfish/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/blowfish
	$(CHECK)
.PHONY: crypto/blowfish/check

crypto/cast5.lo: $(go_crypto_cast5_files) os.gox
	$(BUILDPACKAGE)
crypt/cast5/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/cast5
	$(CHECK)
.PHONY: crypto/cast5/check

crypto/cipher.lo: $(go_crypto_cipher_files) io.gox os.gox
	$(BUILDPACKAGE)
crypto/cipher/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/cipher
	$(CHECK)
.PHONY: crypto/cipher/check

crypto/elliptic.lo: $(go_crypto_elliptic_files) big.gox io.gox os.gox sync.gox
	$(BUILDPACKAGE)
crypto/elliptic/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/elliptic
	$(CHECK)
.PHONY: crypto/elliptic/check

crypto/hmac.lo: $(go_crypto_hmac_files) crypto/md5.gox crypto/sha1.gox \
		crypto/sha256.gox hash.gox os.gox
	$(BUILDPACKAGE)
crypto/hmac/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/hmac
	$(CHECK)
.PHONY: crypto/hmac/check

crypto/md4.lo: $(go_crypto_md4_files) hash.gox os.gox
	$(BUILDPACKAGE)
crypto/md4/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/md4
	$(CHECK)
.PHONY: crypto/md4/check

crypto/md5.lo: $(go_crypto_md5_files) hash.gox os.gox
	$(BUILDPACKAGE)
crypto/md5/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/md5
	$(CHECK)
.PHONY: crypto/md5/check

crypto/ocsp.lo: $(go_crypto_ocsp_files) asn1.gox crypto/rsa.gox \
		crypto/sha1.gox crypto/x509.gox os.gox time.gox
	$(BUILDPACKAGE)
crypto/ocsp/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/ocsp
	$(CHECK)
.PHONY: crypto/ocsp/check

crypto/rand.lo: $(go_crypto_rand_files) crypto/aes.gox io.gox os.gox sync.gox \
		time.gox
	$(BUILDPACKAGE)
crypto/rand/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rand
	$(CHECK)
.PHONY: crypto/rand/check

crypto/rc4.lo: $(go_crypto_rc4_files) os.gox strconv.gox
	$(BUILDPACKAGE)
crypto/rc4/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rc4
	$(CHECK)
.PHONY: crypto/rc4/check

crypto/ripemd160.lo: $(go_crypto_ripemd160_files) hash.gox os.gox
	$(BUILDPACKAGE)
crypto/ripemd160/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/ripemd160
	$(CHECK)
.PHONY: crypto/ripemd160/check

crypto/rsa.lo: $(go_crypto_rsa_files) big.gox crypto/sha1.gox \
		crypto/subtle.gox encoding/hex.gox hash.gox io.gox os.gox
	$(BUILDPACKAGE)
crypto/rsa/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rsa
	$(CHECK)
.PHONY: crypto/rsa/check

crypto/sha1.lo: $(go_crypto_sha1_files) hash.gox os.gox
	$(BUILDPACKAGE)
crypto/sha1/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha1
	$(CHECK)
.PHONY: crypto/sha1/check

crypto/sha256.lo: $(go_crypto_sha256_files) hash.gox os.gox
	$(BUILDPACKAGE)
crypto/sha256/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha256
	$(CHECK)
.PHONY: crypto/sha256/check

crypto/sha512.lo: $(go_crypto_sha512_files) hash.gox os.gox
	$(BUILDPACKAGE)
crypto/sha512/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha512
	$(CHECK)
.PHONY: crypto/sha512/check

crypto/subtle.lo: $(go_crypto_subtle_files)
	$(BUILDPACKAGE)
crypto/subtle/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/subtle
	$(CHECK)
.PHONY: crypto/subtle/check

crypto/tls.lo: $(go_crypto_tls_files) big.gox bufio.gox bytes.gox \
		container/list.gox crypto/aes.gox crypto/cipher.gox \
		crypto/elliptic.gox crypto/hmac.gox crypto/md5.gox \
		crypto/rc4.gox crypto/rand.gox crypto/rsa.gox crypto/sha1.gox \
		crypto/subtle.gox crypto/rsa.gox crypto/sha1.gox \
		crypto/x509.gox encoding/pem.gox fmt.gox hash.gox io.gox \
		io/ioutil.gox net.gox os.gox strings.gox sync.gox time.gox
	$(BUILDPACKAGE)
crypto/tls/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/tls
	$(CHECK)
.PHONY: crypto/tls/check

crypto/twofish.lo: $(go_crypto_twofish_files) os.gox strconv.gox
	$(BUILDPACKAGE)
crypto/twofish/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/twofish
	$(CHECK)
.PHONY: crypto/twofish/check

crypto/x509.lo: $(go_crypto_x509_files) asn1.gox big.gox container/vector.gox \
		crypto/rsa.gox crypto/sha1.gox hash.gox os.gox strings.gox \
		time.gox
	$(BUILDPACKAGE)
crypto/x509/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/x509
	$(CHECK)
.PHONY: crypto/x509/check

crypto/xtea.lo: $(go_crypto_xtea_files) os.gox strconv.gox
	$(BUILDPACKAGE)
crypto/xtea/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/xtea
	$(CHECK)
.PHONY: crypto/xtea/check

crypto/openpgp/armor.lo: $(go_crypto_openpgp_armor_files) bytes.gox \
		crypto/openpgp/error.gox encoding/base64.gox \
		encoding/line.gox io.gox os.gox
	$(BUILDPACKAGE)
crypto/openpgp/armor/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/armor
	$(CHECK)
.PHONY: crypto/openpgp/armor/check

crypto/openpgp/error.lo: $(go_crypto_openpgp_error_files)
	$(BUILDPACKAGE)
crypto/openpgp/error/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/error
	$(CHECK)
.PHONY: crypto/openpgp/error/check

crypto/openpgp/s2k.lo: $(go_crypto_openpgp_s2k_files) crypto/md5.gox \
		crypto/openpgp/error.gox crypto/ripemd160.gox crypto/sha1.gox \
		crypto/sha256.gox crypto/sha512.gox hash.gox io.gox os.gox
	$(BUILDPACKAGE)
crypto/openpgp/s2k/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/s2k
	$(CHECK)
.PHONY: crypto/openpgp/s2k/check

debug/dwarf.lo: $(go_debug_dwarf_files) encoding/binary.gox os.gox strconv.gox
	$(BUILDPACKAGE)
debug/dwarf/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/dwarf
	$(CHECK)
.PHONY: debug/dwarf/check

debug/elf.lo: $(go_debug_elf_files) bytes.gox debug/dwarf.gox \
		encoding/binary.gox fmt.gox io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
debug/elf/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/elf
	$(CHECK)
.PHONY: debug/elf/check

debug/gosym.lo: $(go_debug_gosym_files) encoding/binary.gox fmt.gox os.gox \
		strconv.gox strings.gox
	$(BUILDPACKAGE)
debug/gosym/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/gosym
	$(CHECK)
.PHONY: debug/gosym/check

debug/macho.lo: $(go_debug_macho_files) bytes.gox debug/dwarf.gox \
		encoding/binary.gox fmt.gox io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
debug/macho/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/macho
	$(CHECK)
.PHONY: debug/macho/check

debug/pe.lo: $(go_debug_pe_files) debug/dwarf.gox \
		encoding/binary.gox fmt.gox io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
debug/pe/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/pe
	$(CHECK)
.PHONY: debug/pe/check

debug/proc.lo: $(go_debug_proc_files) container/vector.gox fmt.gox \
		io/ioutil.gox os.gox runtime.gox strconv.gox strings.gox \
		sync.gox syscall.gox
	$(BUILDPACKAGE)
debug/proc/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/proc
	$(CHECK)
.PHONY: debug/proc/check

encoding/ascii85.lo: $(go_encoding_ascii85_files) io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
encoding/ascii85/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/ascii85
	$(CHECK)
.PHONY: encoding/ascii85/check

encoding/base32.lo: $(go_encoding_base32_files) io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
encoding/base32/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/base32
	$(CHECK)
.PHONY: encoding/base32/check

encoding/base64.lo: $(go_encoding_base64_files) io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
encoding/base64/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/base64
	$(CHECK)
.PHONY: encoding/base64/check

encoding/binary.lo: $(go_encoding_binary_files) io.gox math.gox os.gox \
		reflect.gox
	$(BUILDPACKAGE)
encoding/binary/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/binary
	$(CHECK)
.PHONY: encoding/binary/check

encoding/git85.lo: $(go_encoding_git85_files) bytes.gox io.gox os.gox \
		strconv.gox
	$(BUILDPACKAGE)
encoding/git85/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/git85
	$(CHECK)
.PHONY: encoding/git85/check

encoding/hex.lo: $(go_encoding_hex_files) os.gox strconv.gox
	$(BUILDPACKAGE)
encoding/hex/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/hex
	$(CHECK)
.PHONY: encoding/hex/check

encoding/line.lo: $(go_encoding_line_files) io.gox os.gox
	$(BUILDPACKAGE)
encoding/line/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/line
	$(CHECK)
.PHONY: encoding/line/check

encoding/pem.lo: $(go_encoding_pem_files) bytes.gox encoding/base64.gox
	$(BUILDPACKAGE)
encoding/pem/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/pem
	$(CHECK)
.PHONY: encoding/pem/check

exp/datafmt.lo: $(go_exp_datafmt_files) bytes.gox container/vector.gox \
		fmt.gox go/scanner.gox go/token.gox io.gox os.gox reflect.gox \
		runtime.gox strconv.gox strings.gox
	$(BUILDPACKAGE)
exp/datafmt/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/datafmt
	$(CHECK)
.PHONY: exp/datafmt/check

exp/draw.lo: $(go_exp_draw_files) image.gox os.gox
	$(BUILDPACKAGE)
exp/draw/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/draw
	$(CHECK)
.PHONY: exp/draw/check

exp/eval.lo: $(go_exp_eval_files) big.gox go/ast.gox go/parser.gox \
		go/scanner.gox go/token.gox fmt.gox log.gox strconv.gox \
		strings.gox os.gox reflect.gox runtime.gox sort.gox template.gox
	$(BUILDPACKAGE)
exp/eval/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/eval
	$(CHECK)
.PHONY: exp/eval/check

go/ast.lo: $(go_go_ast_files) fmt.gox go/token.gox io.gox os.gox reflect.gox \
		unicode.gox utf8.gox
	$(BUILDPACKAGE)
go/ast/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/ast
	$(CHECK)
.PHONY: go/ast/check

go/doc.lo: $(go_go_doc_files) go/ast.gox go/token.gox io.gox regexp.gox \
		sort.gox strings.gox template.gox
	$(BUILDPACKAGE)
go/doc/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/doc
	$(CHECK)
.PHONY: go/doc/check

go/parser.lo: $(go_go_parser_files) bytes.gox fmt.gox go/ast.gox \
		go/scanner.gox go/token.gox io.gox io/ioutil.gox os.gox \
		path.gox strings.gox
	$(BUILDPACKAGE)
go/parser/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/parser
	$(CHECK)
.PHONY: go/parser/check

go/printer.lo: $(go_go_printer_files) bytes.gox fmt.gox go/ast.gox \
		go/token.gox io.gox os.gox reflect.gox runtime.gox \
		strings.gox tabwriter.gox
	$(BUILDPACKAGE)
go/printer/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/printer
	$(CHECK)
.PHONY: go/printer/check

go/scanner.lo: $(go_go_scanner_files) bytes.gox container/vector.gox fmt.gox \
		go/token.gox io.gox os.gox path.gox sort.gox strconv.gox \
		unicode.gox utf8.gox
	$(BUILDPACKAGE)
go/scanner/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/scanner
	$(CHECK)
.PHONY: go/scanner/check

go/token.lo: $(go_go_token_files) fmt.gox strconv.gox
	$(BUILDPACKAGE)
go/token/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/token
	$(CHECK)
.PHONY: go/token/check

go/typechecker.lo: $(go_go_typechecker_files) fmt.gox go/ast.gox go/token.gox \
		go/scanner.gox os.gox
	$(BUILDPACKAGE)
go/typechecker/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/typechecker
	$(CHECK)
.PHONY: go/typechecker/check

hash/adler32.lo: $(go_hash_adler32_files) hash.gox os.gox
	$(BUILDPACKAGE)
hash/adler32/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/adler32
	$(CHECK)
.PHONY: hash/adler32/check

hash/crc32.lo: $(go_hash_crc32_files) hash.gox os.gox
	$(BUILDPACKAGE)
hash/crc32/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/crc32
	$(CHECK)
.PHONY: hash/crc32/check

hash/crc64.lo: $(go_hash_crc64_files) hash.gox os.gox
	$(BUILDPACKAGE)
hash/crc64/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/crc64
	$(CHECK)
.PHONY: hash/crc64/check

http/pprof.lo: $(go_http_pprof_files) bufio.gox fmt.gox http.gox os.gox \
		runtime.gox runtime/pprof.gox strconv.gox strings.gox
	$(BUILDPACKAGE)
http/pprof/check: $(CHECK_DEPS)
	@$(MKDIR_P) http/pprof
	$(CHECK)
.PHONY: http/pprof/check

image/jpeg.lo: $(go_image_jpeg_files) bufio.gox image.gox io.gox os.gox
	$(BUILDPACKAGE)
image/jpeg/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/jpeg
	$(CHECK)
.PHONY: image/jpeg/check

image/png.lo: $(go_image_png_files) bufio.gox compress/zlib.gox fmt.gox \
		hash.gox hash/crc32.gox image.gox io.gox os.gox strconv.gox
	$(BUILDPACKAGE)
image/png/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/png
	$(CHECK)
.PHONY: image/png/check

index/suffixarray.lo: $(go_index_suffixarray_files) bytes.gox regexp.gox \
		sort.gox
	$(BUILDPACKAGE)
index/suffixarray/check: $(CHECK_DEPS)
	@$(MKDIR_P) index/suffixarray
	$(CHECK)
.PHONY: index/suffixarray/check

io/ioutil.lo: $(go_io_ioutil_files) bytes.gox io.gox os.gox sort.gox \
		strconv.gox
	$(BUILDPACKAGE)
io/ioutil/check: $(CHECK_DEPS)
	@$(MKDIR_P) io/ioutil
	$(CHECK)
.PHONY: io/ioutil/check

mime/multipart.lo: $(go_mime_multipart_files) bufio.gox bytes.gox io.gox \
		mime.gox os.gox regexp.gox strings.gox
	$(BUILDPACKAGE)
mime/multipart/check: $(CHECK_DEPS)
	@$(MKDIR_P) mime/multipart
	$(CHECK)
.PHONY: mime/multipart/check

net/dict.lo: $(go_net_dict_files) container/vector.gox net/textproto.gox \
		os.gox strconv.gox strings.gox
	$(BUILDPACKAGE)

net/textproto.lo: $(go_net_textproto_files) bufio.gox bytes.gox \
		container/vector.gox fmt.gox io.gox io/ioutil.gox net.gox \
		os.gox strconv.gox sync.gox
	$(BUILDPACKAGE)
net/textproto/check: $(CHECK_DEPS)
	@$(MKDIR_P) net/textproto
	$(CHECK)
.PHONY: net/textproto/check

os/inotify.lo: $(go_os_inotify_files) fmt.gox os.gox strings.gox syscall.gox
	$(BUILDPACKAGE)
os/inotify/check: $(CHECK_DEPS)
	@$(MKDIR_P) os/inotify
	$(CHECK)
.PHONY: os/inotify/check

os/signal.lo: $(go_os_signal_files) runtime.gox strconv.gox
	$(BUILDPACKAGE)
os/signal/check: $(CHECK_DEPS)
	@$(MKDIR_P) os/signal
	$(CHECK)
.PHONY: os/signal/check

unix.go: $(srcdir)/go/os/signal/mkunix.sh sysinfo.go
	$(SHELL) $(srcdir)/go/os/signal/mkunix.sh sysinfo.go > $@.tmp
	mv -f $@.tmp $@

rpc/jsonrpc.lo: $(go_rpc_jsonrpc_files) fmt.gox io.gox json.gox net.gox \
		os.gox rpc.gox sync.gox
	$(BUILDPACKAGE)
rpc/jsonrpc/check: $(CHECK_DEPS)
	@$(MKDIR_P) rpc/jsonrpc
	$(CHECK)
.PHONY: rpc/jsonrpc/check

runtime/debug.lo: $(go_runtime_debug_files) bytes.gox fmt.gox io/ioutil.gox \
		os.gox runtime.gox
	$(BUILDPACKAGE)
runtime/debug/check: $(CHECK_DEPS)
	@$(MKDIR_P) runtime/debug
	$(CHECK)
.PHONY: runtime/debug/check

runtime/pprof.lo: $(go_runtime_pprof_files) bufio.gox fmt.gox io.gox os.gox \
		runtime.gox
	$(BUILDPACKAGE)
runtime/pprof/check: $(CHECK_DEPS)
	@$(MKDIR_P) runtime/pprof
	$(CHECK)
.PHONY: runtime/pprof/check

testing/iotest.lo: $(go_testing_iotest_files) io.gox log.gox os.gox
	$(BUILDPACKAGE)
testing/iotest/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/iotest
	$(CHECK)
.PHONY: testing/iotest/check

testing/quick.lo: $(go_testing_quick_files) flag.gox fmt.gox math.gox os.gox \
		rand.gox reflect.gox strings.gox
	$(BUILDPACKAGE)
testing/quick/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/quick
	$(CHECK)
.PHONY: testing/quick/check

testing/script.lo: $(go_testing_script_files) fmt.gox os.gox rand.gox \
		reflect.gox strings.gox
	$(BUILDPACKAGE)
testing/script/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/script
	$(CHECK)
.PHONY: testing/script/check

sysinfo.go: s-sysinfo; @true
s-sysinfo: $(srcdir)/mksysinfo.sh config.h
	CC="$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS)" $(SHELL) $(srcdir)/mksysinfo.sh
	$(SHELL) $(srcdir)/../move-if-change tmp-sysinfo.go sysinfo.go
	$(STAMP) $@

syscalls/syscall.lo: $(go_syscall_files) sync.gox
	$(BUILDPACKAGE)
syscalls/errno.lo: $(go_syscall_c_files) syscalls/syscall.lo
	$(LTCOMPILE) -c -o $@ $(srcdir)/syscalls/errno.c

# How to build a .gox file from a .lo file.
BUILDGOX = \
	f=`echo $< | sed -e 's/.lo$$/.o/'`; \
	$(OBJCOPY) -j .go_export $$f $@.tmp && mv -f $@.tmp $@

asn1.gox: asn1/asn1.lo
	$(BUILDGOX)
big.gox: big/big.lo
	$(BUILDGOX)
bufio.gox: bufio/bufio.lo
	$(BUILDGOX)
bytes.gox: bytes/bytes.lo
	$(BUILDGOX)
cmath.gox: cmath/cmath.lo
	$(BUILDGOX)
ebnf.gox: ebnf/ebnf.lo
	$(BUILDGOX)
exec.gox: exec/exec.lo
	$(BUILDGOX)
expvar.gox: expvar/expvar.lo
	$(BUILDGOX)
flag.gox: flag/flag.lo
	$(BUILDGOX)
fmt.gox: fmt/fmt.lo
	$(BUILDGOX)
gob.gox: gob/gob.lo
	$(BUILDGOX)
hash.gox: hash/hash.lo
	$(BUILDGOX)
html.gox: html/html.lo
	$(BUILDGOX)
http.gox: http/http.lo
	$(BUILDGOX)
image.gox: image/image.lo
	$(BUILDGOX)
io.gox: io/io.lo
	$(BUILDGOX)
json.gox: json/json.lo
	$(BUILDGOX)
log.gox: log/log.lo
	$(BUILDGOX)
math.gox: math/math.lo
	$(BUILDGOX)
mime.gox: mime/mime.lo
	$(BUILDGOX)
net.gox: net/net.lo
	$(BUILDGOX)
netchan.gox: netchan/netchan.lo
	$(BUILDGOX)
os.gox: os/os.lo
	$(BUILDGOX)
patch.gox: patch/patch.lo
	$(BUILDGOX)
path.gox: path/path.lo
	$(BUILDGOX)
rand.gox: rand/rand.lo
	$(BUILDGOX)
reflect.gox: reflect/reflect.lo
	$(BUILDGOX)
regexp.gox: regexp/regexp.lo
	$(BUILDGOX)
rpc.gox: rpc/rpc.lo
	$(BUILDGOX)
runtime.gox: runtime/runtime.lo
	$(BUILDGOX)
scanner.gox: scanner/scanner.lo
	$(BUILDGOX)
smtp.gox: smtp/smtp.lo
	$(BUILDGOX)
sort.gox: sort/sort.lo
	$(BUILDGOX)
strconv.gox: strconv/strconv.lo
	$(BUILDGOX)
strings.gox: strings/strings.lo
	$(BUILDGOX)
sync.gox: sync/mutex.lo
	$(BUILDGOX)
syslog.gox: syslog/syslog.lo
	$(BUILDGOX)
syscall.gox: syscalls/syscall.lo
	$(BUILDGOX)
tabwriter.gox: tabwriter/tabwriter.lo
	$(BUILDGOX)
template.gox: template/template.lo
	$(BUILDGOX)
testing.gox: testing/testing.lo
	$(BUILDGOX)
time.gox: time/time.lo
	$(BUILDGOX)
try.gox: try/try.lo
	$(BUILDGOX)
unicode.gox: unicode/unicode.lo
	$(BUILDGOX)
utf16.gox: utf16/utf16.lo
	$(BUILDGOX)
utf8.gox: utf8/utf8.lo
	$(BUILDGOX)
websocket.gox: websocket/websocket.lo
	$(BUILDGOX)
xml.gox: xml/xml.lo
	$(BUILDGOX)

archive/tar.gox: archive/tar.lo
	$(BUILDGOX)
archive/zip.gox: archive/zip.lo
	$(BUILDGOX)

compress/flate.gox: compress/flate.lo
	$(BUILDGOX)
compress/gzip.gox: compress/gzip.lo
	$(BUILDGOX)
compress/zlib.gox: compress/zlib.lo
	$(BUILDGOX)

container/heap.gox: container/heap.lo
	$(BUILDGOX)
container/list.gox: container/list.lo
	$(BUILDGOX)
container/ring.gox: container/ring.lo
	$(BUILDGOX)
container/vector.gox: container/vector.lo
	$(BUILDGOX)

crypto/aes.gox: crypto/aes.lo
	$(BUILDGOX)
crypto/block.gox: crypto/block.lo
	$(BUILDGOX)
crypto/blowfish.gox: crypto/blowfish.lo
	$(BUILDGOX)
crypto/cast5.gox: crypto/cast5.lo
	$(BUILDGOX)
crypto/cipher.gox: crypto/cipher.lo
	$(BUILDGOX)
crypto/elliptic.gox: crypto/elliptic.lo
	$(BUILDGOX)
crypto/hmac.gox: crypto/hmac.lo
	$(BUILDGOX)
crypto/md4.gox: crypto/md4.lo
	$(BUILDGOX)
crypto/md5.gox: crypto/md5.lo
	$(BUILDGOX)
crypto/ocsp.gox: crypto/ocsp.lo
	$(BUILDGOX)
crypto/rand.gox: crypto/rand.lo
	$(BUILDGOX)
crypto/rc4.gox: crypto/rc4.lo
	$(BUILDGOX)
crypto/ripemd160.gox: crypto/ripemd160.lo
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
crypto/twofish.gox: crypto/twofish.lo
	$(BUILDGOX)
crypto/x509.gox: crypto/x509.lo
	$(BUILDGOX)
crypto/xtea.gox: crypto/xtea.lo
	$(BUILDGOX)

crypto/openpgp/armor.gox: crypto/openpgp/armor.lo
	$(BUILDGOX)
crypto/openpgp/error.gox: crypto/openpgp/error.lo
	$(BUILDGOX)
crypto/openpgp/s2k.gox: crypto/openpgp/s2k.lo
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
debug/proc.gox: debug/proc.lo
	$(BUILDGOX)

encoding/ascii85.gox: encoding/ascii85.lo
	$(BUILDGOX)
encoding/base32.gox: encoding/base32.lo
	$(BUILDGOX)
encoding/base64.gox: encoding/base64.lo
	$(BUILDGOX)
encoding/binary.gox: encoding/binary.lo
	$(BUILDGOX)
encoding/git85.gox: encoding/git85.lo
	$(BUILDGOX)
encoding/hex.gox: encoding/hex.lo
	$(BUILDGOX)
encoding/line.gox: encoding/line.lo
	$(BUILDGOX)
encoding/pem.gox: encoding/pem.lo
	$(BUILDGOX)

exp/datafmt.gox: exp/datafmt.lo
	$(BUILDGOX)
exp/draw.gox: exp/draw.lo
	$(BUILDGOX)
exp/eval.gox: exp/eval.lo
	$(BUILDGOX)

go/ast.gox: go/ast.lo
	$(BUILDGOX)
go/doc.gox: go/doc.lo
	$(BUILDGOX)
go/parser.gox: go/parser.lo
	$(BUILDGOX)
go/printer.gox: go/printer.lo
	$(BUILDGOX)
go/scanner.gox: go/scanner.lo
	$(BUILDGOX)
go/token.gox: go/token.lo
	$(BUILDGOX)
go/typechecker.gox: go/typechecker.lo
	$(BUILDGOX)

hash/adler32.gox: hash/adler32.lo
	$(BUILDGOX)
hash/crc32.gox: hash/crc32.lo
	$(BUILDGOX)
hash/crc64.gox: hash/crc64.lo
	$(BUILDGOX)

http/pprof.gox: http/pprof.lo
	$(BUILDGOX)

image/jpeg.gox: image/jpeg.lo
	$(BUILDGOX)
image/png.gox: image/png.lo
	$(BUILDGOX)

index/suffixarray.gox: index/suffixarray.lo
	$(BUILDGOX)

io/ioutil.gox: io/ioutil.lo
	$(BUILDGOX)

mime/multipart.gox: mime/multipart.lo
	$(BUILDGOX)

net/dict.gox: net/dict.lo
	$(BUILDGOX)
net/textproto.gox: net/textproto.lo
	$(BUILDGOX)

os/inotify.gox: os/inotify.lo
	$(BUILDGOX)
os/signal.gox: os/signal.lo
	$(BUILDGOX)

rpc/jsonrpc.gox: rpc/jsonrpc.lo
	$(BUILDGOX)

runtime/debug.gox: runtime/debug.lo
	$(BUILDGOX)
runtime/pprof.gox: runtime/pprof.lo
	$(BUILDGOX)

testing/iotest.gox: testing/iotest.lo
	$(BUILDGOX)
testing/quick.gox: testing/quick.lo
	$(BUILDGOX)
testing/script.gox: testing/script.lo
	$(BUILDGOX)

if LIBGO_IS_LINUX
# os_inotify_check = os/inotify/check
os_inotify_check =
else
os_inotify_check =
endif

TEST_PACKAGES = \
	asn1/check \
	big/check \
	bufio/check \
	bytes/check \
	cmath/check \
	ebnf/check \
	exec/check \
	expvar/check \
	flag/check \
	fmt/check \
	gob/check \
	html/check \
	$(if $(GCCGO_RUN_ALL_TESTS),http/check) \
	io/check \
	json/check \
	log/check \
	math/check \
	mime/check \
	$(if $(GCCGO_RUN_ALL_TESTS),net/check) \
	netchan/check \
	os/check \
	patch/check \
	path/check \
	rand/check \
	reflect/check \
	regexp/check \
	rpc/check \
	runtime/check \
	scanner/check \
	smtp/check \
	sort/check \
	strconv/check \
	strings/check \
	sync/check \
	$(if $(GCCGO_RUN_ALL_TESTS),syslog/check) \
	tabwriter/check \
	template/check \
	time/check \
	try/check \
	unicode/check \
	utf16/check \
	utf8/check \
	websocket/check \
	xml/check \
	archive/tar/check \
	archive/zip/check \
	compress/flate/check \
	compress/gzip/check \
	compress/zlib/check \
	container/heap/check \
	container/list/check \
	container/ring/check \
	container/vector/check \
	crypto/aes/check \
	crypto/block/check \
	crypto/blowfish/check \
	crypto/cast5/check \
	crypto/cipher/check \
	crypto/elliptic/check \
	crypto/hmac/check \
	crypto/md4/check \
	crypto/md5/check \
	crypto/ocsp/check \
	crypto/rand/check \
	crypto/rc4/check \
	crypto/ripemd160/check \
	crypto/rsa/check \
	crypto/sha1/check \
	crypto/sha256/check \
	crypto/sha512/check \
	crypto/subtle/check \
	crypto/tls/check \
	crypto/twofish/check \
	crypto/x509/check \
	crypto/xtea/check \
	crypto/openpgp/armor/check \
	crypto/openpgp/s2k/check \
	debug/dwarf/check \
	debug/elf/check \
	debug/macho/check \
	debug/pe/check \
	encoding/ascii85/check \
	encoding/base32/check \
	encoding/base64/check \
	encoding/binary/check \
	encoding/git85/check \
	encoding/hex/check \
	encoding/line/check \
	encoding/pem/check \
	exp/datafmt/check \
	exp/draw/check \
	exp/eval/check \
	go/parser/check \
	go/printer/check \
	go/scanner/check \
	go/token/check \
	go/typechecker/check \
	hash/adler32/check \
	hash/crc32/check \
	hash/crc64/check \
	image/png/check \
	index/suffixarray/check \
	io/ioutil/check \
	mime/multipart/check \
	net/textproto/check \
	$(os_inotify_check) \
	os/signal/check \
	rpc/jsonrpc/check \
	testing/quick/check \
	testing/script/check

check-recursive: $(TEST_PACKAGES)

mostlyclean-local:
	find . -name '*.lo' -print | xargs $(LIBTOOL) --mode=clean rm -f
	find . -name '*.$(OBJEXT)' -print | xargs rm -f

clean-local:
	find . -name '*.la' -print | xargs $(LIBTOOL) --mode=clean rm -f
	find . -name '*.a' -print | xargs rm -f

CLEANFILES = *.go *.gox goc2c *.c s-version
