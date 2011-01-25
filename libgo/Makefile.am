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

go_os_files = \
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
	go/os/sys_linux.go \
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

go_syslog_files = \
	go/syslog/syslog.go

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
else # !LIBGO_IS_LINUX
if LIBGO_IS_SOLARIS
if LIBGO_IS_386
# Use lseek64 on 386 Solaris.
syscall_filesize_file = syscalls/sysfile_largefile.go
else # !LIBGO_IS_LINUX && LIBGO_IS_SOLARIS && !LIBGO_IS_386
# Use lseek on amd64 Solaris.
syscall_filesize_file = syscalls/sysfile_regfile.go
endif # !LIBGO_IS_386
else # !LIBGO_IS_LINUX && !LIBGO_IS_SOLARIS
# Use lseek by default.
syscall_filesize_file = syscalls/sysfile_regfile.go
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
	$(syscall_sleep_file) \
	syscalls/socket.go \
	$(syscall_socket_os_file) \
	$(syscall_socket_epoll_file) \
	$(syscall_syscall_file) \
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
# os_lib_inotify_la = os/libinotify.la
os_lib_inotify_la =
else
os_lib_inotify_la =
endif

libgo_go_objs = \
	asn1/libasn1.la \
	big/libbig.la \
	bufio/libbufio.la \
	bytes/libbytes.la \
	cmath/libcmath.la \
	ebnf/libebnf.la \
	exec/libexec.la \
	expvar/libexpvar.la \
	flag/libflag.la \
	fmt/libfmt.la \
	gob/libgob.la \
	hash/libhash.la \
	html/libhtml.la \
	http/libhttp.la \
	image/libimage.la \
	io/libio.la \
	json/libjson.la \
	log/liblog.la \
	math/libmath.la \
	mime/libmime.la \
	net/libnet.la \
	netchan/libnetchan.la \
	os/libos.la \
	patch/libpatch.la \
	path/libpath.la \
	rand/librand.la \
	reflect/libreflect.la \
	regexp/libregexp.la \
	rpc/librpc.la \
	runtime/libruntime.la \
	scanner/libscanner.la \
	smtp/libsmtp.la \
	sort/libsort.la \
	strconv/libstrconv.la \
	strings/libstrings.la \
	sync/libsync.la \
	syslog/libsyslog.la \
	tabwriter/libtabwriter.la \
	template/libtemplate.la \
	time/libtime.la \
	try/libtry.la \
	unicode/libunicode.la \
	utf16/libutf16.la \
	utf8/libutf8.la \
	websocket/libwebsocket.la \
	xml/libxml.la \
	archive/libtar.la \
	archive/libzip.la \
	compress/libflate.la \
	compress/libgzip.la \
	compress/libzlib.la \
	container/libheap.la \
	container/liblist.la \
	container/libring.la \
	container/libvector.la \
	crypto/libaes.la \
	crypto/libblock.la \
	crypto/libblowfish.la \
	crypto/libcast5.la \
	crypto/libcipher.la \
	crypto/libelliptic.la \
	crypto/libhmac.la \
	crypto/libmd4.la \
	crypto/libmd5.la \
	crypto/libocsp.la \
	crypto/librand.la \
	crypto/librc4.la \
	crypto/libripemd160.la \
	crypto/librsa.la \
	crypto/libsha1.la \
	crypto/libsha256.la \
	crypto/libsha512.la \
	crypto/libsubtle.la \
	crypto/libtls.la \
	crypto/libtwofish.la \
	crypto/libx509.la \
	crypto/libxtea.la \
	crypto/openpgp/libarmor.la \
	crypto/openpgp/liberror.la \
	crypto/openpgp/libs2k.la \
	debug/libdwarf.la \
	debug/libelf.la \
	debug/libgosym.la \
	debug/libmacho.la \
	debug/libpe.la \
	debug/libproc.la \
	encoding/libascii85.la \
	encoding/libbase32.la \
	encoding/libbase64.la \
	encoding/libbinary.la \
	encoding/libgit85.la \
	encoding/libhex.la \
	encoding/libline.la \
	encoding/libpem.la \
	exp/libdatafmt.la \
	exp/libdraw.la \
	exp/libeval.la \
	go/libast.la \
	go/libdoc.la \
	go/libparser.la \
	go/libprinter.la \
	go/libscanner.la \
	go/libtoken.la \
	go/libtypechecker.la \
	hash/libadler32.la \
	hash/libcrc32.la \
	hash/libcrc64.la \
	http/libpprof.la \
	image/libjpeg.la \
	image/libpng.la \
	index/libsuffixarray.la \
	io/libioutil.la \
	mime/libmultipart.la \
	net/libdict.la \
	net/libtextproto.la \
	$(os_lib_inotify_la) \
	os/libsignal.la \
	rpc/libjsonrpc.la \
	runtime/libdebug.la \
	runtime/libpprof.la \
	syscalls/libsyscall.la \
	testing/libtesting.la \
	testing/libiotest.la \
	testing/libquick.la \
	testing/libscript.la

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

# Build a package.
BUILDARCHIVE = \
	rm -f `echo $@ | sed -e 's|/lib|/|' -e 's/\.a/.gox/'`; \
	test -d $(@D) || $(MKDIR_P) $(@D); \
	rm -f $@; \
	files=`echo $^ | sed -e 's/[^ ]*\.gox//g'`; \
	if $(LTGOCOMPILE) -I . -c -fgo-prefix="libgo_$(@D)" -o $@.$(OBJEXT) $$files; then \
	  $(AR) rc $@ $@.$(OBJEXT); \
	else exit 1; fi

# Build a .la file from a .a file.
.a.la:
	$(LINK) $<.lo

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

asn1/libasn1.a: $(go_asn1_files) bytes.gox fmt.gox io.gox os.gox reflect.gox \
		strconv.gox strings.gox time.gox
	$(BUILDARCHIVE)
asn1/libasn1.la: asn1/libasn1.a
asn1/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: asn1/check

big/libbig.a: $(go_big_files) fmt.gox rand.gox strings.gox
	$(BUILDARCHIVE)
big/libbig.la: big/libbig.a
big/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: big/check

bufio/libbufio.a: $(go_bufio_files) bytes.gox io.gox os.gox strconv.gox \
		utf8.gox
	$(BUILDARCHIVE)
bufio/libbufio.la: bufio/libbufio.a
bufio/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: bufio/check

bytes/libbytes.a: $(go_bytes_files) $(go_bytes_c_files) io.gox os.gox \
		unicode.gox utf8.gox
	test -d bytes || $(MKDIR_P) bytes
	$(LTGOCOMPILE) -I . -c -o bytes/bytes.$(OBJEXT) -fgo-prefix=libgo_bytes $(srcdir)/go/bytes/buffer.go $(srcdir)/go/bytes/bytes.go $(srcdir)/go/bytes/bytes_decl.go
	$(LTCOMPILE) -c -o bytes/index.$(OBJEXT) $(srcdir)/go/bytes/indexbyte.c
	rm -f $@
	$(AR) rc $@ bytes/bytes.$(OBJEXT) bytes/index.$(OBJEXT)
bytes/libbytes.la: bytes/libbytes.a
	$(LINK) bytes/bytes.lo bytes/index.lo
bytes/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: bytes/check

cmath/libcmath.a: $(go_cmath_files) math.gox
	$(BUILDARCHIVE)
cmath/libcmath.la: cmath/libcmath.a
cmath/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: cmath/check

ebnf/libebnf.a: $(go_ebnf_files) container/vector.gox go/scanner.gox \
		go/token.gox os.gox strconv.gox unicode.gox utf8.gox
	$(BUILDARCHIVE)
ebnf/libebnf.la: ebnf/libebnf.a
ebnf/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: ebnf/check

exec/libexec.a: $(go_exec_files) os.gox strings.gox
	$(BUILDARCHIVE)
exec/libexec.la: exec/libexec.a
exec/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: exec/check

expvar/libexpvar.a: $(go_expvar_files) bytes.gox fmt.gox http.gox json.gox \
		log.gox os.gox runtime.gox strconv.gox sync.gox
	$(BUILDARCHIVE)
expvar/libexpvar.la: expvar/libexpvar.a
expvar/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: expvar/check

flag/libflag.a: $(go_flag_files) fmt.gox os.gox strconv.gox
	$(BUILDARCHIVE)
flag/libflag.la: flag/libflag.a
flag/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: flag/check

fmt/libfmt.a: $(go_fmt_files) bytes.gox io.gox os.gox reflect.gox strconv.gox \
		strings.gox unicode.gox utf8.gox
	$(BUILDARCHIVE)
fmt/libfmt.la: fmt/libfmt.a
fmt/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: fmt/check

gob/libgob.a: $(go_gob_files) bytes.gox fmt.gox io.gox math.gox os.gox \
		reflect.gox runtime.gox strings.gox sync.gox unicode.gox \
		utf8.gox
	$(BUILDARCHIVE)
gob/libgob.la: gob/libgob.a
gob/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: gob/check

hash/libhash.a: $(go_hash_files) io.gox
	$(BUILDARCHIVE)
hash/libhash.la: hash/libhash.a
hash/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: hash/check

html/libhtml.a: $(go_html_files) bytes.gox io.gox os.gox strconv.gox \
		strings.gox utf8.gox
	$(BUILDARCHIVE)
html/libhtml.la: html/libhtml.a
html/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: html/check

http/libhttp.a: $(go_http_files) bufio.gox bytes.gox container/list.gox \
		container/vector.gox crypto/rand.gox crypto/tls.gox \
		encoding/base64.gox fmt.gox io.gox io/ioutil.gox log.gox \
		mime.gox mime/multipart.gox net.gox os.gox path.gox sort.gox \
		strconv.gox strings.gox sync.gox time.gox utf8.gox
	$(BUILDARCHIVE)
http/libhttp.la: http/libhttp.a
http/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: http/check

image/libimage.a: $(go_image_files) bufio.gox io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
image/libimage.la: image/libimage.a
image/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: image/check

io/libio.a: $(go_io_files) os.gox runtime.gox sync.gox
	$(BUILDARCHIVE)
io/libio.la: io/libio.a
io/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: io/check

json/libjson.a: $(go_json_files) bytes.gox container/vector.gox fmt.gox \
		io.gox math.gox os.gox reflect.gox runtime.gox strconv.gox \
		strings.gox unicode.gox utf16.gox utf8.gox
	$(BUILDARCHIVE)
json/libjson.la: json/libjson.a
json/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: json/check

log/liblog.a: $(go_log_files) bytes.gox fmt.gox io.gox runtime.gox os.gox \
		sync.gox time.gox
	$(BUILDARCHIVE)
log/liblog.la: log/liblog.a
log/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: log/check

math/libmath.a: $(go_math_files)
	$(BUILDARCHIVE)
math/libmath.la: math/libmath.a
math/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: math/check

mime/libmime.a: $(go_mime_files) bufio.gox bytes.gox os.gox strings.gox \
		sync.gox unicode.gox
	$(BUILDARCHIVE)
mime/libmime.la: mime/libmime.a
mime/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: mime/check

net/libnet.a: $(go_net_files) bytes.gox fmt.gox io.gox os.gox reflect.gox \
		strconv.gox strings.gox sync.gox syscall.gox
	$(BUILDARCHIVE)
net/libnet.la: net/libnet.a
net/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: net/check

netchan/libnetchan.a: $(go_netchan_files) gob.gox log.gox net.gox os.gox \
		reflect.gox strconv.gox sync.gox time.gox
	$(BUILDARCHIVE)
netchan/libnetchan.la: netchan/libnetchan.a
netchan/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: netchan/check

os/libos.a: $(go_os_files) sync.gox syscall.gox
	$(BUILDARCHIVE)
os/libos.la: os/libos.a
os/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: os/check

patch/libpatch.a: $(go_patch_files) bytes.gox compress/zlib.gox \
		crypto/sha1.gox encoding/git85.gox fmt.gox io.gox os.gox \
		path.gox strings.gox
	$(BUILDARCHIVE)
patch/libpatch.la: patch/libpatch.a
patch/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: patch/check

path/libpath.a: $(go_path_files) io/ioutil.gox os.gox sort.gox strings.gox \
		utf8.gox
	$(BUILDARCHIVE)
path/libpath.la: path/libpath.a
path/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: path/check

rand/librand.a: $(go_rand_files) math.gox sync.gox
	$(BUILDARCHIVE)
rand/librand.la: rand/librand.a
rand/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: rand/check

reflect/libreflect.a: $(go_reflect_files) math.gox runtime.gox strconv.gox \
		sync.gox
	$(BUILDARCHIVE)
reflect/libreflect.la: reflect/libreflect.a
reflect/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: reflect/check

regexp/libregexp.a: $(go_regexp_files) bytes.gox io.gox os.gox strings.gox \
		utf8.gox
	$(BUILDARCHIVE)
regexp/libregexp.la: regexp/libregexp.a
regexp/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: regexp/check

rpc/librpc.a: $(go_rpc_files) bufio.gox fmt.gox gob.gox http.gox io.gox \
		log.gox net.gox os.gox reflect.gox sort.gox strings.gox \
		strconv.gox sync.gox template.gox unicode.gox utf8.gox
	$(BUILDARCHIVE)
rpc/librpc.la: rpc/librpc.a
rpc/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: rpc/check

runtime/libruntime.a: $(go_runtime_files)
	$(BUILDARCHIVE)
runtime/libruntime.la: runtime/libruntime.a
runtime/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: runtime/check

scanner/libscanner.a: $(go_scanner_files) bytes.gox fmt.gox io.gox os.gox \
		unicode.gox utf8.gox
	$(BUILDARCHIVE)
scanner/libscanner.la: scanner/libscanner.a
scanner/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: scanner/check

smtp/libsmtp.a: $(go_smtp_files) crypto/tls.gox encoding/base64.gox io.gox \
		net.gox net/textproto.gox os.gox strings.gox
	$(BUILDARCHIVE)
smtp/libsmtp.la: smtp/libsmtp.a
smtp/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: smtp/check

sort/libsort.a: $(go_sort_files)
	$(BUILDARCHIVE)
sort/libsort.la: sort/libsort.a
sort/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: sort/check

strconv/libstrconv.a: $(go_strconv_files) bytes.gox math.gox os.gox \
		strings.gox unicode.gox utf8.gox
	$(BUILDARCHIVE)
strconv/libstrconv.la: strconv/libstrconv.a
strconv/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: strconv/check

strings/libstrings.a: $(go_strings_files) os.gox unicode.gox utf8.gox
	$(BUILDARCHIVE)
strings/libstrings.la: strings/libstrings.a
strings/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: strings/check

sync/libsync.a: $(go_sync_files) $(go_sync_c_files) runtime.gox
	test -d sync || $(MKDIR_P) sync
	$(LTGOCOMPILE) -I . -c -o sync/mutex.$(OBJEXT) -fgo-prefix=libgo_sync $(srcdir)/go/sync/mutex.go $(srcdir)/go/sync/once.go $(srcdir)/go/sync/rwmutex.go
	$(LTCOMPILE) -c -o sync/cas.$(OBJEXT) $(srcdir)/go/sync/cas.c
	rm -f $@
	$(AR) rc $@ sync/mutex.$(OBJEXT) sync/cas.$(OBJEXT)
sync/libsync.la: sync/libsync.a
	$(LINK) sync/mutex.lo sync/cas.lo
sync/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: sync/check

syslog/libsyslog.a: $(go_syslog_files) fmt.gox log.gox net.gox os.gox
	$(BUILDARCHIVE)
syslog/libsyslog.la: syslog/libsyslog.a
syslog/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: syslog/check

tabwriter/libtabwriter.a: $(go_tabwriter_files) bytes.gox io.gox os.gox \
		utf8.gox
	$(BUILDARCHIVE)
tabwriter/libtabwriter.la: tabwriter/libtabwriter.a
tabwriter/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: tabwriter/check

template/libtemplate.a: $(go_template_files) bytes.gox fmt.gox io.gox os.gox \
		reflect.gox runtime.gox strings.gox container/vector.gox
	$(BUILDARCHIVE)
template/libtemplate.la: template/libtemplate.a
template/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: template/check

testing/libtesting.a: $(go_testing_files) flag.gox fmt.gox os.gox regexp.gox \
		runtime.gox time.gox
	$(BUILDARCHIVE)
testing/libtesting.la: testing/libtesting.a
testing/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: testing/check

time/libtime.a: $(go_time_files) bytes.gox container/heap.gox io/ioutil.gox \
		os.gox strconv.gox sync.gox syscall.gox
	$(BUILDARCHIVE)
time/libtime.la: time/libtime.a
time/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: time/check

try/libtry.a: $(go_try_files) fmt.gox io.gox os.gox reflect.gox unicode.gox
	$(BUILDARCHIVE)
try/libtry.la: try/libtry.a
try/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: try/check

unicode/libunicode.a: $(go_unicode_files)
	$(BUILDARCHIVE)
unicode/libunicode.la: unicode/libunicode.a
unicode/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: unicode/check

utf16/libutf16.a: $(go_utf16_files) unicode.gox
	$(BUILDARCHIVE)
utf16/libutf16.la: utf16/libutf16.a
utf16/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: utf16/check

utf8/libutf8.a: $(go_utf8_files) unicode.gox
	$(BUILDARCHIVE)
utf8/libutf8.la: utf8/libutf8.a
utf8/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: utf8/check

websocket/libwebsocket.a: $(go_websocket_files) bufio.gox bytes.gox \
		container/vector.gox crypto/md5.gox crypto/tls.gox \
		encoding/binary.gox fmt.gox http.gox io.gox net.gox os.gox \
		rand.gox strings.gox
	$(BUILDARCHIVE)
websocket/libwebsocket.la: websocket/libwebsocket.a
websocket/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: websocket/check

xml/libxml.a: $(go_xml_files) bufio.gox bytes.gox fmt.gox io.gox os.gox \
		reflect.gox strconv.gox strings.gox unicode.gox utf8.gox
	$(BUILDARCHIVE)
xml/libxml.la: xml/libxml.a
xml/check: $(CHECK_DEPS)
	$(CHECK)
.PHONY: xml/check

archive/libtar.a: $(go_archive_tar_files) bytes.gox io.gox os.gox strconv.gox \
		strings.gox
	$(BUILDARCHIVE)
archive/libtar.la: archive/libtar.a
archive/tar/check: $(CHECK_DEPS)
	@$(MKDIR_P) archive/tar
	$(CHECK)
.PHONY: archive/tar/check

archive/libzip.a: $(go_archive_zip_files) bufio.gox bytes.gox \
		compress/flate.gox hash.gox hash/crc32.gox \
		encoding/binary.gox io.gox os.gox
	$(BUILDARCHIVE)
archive/libzip.la: archive/libzip.a
archive/zip/check: $(CHECK_DEPS)
	@$(MKDIR_P) archive/zip
	$(CHECK)
.PHONY: archive/zip/check

compress/libflate.a: $(go_compress_flate_files) bufio.gox io.gox math.gox \
		os.gox sort.gox strconv.gox
	$(BUILDARCHIVE)
compress/libflate.la: compress/libflate.a
compress/flate/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/flate
	$(CHECK)
.PHONY: compress/flate/check

compress/libgzip.a: $(go_compress_gzip_files) bufio.gox compress/flate.gox \
		hash.gox hash/crc32.gox io.gox os.gox
	$(BUILDARCHIVE)
compress/libgzip.la: compress/libgzip.a
compress/gzip/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/gzip
	$(CHECK)
.PHONY: compress/gzip/check

compress/libzlib.a: $(go_compress_zlib_files) bufio.gox compress/flate.gox \
		hash.gox hash/adler32.gox io.gox os.gox
	$(BUILDARCHIVE)
compress/libzlib.la: compress/libzlib.a
compress/zlib/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/zlib
	$(CHECK)
.PHONY: compress/zlib/check

container/libheap.a: $(go_container_heap_files) sort.gox
	$(BUILDARCHIVE)
container/libheap.la: container/libheap.a
container/heap/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/heap
	$(CHECK)
.PHONY: container/heap/check

container/liblist.a: $(go_container_list_files)
	$(BUILDARCHIVE)
container/liblist.la: container/liblist.a
container/list/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/list
	$(CHECK)
.PHONY: container/list/check

container/libring.a: $(go_container_ring_files)
	$(BUILDARCHIVE)
container/libring.la: container/libring.a
container/ring/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/ring
	$(CHECK)
.PHONY: container/ring/check

container/libvector.a: $(go_container_vector_files)
	$(BUILDARCHIVE)
container/libvector.la: container/libvector.a
container/vector/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/vector
	$(CHECK)
.PHONY: container/vector/check

crypto/libaes.a: $(go_crypto_aes_files) os.gox strconv.gox
	$(BUILDARCHIVE)
crypto/libaes.la: crypto/libaes.a
crypto/aes/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/aes
	$(CHECK)
.PHONY: crypto/aes/check

crypto/libblock.a: $(go_crypto_block_files) fmt.gox io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
crypto/libblock.la: crypto/libblock.a
crypto/block/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/block
	$(CHECK)
.PHONY: crypto/block/check

crypto/libblowfish.a: $(go_crypto_blowfish_files) os.gox strconv.gox
	$(BUILDARCHIVE)
crypto/libblowfish.la: crypto/libblowfish.a
crypto/blowfish/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/blowfish
	$(CHECK)
.PHONY: crypto/blowfish/check

crypto/libcast5.a: $(go_crypto_cast5_files) os.gox
	$(BUILDARCHIVE)
crypto/libcast5.la: crypto/libcast5.a
crypt/cast5/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/cast5
	$(CHECK)
.PHONY: crypto/cast5/check

crypto/libcipher.a: $(go_crypto_cipher_files) io.gox os.gox
	$(BUILDARCHIVE)
crypto/libcipher.la: crypto/libcipher.a
crypto/cipher/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/cipher
	$(CHECK)
.PHONY: crypto/cipher/check

crypto/libelliptic.a: $(go_crypto_elliptic_files) big.gox io.gox os.gox \
		sync.gox
	$(BUILDARCHIVE)
crypto/libelliptic.la: crypto/libelliptic.a
crypto/elliptic/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/elliptic
	$(CHECK)
.PHONY: crypto/elliptic/check

crypto/libhmac.a: $(go_crypto_hmac_files) crypto/md5.gox crypto/sha1.gox \
		crypto/sha256.gox hash.gox os.gox
	$(BUILDARCHIVE)
crypto/libhmac.la: crypto/libhmac.a
crypto/hmac/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/hmac
	$(CHECK)
.PHONY: crypto/hmac/check

crypto/libmd4.a: $(go_crypto_md4_files) hash.gox os.gox
	$(BUILDARCHIVE)
crypto/libmd4.la: crypto/libmd4.a
crypto/md4/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/md4
	$(CHECK)
.PHONY: crypto/md4/check

crypto/libmd5.a: $(go_crypto_md5_files) hash.gox os.gox
	$(BUILDARCHIVE)
crypto/libmd5.la: crypto/libmd5.a
crypto/md5/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/md5
	$(CHECK)
.PHONY: crypto/md5/check

crypto/libocsp.a: $(go_crypto_ocsp_files) asn1.gox crypto/rsa.gox \
		crypto/sha1.gox crypto/x509.gox os.gox time.gox
	$(BUILDARCHIVE)
crypto/libocsp.la: crypto/libocsp.a
crypto/ocsp/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/ocsp
	$(CHECK)
.PHONY: crypto/ocsp/check

crypto/librand.a: $(go_crypto_rand_files) crypto/aes.gox io.gox os.gox \
		sync.gox time.gox
	$(BUILDARCHIVE)
crypto/librand.la: crypto/librand.a
crypto/rand/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rand
	$(CHECK)
.PHONY: crypto/rand/check

crypto/librc4.a: $(go_crypto_rc4_files) os.gox strconv.gox
	$(BUILDARCHIVE)
crypto/librc4.la: crypto/librc4.a
crypto/rc4/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rc4
	$(CHECK)
.PHONY: crypto/rc4/check

crypto/libripemd160.a: $(go_crypto_ripemd160_files) hash.gox os.gox
	$(BUILDARCHIVE)
crypto/libripemd160.la: crypto/libripemd160.a
crypto/ripemd160/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/ripemd160
	$(CHECK)
.PHONY: crypto/ripemd160/check

crypto/librsa.a: $(go_crypto_rsa_files) big.gox crypto/sha1.gox \
		crypto/subtle.gox encoding/hex.gox hash.gox io.gox os.gox
	$(BUILDARCHIVE)
crypto/librsa.la: crypto/librsa.a
crypto/rsa/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rsa
	$(CHECK)
.PHONY: crypto/rsa/check

crypto/libsha1.a: $(go_crypto_sha1_files) hash.gox os.gox
	$(BUILDARCHIVE)
crypto/libsha1.la: crypto/libsha1.a
crypto/sha1/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha1
	$(CHECK)
.PHONY: crypto/sha1/check

crypto/libsha256.a: $(go_crypto_sha256_files) hash.gox os.gox
	$(BUILDARCHIVE)
crypto/libsha256.la: crypto/libsha256.a
crypto/sha256/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha256
	$(CHECK)
.PHONY: crypto/sha256/check

crypto/libsha512.a: $(go_crypto_sha512_files) hash.gox os.gox
	$(BUILDARCHIVE)
crypto/libsha512.la: crypto/libsha512.a
crypto/sha512/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha512
	$(CHECK)
.PHONY: crypto/sha512/check

crypto/libsubtle.a: $(go_crypto_subtle_files)
	$(BUILDARCHIVE)
crypto/libsubtle.la: crypto/libsubtle.a
crypto/subtle/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/subtle
	$(CHECK)
.PHONY: crypto/subtle/check

crypto/libtls.a: $(go_crypto_tls_files) big.gox bufio.gox bytes.gox \
		container/list.gox crypto/aes.gox crypto/cipher.gox \
		crypto/elliptic.gox crypto/hmac.gox crypto/md5.gox \
		crypto/rc4.gox crypto/rand.gox crypto/rsa.gox crypto/sha1.gox \
		crypto/subtle.gox crypto/rsa.gox crypto/sha1.gox \
		crypto/x509.gox encoding/pem.gox fmt.gox hash.gox io.gox \
		io/ioutil.gox net.gox os.gox strings.gox sync.gox time.gox
	$(BUILDARCHIVE)
crypto/libtls.la: crypto/libtls.a
crypto/tls/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/tls
	$(CHECK)
.PHONY: crypto/tls/check

crypto/libtwofish.a: $(go_crypto_twofish_files) os.gox strconv.gox
	$(BUILDARCHIVE)
crypto/libtwofish.la: crypto/libtwofish.a
crypto/twofish/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/twofish
	$(CHECK)
.PHONY: crypto/twofish/check

crypto/libx509.a: $(go_crypto_x509_files) asn1.gox big.gox \
		container/vector.gox crypto/rsa.gox crypto/sha1.gox hash.gox \
		os.gox strings.gox time.gox
	$(BUILDARCHIVE)
crypto/libx509.la: crypto/libx509.a
crypto/x509/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/x509
	$(CHECK)
.PHONY: crypto/x509/check

crypto/libxtea.a: $(go_crypto_xtea_files) os.gox strconv.gox
	$(BUILDARCHIVE)
crypto/libxtea.la: crypto/libxtea.a
crypto/xtea/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/xtea
	$(CHECK)
.PHONY: crypto/xtea/check

crypto/openpgp/libarmor.a: $(go_crypto_openpgp_armor_files) bytes.gox \
		crypto/openpgp/error.gox encoding/base64.gox \
		encoding/line.gox io.gox os.gox
	$(BUILDARCHIVE)
crypto/openpgp/libarmor.la: crypto/openpgp/libarmor.a
crypto/openpgp/armor/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/armor
	$(CHECK)
.PHONY: crypto/openpgp/armor/check

crypto/openpgp/liberror.a: $(go_crypto_openpgp_error_files)
	$(BUILDARCHIVE)
crypto/openpgp/liberror.la: crypto/openpgp/liberror.a
crypto/openpgp/error/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/error
	$(CHECK)
.PHONY: crypto/openpgp/error/check

crypto/openpgp/libs2k.a: $(go_crypto_openpgp_s2k_files) crypto/md5.gox \
		crypto/openpgp/error.gox crypto/ripemd160.gox crypto/sha1.gox \
		crypto/sha256.gox crypto/sha512.gox hash.gox io.gox os.gox
	$(BUILDARCHIVE)
crypto/openpgp/libs2k.la: crypto/openpgp/libs2k.a
crypto/openpgp/s2k/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/s2k
	$(CHECK)
.PHONY: crypto/openpgp/s2k/check

debug/libdwarf.a: $(go_debug_dwarf_files) encoding/binary.gox os.gox \
		strconv.gox
	$(BUILDARCHIVE)
debug/libdwarf.la: debug/libdwarf.a
debug/dwarf/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/dwarf
	$(CHECK)
.PHONY: debug/dwarf/check

debug/libelf.a: $(go_debug_elf_files) bytes.gox debug/dwarf.gox \
		encoding/binary.gox fmt.gox io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
debug/libelf.la: debug/libelf.a
debug/elf/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/elf
	$(CHECK)
.PHONY: debug/elf/check

debug/libgosym.a: $(go_debug_gosym_files) encoding/binary.gox fmt.gox os.gox \
		strconv.gox strings.gox
	$(BUILDARCHIVE)
debug/libgosym.la: debug/libgosym.a
debug/gosym/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/gosym
	$(CHECK)
.PHONY: debug/gosym/check

debug/libmacho.a: $(go_debug_macho_files) bytes.gox debug/dwarf.gox \
		encoding/binary.gox fmt.gox io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
debug/libmacho.la: debug/libmacho.a
debug/macho/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/macho
	$(CHECK)
.PHONY: debug/macho/check

debug/libpe.a: $(go_debug_pe_files) debug/dwarf.gox encoding/binary.gox \
		fmt.gox io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
debug/libpe.la: debug/libpe.a
debug/pe/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/pe
	$(CHECK)
.PHONY: debug/pe/check

debug/libproc.a: $(go_debug_proc_files) container/vector.gox fmt.gox \
		io/ioutil.gox os.gox runtime.gox strconv.gox strings.gox \
		sync.gox syscall.gox
	$(BUILDARCHIVE)
debug/libproc.la: debug/libproc.a
debug/proc/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/proc
	$(CHECK)
.PHONY: debug/proc/check

encoding/libascii85.a: $(go_encoding_ascii85_files) io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
encoding/libascii85.la: encoding/libascii85.a
encoding/ascii85/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/ascii85
	$(CHECK)
.PHONY: encoding/ascii85/check

encoding/libbase32.a: $(go_encoding_base32_files) io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
encoding/libbase32.la: encoding/libbase32.a
encoding/base32/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/base32
	$(CHECK)
.PHONY: encoding/base32/check

encoding/libbase64.a: $(go_encoding_base64_files) io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
encoding/libbase64.la: encoding/libbase64.a
encoding/base64/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/base64
	$(CHECK)
.PHONY: encoding/base64/check

encoding/libbinary.a: $(go_encoding_binary_files) io.gox math.gox os.gox \
		reflect.gox
	$(BUILDARCHIVE)
encoding/libbinary.la: encoding/libbinary.a
encoding/binary/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/binary
	$(CHECK)
.PHONY: encoding/binary/check

encoding/libgit85.a: $(go_encoding_git85_files) bytes.gox io.gox os.gox \
		strconv.gox
	$(BUILDARCHIVE)
encoding/libgit85.la: encoding/libgit85.a
encoding/git85/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/git85
	$(CHECK)
.PHONY: encoding/git85/check

encoding/libhex.a: $(go_encoding_hex_files) os.gox strconv.gox
	$(BUILDARCHIVE)
encoding/libhex.la: encoding/libhex.a
encoding/hex/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/hex
	$(CHECK)
.PHONY: encoding/hex/check

encoding/libline.a: $(go_encoding_line_files) io.gox os.gox
	$(BUILDARCHIVE)
encoding/libline.la: encoding/libline.a
encoding/line/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/line
	$(CHECK)
.PHONY: encoding/line/check

encoding/libpem.a: $(go_encoding_pem_files) bytes.gox encoding/base64.gox
	$(BUILDARCHIVE)
encoding/libpem.la: encoding/libpem.a
encoding/pem/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/pem
	$(CHECK)
.PHONY: encoding/pem/check

exp/libdatafmt.a: $(go_exp_datafmt_files) bytes.gox container/vector.gox \
		fmt.gox go/scanner.gox go/token.gox io.gox os.gox reflect.gox \
		runtime.gox strconv.gox strings.gox
	$(BUILDARCHIVE)
exp/libdatafmt.la: exp/libdatafmt.a
exp/datafmt/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/datafmt
	$(CHECK)
.PHONY: exp/datafmt/check

exp/libdraw.a: $(go_exp_draw_files) image.gox os.gox
	$(BUILDARCHIVE)
exp/libdraw.la: exp/libdraw.a
exp/draw/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/draw
	$(CHECK)
.PHONY: exp/draw/check

exp/libeval.a: $(go_exp_eval_files) big.gox go/ast.gox go/parser.gox \
		go/scanner.gox go/token.gox fmt.gox log.gox strconv.gox \
		strings.gox os.gox reflect.gox runtime.gox sort.gox template.gox
	$(BUILDARCHIVE)
exp/libeval.la: exp/libeval.a
exp/eval/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/eval
	$(CHECK)
.PHONY: exp/eval/check

go/libast.a: $(go_go_ast_files) fmt.gox go/token.gox io.gox os.gox \
		reflect.gox unicode.gox utf8.gox
	$(BUILDARCHIVE)
go/libast.la: go/libast.a
go/ast/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/ast
	$(CHECK)
.PHONY: go/ast/check

go/libdoc.a: $(go_go_doc_files) go/ast.gox go/token.gox io.gox regexp.gox \
		sort.gox strings.gox template.gox
	$(BUILDARCHIVE)
go/libdoc.la: go/libdoc.a
go/doc/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/doc
	$(CHECK)
.PHONY: go/doc/check

go/libparser.a: $(go_go_parser_files) bytes.gox fmt.gox go/ast.gox \
		go/scanner.gox go/token.gox io.gox io/ioutil.gox os.gox \
		path.gox strings.gox
	$(BUILDARCHIVE)
go/libparser.la: go/libparser.a
go/parser/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/parser
	$(CHECK)
.PHONY: go/parser/check

go/libprinter.a: $(go_go_printer_files) bytes.gox fmt.gox go/ast.gox \
		go/token.gox io.gox os.gox reflect.gox runtime.gox \
		strings.gox tabwriter.gox
	$(BUILDARCHIVE)
go/libprinter.la: go/libprinter.a
go/printer/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/printer
	$(CHECK)
.PHONY: go/printer/check

go/libscanner.a: $(go_go_scanner_files) bytes.gox container/vector.gox fmt.gox \
		go/token.gox io.gox os.gox sort.gox strconv.gox unicode.gox \
		utf8.gox
	$(BUILDARCHIVE)
go/libscanner.la: go/libscanner.a
go/scanner/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/scanner
	$(CHECK)
.PHONY: go/scanner/check

go/libtoken.a: $(go_go_token_files) fmt.gox strconv.gox
	$(BUILDARCHIVE)
go/libtoken.la: go/libtoken.a
go/token/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/token
	$(CHECK)
.PHONY: go/token/check

go/libtypechecker.a: $(go_go_typechecker_files) fmt.gox go/ast.gox \
		go/token.gox go/scanner.gox os.gox
	$(BUILDARCHIVE)
go/libtypechecker.la: go/libtypechecker.a
go/typechecker/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/typechecker
	$(CHECK)
.PHONY: go/typechecker/check

hash/libadler32.a: $(go_hash_adler32_files) hash.gox os.gox
	$(BUILDARCHIVE)
hash/libadler32.la: hash/libadler32.a
hash/adler32/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/adler32
	$(CHECK)
.PHONY: hash/adler32/check

hash/libcrc32.a: $(go_hash_crc32_files) hash.gox os.gox
	$(BUILDARCHIVE)
hash/libcrc32.la: hash/libcrc32.a
hash/crc32/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/crc32
	$(CHECK)
.PHONY: hash/crc32/check

hash/libcrc64.a: $(go_hash_crc64_files) hash.gox os.gox
	$(BUILDARCHIVE)
hash/libcrc64.la: hash/libcrc64.a
hash/crc64/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/crc64
	$(CHECK)
.PHONY: hash/crc64/check

http/libpprof.a: $(go_http_pprof_files) bufio.gox fmt.gox http.gox os.gox \
		runtime.gox runtime/pprof.gox strconv.gox strings.gox
	$(BUILDARCHIVE)
http/libpprof.la: http/libpprof.a
http/pprof/check: $(CHECK_DEPS)
	@$(MKDIR_P) http/pprof
	$(CHECK)
.PHONY: http/pprof/check

image/libjpeg.a: $(go_image_jpeg_files) bufio.gox image.gox io.gox os.gox
	$(BUILDARCHIVE)
image/libjpeg.la: image/libjpeg.a
image/jpeg/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/jpeg
	$(CHECK)
.PHONY: image/jpeg/check

image/libpng.a: $(go_image_png_files) bufio.gox compress/zlib.gox fmt.gox \
		hash.gox hash/crc32.gox image.gox io.gox os.gox strconv.gox
	$(BUILDARCHIVE)
image/libpng.la: image/libpng.a
image/png/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/png
	$(CHECK)
.PHONY: image/png/check

index/libsuffixarray.a: $(go_index_suffixarray_files) bytes.gox regexp.gox \
		sort.gox
	$(BUILDARCHIVE)
index/libsuffixarray.la: index/libsuffixarray.a
index/suffixarray/check: $(CHECK_DEPS)
	@$(MKDIR_P) index/suffixarray
	$(CHECK)
.PHONY: index/suffixarray/check

io/libioutil.a: $(go_io_ioutil_files) bytes.gox io.gox os.gox sort.gox \
		strconv.gox
	$(BUILDARCHIVE)
io/libioutil.la: io/libioutil.a
io/ioutil/check: $(CHECK_DEPS)
	@$(MKDIR_P) io/ioutil
	$(CHECK)
.PHONY: io/ioutil/check

mime/libmultipart.a: $(go_mime_multipart_files) bufio.gox bytes.gox io.gox \
		mime.gox os.gox regexp.gox strings.gox
	$(BUILDARCHIVE)
mime/libmultipart.la: mime/libmultipart.a
mime/multipart/check: $(CHECK_DEPS)
	@$(MKDIR_P) mime/multipart
	$(CHECK)
.PHONY: mime/multipart/check

net/libdict.a: $(go_net_dict_files) container/vector.gox net/textproto.gox \
		os.gox strconv.gox strings.gox
	$(BUILDARCHIVE)
net/libdict.la: net/libdict.a

net/libtextproto.a: $(go_net_textproto_files) bufio.gox bytes.gox \
		container/vector.gox fmt.gox io.gox io/ioutil.gox net.gox \
		os.gox strconv.gox sync.gox
	$(BUILDARCHIVE)
net/libtextproto.la: net/libtextproto.a
net/textproto/check: $(CHECK_DEPS)
	@$(MKDIR_P) net/textproto
	$(CHECK)
.PHONY: net/textproto/check

os/libinotify.a: $(go_os_inotify_files) fmt.gox os.gox strings.gox \
		syscall.gox
	$(BUILDARCHIVE)
os/libinotify.la: os/libinotify.a
os/inotify/check: $(CHECK_DEPS)
	@$(MKDIR_P) os/inotify
	$(CHECK)
.PHONY: os/inotify/check

os/libsignal.a: $(go_os_signal_files) runtime.gox strconv.gox
	$(BUILDARCHIVE)
os/libsignal.la: os/libsignal.a
os/signal/check: $(CHECK_DEPS)
	@$(MKDIR_P) os/signal
	$(CHECK)
.PHONY: os/signal/check

unix.go: $(srcdir)/go/os/signal/mkunix.sh sysinfo.go
	$(SHELL) $(srcdir)/go/os/signal/mkunix.sh sysinfo.go > $@.tmp
	mv -f $@.tmp $@

rpc/libjsonrpc.a: $(go_rpc_jsonrpc_files) fmt.gox io.gox json.gox net.gox \
		os.gox rpc.gox sync.gox
	$(BUILDARCHIVE)
rpc/libjsonrpc.la: rpc/libjsonrpc.a
rpc/jsonrpc/check: $(CHECK_DEPS)
	@$(MKDIR_P) rpc/jsonrpc
	$(CHECK)
.PHONY: rpc/jsonrpc/check

runtime/libdebug.a: $(go_runtime_debug_files) bytes.gox fmt.gox io/ioutil.gox \
		os.gox runtime.gox
	$(BUILDARCHIVE)
runtime/libdebug.la: runtime/libdebug.a
runtime/debug/check: $(CHECK_DEPS)
	@$(MKDIR_P) runtime/debug
	$(CHECK)
.PHONY: runtime/debug/check

runtime/libpprof.a: $(go_runtime_pprof_files) bufio.gox fmt.gox io.gox os.gox \
		runtime.gox
	$(BUILDARCHIVE)
runtime/libpprof.la: runtime/libpprof.a
runtime/pprof/check: $(CHECK_DEPS)
	@$(MKDIR_P) runtime/pprof
	$(CHECK)
.PHONY: runtime/pprof/check

testing/libiotest.a: $(go_testing_iotest_files) io.gox log.gox os.gox
	$(BUILDARCHIVE)
testing/libiotest.la: testing/libiotest.a
testing/iotest/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/iotest
	$(CHECK)
.PHONY: testing/iotest/check

testing/libquick.a: $(go_testing_quick_files) flag.gox fmt.gox math.gox \
		os.gox rand.gox reflect.gox strings.gox
	$(BUILDARCHIVE)
testing/libquick.la: testing/libquick.a
testing/quick/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/quick
	$(CHECK)
.PHONY: testing/quick/check

testing/libscript.a: $(go_testing_script_files) fmt.gox os.gox rand.gox \
		reflect.gox strings.gox
	$(BUILDARCHIVE)
testing/libscript.la: testing/libscript.a
testing/script/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/script
	$(CHECK)
.PHONY: testing/script/check

sysinfo.go: $(srcdir)/mksysinfo.sh config.h
	CC="$(CC)" $(SHELL) $(srcdir)/mksysinfo.sh
syscalls/libsyscall.a: $(go_syscall_files) $(go_syscall_c_files) sync.gox
	rm -f syscall.gox syscalls/libsyscall.a
	test -d syscalls || $(MKDIR_P) syscalls
	files=`echo $^ | sed -e 's/[^ ]*\.gox//g' -e's/[^ ]*\.c//g'`; \
	$(LTGOCOMPILE) -I . -c -fgo-prefix="libgo_syscalls" -o syscalls/syscall.$(OBJEXT) $$files
	$(LTCOMPILE) -c -o syscalls/errno.$(OBJEXT) $(srcdir)/syscalls/errno.c
	$(AR) rc syscalls/libsyscall.a syscalls/syscall.$(OBJEXT) syscalls/errno.$(OBJEXT)
syscalls/libsyscall.la: syscalls/libsyscall.a
	$(LINK) syscalls/syscall.lo syscalls/errno.lo

# How to build a .gox file from a .a file.
BUILDGOX = \
	set -e; \
	rm -f $@.$(OBJEXT); \
	$(CC) -r -nostdlib -o $@.$(OBJEXT) -Wl,--whole-archive $<; \
	$(OBJCOPY) -j .go_export $@.$(OBJEXT) $@.tmp; \
	mv -f $@.tmp $@; \
	rm -f $@.$(OBJEXT)

asn1.gox: asn1/libasn1.a
	$(BUILDGOX)
big.gox: big/libbig.a
	$(BUILDGOX)
bufio.gox: bufio/libbufio.a
	$(BUILDGOX)
bytes.gox: bytes/libbytes.a
	$(BUILDGOX)
cmath.gox: cmath/libcmath.a
	$(BUILDGOX)
ebnf.gox: ebnf/libebnf.a
	$(BUILDGOX)
exec.gox: exec/libexec.a
	$(BUILDGOX)
expvar.gox: expvar/libexpvar.a
	$(BUILDGOX)
flag.gox: flag/libflag.a
	$(BUILDGOX)
fmt.gox: fmt/libfmt.a
	$(BUILDGOX)
gob.gox: gob/libgob.a
	$(BUILDGOX)
hash.gox: hash/libhash.a
	$(BUILDGOX)
html.gox: html/libhtml.a
	$(BUILDGOX)
http.gox: http/libhttp.a
	$(BUILDGOX)
image.gox: image/libimage.a
	$(BUILDGOX)
io.gox: io/libio.a
	$(BUILDGOX)
json.gox: json/libjson.a
	$(BUILDGOX)
log.gox: log/liblog.a
	$(BUILDGOX)
math.gox: math/libmath.a
	$(BUILDGOX)
mime.gox: mime/libmime.a
	$(BUILDGOX)
net.gox: net/libnet.a
	$(BUILDGOX)
netchan.gox: netchan/libnetchan.a
	$(BUILDGOX)
os.gox: os/libos.a
	$(BUILDGOX)
patch.gox: patch/libpatch.a
	$(BUILDGOX)
path.gox: path/libpath.a
	$(BUILDGOX)
rand.gox: rand/librand.a
	$(BUILDGOX)
reflect.gox: reflect/libreflect.a
	$(BUILDGOX)
regexp.gox: regexp/libregexp.a
	$(BUILDGOX)
rpc.gox: rpc/librpc.a
	$(BUILDGOX)
runtime.gox: runtime/libruntime.a
	$(BUILDGOX)
scanner.gox: scanner/libscanner.a
	$(BUILDGOX)
smtp.gox: smtp/libsmtp.a
	$(BUILDGOX)
sort.gox: sort/libsort.a
	$(BUILDGOX)
strconv.gox: strconv/libstrconv.a
	$(BUILDGOX)
strings.gox: strings/libstrings.a
	$(BUILDGOX)
sync.gox: sync/libsync.a
	$(BUILDGOX)
syslog.gox: syslog/libsyslog.a
	$(BUILDGOX)
syscall.gox: syscalls/libsyscall.a
	$(BUILDGOX)
tabwriter.gox: tabwriter/libtabwriter.a
	$(BUILDGOX)
template.gox: template/libtemplate.a
	$(BUILDGOX)
testing.gox: testing/libtesting.a
	$(BUILDGOX)
time.gox: time/libtime.a
	$(BUILDGOX)
try.gox: try/libtry.a
	$(BUILDGOX)
unicode.gox: unicode/libunicode.a
	$(BUILDGOX)
utf16.gox: utf16/libutf16.a
	$(BUILDGOX)
utf8.gox: utf8/libutf8.a
	$(BUILDGOX)
websocket.gox: websocket/libwebsocket.a
	$(BUILDGOX)
xml.gox: xml/libxml.a
	$(BUILDGOX)

archive/tar.gox: archive/libtar.a
	$(BUILDGOX)
archive/zip.gox: archive/libzip.a
	$(BUILDGOX)

compress/flate.gox: compress/libflate.a
	$(BUILDGOX)
compress/gzip.gox: compress/libgzip.a
	$(BUILDGOX)
compress/zlib.gox: compress/libzlib.a
	$(BUILDGOX)

container/heap.gox: container/libheap.a
	$(BUILDGOX)
container/list.gox: container/liblist.a
	$(BUILDGOX)
container/ring.gox: container/libring.a
	$(BUILDGOX)
container/vector.gox: container/libvector.a
	$(BUILDGOX)

crypto/aes.gox: crypto/libaes.a
	$(BUILDGOX)
crypto/block.gox: crypto/libblock.a
	$(BUILDGOX)
crypto/blowfish.gox: crypto/libblowfish.a
	$(BUILDGOX)
crypto/cast5.gox: crypto/libcast5.a
	$(BUILDGOX)
crypto/cipher.gox: crypto/libcipher.a
	$(BUILDGOX)
crypto/elliptic.gox: crypto/libelliptic.a
	$(BUILDGOX)
crypto/hmac.gox: crypto/libhmac.a
	$(BUILDGOX)
crypto/md4.gox: crypto/libmd4.a
	$(BUILDGOX)
crypto/md5.gox: crypto/libmd5.a
	$(BUILDGOX)
crypto/ocsp.gox: crypto/libocsp.a
	$(BUILDGOX)
crypto/rand.gox: crypto/librand.a
	$(BUILDGOX)
crypto/rc4.gox: crypto/librc4.a
	$(BUILDGOX)
crypto/ripemd160.gox: crypto/libripemd160.a
	$(BUILDGOX)
crypto/rsa.gox: crypto/librsa.a
	$(BUILDGOX)
crypto/sha1.gox: crypto/libsha1.a
	$(BUILDGOX)
crypto/sha256.gox: crypto/libsha256.a
	$(BUILDGOX)
crypto/sha512.gox: crypto/libsha512.a
	$(BUILDGOX)
crypto/subtle.gox: crypto/libsubtle.a
	$(BUILDGOX)
crypto/tls.gox: crypto/libtls.a
	$(BUILDGOX)
crypto/twofish.gox: crypto/libtwofish.a
	$(BUILDGOX)
crypto/x509.gox: crypto/libx509.a
	$(BUILDGOX)
crypto/xtea.gox: crypto/libxtea.a
	$(BUILDGOX)

crypto/openpgp/armor.gox: crypto/openpgp/libarmor.a
	$(BUILDGOX)
crypto/openpgp/error.gox: crypto/openpgp/liberror.a
	$(BUILDGOX)
crypto/openpgp/s2k.gox: crypto/openpgp/libs2k.a
	$(BUILDGOX)

debug/dwarf.gox: debug/libdwarf.a
	$(BUILDGOX)
debug/elf.gox: debug/libelf.a
	$(BUILDGOX)
debug/gosym.gox: debug/libgosym.a
	$(BUILDGOX)
debug/macho.gox: debug/libmacho.a
	$(BUILDGOX)
debug/pe.gox: debug/libpe.a
	$(BUILDGOX)
debug/proc.gox: debug/libproc.a
	$(BUILDGOX)

encoding/ascii85.gox: encoding/libascii85.a
	$(BUILDGOX)
encoding/base32.gox: encoding/libbase32.a
	$(BUILDGOX)
encoding/base64.gox: encoding/libbase64.a
	$(BUILDGOX)
encoding/binary.gox: encoding/libbinary.a
	$(BUILDGOX)
encoding/git85.gox: encoding/libgit85.a
	$(BUILDGOX)
encoding/hex.gox: encoding/libhex.a
	$(BUILDGOX)
encoding/line.gox: encoding/libline.a
	$(BUILDGOX)
encoding/pem.gox: encoding/libpem.a
	$(BUILDGOX)

exp/datafmt.gox: exp/libdatafmt.a
	$(BUILDGOX)
exp/draw.gox: exp/libdraw.a
	$(BUILDGOX)
exp/eval.gox: exp/libeval.a
	$(BUILDGOX)

go/ast.gox: go/libast.a
	$(BUILDGOX)
go/doc.gox: go/libdoc.a
	$(BUILDGOX)
go/parser.gox: go/libparser.a
	$(BUILDGOX)
go/printer.gox: go/libprinter.a
	$(BUILDGOX)
go/scanner.gox: go/libscanner.a
	$(BUILDGOX)
go/token.gox: go/libtoken.a
	$(BUILDGOX)
go/typechecker.gox: go/libtypechecker.a
	$(BUILDGOX)

hash/adler32.gox: hash/libadler32.a
	$(BUILDGOX)
hash/crc32.gox: hash/libcrc32.a
	$(BUILDGOX)
hash/crc64.gox: hash/libcrc64.a
	$(BUILDGOX)

http/pprof.gox: http/libpprof.a
	$(BUILDGOX)

image/jpeg.gox: image/libjpeg.a
	$(BUILDGOX)
image/png.gox: image/libpng.a
	$(BUILDGOX)

index/suffixarray.gox: index/libsuffixarray.a
	$(BUILDGOX)

io/ioutil.gox: io/libioutil.a
	$(BUILDGOX)

mime/multipart.gox: mime/libmultipart.a
	$(BUILDGOX)

net/dict.gox: net/libdict.a
	$(BUILDGOX)
net/textproto.gox: net/libtextproto.a
	$(BUILDGOX)

os/inotify.gox: os/libinotify.a
	$(BUILDGOX)
os/signal.gox: os/libsignal.a
	$(BUILDGOX)

rpc/jsonrpc.gox: rpc/libjsonrpc.a
	$(BUILDGOX)

runtime/debug.gox: runtime/libdebug.a
	$(BUILDGOX)
runtime/pprof.gox: runtime/libpprof.a
	$(BUILDGOX)

testing/iotest.gox: testing/libiotest.a
	$(BUILDGOX)
testing/quick.gox: testing/libquick.a
	$(BUILDGOX)
testing/script.gox: testing/libscript.a
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
	http/check \
	io/check \
	json/check \
	log/check \
	math/check \
	mime/check \
	net/check \
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
	syslog/check \
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
