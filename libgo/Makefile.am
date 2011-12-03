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
	-I $(srcdir)/../libgcc -I $(MULTIBUILDTOP)../../gcc/include

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
	crypto.gox \
	csv.gox \
	errors.gox \
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
	mail.gox \
	mime.gox \
	net.gox \
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
	unicode.gox \
	url.gox \
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
	crypto/bcrypt.gox \
	crypto/blowfish.gox \
	crypto/cast5.gox \
	crypto/cipher.gox \
	crypto/des.gox \
	crypto/dsa.gox \
	crypto/ecdsa.gox \
	crypto/elliptic.gox \
	crypto/hmac.gox \
	crypto/md4.gox \
	crypto/md5.gox \
	crypto/ocsp.gox \
	crypto/openpgp.gox \
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
	crypto/openpgp/elgamal.gox \
	crypto/openpgp/error.gox \
	crypto/openpgp/packet.gox \
	crypto/openpgp/s2k.gox

toolexeclibgocryptox509dir = $(toolexeclibgocryptodir)/x509

toolexeclibgocryptox509_DATA = \
	crypto/x509/pkix.gox

toolexeclibgodebugdir = $(toolexeclibgodir)/debug

toolexeclibgodebug_DATA = \
	debug/dwarf.gox \
	debug/elf.gox \
	debug/gosym.gox \
	debug/macho.gox \
	debug/pe.gox

toolexeclibgoencodingdir = $(toolexeclibgodir)/encoding

toolexeclibgoencoding_DATA = \
	encoding/ascii85.gox \
	encoding/base32.gox \
	encoding/base64.gox \
	encoding/binary.gox \
	encoding/git85.gox \
	encoding/hex.gox \
	encoding/pem.gox

if LIBGO_IS_LINUX
# exp_inotify_gox = exp/inotify.gox
exp_inotify_gox =
else
exp_inotify_gox =
endif

toolexeclibgoexpdir = $(toolexeclibgodir)/exp

toolexeclibgoexp_DATA = \
	exp/ebnf.gox \
	exp/gui.gox \
	$(exp_inotify_gox) \
	exp/norm.gox \
	exp/spdy.gox \
	exp/sql.gox \
	exp/ssh.gox \
	exp/terminal.gox \
	exp/types.gox

toolexeclibgoexpguidir = $(toolexeclibgoexpdir)/gui

toolexeclibgoexpgui_DATA = \
	exp/gui/x11.gox

toolexeclibgoexpsqldir = $(toolexeclibgoexpdir)/sql

toolexeclibgoexpsql_DATA = \
	exp/sql/driver.gox

toolexeclibgoexptemplatedir = $(toolexeclibgoexpdir)/template

toolexeclibgoexptemplate_DATA = \
	exp/template/html.gox

toolexeclibgogodir = $(toolexeclibgodir)/go

toolexeclibgogo_DATA = \
	go/ast.gox \
	go/build.gox \
	go/doc.gox \
	go/parser.gox \
	go/printer.gox \
	go/scanner.gox \
	go/token.gox

toolexeclibgohashdir = $(toolexeclibgodir)/hash

toolexeclibgohash_DATA = \
	hash/adler32.gox \
	hash/crc32.gox \
	hash/crc64.gox \
	hash/fnv.gox

toolexeclibgohttpdir = $(toolexeclibgodir)/http

toolexeclibgohttp_DATA = \
	http/cgi.gox \
	http/fcgi.gox \
	http/httptest.gox \
	http/pprof.gox

toolexeclibgoimagedir = $(toolexeclibgodir)/image

toolexeclibgoimage_DATA = \
	image/bmp.gox \
	image/color.gox \
	image/draw.gox \
	image/gif.gox \
	image/jpeg.gox \
	image/png.gox \
	image/tiff.gox \
	image/ycbcr.gox

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

toolexeclibgoolddir = $(toolexeclibgodir)/old

toolexeclibgoold_DATA = \
	old/netchan.gox \
	old/regexp.gox \
	old/template.gox

toolexeclibgoosdir = $(toolexeclibgodir)/os

toolexeclibgoos_DATA = \
	os/user.gox \
	os/signal.gox

toolexeclibgopathdir = $(toolexeclibgodir)/path

toolexeclibgopath_DATA = \
	path/filepath.gox

toolexeclibgoregexpdir = $(toolexeclibgodir)/regexp

toolexeclibgoregexp_DATA = \
	regexp/syntax.gox

toolexeclibgorpcdir = $(toolexeclibgodir)/rpc

toolexeclibgorpc_DATA = \
	rpc/jsonrpc.gox

toolexeclibgoruntimedir = $(toolexeclibgodir)/runtime

toolexeclibgoruntime_DATA = \
	runtime/debug.gox \
	runtime/pprof.gox

toolexeclibgotemplatedir = $(toolexeclibgodir)/template

toolexeclibgotemplate_DATA = \
	template/parse.gox

toolexeclibgosyncdir = $(toolexeclibgodir)/sync

toolexeclibgosync_DATA = \
	sync/atomic.gox

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

if LIBGO_IS_LINUX
runtime_lock_files = runtime/lock_futex.c runtime/thread-linux.c
else
runtime_lock_files = runtime/lock_sema.c runtime/thread-sema.c
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
	runtime/go-check-interface.c \
	runtime/go-construct-map.c \
	runtime/go-convert-interface.c \
	runtime/go-copy.c \
	runtime/go-defer.c \
	runtime/go-deferred-recover.c \
	runtime/go-eface-compare.c \
	runtime/go-eface-val-compare.c \
	runtime/go-getgoroot.c \
	runtime/go-int-array-to-string.c \
	runtime/go-int-to-string.c \
	runtime/go-interface-compare.c \
	runtime/go-interface-eface-compare.c \
	runtime/go-interface-val-compare.c \
	runtime/go-make-slice.c \
	runtime/go-map-delete.c \
	runtime/go-map-index.c \
	runtime/go-map-len.c \
	runtime/go-map-range.c \
	runtime/go-nanotime.c \
	runtime/go-new-map.c \
	runtime/go-new.c \
	runtime/go-panic.c \
	runtime/go-print.c \
	runtime/go-recover.c \
	runtime/go-reflect.c \
	runtime/go-reflect-call.c \
	runtime/go-reflect-map.c \
	runtime/go-rune.c \
	runtime/go-runtime-error.c \
	runtime/go-setenv.c \
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
	runtime/chan.c \
	runtime/cpuprof.c \
	$(runtime_lock_files) \
	runtime/mcache.c \
	runtime/mcentral.c \
	$(runtime_mem_file) \
	runtime/mfinal.c \
	runtime/mfixalloc.c \
	runtime/mgc0.c \
	runtime/mheap.c \
	runtime/msize.c \
	runtime/proc.c \
	runtime/runtime.c \
	runtime/thread.c \
	runtime/yield.c \
	$(rtems_task_variable_add_file) \
	iface.c \
	malloc.c \
	map.c \
	mprof.c \
	reflect.c \
	runtime1.c \
	sema.c \
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

runtime1.c: $(srcdir)/runtime/runtime1.goc goc2c
	./goc2c --gcc --go-prefix libgo_runtime $< > $@.tmp
	mv -f $@.tmp $@

sema.c: $(srcdir)/runtime/sema.goc goc2c
	./goc2c --gcc --go-prefix libgo_runtime $< > $@.tmp
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

go_crypto_files = \
	go/crypto/crypto.go

go_csv_files = \
	go/csv/reader.go \
	go/csv/writer.go

go_errors_files = \
	go/errors/errors.go

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
	go/html/const.go \
	go/html/doc.go \
	go/html/entity.go \
	go/html/escape.go \
	go/html/node.go \
	go/html/parse.go \
	go/html/render.go \
	go/html/token.go

go_http_files = \
	go/http/chunked.go \
	go/http/client.go \
	go/http/cookie.go \
	go/http/dump.go \
	go/http/filetransport.go \
	go/http/fs.go \
	go/http/header.go \
	go/http/lex.go \
	go/http/persist.go \
	go/http/request.go \
	go/http/response.go \
	go/http/reverseproxy.go \
	go/http/server.go \
	go/http/sniff.go \
	go/http/status.go \
	go/http/transfer.go \
	go/http/transport.go

go_image_files = \
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
	go/json/stream.go \
	go/json/tags.go

go_log_files = \
	go/log/log.go

go_math_files = \
	go/math/abs.go \
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
	go/math/dim.go \
	go/math/erf.go \
	go/math/exp.go \
	go/math/exp_port.go \
	go/math/exp2.go \
	go/math/expm1.go \
	go/math/floor.go \
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
	go/math/mod.go \
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

go_mail_files = \
	go/mail/message.go

go_mime_files = \
	go/mime/grammar.go \
	go/mime/mediatype.go \
	go/mime/type.go

if LIBGO_IS_RTEMS
go_net_fd_os_file = go/net/fd_select.go
go_net_newpollserver_file = go/net/newpollserver_rtems.go
else # !LIBGO_IS_RTEMS
if LIBGO_IS_LINUX
go_net_fd_os_file = go/net/fd_linux.go
go_net_newpollserver_file = go/net/newpollserver.go
else # !LIBGO_IS_LINUX && !LIBGO_IS_RTEMS
# By default use select with pipes.  Most systems should have
# something better.
go_net_fd_os_file = go/net/fd_select.go
go_net_newpollserver_file = go/net/newpollserver.go
endif # !LIBGO_IS_LINUX
endif # !LIBGO_IS_RTEMS

if LIBGO_IS_LINUX
go_net_cgo_file = go/net/cgo_linux.go
go_net_sock_file = go/net/sock_linux.go
else
if LIBGO_IS_IRIX
go_net_cgo_file = go/net/cgo_linux.go
go_net_sock_file = go/net/sock_linux.go
else
if LIBGO_IS_SOLARIS
go_net_cgo_file = go/net/cgo_linux.go
go_net_sock_file = go/net/sock_linux.go
else
go_net_cgo_file = go/net/cgo_bsd.go
go_net_sock_file = go/net/sock_bsd.go
endif
endif
endif

if LIBGO_IS_LINUX
go_net_sendfile_file = go/net/sendfile_linux.go
else
go_net_sendfile_file = go/net/sendfile_stub.go
endif

if LIBGO_IS_LINUX
go_net_interface_file = go/net/interface_linux.go
else
go_net_interface_file = go/net/interface_stub.go
endif

go_net_files = \
	go/net/cgo_unix.go \
	$(go_net_cgo_file) \
	go/net/dial.go \
	go/net/dnsclient.go \
	go/net/dnsclient_unix.go \
	go/net/dnsconfig.go \
	go/net/dnsmsg.go \
	$(go_net_newpollserver_file) \
	go/net/fd.go \
	$(go_net_fd_os_file) \
	go/net/file.go \
	go/net/hosts.go \
	go/net/interface.go \
	$(go_net_interface_file) \
	go/net/ip.go \
	go/net/iprawsock.go \
	go/net/iprawsock_posix.go \
	go/net/ipsock.go \
	go/net/ipsock_posix.go \
	go/net/lookup_unix.go \
	go/net/net.go \
	go/net/parse.go \
	go/net/pipe.go \
	go/net/port.go \
	$(go_net_sendfile_file) \
	go/net/sock.go \
	$(go_net_sock_file) \
	go/net/tcpsock.go \
	go/net/tcpsock_posix.go \
	go/net/udpsock.go \
	go/net/udpsock_posix.go \
	go/net/unixsock.go \
	go/net/unixsock_posix.go

if LIBGO_IS_SOLARIS
if LIBGO_IS_386
go_os_dir_file = go/os/dir_largefile.go
else
if LIBGO_IS_SPARC
go_os_dir_file = go/os/dir_largefile.go
else
go_os_dir_file = go/os/dir_regfile.go
endif
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
if LIBGO_IS_IRIX
go_os_sys_file = go/os/sys_uname.go
else
if LIBGO_IS_RTEMS
go_os_sys_file = go/os/sys_uname.go
else
go_os_sys_file = go/os/sys_bsd.go
endif
endif
endif
endif

go_os_files = \
	$(go_os_dir_file) \
	go/os/dir.go \
	go/os/env.go \
	go/os/env_unix.go \
	go/os/error.go \
	go/os/error_posix.go \
	go/os/exec.go \
	go/os/exec_posix.go \
	go/os/exec_unix.go \
	go/os/file.go \
	go/os/file_posix.go \
	go/os/file_unix.go \
	go/os/getwd.go \
	go/os/path.go \
	go/os/path_unix.go \
	go/os/proc.go \
	go/os/stat.go \
	go/os/str.go \
	$(go_os_sys_file) \
	go/os/time.go \
	go/os/types.go \
	signal_unix.go

go_patch_files = \
	go/patch/apply.go \
	go/patch/git.go \
	go/patch/patch.go \
	go/patch/textdiff.go

go_path_files = \
	go/path/match.go \
	go/path/path.go

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
	go/regexp/exec.go \
	go/regexp/regexp.go

go_rpc_files = \
	go/rpc/client.go \
	go/rpc/debug.go \
	go/rpc/server.go

go_runtime_files = \
	go/runtime/debug.go \
	go/runtime/error.go \
	go/runtime/extern.go \
	go/runtime/mem.go \
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
	go/strings/replace.go \
	go/strings/strings.go

go_sync_files = \
	go/sync/cond.go \
	go/sync/mutex.go \
	go/sync/once.go \
	go/sync/rwmutex.go \
	go/sync/waitgroup.go

if LIBGO_IS_SOLARIS
go_syslog_file = go/syslog/syslog_libc.go
else
if LIBGO_IS_IRIX
go_syslog_file = go/syslog/syslog_libc.go
else
go_syslog_file = go/syslog/syslog_unix.go
endif
endif

go_syslog_files = \
	go/syslog/syslog.go \
	$(go_syslog_file)
go_syslog_c_files = \
	go/syslog/syslog_c.c

go_tabwriter_files = \
	go/tabwriter/tabwriter.go

go_template_files = \
	go/template/doc.go \
	go/template/exec.go \
	go/template/funcs.go \
	go/template/helper.go \
	go/template/parse.go \
	go/template/set.go

go_testing_files = \
	go/testing/benchmark.go \
	go/testing/example.go \
	go/testing/testing.go

go_time_files = \
	go/time/format.go \
	go/time/sleep.go \
	go/time/sys.go \
	go/time/sys_unix.go \
	go/time/tick.go \
	go/time/time.go \
	go/time/zoneinfo_posix.go \
	go/time/zoneinfo_unix.go

go_unicode_files = \
	go/unicode/casetables.go \
	go/unicode/digit.go \
	go/unicode/graphic.go \
	go/unicode/letter.go \
	go/unicode/tables.go

go_url_files = \
	go/url/url.go

go_utf16_files = \
	go/utf16/utf16.go

go_utf8_files = \
	go/utf8/string.go \
	go/utf8/utf8.go

go_websocket_files = \
	go/websocket/client.go \
	go/websocket/hixie.go \
	go/websocket/hybi.go \
	go/websocket/server.go \
	go/websocket/websocket.go

go_xml_files = \
	go/xml/marshal.go \
	go/xml/read.go \
	go/xml/xml.go

go_archive_tar_files = \
	go/archive/tar/common.go \
	go/archive/tar/reader.go \
	go/archive/tar/writer.go

go_archive_zip_files = \
	go/archive/zip/reader.go \
	go/archive/zip/struct.go \
	go/archive/zip/writer.go

go_compress_bzip2_files = \
	go/compress/bzip2/bit_reader.go \
	go/compress/bzip2/bzip2.go \
	go/compress/bzip2/huffman.go \
	go/compress/bzip2/move_to_front.go

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

go_compress_lzw_files = \
	go/compress/lzw/reader.go \
	go/compress/lzw/writer.go

go_compress_zlib_files = \
	go/compress/zlib/reader.go \
	go/compress/zlib/writer.go

go_container_heap_files = \
	go/container/heap/heap.go

go_container_list_files = \
	go/container/list/list.go

go_container_ring_files = \
	go/container/ring/ring.go

go_crypto_aes_files = \
	go/crypto/aes/block.go \
	go/crypto/aes/cipher.go \
	go/crypto/aes/const.go
go_crypto_bcrypt_files = \
	go/crypto/bcrypt/base64.go \
	go/crypto/bcrypt/bcrypt.go
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
go_crypto_des_files = \
	go/crypto/des/block.go \
	go/crypto/des/cipher.go \
	go/crypto/des/const.go
go_crypto_dsa_files = \
	go/crypto/dsa/dsa.go
go_crypto_ecdsa_files = \
	go/crypto/ecdsa/ecdsa.go
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
go_crypto_openpgp_files = \
	go/crypto/openpgp/canonical_text.go \
	go/crypto/openpgp/keys.go \
	go/crypto/openpgp/read.go \
	go/crypto/openpgp/write.go
go_crypto_rand_files = \
	go/crypto/rand/rand.go \
	go/crypto/rand/rand_unix.go \
	go/crypto/rand/util.go
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
	go/crypto/tls/cipher_suites.go \
	go/crypto/tls/common.go \
	go/crypto/tls/conn.go \
	go/crypto/tls/handshake_client.go \
	go/crypto/tls/handshake_messages.go \
	go/crypto/tls/handshake_server.go \
	go/crypto/tls/key_agreement.go \
	go/crypto/tls/prf.go \
	go/crypto/tls/root_unix.go \
	go/crypto/tls/tls.go
go_crypto_twofish_files = \
	go/crypto/twofish/twofish.go
go_crypto_x509_files = \
	go/crypto/x509/cert_pool.go \
	go/crypto/x509/pkcs1.go \
	go/crypto/x509/verify.go \
	go/crypto/x509/x509.go
go_crypto_xtea_files = \
	go/crypto/xtea/block.go \
	go/crypto/xtea/cipher.go

go_crypto_openpgp_armor_files = \
	go/crypto/openpgp/armor/armor.go \
	go/crypto/openpgp/armor/encode.go
go_crypto_openpgp_elgamal_files = \
	go/crypto/openpgp/elgamal/elgamal.go
go_crypto_openpgp_error_files = \
	go/crypto/openpgp/error/error.go
go_crypto_openpgp_packet_files = \
	go/crypto/openpgp/packet/compressed.go \
	go/crypto/openpgp/packet/encrypted_key.go \
	go/crypto/openpgp/packet/literal.go \
	go/crypto/openpgp/packet/one_pass_signature.go \
	go/crypto/openpgp/packet/packet.go \
	go/crypto/openpgp/packet/private_key.go \
	go/crypto/openpgp/packet/public_key.go \
	go/crypto/openpgp/packet/reader.go \
	go/crypto/openpgp/packet/signature.go \
	go/crypto/openpgp/packet/symmetric_key_encrypted.go \
	go/crypto/openpgp/packet/symmetrically_encrypted.go \
	go/crypto/openpgp/packet/userid.go
go_crypto_openpgp_s2k_files = \
	go/crypto/openpgp/s2k/s2k.go

go_crypto_x509_pkix_files = \
	go/crypto/x509/pkix/pkix.go

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

go_encoding_ascii85_files = \
	go/encoding/ascii85/ascii85.go
go_encoding_base32_files = \
	go/encoding/base32/base32.go
go_encoding_base64_files = \
	go/encoding/base64/base64.go
go_encoding_binary_files = \
	go/encoding/binary/binary.go \
	go/encoding/binary/varint.go
go_encoding_git85_files = \
	go/encoding/git85/git.go
go_encoding_hex_files = \
	go/encoding/hex/hex.go
go_encoding_pem_files = \
	go/encoding/pem/pem.go

go_exp_ebnf_files = \
	go/exp/ebnf/ebnf.go \
	go/exp/ebnf/parser.go
go_exp_gui_files = \
	go/exp/gui/gui.go
go_exp_inotify_files = \
	go/exp/inotify/inotify_linux.go
go_exp_norm_files = \
	go/exp/norm/composition.go \
	go/exp/norm/forminfo.go \
	go/exp/norm/input.go \
	go/exp/norm/normalize.go \
	go/exp/norm/readwriter.go \
	go/exp/norm/tables.go \
	go/exp/norm/trie.go
go_exp_spdy_files = \
	go/exp/spdy/read.go \
	go/exp/spdy/types.go \
	go/exp/spdy/write.go
go_exp_sql_files = \
	go/exp/sql/convert.go \
	go/exp/sql/sql.go
go_exp_ssh_files = \
	go/exp/ssh/channel.go \
	go/exp/ssh/client.go \
	go/exp/ssh/common.go \
	go/exp/ssh/doc.go \
	go/exp/ssh/messages.go \
	go/exp/ssh/server.go \
	go/exp/ssh/server_shell.go \
	go/exp/ssh/session.go \
	go/exp/ssh/transport.go
go_exp_terminal_files = \
	go/exp/terminal/shell.go \
	go/exp/terminal/terminal.go
go_exp_types_files = \
	go/exp/types/check.go \
	go/exp/types/const.go \
	go/exp/types/exportdata.go \
	go/exp/types/gcimporter.go \
	go/exp/types/types.go \
	go/exp/types/universe.go

go_exp_gui_x11_files = \
	go/exp/gui/x11/auth.go \
	go/exp/gui/x11/conn.go

go_exp_sql_driver_files = \
	go/exp/sql/driver/driver.go \
	go/exp/sql/driver/types.go

go_exp_template_html_files = \
	go/exp/template/html/attr.go \
	go/exp/template/html/clone.go \
	go/exp/template/html/content.go \
	go/exp/template/html/context.go \
	go/exp/template/html/css.go \
	go/exp/template/html/doc.go \
	go/exp/template/html/error.go \
	go/exp/template/html/escape.go \
	go/exp/template/html/html.go \
	go/exp/template/html/js.go \
	go/exp/template/html/transition.go \
	go/exp/template/html/url.go

go_go_ast_files = \
	go/go/ast/ast.go \
	go/go/ast/filter.go \
	go/go/ast/print.go \
	go/go/ast/resolve.go \
	go/go/ast/scope.go \
	go/go/ast/walk.go
go_go_build_files = \
	go/go/build/build.go \
	go/go/build/dir.go \
	go/go/build/path.go \
	syslist.go
go_go_doc_files = \
	go/go/doc/comment.go \
	go/go/doc/doc.go \
	go/go/doc/example.go
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
	go/go/token/serialize.go \
	go/go/token/token.go

go_hash_adler32_files = \
	go/hash/adler32/adler32.go
go_hash_crc32_files = \
	go/hash/crc32/crc32.go \
	go/hash/crc32/crc32_generic.go
go_hash_crc64_files = \
	go/hash/crc64/crc64.go
go_hash_fnv_files = \
	go/hash/fnv/fnv.go

go_http_cgi_files = \
	go/http/cgi/child.go \
	go/http/cgi/host.go
go_http_fcgi_files = \
	go/http/fcgi/child.go \
	go/http/fcgi/fcgi.go
go_http_httptest_files = \
	go/http/httptest/recorder.go \
	go/http/httptest/server.go
go_http_pprof_files = \
	go/http/pprof/pprof.go

go_image_bmp_files = \
	go/image/bmp/reader.go

go_image_color_files = \
	go/image/color/color.go

go_image_draw_files = \
	go/image/draw/draw.go

go_image_gif_files = \
	go/image/gif/reader.go

go_image_jpeg_files = \
	go/image/jpeg/fdct.go \
	go/image/jpeg/huffman.go \
	go/image/jpeg/idct.go \
	go/image/jpeg/reader.go \
	go/image/jpeg/writer.go

go_image_png_files = \
	go/image/png/reader.go \
	go/image/png/writer.go

go_image_tiff_files = \
	go/image/tiff/buffer.go \
	go/image/tiff/compress.go \
	go/image/tiff/consts.go \
	go/image/tiff/reader.go

go_image_ycbcr_files = \
	go/image/ycbcr/ycbcr.go

go_index_suffixarray_files = \
	go/index/suffixarray/qsufsort.go \
	go/index/suffixarray/suffixarray.go

go_io_ioutil_files = \
	go/io/ioutil/ioutil.go \
	go/io/ioutil/tempfile.go

go_mime_multipart_files = \
	go/mime/multipart/formdata.go \
	go/mime/multipart/multipart.go \
	go/mime/multipart/writer.go

go_net_dict_files = \
	go/net/dict/dict.go

go_net_textproto_files = \
	go/net/textproto/header.go \
	go/net/textproto/pipeline.go \
	go/net/textproto/reader.go \
	go/net/textproto/textproto.go \
	go/net/textproto/writer.go

go_old_netchan_files = \
	go/old/netchan/common.go \
	go/old/netchan/export.go \
	go/old/netchan/import.go
go_old_regexp_files = \
	go/old/regexp/regexp.go
go_old_template_files = \
	go/old/template/doc.go \
	go/old/template/execute.go \
	go/old/template/format.go \
	go/old/template/parse.go

go_os_user_files = \
	go/os/user/user.go \
	go/os/user/lookup_unix.go

go_os_signal_files = \
	go/os/signal/signal.go

go_path_filepath_files = \
	go/path/filepath/match.go \
	go/path/filepath/path.go \
	go/path/filepath/path_unix.go

go_regexp_syntax_files = \
	go/regexp/syntax/compile.go \
	go/regexp/syntax/parse.go \
	go/regexp/syntax/perl_groups.go \
	go/regexp/syntax/prog.go \
	go/regexp/syntax/regexp.go \
	go/regexp/syntax/simplify.go

go_rpc_jsonrpc_files = \
	go/rpc/jsonrpc/client.go \
	go/rpc/jsonrpc/server.go

go_runtime_debug_files = \
	go/runtime/debug/stack.go
go_runtime_pprof_files = \
	go/runtime/pprof/pprof.go

go_template_parse_files = \
	go/template/parse/lex.go \
	go/template/parse/node.go \
	go/template/parse/parse.go \
	go/template/parse/set.go

go_sync_atomic_files = \
	go/sync/atomic/doc.go
go_sync_atomic_c_files = \
	go/sync/atomic/atomic.c

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
syscall_syscall_file = go/syscall/syscall_stubs.go
else
syscall_syscall_file = go/syscall/syscall_unix.go
endif

# Define ForkExec and Exec.
if LIBGO_IS_RTEMS
syscall_exec_file = go/syscall/exec_stubs.go
else
syscall_exec_file = go/syscall/exec_unix.go
endif

# Define Wait4.
if HAVE_WAIT4
syscall_wait_file = go/syscall/libcall_wait4.go
else
syscall_wait_file = go/syscall/libcall_waitpid.go
endif

# Define Sleep.
if LIBGO_IS_RTEMS
syscall_sleep_file = go/syscall/sleep_rtems.go
else
syscall_sleep_file = go/syscall/sleep_select.go
endif

# Define Errstr.
if LIBGO_IS_LINUX
syscall_errstr_file = go/syscall/errstr_linux.go
else
if LIBGO_IS_RTEMS
syscall_errstr_file = go/syscall/errstr_linux.go
else
if HAVE_STRERROR_R
syscall_errstr_file = go/syscall/errstr.go
else
syscall_errstr_file = go/syscall/errstr_nor.go
endif
endif
endif

# Declare libc functions that vary for largefile systems.
if LIBGO_IS_LINUX
# Always use lseek64 on GNU/Linux.
syscall_size_file = go/syscall/libcall_posix_largefile.go
else # !LIBGO_IS_LINUX
if LIBGO_IS_SOLARIS
if LIBGO_IS_386
# Use lseek64 on 32-bit Solaris/x86.
syscall_size_file = go/syscall/libcall_posix_largefile.go
else # !LIBGO_IS_386
if LIBGO_IS_SPARC
# Use lseek64 on 32-bit Solaris/SPARC.
syscall_size_file = go/syscall/libcall_posix_largefile.go
else # !LIBGO_IS_386 && !LIBGO_IS_SPARC
# Use lseek on 64-bit Solaris.
syscall_size_file = go/syscall/libcall_posix_regfile.go
endif # !LIBGO_IS_386 && !LIBGO_IS_SPARC
endif # !LIBGO_IS_SOLARIS
else # !LIBGO_IS_LINUX && !LIBGO_IS_SOLARIS
# Use lseek by default.
syscall_size_file = go/syscall/libcall_posix_regfile.go
endif # !LIBGO_IS_SOLARIS
endif # !LIBGO_IS_LINUX

# Define socket sizes and types.
if LIBGO_IS_LINUX
syscall_socket_file = go/syscall/socket_linux.go epoll.go
else
if LIBGO_IS_SOLARIS
syscall_socket_file = go/syscall/socket_solaris.go
else
if LIBGO_IS_IRIX
syscall_socket_file = go/syscall/socket_irix.go
else
syscall_socket_file = go/syscall/socket_bsd.go
endif
endif
endif

# Support for uname.
if LIBGO_IS_SOLARIS
if LIBGO_IS_386
# 32-bit Solaris 2/x86 needs _nuname, handled in libcall_solaris_386.go.
syscall_uname_file =
else # !LIBGO_IS_386 && LIBGO_IS_SOLARIS
syscall_uname_file = go/syscall/libcall_uname.go
endif
else # !LIBGO_IS_SOLARIS
syscall_uname_file = go/syscall/libcall_uname.go
endif

# Support for netlink sockets and messages.
if LIBGO_IS_LINUX
syscall_netlink_file = go/syscall/netlink_linux.go
else
syscall_netlink_file =
endif

go_base_syscall_files = \
	go/syscall/libcall_support.go \
	go/syscall/libcall_posix.go \
	go/syscall/socket.go \
	go/syscall/str.go \
	go/syscall/syscall.go \
	$(syscall_syscall_file) \
	$(syscall_exec_file) \
	$(syscall_wait_file) \
	$(syscall_sleep_file) \
	$(syscall_errstr_file) \
	$(syscall_size_file) \
	$(syscall_socket_file) \
	$(syscall_uname_file) \
	$(syscall_netlink_file) \
	$(GO_LIBCALL_OS_FILE) \
	$(GO_LIBCALL_OS_ARCH_FILE) \
	$(GO_SYSCALL_OS_FILE) \
	$(GO_SYSCALL_OS_ARCH_FILE)

go_syscall_files = \
	$(go_base_syscall_files) \
	libcalls.go \
	sysinfo.go \
	syscall_arch.go
go_syscall_c_files = \
	go/syscall/errno.c \
	go/syscall/wait.c

libcalls.go: s-libcalls; @true
s-libcalls: Makefile go/syscall/mksyscall.awk $(go_base_syscall_files)
	rm -f libcalls.go.tmp
	files=`echo $^ | sed -e 's/Makefile//' -e 's|[^ ]*go/syscall/mksyscall.awk||'`; \
	$(AWK) -f $(srcdir)/go/syscall/mksyscall.awk $${files} > libcalls.go.tmp
	$(SHELL) $(srcdir)/../move-if-change libcalls.go.tmp libcalls.go
	$(STAMP) $@

syscall_arch.go: s-syscall_arch; @true
s-syscall_arch: Makefile
	rm -f syscall_arch.go.tmp
	echo "package syscall" > syscall_arch.go.tmp
	echo 'const ARCH = "'$(GOARCH)'"' >> syscall_arch.go.tmp
	echo 'const OS = "'$(GOOS)'"' >> syscall_arch.go.tmp
	$(SHELL) $(srcdir)/../move-if-change syscall_arch.go.tmp syscall_arch.go
	$(STAMP) $@

sysinfo.go: s-sysinfo; @true
s-sysinfo: $(srcdir)/mksysinfo.sh config.h
	CC="$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(OSCFLAGS)" $(SHELL) $(srcdir)/mksysinfo.sh
	$(SHELL) $(srcdir)/../move-if-change tmp-sysinfo.go sysinfo.go
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
	$(SHELL) $(srcdir)/../move-if-change epoll.go.tmp epoll.go
	$(STAMP) $@

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
	crypto/crypto.lo \
	csv/csv.lo \
	errors/errors.lo \
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
	mail/mail.lo \
	mime/mime.lo \
	net/net.lo \
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
	sync/sync.lo \
	syslog/syslog.lo \
	syslog/syslog_c.lo \
	tabwriter/tabwriter.lo \
	template/template.lo \
	time/time.lo \
	unicode/unicode.lo \
	url/url.lo \
	utf16/utf16.lo \
	utf8/utf8.lo \
	websocket/websocket.lo \
	xml/xml.lo \
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
	crypto/bcrypt.lo \
	crypto/blowfish.lo \
	crypto/cast5.lo \
	crypto/cipher.lo \
	crypto/des.lo \
	crypto/dsa.lo \
	crypto/ecdsa.lo \
	crypto/elliptic.lo \
	crypto/hmac.lo \
	crypto/md4.lo \
	crypto/md5.lo \
	crypto/ocsp.lo \
	crypto/openpgp.lo \
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
	crypto/openpgp/elgamal.lo \
	crypto/openpgp/error.lo \
	crypto/openpgp/packet.lo \
	crypto/openpgp/s2k.lo \
	crypto/x509/pkix.lo \
	debug/dwarf.lo \
	debug/elf.lo \
	debug/gosym.lo \
	debug/macho.lo \
	debug/pe.lo \
	encoding/ascii85.lo \
	encoding/base32.lo \
	encoding/base64.lo \
	encoding/binary.lo \
	encoding/git85.lo \
	encoding/hex.lo \
	encoding/pem.lo \
	exp/ebnf.lo \
	exp/gui.lo \
	exp/norm.lo \
	exp/spdy.lo \
	exp/sql.lo \
	exp/ssh.lo \
	exp/terminal.lo \
	exp/types.lo \
	exp/gui/x11.lo \
	exp/sql/driver.lo \
	exp/template/html.lo \
	go/ast.lo \
	go/build.lo \
	go/doc.lo \
	go/parser.lo \
	go/printer.lo \
	go/scanner.lo \
	go/token.lo \
	hash/adler32.lo \
	hash/crc32.lo \
	hash/crc64.lo \
	hash/fnv.lo \
	http/cgi.lo \
	http/fcgi.lo \
	http/httptest.lo \
	http/pprof.lo \
	image/bmp.lo \
	image/color.lo \
	image/draw.lo \
	image/gif.lo \
	image/jpeg.lo \
	image/png.lo \
	image/tiff.lo \
	image/ycbcr.lo \
	index/suffixarray.lo \
	io/ioutil.lo \
	mime/multipart.lo \
	net/dict.lo \
	net/textproto.lo \
	old/netchan.lo \
	old/regexp.lo \
	old/template.lo \
	$(os_lib_inotify_lo) \
	os/user.lo \
	os/signal.lo \
	path/filepath.lo \
	regexp/syntax.lo \
	rpc/jsonrpc.lo \
	runtime/debug.lo \
	runtime/pprof.lo \
	sync/atomic.lo \
	sync/atomic_c.lo \
	syscall/syscall.lo \
	syscall/errno.lo \
	syscall/wait.lo \
	template/parse.lo \
	testing/testing.lo \
	testing/iotest.lo \
	testing/quick.lo \
	testing/script.lo

libgo_la_SOURCES = $(runtime_files)

libgo_la_LDFLAGS = $(PTHREAD_CFLAGS) $(AM_LDFLAGS)

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

# Build the dependencies for a Go package.
BUILDDEPS = \
	$(MKDIR_P) $(@D); \
	$(SHELL) $(srcdir)/godeps.sh `echo $@ | sed -e 's/.dep$$//'` $^ > $@.tmp; \
	mv -f $@.tmp $@

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

GOTESTFLAGS =

# Check a package.
CHECK = \
	GC="$(GOC) $(GOCFLAGS) -L `${PWD_COMMAND}` -L `${PWD_COMMAND}`/.libs"; \
	export GC; \
	GOLIBS="$(MATH_LIBS) $(NET_LIBS)"; \
	export GOLIBS; \
	RUNTESTFLAGS="$(RUNTESTFLAGS)"; \
	export RUNTESTFLAGS; \
	MAKE="$(MAKE)"; \
	export MAKE; \
	libgccdir=`${GOC} -print-libgcc-file-name | sed -e 's|/[^/]*$$||'`; \
	LD_LIBRARY_PATH="`${PWD_COMMAND}`/.libs:$${libgccdir}:${LD_LIBRARY_PATH}"; \
	LD_LIBRARY_PATH=`echo $${LD_LIBRARY_PATH} | sed 's,::*,:,g;s,^:*,,;s,:*$$,,'`; \
	export LD_LIBRARY_PATH; \
	rm -f $@-testsum $@-testlog; \
	prefix=`if test "$(@D)" = "regexp"; then echo regexp-test; else dirname $(@D); fi`; \
	test "$${prefix}" != "." || prefix="$(@D)"; \
	if test "$(use_dejagnu)" = "yes"; then \
	  $(SHELL) $(srcdir)/testsuite/gotest --dejagnu=yes --basedir=$(srcdir) --srcdir=$(srcdir)/go/$(@D) --prefix="libgo_$${prefix}" --pkgfiles="$(go_$(subst /,_,$(@D))_files)" --testname="$(@D)" $(GOTESTFLAGS); \
	else \
	  if $(SHELL) $(srcdir)/testsuite/gotest --basedir=$(srcdir) --srcdir=$(srcdir)/go/$(@D) --prefix="libgo_$${prefix}" --pkgfiles="$(go_$(subst /,_,$(@D))_files)" $(GOTESTFLAGS) >>$@-testlog 2>&1; then \
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
CHECK_DEPS = libgo.la libgobegin.a \
	$(toolexeclibgo_DATA) \
	$(toolexeclibgoarchive_DATA) \
	$(toolexeclibgocompress_DATA) \
	$(toolexeclibgocontainer_DATA) \
	$(toolexeclibgocrypto_DATA) \
	$(toolexeclibgocryptoopenpgp_DATA) \
	$(toolexeclibgodebug_DATA) \
	$(toolexeclibgoencoding_DATA) \
	$(toolexeclibgoexp_DATA) \
	$(toolexeclibgogo_DATA) \
	$(toolexeclibgohash_DATA) \
	$(toolexeclibgohttp_DATA) \
	$(toolexeclibgoimage_DATA) \
	$(toolexeclibgoindex_DATA) \
	$(toolexeclibgoio_DATA) \
	$(toolexeclibgomime_DATA) \
	$(toolexeclibgonet_DATA) \
	$(toolexeclibgoos_DATA) \
	$(toolexeclibgopath_DATA) \
	$(toolexeclibgorpc_DATA) \
	$(toolexeclibgoruntime_DATA) \
	$(toolexeclibgosync_DATA) \
	$(toolexeclibgotesting_DATA)

@go_include@ asn1/asn1.lo.dep
asn1/asn1.lo.dep: $(go_asn1_files)
	$(BUILDDEPS)
asn1/asn1.lo: $(go_asn1_files)
	$(BUILDPACKAGE)
asn1/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: asn1/check

@go_include@ big/big.lo.dep
big/big.lo.dep: $(go_big_files)
	$(BUILDDEPS)
big/big.lo: $(go_big_files)
	$(BUILDPACKAGE)
big/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: big/check

@go_include@ bufio/bufio.lo.dep
bufio/bufio.lo.dep: $(go_bufio_files)
	$(BUILDDEPS)
bufio/bufio.lo: $(go_bufio_files)
	$(BUILDPACKAGE)
bufio/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: bufio/check

@go_include@ bytes/bytes.lo.dep
bytes/bytes.lo.dep: $(go_bytes_files)
	$(BUILDDEPS)
bytes/bytes.lo: $(go_bytes_files)
	$(BUILDPACKAGE)
bytes/index.lo: $(go_bytes_c_files) bytes/bytes.lo
	$(LTCOMPILE) -c -o bytes/index.lo $(srcdir)/go/bytes/indexbyte.c
bytes/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: bytes/check

@go_include@ cmath/cmath.lo.dep
cmath/cmath.lo.dep: $(go_cmath_files)
	$(BUILDDEPS)
cmath/cmath.lo: $(go_cmath_files)
	$(BUILDPACKAGE)
cmath/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: cmath/check

@go_include@ crypto/crypto.lo.dep
crypto/crypto.lo.dep: $(go_crypto_files)
	$(BUILDDEPS)
crypto/crypto.lo: $(go_crypto_files)
	$(BUILDPACKAGE)
crypto/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: crypto/check

@go_include@ csv/csv.lo.dep
csv/csv.lo.dep: $(go_csv_files)
	$(BUILDDEPS)
csv/csv.lo: $(go_csv_files)
	$(BUILDPACKAGE)
csv/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: csv/check

@go_include@ errors/errors.lo.dep
errors/errors.lo.dep: $(go_errors_files)
	$(BUILDDEPS)
errors/errors.lo: $(go_errors_files)
	$(BUILDPACKAGE)
errors/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: errors/check

@go_include@ exec/exec.lo.dep
exec/exec.lo.dep: $(go_exec_files)
	$(BUILDDEPS)
exec/exec.lo: $(go_exec_files)
	$(BUILDPACKAGE)
exec/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: exec/check

@go_include@ expvar/expvar.lo.dep
expvar/expvar.lo.dep: $(go_expvar_files)
	$(BUILDDEPS)
expvar/expvar.lo: $(go_expvar_files)
	$(BUILDPACKAGE)
expvar/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: expvar/check

@go_include@ flag/flag.lo.dep
flag/flag.lo.dep: $(go_flag_files)
	$(BUILDDEPS)
flag/flag.lo: $(go_flag_files)
	$(BUILDPACKAGE)
flag/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: flag/check

@go_include@ fmt/fmt.lo.dep
fmt/fmt.lo.dep: $(go_fmt_files)
	$(BUILDDEPS)
fmt/fmt.lo: $(go_fmt_files)
	$(BUILDPACKAGE)
fmt/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: fmt/check

@go_include@ gob/gob.lo.dep
gob/gob.lo.dep: $(go_gob_files)
	$(BUILDDEPS)
gob/gob.lo: $(go_gob_files)
	$(BUILDPACKAGE)
gob/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: gob/check

@go_include@ hash/hash.lo.dep
hash/hash.lo.dep: $(go_hash_files)
	$(BUILDDEPS)
hash/hash.lo: $(go_hash_files)
	$(BUILDPACKAGE)
hash/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: hash/check

@go_include@ html/html.lo.dep
html/html.lo.dep: $(go_html_files)
	$(BUILDDEPS)
html/html.lo: $(go_html_files)
	$(BUILDPACKAGE)
html/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: html/check

@go_include@ http/http.lo.dep
http/http.lo.dep: $(go_http_files)
	$(BUILDDEPS)
http/http.lo: $(go_http_files)
	$(BUILDPACKAGE)
http/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: http/check

@go_include@ image/image.lo.dep
image/image.lo.dep: $(go_image_files)
	$(BUILDDEPS)
image/image.lo: $(go_image_files)
	$(BUILDPACKAGE)
image/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: image/check

@go_include@ io/io.lo.dep
io/io.lo.dep: $(go_io_files)
	$(BUILDDEPS)
io/io.lo: $(go_io_files)
	$(BUILDPACKAGE)
io/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: io/check

@go_include@ json/json.lo.dep
json/json.lo.dep: $(go_json_files)
	$(BUILDDEPS)
json/json.lo: $(go_json_files)
	$(BUILDPACKAGE)
json/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: json/check

@go_include@ log/log.lo.dep
log/log.lo.dep: $(go_log_files)
	$(BUILDDEPS)
log/log.lo: $(go_log_files)
	$(BUILDPACKAGE)
log/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: log/check

@go_include@ math/math.lo.dep
math/math.lo.dep: $(go_math_files)
	$(BUILDDEPS)
math/math.lo: $(go_math_files)
	$(BUILDPACKAGE)
math/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: math/check

@go_include@ mail/mail.lo.dep
mail/mail.lo.dep: $(go_mail_files)
	$(BUILDDEPS)
mail/mail.lo: $(go_mail_files)
	$(BUILDPACKAGE)
mail/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: mail/check

@go_include@ mime/mime.lo.dep
mime/mime.lo.dep: $(go_mime_files)
	$(BUILDDEPS)
mime/mime.lo: $(go_mime_files)
	$(BUILDPACKAGE)
mime/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: mime/check

@go_include@ net/net.lo.dep
net/net.lo.dep: $(go_net_files)
	$(BUILDDEPS)
net/net.lo: $(go_net_files)
	$(BUILDPACKAGE)
net/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: net/check

@go_include@ os/os.lo.dep
os/os.lo.dep: $(go_os_files)
	$(BUILDDEPS)
os/os.lo: $(go_os_files)
	$(BUILDPACKAGE)
os/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: os/check

signal_unix.go: $(srcdir)/go/os/mkunixsignals.sh sysinfo.go
	$(SHELL) $(srcdir)/go/os/mkunixsignals.sh sysinfo.go > $@.tmp
	mv -f $@.tmp $@

@go_include@ patch/patch.lo.dep
patch/patch.lo.dep: $(go_patch_files)
	$(BUILDDEPS)
patch/patch.lo: $(go_patch_files)
	$(BUILDPACKAGE)
patch/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: patch/check

@go_include@ path/path.lo.dep
path/path.lo.dep: $(go_path_files)
	$(BUILDDEPS)
path/path.lo: $(go_path_files)
	$(BUILDPACKAGE)
path/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: path/check

@go_include@ rand/rand.lo.dep
rand/rand.lo.dep: $(go_rand_files)
	$(BUILDDEPS)
rand/rand.lo: $(go_rand_files)
	$(BUILDPACKAGE)
rand/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: rand/check

@go_include@ reflect/reflect.lo.dep
reflect/reflect.lo.dep: $(go_reflect_files)
	$(BUILDDEPS)
reflect/reflect.lo: $(go_reflect_files)
	$(BUILDPACKAGE)
reflect/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: reflect/check

@go_include@ regexp/regexp.lo.dep
regexp/regexp.lo.dep: $(go_regexp_files)
	$(BUILDDEPS)
regexp/regexp.lo: $(go_regexp_files)
	$(BUILDPACKAGE)
regexp/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: regexp/check

@go_include@ rpc/rpc.lo.dep
rpc/rpc.lo.dep: $(go_rpc_files)
	$(BUILDDEPS)
rpc/rpc.lo: $(go_rpc_files)
	$(BUILDPACKAGE)
rpc/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: rpc/check

@go_include@ runtime/runtime.lo.dep
runtime/runtime.lo.dep: $(go_runtime_files)
	$(BUILDDEPS)
runtime/runtime.lo: $(go_runtime_files)
	$(BUILDPACKAGE)
runtime/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: runtime/check

@go_include@ scanner/scanner.lo.dep
scanner/scanner.lo.dep: $(go_scanner_files)
	$(BUILDDEPS)
scanner/scanner.lo: $(go_scanner_files)
	$(BUILDPACKAGE)
scanner/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: scanner/check

@go_include@ smtp/smtp.lo.dep
smtp/smtp.lo.dep: $(go_smtp_files)
	$(BUILDDEPS)
smtp/smtp.lo: $(go_smtp_files)
	$(BUILDPACKAGE)
smtp/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: smtp/check

@go_include@ sort/sort.lo.dep
sort/sort.lo.dep: $(go_sort_files)
	$(BUILDDEPS)
sort/sort.lo: $(go_sort_files)
	$(BUILDPACKAGE)
sort/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: sort/check

@go_include@ strconv/strconv.lo.dep
strconv/strconv.lo.dep: $(go_strconv_files)
	$(BUILDDEPS)
strconv/strconv.lo: $(go_strconv_files)
	$(BUILDPACKAGE)
strconv/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: strconv/check

@go_include@ strings/strings.lo.dep
strings/strings.lo.dep: $(go_strings_files)
	$(BUILDDEPS)
strings/strings.lo: $(go_strings_files)
	$(BUILDPACKAGE)
strings/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: strings/check

@go_include@ sync/sync.lo.dep
sync/sync.lo.dep: $(go_sync_files)
	$(BUILDDEPS)
sync/sync.lo: $(go_sync_files)
	$(BUILDPACKAGE)
sync/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: sync/check

@go_include@ syslog/syslog.lo.dep
syslog/syslog.lo.dep: $(go_syslog_files)
	$(BUILDDEPS)
syslog/syslog.lo: $(go_syslog_files)
	$(BUILDPACKAGE)
syslog/syslog_c.lo: $(go_syslog_c_files) syslog/syslog.lo
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/syslog/syslog_c.c
syslog/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: syslog/check

@go_include@ tabwriter/tabwriter.lo.dep
tabwriter/tabwriter.lo.dep: $(go_tabwriter_files)
	$(BUILDDEPS)
tabwriter/tabwriter.lo: $(go_tabwriter_files)
	$(BUILDPACKAGE)
tabwriter/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: tabwriter/check

@go_include@ template/template.lo.dep
template/template.lo.dep: $(go_template_files)
	$(BUILDDEPS)
template/template.lo: $(go_template_files)
	$(BUILDPACKAGE)
template/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: template/check

@go_include@ testing/testing.lo.dep
testing/testing.lo.dep: $(go_testing_files)
	$(BUILDDEPS)
testing/testing.lo: $(go_testing_files)
	$(BUILDPACKAGE)
testing/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: testing/check

@go_include@ time/time.lo.dep
time/time.lo.dep: $(go_time_files)
	$(BUILDDEPS)
time/time.lo: $(go_time_files)
	$(BUILDPACKAGE)
time/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: time/check

@go_include@ unicode/unicode.lo.dep
unicode/unicode.lo.dep: $(go_unicode_files)
	$(BUILDDEPS)
unicode/unicode.lo: $(go_unicode_files)
	$(BUILDPACKAGE)
unicode/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: unicode/check

@go_include@ url/url.lo.dep
url/url.lo.dep: $(go_url_files)
	$(BUILDDEPS)
url/url.lo: $(go_url_files)
	$(BUILDPACKAGE)
url/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: url/check

@go_include@ utf16/utf16.lo.dep
utf16/utf16.lo.dep: $(go_utf16_files)
	$(BUILDDEPS)
utf16/utf16.lo: $(go_utf16_files)
	$(BUILDPACKAGE)
utf16/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: utf16/check

@go_include@ utf8/utf8.lo.dep
utf8/utf8.lo.dep: $(go_utf8_files)
	$(BUILDDEPS)
utf8/utf8.lo: $(go_utf8_files)
	$(BUILDPACKAGE)
utf8/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: utf8/check

@go_include@ websocket/websocket.lo.dep
websocket/websocket.lo.dep: $(go_websocket_files)
	$(BUILDDEPS)
websocket/websocket.lo: $(go_websocket_files)
	$(BUILDPACKAGE)
websocket/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: websocket/check

@go_include@ xml/xml.lo.dep
xml/xml.lo.dep: $(go_xml_files)
	$(BUILDDEPS)
xml/xml.lo: $(go_xml_files)
	$(BUILDPACKAGE)
xml/check: $(CHECK_DEPS)
	@$(CHECK)
.PHONY: xml/check

@go_include@ archive/tar.lo.dep
archive/tar.lo.dep: $(go_archive_tar_files)
	$(BUILDDEPS)
archive/tar.lo: $(go_archive_tar_files)
	$(BUILDPACKAGE)
archive/tar/check: $(CHECK_DEPS)
	@$(MKDIR_P) archive/tar
	@$(CHECK)
.PHONY: archive/tar/check

@go_include@ archive/zip.lo.dep
archive/zip.lo.dep: $(go_archive_zip_files)
	$(BUILDDEPS)
archive/zip.lo: $(go_archive_zip_files)
	$(BUILDPACKAGE)
archive/zip/check: $(CHECK_DEPS)
	@$(MKDIR_P) archive/zip
	@$(CHECK)
.PHONY: archive/zip/check

@go_include@ compress/bzip2.lo.dep
compress/bzip2.lo.dep: $(go_compress_bzip2_files)
	$(BUILDDEPS)
compress/bzip2.lo: $(go_compress_bzip2_files)
	$(BUILDPACKAGE)
compress/bzip2/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/bzip2
	@$(CHECK)
.PHONY: compress/bzip2/check

@go_include@ compress/flate.lo.dep
compress/flate.lo.dep: $(go_compress_flate_files)
	$(BUILDDEPS)
compress/flate.lo: $(go_compress_flate_files)
	$(BUILDPACKAGE)
compress/flate/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/flate
	@$(CHECK)
.PHONY: compress/flate/check

@go_include@ compress/gzip.lo.dep
compress/gzip.lo.dep: $(go_compress_gzip_files)
	$(BUILDDEPS)
compress/gzip.lo: $(go_compress_gzip_files)
	$(BUILDPACKAGE)
compress/gzip/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/gzip
	@$(CHECK)
.PHONY: compress/gzip/check

@go_include@ compress/lzw.lo.dep
compress/lzw.lo.dep: $(go_compress_lzw_files)
	$(BUILDDEPS)
compress/lzw.lo: $(go_compress_lzw_files)
	$(BUILDPACKAGE)
compress/lzw/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/lzw
	@$(CHECK)
.PHONY: compress/lzw/check

@go_include@ compress/zlib.lo.dep
compress/zlib.lo.dep: $(go_compress_zlib_files)
	$(BUILDDEPS)
compress/zlib.lo: $(go_compress_zlib_files)
	$(BUILDPACKAGE)
compress/zlib/check: $(CHECK_DEPS)
	@$(MKDIR_P) compress/zlib
	@$(CHECK)
.PHONY: compress/zlib/check

@go_include@ container/heap.lo.dep
container/heap.lo.dep: $(go_container_heap_files)
	$(BUILDDEPS)
container/heap.lo: $(go_container_heap_files)
	$(BUILDPACKAGE)
container/heap/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/heap
	@$(CHECK)
.PHONY: container/heap/check

@go_include@ container/list.lo.dep
container/list.lo.dep: $(go_container_list_files)
	$(BUILDDEPS)
container/list.lo: $(go_container_list_files)
	$(BUILDPACKAGE)
container/list/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/list
	@$(CHECK)
.PHONY: container/list/check

@go_include@ container/ring.lo.dep
container/ring.lo.dep: $(go_container_ring_files)
	$(BUILDDEPS)
container/ring.lo: $(go_container_ring_files)
	$(BUILDPACKAGE)
container/ring/check: $(CHECK_DEPS)
	@$(MKDIR_P) container/ring
	@$(CHECK)
.PHONY: container/ring/check

@go_include@ crypto/aes.lo.dep
crypto/aes.lo.dep: $(go_crypto_aes_files)
	$(BUILDDEPS)
crypto/aes.lo: $(go_crypto_aes_files)
	$(BUILDPACKAGE)
crypto/aes/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/aes
	@$(CHECK)
.PHONY: crypto/aes/check

@go_include@ crypto/bcrypt.lo.dep
crypto/bcrypt.lo.dep: $(go_crypto_bcrypt_files)
	$(BUILDDEPS)
crypto/bcrypt.lo: $(go_crypto_bcrypt_files)
	$(BUILDPACKAGE)
crypto/bcrypt/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/bcrypt
	@$(CHECK)
.PHONY: crypto/bcrypt/check

@go_include@ crypto/blowfish.lo.dep
crypto/blowfish.lo.dep: $(go_crypto_blowfish_files)
	$(BUILDDEPS)
crypto/blowfish.lo: $(go_crypto_blowfish_files)
	$(BUILDPACKAGE)
crypto/blowfish/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/blowfish
	@$(CHECK)
.PHONY: crypto/blowfish/check

@go_include@ crypto/cast5.lo.dep
crypto/cast5.lo.dep: $(go_crypto_cast5_files)
	$(BUILDDEPS)
crypto/cast5.lo: $(go_crypto_cast5_files)
	$(BUILDPACKAGE)
crypto/cast5/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/cast5
	@$(CHECK)
.PHONY: crypto/cast5/check

@go_include@ crypto/cipher.lo.dep
crypto/cipher.lo.dep: $(go_crypto_cipher_files)
	$(BUILDDEPS)
crypto/cipher.lo: $(go_crypto_cipher_files)
	$(BUILDPACKAGE)
crypto/cipher/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/cipher
	@$(CHECK)
.PHONY: crypto/cipher/check

@go_include@ crypto/des.lo.dep
crypto/des.lo.dep: $(go_crypto_des_files)
	$(BUILDDEPS)
crypto/des.lo: $(go_crypto_des_files)
	$(BUILDPACKAGE)
crypto/des/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/des
	@$(CHECK)
.PHONY: crypto/des/check

@go_include@ crypto/dsa.lo.dep
crypto/dsa.lo.dep: $(go_crypto_dsa_files)
	$(BUILDDEPS)
crypto/dsa.lo: $(go_crypto_dsa_files)
	$(BUILDPACKAGE)
crypto/dsa/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/dsa
	@$(CHECK)
.PHONY: crypto/dsa/check

@go_include@ crypto/ecdsa.lo.dep
crypto/ecdsa.lo.dep: $(go_crypto_ecdsa_files)
	$(BUILDDEPS)
crypto/ecdsa.lo: $(go_crypto_ecdsa_files)
	$(BUILDPACKAGE)
crypto/ecdsa/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/ecdsa
	@$(CHECK)
.PHONY: crypto/ecdsa/check

@go_include@ crypto/elliptic.lo.dep
crypto/elliptic.lo.dep: $(go_crypto_elliptic_files)
	$(BUILDDEPS)
crypto/elliptic.lo: $(go_crypto_elliptic_files)
	$(BUILDPACKAGE)
crypto/elliptic/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/elliptic
	@$(CHECK)
.PHONY: crypto/elliptic/check

@go_include@ crypto/hmac.lo.dep
crypto/hmac.lo.dep: $(go_crypto_hmac_files)
	$(BUILDDEPS)
crypto/hmac.lo: $(go_crypto_hmac_files)
	$(BUILDPACKAGE)
crypto/hmac/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/hmac
	@$(CHECK)
.PHONY: crypto/hmac/check

@go_include@ crypto/md4.lo.dep
crypto/md4.lo.dep: $(go_crypto_md4_files)
	$(BUILDDEPS)
crypto/md4.lo: $(go_crypto_md4_files)
	$(BUILDPACKAGE)
crypto/md4/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/md4
	@$(CHECK)
.PHONY: crypto/md4/check

@go_include@ crypto/md5.lo.dep
crypto/md5.lo.dep: $(go_crypto_md5_files)
	$(BUILDDEPS)
crypto/md5.lo: $(go_crypto_md5_files)
	$(BUILDPACKAGE)
crypto/md5/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/md5
	@$(CHECK)
.PHONY: crypto/md5/check

@go_include@ crypto/ocsp.lo.dep
crypto/ocsp.lo.dep: $(go_crypto_ocsp_files)
	$(BUILDDEPS)
crypto/ocsp.lo: $(go_crypto_ocsp_files)
	$(BUILDPACKAGE)
crypto/ocsp/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/ocsp
	@$(CHECK)
.PHONY: crypto/ocsp/check

@go_include@ crypto/openpgp.lo.dep
crypto/openpgp.lo.dep: $(go_crypto_openpgp_files)
	$(BUILDDEPS)
crypto/openpgp.lo: $(go_crypto_openpgp_files)
	$(BUILDPACKAGE)
crypto/openpgp/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp
	@$(CHECK)
.PHONY: crypto/openpgp/check

@go_include@ crypto/rand.lo.dep
crypto/rand.lo.dep: $(go_crypto_rand_files)
	$(BUILDDEPS)
crypto/rand.lo: $(go_crypto_rand_files)
	$(BUILDPACKAGE)
crypto/rand/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rand
	@$(CHECK)
.PHONY: crypto/rand/check

@go_include@ crypto/rc4.lo.dep
crypto/rc4.lo.dep: $(go_crypto_rc4_files)
	$(BUILDDEPS)
crypto/rc4.lo: $(go_crypto_rc4_files)
	$(BUILDPACKAGE)
crypto/rc4/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rc4
	@$(CHECK)
.PHONY: crypto/rc4/check

@go_include@ crypto/ripemd160.lo.dep
crypto/ripemd160.lo.dep: $(go_crypto_ripemd160_files)
	$(BUILDDEPS)
crypto/ripemd160.lo: $(go_crypto_ripemd160_files)
	$(BUILDPACKAGE)
crypto/ripemd160/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/ripemd160
	@$(CHECK)
.PHONY: crypto/ripemd160/check

@go_include@ crypto/rsa.lo.dep
crypto/rsa.lo.dep: $(go_crypto_rsa_files)
	$(BUILDDEPS)
crypto/rsa.lo: $(go_crypto_rsa_files)
	$(BUILDPACKAGE)
crypto/rsa/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/rsa
	@$(CHECK)
.PHONY: crypto/rsa/check

@go_include@ crypto/sha1.lo.dep
crypto/sha1.lo.dep: $(go_crypto_sha1_files)
	$(BUILDDEPS)
crypto/sha1.lo: $(go_crypto_sha1_files)
	$(BUILDPACKAGE)
crypto/sha1/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha1
	@$(CHECK)
.PHONY: crypto/sha1/check

@go_include@ crypto/sha256.lo.dep
crypto/sha256.lo.dep: $(go_crypto_sha256_files)
	$(BUILDDEPS)
crypto/sha256.lo: $(go_crypto_sha256_files)
	$(BUILDPACKAGE)
crypto/sha256/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha256
	@$(CHECK)
.PHONY: crypto/sha256/check

@go_include@ crypto/sha512.lo.dep
crypto/sha512.lo.dep: $(go_crypto_sha512_files)
	$(BUILDDEPS)
crypto/sha512.lo: $(go_crypto_sha512_files)
	$(BUILDPACKAGE)
crypto/sha512/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/sha512
	@$(CHECK)
.PHONY: crypto/sha512/check

@go_include@ crypto/subtle.lo.dep
crypto/subtle.lo.dep: $(go_crypto_subtle_files)
	$(BUILDDEPS)
crypto/subtle.lo: $(go_crypto_subtle_files)
	$(BUILDPACKAGE)
crypto/subtle/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/subtle
	@$(CHECK)
.PHONY: crypto/subtle/check

@go_include@ crypto/tls.lo.dep
crypto/tls.lo.dep: $(go_crypto_tls_files)
	$(BUILDDEPS)
crypto/tls.lo: $(go_crypto_tls_files)
	$(BUILDPACKAGE)
crypto/tls/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/tls
	@$(CHECK)
.PHONY: crypto/tls/check

@go_include@ crypto/twofish.lo.dep
crypto/twofish.lo.dep: $(go_crypto_twofish_files)
	$(BUILDDEPS)
crypto/twofish.lo: $(go_crypto_twofish_files)
	$(BUILDPACKAGE)
crypto/twofish/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/twofish
	@$(CHECK)
.PHONY: crypto/twofish/check

@go_include@ crypto/x509.lo.dep
crypto/x509.lo.dep: $(go_crypto_x509_files)
	$(BUILDDEPS)
crypto/x509.lo: $(go_crypto_x509_files)
	$(BUILDPACKAGE)
crypto/x509/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/x509
	@$(CHECK)
.PHONY: crypto/x509/check

@go_include@ crypto/xtea.lo.dep
crypto/xtea.lo.dep: $(go_crypto_xtea_files)
	$(BUILDDEPS)
crypto/xtea.lo: $(go_crypto_xtea_files)
	$(BUILDPACKAGE)
crypto/xtea/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/xtea
	@$(CHECK)
.PHONY: crypto/xtea/check

@go_include@ crypto/openpgp/armor.lo.dep
crypto/openpgp/armor.lo.dep: $(go_crypto_openpgp_armor_files)
	$(BUILDDEPS)
crypto/openpgp/armor.lo: $(go_crypto_openpgp_armor_files)
	$(BUILDPACKAGE)
crypto/openpgp/armor/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/armor
	@$(CHECK)
.PHONY: crypto/openpgp/armor/check

@go_include@ crypto/openpgp/elgamal.lo.dep
crypto/openpgp/elgamal.lo.dep: $(go_crypto_openpgp_elgamal_files)
	$(BUILDDEPS)
crypto/openpgp/elgamal.lo: $(go_crypto_openpgp_elgamal_files)
	$(BUILDPACKAGE)
crypto/openpgp/elgamal/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/elgamal
	@$(CHECK)
.PHONY: crypto/openpgp/elgamal/check

@go_include@ crypto/openpgp/error.lo.dep
crypto/openpgp/error.lo.dep: $(go_crypto_openpgp_error_files)
	$(BUILDDEPS)
crypto/openpgp/error.lo: $(go_crypto_openpgp_error_files)
	$(BUILDPACKAGE)
crypto/openpgp/error/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/error
	@$(CHECK)
.PHONY: crypto/openpgp/error/check

@go_include@ crypto/openpgp/packet.lo.dep
crypto/openpgp/packet.lo.dep: $(go_crypto_openpgp_packet_files)
	$(BUILDDEPS)
crypto/openpgp/packet.lo: $(go_crypto_openpgp_packet_files)
	$(BUILDPACKAGE)
crypto/openpgp/packet/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/packet
	@$(CHECK)
.PHONY: crypto/openpgp/packet/check

@go_include@ crypto/openpgp/s2k.lo.dep
crypto/openpgp/s2k.lo.dep: $(go_crypto_openpgp_s2k_files)
	$(BUILDDEPS)
crypto/openpgp/s2k.lo: $(go_crypto_openpgp_s2k_files)
	$(BUILDPACKAGE)
crypto/openpgp/s2k/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/openpgp/s2k
	@$(CHECK)
.PHONY: crypto/openpgp/s2k/check

@go_include@ crypto/x509/pkix.lo.dep
crypto/x509/pkix.lo.dep: $(go_crypto_x509_pkix_files)
	$(BUILDDEPS)
crypto/x509/pkix.lo: $(go_crypto_x509_pkix_files)
	$(BUILDPACKAGE)
crypto/x509/pkix/check: $(CHECK_DEPS)
	@$(MKDIR_P) crypto/x509/pkix
	@$(CHECK)
.PHONY: crypto/x509/pkix/check

@go_include@ debug/dwarf.lo.dep
debug/dwarf.lo.dep: $(go_debug_dwarf_files)
	$(BUILDDEPS)
debug/dwarf.lo: $(go_debug_dwarf_files)
	$(BUILDPACKAGE)
debug/dwarf/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/dwarf
	@$(CHECK)
.PHONY: debug/dwarf/check

@go_include@ debug/elf.lo.dep
debug/elf.lo.dep: $(go_debug_elf_files)
	$(BUILDDEPS)
debug/elf.lo: $(go_debug_elf_files)
	$(BUILDPACKAGE)
debug/elf/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/elf
	@$(CHECK)
.PHONY: debug/elf/check

@go_include@ debug/gosym.lo.dep
debug/gosym.lo.dep: $(go_debug_gosym_files)
	$(BUILDDEPS)
debug/gosym.lo: $(go_debug_gosym_files)
	$(BUILDPACKAGE)
debug/gosym/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/gosym
	@$(CHECK)
.PHONY: debug/gosym/check

@go_include@ debug/macho.lo.dep
debug/macho.lo.dep: $(go_debug_macho_files)
	$(BUILDDEPS)
debug/macho.lo: $(go_debug_macho_files)
	$(BUILDPACKAGE)
debug/macho/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/macho
	@$(CHECK)
.PHONY: debug/macho/check

@go_include@ debug/pe.lo.dep
debug/pe.lo.dep: $(go_debug_pe_files)
	$(BUILDDEPS)
debug/pe.lo: $(go_debug_pe_files)
	$(BUILDPACKAGE)
debug/pe/check: $(CHECK_DEPS)
	@$(MKDIR_P) debug/pe
	@$(CHECK)
.PHONY: debug/pe/check

@go_include@ encoding/ascii85.lo.dep
encoding/ascii85.lo.dep: $(go_encoding_ascii85_files)
	$(BUILDDEPS)
encoding/ascii85.lo: $(go_encoding_ascii85_files)
	$(BUILDPACKAGE)
encoding/ascii85/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/ascii85
	@$(CHECK)
.PHONY: encoding/ascii85/check

@go_include@ encoding/base32.lo.dep
encoding/base32.lo.dep: $(go_encoding_base32_files)
	$(BUILDDEPS)
encoding/base32.lo: $(go_encoding_base32_files)
	$(BUILDPACKAGE)
encoding/base32/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/base32
	@$(CHECK)
.PHONY: encoding/base32/check

@go_include@ encoding/base64.lo.dep
encoding/base64.lo.dep: $(go_encoding_base64_files)
	$(BUILDDEPS)
encoding/base64.lo: $(go_encoding_base64_files)
	$(BUILDPACKAGE)
encoding/base64/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/base64
	@$(CHECK)
.PHONY: encoding/base64/check

@go_include@ encoding/binary.lo.dep
encoding/binary.lo.dep: $(go_encoding_binary_files)
	$(BUILDDEPS)
encoding/binary.lo: $(go_encoding_binary_files)
	$(BUILDPACKAGE)
encoding/binary/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/binary
	@$(CHECK)
.PHONY: encoding/binary/check

@go_include@ encoding/git85.lo.dep
encoding/git85.lo.dep: $(go_encoding_git85_files)
	$(BUILDDEPS)
encoding/git85.lo: $(go_encoding_git85_files)
	$(BUILDPACKAGE)
encoding/git85/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/git85
	@$(CHECK)
.PHONY: encoding/git85/check

@go_include@ encoding/hex.lo.dep
encoding/hex.lo.dep: $(go_encoding_hex_files)
	$(BUILDDEPS)
encoding/hex.lo: $(go_encoding_hex_files)
	$(BUILDPACKAGE)
encoding/hex/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/hex
	@$(CHECK)
.PHONY: encoding/hex/check

@go_include@ encoding/pem.lo.dep
encoding/pem.lo.dep: $(go_encoding_pem_files)
	$(BUILDDEPS)
encoding/pem.lo: $(go_encoding_pem_files)
	$(BUILDPACKAGE)
encoding/pem/check: $(CHECK_DEPS)
	@$(MKDIR_P) encoding/pem
	@$(CHECK)
.PHONY: encoding/pem/check

@go_include@ exp/ebnf.lo.dep
exp/ebnf.lo.dep: $(go_exp_ebnf_files)
	$(BUILDDEPS)
exp/ebnf.lo: $(go_exp_ebnf_files)
	$(BUILDPACKAGE)
exp/ebnf/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/ebnf
	@$(CHECK)
.PHONY: exp/ebnf/check

@go_include@ exp/gui.lo.dep
exp/gui.lo.dep: $(go_exp_gui_files)
	$(BUILDDEPS)
exp/gui.lo: $(go_exp_gui_files)
	$(BUILDPACKAGE)
exp/gui/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/gui
	@$(CHECK)
.PHONY: exp/gui/check

@go_include@ exp/norm.lo.dep
exp/norm.lo.dep: $(go_exp_norm_files)
	$(BUILDDEPS)
exp/norm.lo: $(go_exp_norm_files)
	$(BUILDPACKAGE)
exp/norm/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/norm
	@$(CHECK)
.PHONY: exp/norm/check

@go_include@ exp/spdy.lo.dep
exp/spdy.lo.dep: $(go_exp_spdy_files)
	$(BUILDDEPS)
exp/spdy.lo: $(go_exp_spdy_files)
	$(BUILDPACKAGE)
exp/spdy/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/spdy
	@$(CHECK)
.PHONY: exp/spdy/check

@go_include@ exp/sql.lo.dep
exp/sql.lo.dep: $(go_exp_sql_files)
	$(BUILDDEPS)
exp/sql.lo: $(go_exp_sql_files)
	$(BUILDPACKAGE)
exp/sql/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/sql
	@$(CHECK)
.PHONY: exp/sql/check

@go_include@ exp/ssh.lo.dep
exp/ssh.lo.dep: $(go_exp_ssh_files)
	$(BUILDDEPS)
exp/ssh.lo: $(go_exp_ssh_files)
	$(BUILDPACKAGE)
exp/ssh/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/ssh
	@$(CHECK)
.PHONY: exp/ssh/check

@go_include@ exp/terminal.lo.dep
exp/terminal.lo.dep: $(go_exp_terminal_files)
	$(BUILDDEPS)
exp/terminal.lo: $(go_exp_terminal_files)
	$(BUILDPACKAGE)
exp/terminal/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/terminal
	@$(CHECK)
.PHONY: exp/terminal/check

@go_include@ exp/types.lo.dep
exp/types.lo.dep: $(go_exp_types_files)
	$(BUILDDEPS)
exp/types.lo: $(go_exp_types_files)
	$(BUILDPACKAGE)
exp/types/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/types
	@$(CHECK)
.PHONY: exp/types/check

@go_include@ exp/gui/x11.lo.dep
exp/gui/x11.lo.dep: $(go_exp_gui_x11_files)
	$(BUILDDEPS)
exp/gui/x11.lo: $(go_exp_gui_x11_files)
	$(BUILDPACKAGE)
exp/gui/x11/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/gui/x11
	@$(CHECK)
.PHONY: exp/gui/x11/check

@go_include@ exp/inotify.lo.dep
exp/inotify.lo.dep: $(go_exp_inotify_files)
	$(BUILDDEPS)
exp/inotify.lo: $(go_exp_inotify_files)
	$(BUILDPACKAGE)
exp/inotify/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/inotify
	@$(CHECK)
.PHONY: exp/inotify/check

@go_include@ exp/sql/driver.lo.dep
exp/sql/driver.lo.dep: $(go_exp_sql_driver_files)
	$(BUILDDEPS)
exp/sql/driver.lo: $(go_exp_sql_driver_files)
	$(BUILDPACKAGE)
exp/sql/driver/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/sql/driver
	@$(CHECK)
.PHONY: exp/sql/driver/check

@go_include@ exp/template/html.lo.dep
exp/template/html.lo.dep: $(go_exp_template_html_files)
	$(BUILDDEPS)
exp/template/html.lo: $(go_exp_template_html_files)
	$(BUILDPACKAGE)
exp/template/html/check: $(CHECK_DEPS)
	@$(MKDIR_P) exp/template/html
	@$(CHECK)
.PHONY: exp/template/html/check

@go_include@ go/ast.lo.dep
go/ast.lo.dep: $(go_go_ast_files)
	$(BUILDDEPS)
go/ast.lo: $(go_go_ast_files)
	$(BUILDPACKAGE)
go/ast/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/ast
	@$(CHECK)
.PHONY: go/ast/check

@go_include@ go/build.lo.dep
go/build.lo.dep: $(go_go_build_files)
	$(BUILDDEPS)
go/build.lo: $(go_go_build_files)
	$(BUILDPACKAGE)
go/build/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/build
	@$(CHECK)
.PHONY: go/build/check

syslist.go: s-syslist; @true
s-syslist: Makefile
	echo '// Generated automatically by make.' >syslist.go.tmp
	echo 'package build' >>syslist.go.tmp
	echo 'const goosList = "$(GOOS)"' >>syslist.go.tmp
	echo 'const goarchList = "$(GOARCH)"' >>syslist.go.tmp
	$(SHELL) $(srcdir)/../move-if-change syslist.go.tmp syslist.go
	$(STAMP) $@

@go_include@ go/doc.lo.dep
go/doc.lo.dep: $(go_go_doc_files)
	$(BUILDDEPS)
go/doc.lo: $(go_go_doc_files)
	$(BUILDPACKAGE)
go/doc/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/doc
	@$(CHECK)
.PHONY: go/doc/check

@go_include@ go/parser.lo.dep
go/parser.lo.dep: $(go_go_parser_files)
	$(BUILDDEPS)
go/parser.lo: $(go_go_parser_files)
	$(BUILDPACKAGE)
go/parser/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/parser
	@$(CHECK)
.PHONY: go/parser/check

@go_include@ go/printer.lo.dep
go/printer.lo.dep: $(go_go_printer_files)
	$(BUILDDEPS)
go/printer.lo: $(go_go_printer_files)
	$(BUILDPACKAGE)
go/printer/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/printer
	@$(CHECK)
.PHONY: go/printer/check

@go_include@ go/scanner.lo.dep
go/scanner.lo.dep: $(go_go_scanner_files)
	$(BUILDDEPS)
go/scanner.lo: $(go_go_scanner_files)
	$(BUILDPACKAGE)
go/scanner/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/scanner
	@$(CHECK)
.PHONY: go/scanner/check

@go_include@ go/token.lo.dep
go/token.lo.dep: $(go_go_token_files)
	$(BUILDDEPS)
go/token.lo: $(go_go_token_files)
	$(BUILDPACKAGE)
go/token/check: $(CHECK_DEPS)
	@$(MKDIR_P) go/token
	@$(CHECK)
.PHONY: go/token/check

@go_include@ hash/adler32.lo.dep
hash/adler32.lo.dep: $(go_hash_adler32_files)
	$(BUILDDEPS)
hash/adler32.lo: $(go_hash_adler32_files)
	$(BUILDPACKAGE)
hash/adler32/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/adler32
	@$(CHECK)
.PHONY: hash/adler32/check

@go_include@ hash/crc32.lo.dep
hash/crc32.lo.dep: $(go_hash_crc32_files)
	$(BUILDDEPS)
hash/crc32.lo: $(go_hash_crc32_files)
	$(BUILDPACKAGE)
hash/crc32/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/crc32
	@$(CHECK)
.PHONY: hash/crc32/check

@go_include@ hash/crc64.lo.dep
hash/crc64.lo.dep: $(go_hash_crc64_files)
	$(BUILDDEPS)
hash/crc64.lo: $(go_hash_crc64_files)
	$(BUILDPACKAGE)
hash/crc64/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/crc64
	@$(CHECK)
.PHONY: hash/crc64/check

@go_include@ hash/fnv.lo.dep
hash/fnv.lo.dep: $(go_hash_fnv_files)
	$(BUILDDEPS)
hash/fnv.lo: $(go_hash_fnv_files)
	$(BUILDPACKAGE)
hash/fnv/check: $(CHECK_DEPS)
	@$(MKDIR_P) hash/fnv
	@$(CHECK)
.PHONY: hash/fnv/check

@go_include@ http/cgi.lo.dep
http/cgi.lo.dep: $(go_http_cgi_files)
	$(BUILDDEPS)
http/cgi.lo: $(go_http_cgi_files)
	$(BUILDPACKAGE)
http/cgi/check: $(CHECK_DEPS)
	@$(MKDIR_P) http/cgi
	@$(CHECK)
.PHONY: http/cgi/check

@go_include@ http/fcgi.lo.dep
http/fcgi.lo.dep: $(go_http_fcgi_files)
	$(BUILDDEPS)
http/fcgi.lo: $(go_http_fcgi_files)
	$(BUILDPACKAGE)
http/fcgi/check: $(CHECK_DEPS)
	@$(MKDIR_P) http/fcgi
	@$(CHECK)
.PHONY: http/fcgi/check

@go_include@ http/httptest.lo.dep
http/httptest.lo.dep: $(go_http_httptest_files)
	$(BUILDDEPS)
http/httptest.lo: $(go_http_httptest_files)
	$(BUILDPACKAGE)
http/httptest/check: $(CHECK_DEPS)
	@$(MKDIR_P) http/httptest
	@$(CHECK)
.PHONY: http/httptest/check

@go_include@ http/pprof.lo.dep
http/pprof.lo.dep: $(go_http_pprof_files)
	$(BUILDDEPS)
http/pprof.lo: $(go_http_pprof_files)
	$(BUILDPACKAGE)
http/pprof/check: $(CHECK_DEPS)
	@$(MKDIR_P) http/pprof
	@$(CHECK)
.PHONY: http/pprof/check

@go_include@ image/bmp.lo.dep
image/bmp.lo.dep: $(go_image_bmp_files)
	$(BUILDDEPS)
image/bmp.lo: $(go_image_bmp_files)
	$(BUILDPACKAGE)
image/bmp/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/bmp
	@$(CHECK)
.PHONY: image/bmp/check

@go_include@ image/color.lo.dep
image/color.lo.dep: $(go_image_color_files)
	$(BUILDDEPS)
image/color.lo: $(go_image_color_files)
	$(BUILDPACKAGE)
image/color/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/color
	@$(CHECK)
.PHONY: image/color/check

@go_include@ image/draw.lo.dep
image/draw.lo.dep: $(go_image_draw_files)
	$(BUILDDEPS)
image/draw.lo: $(go_image_draw_files)
	$(BUILDPACKAGE)
image/draw/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/draw
	@$(CHECK)
.PHONY: image/draw/check

@go_include@ image/gif.lo.dep
image/gif.lo.dep: $(go_image_gif_files)
	$(BUILDDEPS)
image/gif.lo: $(go_image_gif_files)
	$(BUILDPACKAGE)
image/gif/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/gif
	@$(CHECK)
.PHONY: image/gif/check

@go_include@ image/jpeg.lo.dep
image/jpeg.lo.dep: $(go_image_jpeg_files)
	$(BUILDDEPS)
image/jpeg.lo: $(go_image_jpeg_files)
	$(BUILDPACKAGE)
image/jpeg/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/jpeg
	@$(CHECK)
.PHONY: image/jpeg/check

@go_include@ image/png.lo.dep
image/png.lo.dep: $(go_image_png_files)
	$(BUILDDEPS)
image/png.lo: $(go_image_png_files)
	$(BUILDPACKAGE)
image/png/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/png
	@$(CHECK)
.PHONY: image/png/check

@go_include@ image/tiff.lo.dep
image/tiff.lo.dep: $(go_image_tiff_files)
	$(BUILDDEPS)
image/tiff.lo: $(go_image_tiff_files)
	$(BUILDPACKAGE)
image/tiff/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/tiff
	@$(CHECK)
.PHONY: image/tiff/check

@go_include@ image/ycbcr.lo.dep
image/ycbcr.lo.dep: $(go_image_ycbcr_files)
	$(BUILDDEPS)
image/ycbcr.lo: $(go_image_ycbcr_files)
	$(BUILDPACKAGE)
image/ycbcr/check: $(CHECK_DEPS)
	@$(MKDIR_P) image/ycbcr
	@$(CHECK)
.PHONY: image/ycbcr/check

@go_include@ index/suffixarray.lo.dep
index/suffixarray.lo.dep: $(go_index_suffixarray_files)
	$(BUILDDEPS)
index/suffixarray.lo: $(go_index_suffixarray_files)
	$(BUILDPACKAGE)
index/suffixarray/check: $(CHECK_DEPS)
	@$(MKDIR_P) index/suffixarray
	@$(CHECK)
.PHONY: index/suffixarray/check

@go_include@ io/ioutil.lo.dep
io/ioutil.lo.dep: $(go_io_ioutil_files)
	$(BUILDDEPS)
io/ioutil.lo: $(go_io_ioutil_files)
	$(BUILDPACKAGE)
io/ioutil/check: $(CHECK_DEPS)
	@$(MKDIR_P) io/ioutil
	@$(CHECK)
.PHONY: io/ioutil/check

@go_include@ mime/multipart.lo.dep
mime/multipart.lo.dep: $(go_mime_multipart_files)
	$(BUILDDEPS)
mime/multipart.lo: $(go_mime_multipart_files)
	$(BUILDPACKAGE)
mime/multipart/check: $(CHECK_DEPS)
	@$(MKDIR_P) mime/multipart
	@$(CHECK)
.PHONY: mime/multipart/check

@go_include@ net/dict.lo.dep
net/dict.lo.dep: $(go_net_dict_files)
	$(BUILDDEPS)
net/dict.lo: $(go_net_dict_files)
	$(BUILDPACKAGE)

@go_include@ net/textproto.lo.dep
net/textproto.lo.dep: $(go_net_textproto_files)
	$(BUILDDEPS)
net/textproto.lo: $(go_net_textproto_files)
	$(BUILDPACKAGE)
net/textproto/check: $(CHECK_DEPS)
	@$(MKDIR_P) net/textproto
	@$(CHECK)
.PHONY: net/textproto/check

@go_include@ old/netchan.lo.dep
old/netchan.lo.dep: $(go_old_netchan_files)
	$(BUILDDEPS)
old/netchan.lo: $(go_old_netchan_files)
	$(BUILDPACKAGE)
old/netchan/check: $(CHECK_DEPS)
	@$(MKDIR_P) old/netchan
	@$(CHECK)
.PHONY: old/netchan/check

@go_include@ old/regexp.lo.dep
old/regexp.lo.dep: $(go_old_regexp_files)
	$(BUILDDEPS)
old/regexp.lo: $(go_old_regexp_files)
	$(BUILDPACKAGE)
old/regexp/check: $(CHECK_DEPS)
	@$(MKDIR_P) old/regexp
	@$(CHECK)
.PHONY: old/regexp/check

@go_include@ old/template.lo.dep
old/template.lo.dep: $(go_old_template_files)
	$(BUILDDEPS)
old/template.lo: $(go_old_template_files)
	$(BUILDPACKAGE)
old/template/check: $(CHECK_DEPS)
	@$(MKDIR_P) old/template
	@$(CHECK)
.PHONY: old/template/check

@go_include@ os/user.lo.dep
os/user.lo.dep: $(go_os_user_files)
	$(BUILDDEPS)
os/user.lo: $(go_os_user_files)
	$(BUILDPACKAGE)
os/user/check: $(CHECK_DEPS)
	@$(MKDIR_P) os/user
	@$(CHECK)
.PHONY: os/user/check

@go_include@ os/signal.lo.dep
os/signal.lo.dep: $(go_os_signal_files)
	$(BUILDDEPS)
os/signal.lo: $(go_os_signal_files)
	$(BUILDPACKAGE)
os/signal/check: $(CHECK_DEPS)
	@$(MKDIR_P) os/signal
	@$(CHECK)
.PHONY: os/signal/check

@go_include@ path/filepath.lo.dep
path/filepath.lo.dep: $(go_path_filepath_files)
	$(BUILDDEPS)
path/filepath.lo: $(go_path_filepath_files)
	$(BUILDPACKAGE)
path/filepath/check: $(CHECK_DEPS)
	@$(MKDIR_P) path/filepath
	@$(CHECK)
.PHONY: path/filepath/check

@go_include@ regexp/syntax.lo.dep
regexp/syntax.lo.dep: $(go_regexp_syntax_files)
	$(BUILDDEPS)
regexp/syntax.lo: $(go_regexp_syntax_files)
	$(BUILDPACKAGE)
regexp/syntax/check: $(CHECK_DEPS)
	@$(MKDIR_P) regexp/syntax
	@$(CHECK)
.PHONY: regexp/syntax/check

@go_include@ rpc/jsonrpc.lo.dep
rpc/jsonrpc.lo.dep: $(go_rpc_jsonrpc_files)
	$(BUILDDEPS)
rpc/jsonrpc.lo: $(go_rpc_jsonrpc_files)
	$(BUILDPACKAGE)
rpc/jsonrpc/check: $(CHECK_DEPS)
	@$(MKDIR_P) rpc/jsonrpc
	@$(CHECK)
.PHONY: rpc/jsonrpc/check

@go_include@ runtime/debug.lo.dep
runtime/debug.lo.dep: $(go_runtime_debug_files)
	$(BUILDDEPS)
runtime/debug.lo: $(go_runtime_debug_files)
	$(BUILDPACKAGE)
runtime/debug/check: $(CHECK_DEPS)
	@$(MKDIR_P) runtime/debug
	@$(CHECK)
.PHONY: runtime/debug/check

@go_include@ runtime/pprof.lo.dep
runtime/pprof.lo.dep: $(go_runtime_pprof_files)
	$(BUILDDEPS)
runtime/pprof.lo: $(go_runtime_pprof_files)
	$(BUILDPACKAGE)
runtime/pprof/check: $(CHECK_DEPS)
	@$(MKDIR_P) runtime/pprof
	@$(CHECK)
.PHONY: runtime/pprof/check

@go_include@ sync/atomic.lo.dep
sync/atomic.lo.dep: $(go_sync_atomic_files)
	$(BUILDDEPS)
sync/atomic.lo: $(go_sync_atomic_files)
	$(BUILDPACKAGE)
sync/atomic_c.lo: $(go_sync_atomic_c_files) sync/atomic.lo
	$(LTCOMPILE) -c -o $@ $(srcdir)/go/sync/atomic/atomic.c
sync/atomic/check: $(CHECK_DEPS)
	@$(MKDIR_P) sync/atomic
	@$(CHECK)
.PHONY: sync/atomic/check

@go_include@ template/parse.lo.dep
template/parse.lo.dep: $(go_template_parse_files)
	$(BUILDDEPS)
template/parse.lo: $(go_template_parse_files)
	$(BUILDPACKAGE)
template/parse/check: $(CHECK_DEPS)
	@$(MKDIR_P) template/parse
	@$(CHECK)
.PHONY: template/parse/check

@go_include@ testing/iotest.lo.dep
testing/iotest.lo.dep: $(go_testing_iotest_files)
	$(BUILDDEPS)
testing/iotest.lo: $(go_testing_iotest_files)
	$(BUILDPACKAGE)
testing/iotest/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/iotest
	@$(CHECK)
.PHONY: testing/iotest/check

@go_include@ testing/quick.lo.dep
testing/quick.lo.dep: $(go_testing_quick_files)
	$(BUILDDEPS)
testing/quick.lo: $(go_testing_quick_files)
	$(BUILDPACKAGE)
testing/quick/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/quick
	@$(CHECK)
.PHONY: testing/quick/check

@go_include@ testing/script.lo.dep
testing/script.lo.dep: $(go_testing_script_files)
	$(BUILDDEPS)
testing/script.lo: $(go_testing_script_files)
	$(BUILDPACKAGE)
testing/script/check: $(CHECK_DEPS)
	@$(MKDIR_P) testing/script
	@$(CHECK)
.PHONY: testing/script/check

@go_include@ syscall/syscall.lo.dep
syscall/syscall.lo.dep: $(go_syscall_files)
	$(BUILDDEPS)
syscall/syscall.lo: $(go_syscall_files)
	$(BUILDPACKAGE)
syscall/errno.lo: go/syscall/errno.c
	$(LTCOMPILE) -c -o $@ $<
syscall/wait.lo: go/syscall/wait.c
	$(LTCOMPILE) -c -o $@ $<

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
crypto.gox: crypto/crypto.lo
	$(BUILDGOX)
csv.gox: csv/csv.lo
	$(BUILDGOX)
errors.gox: errors/errors.lo
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
mail.gox: mail/mail.lo
	$(BUILDGOX)
mime.gox: mime/mime.lo
	$(BUILDGOX)
net.gox: net/net.lo
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
sync.gox: sync/sync.lo
	$(BUILDGOX)
syslog.gox: syslog/syslog.lo
	$(BUILDGOX)
syscall.gox: syscall/syscall.lo
	$(BUILDGOX)
tabwriter.gox: tabwriter/tabwriter.lo
	$(BUILDGOX)
template.gox: template/template.lo
	$(BUILDGOX)
testing.gox: testing/testing.lo
	$(BUILDGOX)
time.gox: time/time.lo
	$(BUILDGOX)
unicode.gox: unicode/unicode.lo
	$(BUILDGOX)
url.gox: url/url.lo
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
crypto/bcrypt.gox: crypto/bcrypt.lo
	$(BUILDGOX)
crypto/blowfish.gox: crypto/blowfish.lo
	$(BUILDGOX)
crypto/cast5.gox: crypto/cast5.lo
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
crypto/md4.gox: crypto/md4.lo
	$(BUILDGOX)
crypto/md5.gox: crypto/md5.lo
	$(BUILDGOX)
crypto/ocsp.gox: crypto/ocsp.lo
	$(BUILDGOX)
crypto/openpgp.gox: crypto/openpgp.lo
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
crypto/openpgp/elgamal.gox: crypto/openpgp/elgamal.lo
	$(BUILDGOX)
crypto/openpgp/error.gox: crypto/openpgp/error.lo
	$(BUILDGOX)
crypto/openpgp/packet.gox: crypto/openpgp/packet.lo
	$(BUILDGOX)
crypto/openpgp/s2k.gox: crypto/openpgp/s2k.lo
	$(BUILDGOX)

crypto/x509/pkix.gox: crypto/x509/pkix.lo
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
encoding/pem.gox: encoding/pem.lo
	$(BUILDGOX)

exp/ebnf.gox: exp/ebnf.lo
	$(BUILDGOX)
exp/gui.gox: exp/gui.lo
	$(BUILDGOX)
exp/inotify.gox: exp/inotify.lo
	$(BUILDGOX)
exp/norm.gox: exp/norm.lo
	$(BUILDGOX)
exp/spdy.gox: exp/spdy.lo
	$(BUILDGOX)
exp/sql.gox: exp/sql.lo
	$(BUILDGOX)
exp/ssh.gox: exp/ssh.lo
	$(BUILDGOX)
exp/terminal.gox: exp/terminal.lo
	$(BUILDGOX)
exp/types.gox: exp/types.lo
	$(BUILDGOX)

exp/gui/x11.gox: exp/gui/x11.lo
	$(BUILDGOX)

exp/sql/driver.gox: exp/sql/driver.lo
	$(BUILDGOX)

exp/template/html.gox: exp/template/html.lo
	$(BUILDGOX)

go/ast.gox: go/ast.lo
	$(BUILDGOX)
go/build.gox: go/build.lo
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

hash/adler32.gox: hash/adler32.lo
	$(BUILDGOX)
hash/crc32.gox: hash/crc32.lo
	$(BUILDGOX)
hash/crc64.gox: hash/crc64.lo
	$(BUILDGOX)
hash/fnv.gox: hash/fnv.lo
	$(BUILDGOX)

http/cgi.gox: http/cgi.lo
	$(BUILDGOX)
http/fcgi.gox: http/fcgi.lo
	$(BUILDGOX)
http/httptest.gox: http/httptest.lo
	$(BUILDGOX)
http/pprof.gox: http/pprof.lo
	$(BUILDGOX)

image/bmp.gox: image/bmp.lo
	$(BUILDGOX)
image/color.gox: image/color.lo
	$(BUILDGOX)
image/draw.gox: image/draw.lo
	$(BUILDGOX)
image/gif.gox: image/gif.lo
	$(BUILDGOX)
image/jpeg.gox: image/jpeg.lo
	$(BUILDGOX)
image/png.gox: image/png.lo
	$(BUILDGOX)
image/tiff.gox: image/tiff.lo
	$(BUILDGOX)
image/ycbcr.gox: image/ycbcr.lo
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

old/netchan.gox: old/netchan.lo
	$(BUILDGOX)
old/regexp.gox: old/regexp.lo
	$(BUILDGOX)
old/template.gox: old/template.lo
	$(BUILDGOX)

os/user.gox: os/user.lo
	$(BUILDGOX)
os/signal.gox: os/signal.lo
	$(BUILDGOX)

path/filepath.gox: path/filepath.lo
	$(BUILDGOX)

regexp/syntax.gox: regexp/syntax.lo
	$(BUILDGOX)

rpc/jsonrpc.gox: rpc/jsonrpc.lo
	$(BUILDGOX)

runtime/debug.gox: runtime/debug.lo
	$(BUILDGOX)
runtime/pprof.gox: runtime/pprof.lo
	$(BUILDGOX)

sync/atomic.gox: sync/atomic.lo
	$(BUILDGOX)

template/parse.gox: template/parse.lo
	$(BUILDGOX)

testing/iotest.gox: testing/iotest.lo
	$(BUILDGOX)
testing/quick.gox: testing/quick.lo
	$(BUILDGOX)
testing/script.gox: testing/script.lo
	$(BUILDGOX)

if LIBGO_IS_LINUX
# exp_inotify_check = exp/inotify/check
exp_inotify_check =
else
exp_inotify_check =
endif

TEST_PACKAGES = \
	asn1/check \
	big/check \
	bufio/check \
	bytes/check \
	cmath/check \
	csv/check \
	errors/check \
	exec/check \
	expvar/check \
	flag/check \
	fmt/check \
	gob/check \
	html/check \
	http/check \
	image/check \
	io/check \
	json/check \
	log/check \
	math/check \
	mail/check \
	mime/check \
	net/check \
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
	unicode/check \
	url/check \
	utf16/check \
	utf8/check \
	websocket/check \
	xml/check \
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
	crypto/bcrypt/check \
	crypto/blowfish/check \
	crypto/cast5/check \
	crypto/cipher/check \
	crypto/des/check \
	crypto/dsa/check \
	crypto/ecdsa/check \
	crypto/elliptic/check \
	crypto/hmac/check \
	crypto/md4/check \
	crypto/md5/check \
	crypto/ocsp/check \
	crypto/openpgp/check \
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
	crypto/openpgp/elgamal/check \
	crypto/openpgp/packet/check \
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
	encoding/pem/check \
	exp/ebnf/check \
	$(exp_inotify_check) \
	exp/norm/check \
	exp/spdy/check \
	exp/sql/check \
	exp/ssh/check \
	exp/terminal/check \
	exp/template/html/check \
	go/ast/check \
	$(go_build_check_omitted_since_it_calls_6g) \
	go/parser/check \
	go/printer/check \
	go/scanner/check \
	go/token/check \
	$(go_types_check_omitted_since_it_calls_6g) \
	hash/adler32/check \
	hash/crc32/check \
	hash/crc64/check \
	hash/fnv/check \
	http/cgi/check \
	http/fcgi/check \
	image/draw/check \
	image/jpeg/check \
	image/png/check \
	image/tiff/check \
	image/ycbcr/check \
	index/suffixarray/check \
	io/ioutil/check \
	mime/multipart/check \
	net/textproto/check \
	old/netchan/check \
	old/regexp/check \
	old/template/check \
	os/user/check \
	os/signal/check \
	path/filepath/check \
	regexp/syntax/check \
	rpc/jsonrpc/check \
	sync/atomic/check \
	template/parse/check \
	testing/quick/check \
	testing/script/check

check: check-tail
check-recursive: check-head

check-head:
	@echo "Test Run By $${USER} on `date`" > libgo.head
	@echo "Native configuration is $(host_triplet)" >> libgo.head
	@echo >> libgo.head
	@echo "		=== libgo tests ===" >> libgo.head
	@echo >> libgo.head

check-tail: check-recursive check-multi
	@lib=`${PWD_COMMAND} | sed -e 's,^.*/\([^/][^/]*\)$$,\1,'`; \
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
