## Makefile for the toplevel directory of the mudflap library.
##
## Copyright (C) 2002, 2003
## Free Software Foundation, Inc.
##

AUTOMAKE_OPTIONS = 1.3 foreign
MAINT_CHARSET = latin1
SUBDIRS = testsuite

AM_CFLAGS = -Wall

if LIBMUDFLAPTH
libmudflapth = libmudflapth.la
else
libmudflapth =
endif

lib_LTLIBRARIES = libmudflap.la $(libmudflapth)
include_HEADERS = mf-runtime.h

libmudflap_la_SOURCES = \
	mf-runtime.c \
	mf-heuristics.c

HOOK1OBJS = \
 malloc-hook.lo \
 free-hook.lo \
 calloc-hook.lo \
 realloc-hook.lo \
 mmap-hook.lo \
 munmap-hook.lo \
 alloca-hook.lo

HOOK2OBJS = \
 memcpy-hook.lo \
 memmove-hook.lo \
 memset-hook.lo \
 memcmp-hook.lo \
 memchr-hook.lo \
 memrchr-hook.lo \
 strcpy-hook.lo \
 strncpy-hook.lo \
 strcat-hook.lo \
 strncat-hook.lo \
 strcmp-hook.lo \
 strcasecmp-hook.lo \
 strncmp-hook.lo \
 strncasecmp-hook.lo \
 strdup-hook.lo \
 strndup-hook.lo \
 strchr-hook.lo \
 strrchr-hook.lo \
 strstr-hook.lo \
 memmem-hook.lo \
 strlen-hook.lo \
 strnlen-hook.lo \
 bzero-hook.lo \
 bcopy-hook.lo \
 bcmp-hook.lo \
 index-hook.lo \
 rindex-hook.lo \
 asctime-hook.lo \
 ctime-hook.lo \
 gmtime-hook.lo \
 localtime-hook.lo \
 time-hook.lo \
 strerror-hook.lo \
 fopen-hook.lo \
 fclose-hook.lo \
 fread-hook.lo \
 fwrite-hook.lo \
 fgetc-hook.lo \
 fgets-hook.lo \
 getc-hook.lo \
 gets-hook.lo \
 ungetc-hook.lo \
 fputc-hook.lo \
 fputs-hook.lo \
 putc-hook.lo \
 puts-hook.lo \
 clearerr-hook.lo \
 feof-hook.lo \
 ferror-hook.lo \
 fileno-hook.lo \
 printf-hook.lo \
 fprintf-hook.lo \
 sprintf-hook.lo \
 snprintf-hook.lo \
 vprintf-hook.lo \
 vfprintf-hook.lo \
 vsprintf-hook.lo \
 vsnprintf-hook.lo \
 access-hook.lo \
 remove-hook.lo \
 fflush-hook.lo \
 fseek-hook.lo \
 ftell-hook.lo \
 rewind-hook.lo \
 fgetpos-hook.lo \
 fsetpos-hook.lo \
 stat-hook.lo \
 fstat-hook.lo \
 lstat-hook.lo \
 mkfifo-hook.lo \
 setvbuf-hook.lo \
 setbuf-hook.lo \
 opendir-hook.lo \
 closedir-hook.lo \
 readdir-hook.lo \
 recv-hook.lo \
 recvfrom-hook.lo \
 recvmsg-hook.lo \
 send-hook.lo \
 sendto-hook.lo \
 sendmsg-hook.lo \
 setsockopt-hook.lo \
 getsockopt-hook.lo \
 accept-hook.lo \
 bind-hook.lo \
 connect-hook.lo \
 gethostname-hook.lo \
 sethostname-hook.lo \
 gethostbyname-hook.lo \
 wait-hook.lo \
 waitpid-hook.lo \
 popen-hook.lo \
 pclose-hook.lo \
 execve-hook.lo \
 execv-hook.lo \
 execvp-hook.lo \
 system-hook.lo \
 dlopen-hook.lo \
 dlerror-hook.lo \
 dlsym-hook.lo \
 dlclose-hook.lo \
 fopen64-hook.lo \
 stat64-hook.lo \
 fseeko64-hook.lo \
 ftello64-hook.lo \
 semop-hook.lo \
 semctl-hook.lo \
 shmctl-hook.lo \
 shmat-hook.lo \
 shmdt-hook.lo
HOOK3OBJS = 

$(HOOK1OBJS): mf-hooks1.c mf-runtime.h mf-impl.h
	hook=`basename $@ -hook.lo`; \
	$(LTCOMPILE) -DWRAP_$$hook -c $(srcdir)/mf-hooks1.c -o $@
$(HOOK2OBJS): mf-hooks2.c mf-runtime.h mf-impl.h
	hook=`basename $@ -hook.lo`; \
	$(LTCOMPILE) -DWRAP_$$hook -c $(srcdir)/mf-hooks2.c -o $@
$(HOOK3OBJS): mf-hooks3.c mf-runtime.h mf-impl.h
	hook=`basename $@ -hook.lo`; \
	$(LTCOMPILE) -DWRAP_$$hook -c $(srcdir)/mf-hooks3.c -o $@

# Hook objects only for libmudflapth use 
PTHHOOK1OBJS= \
 pth/malloc-hook.lo \
 pth/free-hook.lo \
 pth/calloc-hook.lo \
 pth/realloc-hook.lo \
 pth/mmap-hook.lo \
 pth/munmap-hook.lo \
 pth/alloca-hook.lo

PTHHOOK2OBJS= \
 pth/memcpy-hook.lo \
 pth/memmove-hook.lo \
 pth/memset-hook.lo \
 pth/memcmp-hook.lo \
 pth/memchr-hook.lo \
 pth/memrchr-hook.lo \
 pth/strcpy-hook.lo \
 pth/strncpy-hook.lo \
 pth/strcat-hook.lo \
 pth/strncat-hook.lo \
 pth/strcmp-hook.lo \
 pth/strcasecmp-hook.lo \
 pth/strncmp-hook.lo \
 pth/strncasecmp-hook.lo \
 pth/strdup-hook.lo \
 pth/strndup-hook.lo \
 pth/strchr-hook.lo \
 pth/strrchr-hook.lo \
 pth/strstr-hook.lo \
 pth/memmem-hook.lo \
 pth/strlen-hook.lo \
 pth/strnlen-hook.lo \
 pth/bzero-hook.lo \
 pth/bcopy-hook.lo \
 pth/bcmp-hook.lo \
 pth/index-hook.lo \
 pth/rindex-hook.lo \
 pth/asctime-hook.lo \
 pth/ctime-hook.lo \
 pth/gmtime-hook.lo \
 pth/localtime-hook.lo \
 pth/time-hook.lo \
 pth/strerror-hook.lo \
 pth/fopen-hook.lo \
 pth/fclose-hook.lo \
 pth/fread-hook.lo \
 pth/fwrite-hook.lo \
 pth/fgetc-hook.lo \
 pth/fgets-hook.lo \
 pth/getc-hook.lo \
 pth/gets-hook.lo \
 pth/ungetc-hook.lo \
 pth/fputc-hook.lo \
 pth/fputs-hook.lo \
 pth/putc-hook.lo \
 pth/puts-hook.lo \
 pth/clearerr-hook.lo \
 pth/feof-hook.lo \
 pth/ferror-hook.lo \
 pth/fileno-hook.lo \
 pth/printf-hook.lo \
 pth/fprintf-hook.lo \
 pth/sprintf-hook.lo \
 pth/snprintf-hook.lo \
 pth/vprintf-hook.lo \
 pth/vfprintf-hook.lo \
 pth/vsprintf-hook.lo \
 pth/vsnprintf-hook.lo \
 pth/access-hook.lo \
 pth/remove-hook.lo \
 pth/fflush-hook.lo \
 pth/fseek-hook.lo \
 pth/ftell-hook.lo \
 pth/rewind-hook.lo \
 pth/fgetpos-hook.lo \
 pth/fsetpos-hook.lo \
 pth/stat-hook.lo \
 pth/fstat-hook.lo \
 pth/lstat-hook.lo \
 pth/mkfifo-hook.lo \
 pth/setvbuf-hook.lo \
 pth/setbuf-hook.lo \
 pth/opendir-hook.lo \
 pth/closedir-hook.lo \
 pth/readdir-hook.lo \
 pth/recv-hook.lo \
 pth/recvfrom-hook.lo \
 pth/recvmsg-hook.lo \
 pth/send-hook.lo \
 pth/sendto-hook.lo \
 pth/sendmsg-hook.lo \
 pth/setsockopt-hook.lo \
 pth/getsockopt-hook.lo \
 pth/accept-hook.lo \
 pth/bind-hook.lo \
 pth/connect-hook.lo \
 pth/gethostname-hook.lo \
 pth/sethostname-hook.lo \
 pth/gethostbyname-hook.lo \
 pth/wait-hook.lo \
 pth/waitpid-hook.lo \
 pth/popen-hook.lo \
 pth/pclose-hook.lo \
 pth/execve-hook.lo \
 pth/execv-hook.lo \
 pth/execvp-hook.lo \
 pth/system-hook.lo \
 pth/dlopen-hook.lo \
 pth/dlerror-hook.lo \
 pth/dlsym-hook.lo \
 pth/dlclose-hook.lo \
 pth/fopen64-hook.lo \
 pth/stat64-hook.lo \
 pth/fseeko64-hook.lo \
 pth/ftello64-hook.lo \
 pth/semop-hook.lo \
 pth/semctl-hook.lo \
 pth/shmctl-hook.lo \
 pth/shmat-hook.lo \
 pth/shmdt-hook.lo

PTHHOOK3OBJS= \
 pth/pthreadstuff-hook.lo


clean-local:
	rm -f pth/*.o pth/*.lo

libmudflapth_la_SOURCES =
pth/mf-runtime.lo: mf-runtime.c mf-runtime.h mf-impl.h
	$(LTCOMPILE) -DLIBMUDFLAPTH -c $(srcdir)/mf-runtime.c -o $@
pth/mf-heuristics.lo: mf-heuristics.c mf-runtime.h mf-impl.h
	$(LTCOMPILE) -DLIBMUDFLAPTH -c $(srcdir)/mf-heuristics.c -o $@
$(PTHHOOK1OBJS): mf-hooks1.c mf-runtime.h mf-impl.h
	hook=`basename $@ -hook.lo`; \
	$(LTCOMPILE) -DLIBMUDFLAPTH -DWRAP_$$hook -c $(srcdir)/mf-hooks1.c -o $@
$(PTHHOOK2OBJS): mf-hooks2.c mf-runtime.h mf-impl.h
	hook=`basename $@ -hook.lo`; \
	$(LTCOMPILE) -DLIBMUDFLAPTH -DWRAP_$$hook -c $(srcdir)/mf-hooks2.c -o $@
$(PTHHOOK3OBJS): mf-hooks3.c mf-runtime.h mf-impl.h
	hook=`basename $@ -hook.lo`; \
	$(LTCOMPILE) -DLIBMUDFLAPTH -DWRAP_$$hook -c $(srcdir)/mf-hooks3.c -o $@

libmudflap_la_LIBADD = $(HOOK1OBJS) $(HOOK2OBJS) $(HOOK3OBJS)
libmudflap_la_DEPENDENCIES = $(libmudflap_la_LIBADD)

libmudflapth_la_LIBADD = pth/mf-runtime.lo pth/mf-heuristics.lo \
	$(PTHHOOK1OBJS) $(PTHHOOK2OBJS) $(PTHHOOK3OBJS)
libmudflapth_la_DEPENDENCIES = $(libmudflapth_la_LIBADD)



# XXX hack alert
# From libffi/Makefile.am

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"JC1FLAGS=$(JC1FLAGS)" \
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
	"prefix=$(prefix)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)"

MAKEOVERRIDES=

# Multilib support variables.
MULTISRCTOP =
MULTIBUILDTOP =
MULTIDIRS =
MULTISUBDIR =
MULTIDO = true
MULTICLEAN = true

# Multilib support.
.PHONY: all-multi mostlyclean-multi clean-multi distclean-multi \
	maintainer-clean-multi

all-recursive: all-multi
install-recursive: install-multi
mostlyclean-recursive: mostlyclean-multi
clean-recursive: clean-multi
distclean-recursive: distclean-multi
maintainer-clean-recursive: maintainer-clean-multi

all-multi:
	: $(MAKE) ; exec $(MULTIDO) $(AM_MAKEFLAGS) DO=all multi-do
install-multi:
	$(MULTIDO) $(AM_MAKEFLAGS) DO=install multi-do
mostlyclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=mostlyclean multi-clean
clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=clean multi-clean
distclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=distclean multi-clean
maintainer-clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=maintainer-clean multi-clean


## ################################################################

