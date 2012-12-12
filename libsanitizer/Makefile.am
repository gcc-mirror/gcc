ACLOCAL_AMFLAGS = -I .. -I ../config

if TSAN_SUPPORTED
SUBDIRS = interception sanitizer_common asan tsan 
else
SUBDIRS = interception sanitizer_common asan 
endif

if USING_MAC_INTERPOSE
SUBDIRS = sanitizer_common asan
endif

## ################################################################

