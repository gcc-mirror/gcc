nativelib_LTLIBRARIES = libgjsmdssi.la 

libgjsmdssi_la_SOURCES = gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.c \
			 gnu_javax_sound_midi_dssi_DSSISynthesizer.c \
			 dssi_data.h

libgjsmdssi_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo -ljack

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@ 
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@
