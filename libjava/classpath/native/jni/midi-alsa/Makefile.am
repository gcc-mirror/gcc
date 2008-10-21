nativeexeclib_LTLIBRARIES = libgjsmalsa.la 

libgjsmalsa_la_SOURCES = gnu_javax_sound_midi_alsa_AlsaMidiSequencerDevice.c \
gnu_javax_sound_midi_alsa_AlsaPortDevice.c \
gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider.c

libgjsmalsa_la_LIBADD = -lasound
libgjsmalsa_la_LDFLAGS = $(AM_LDFLAGS) -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@ `pkg-config --cflags-only-I alsa`
AM_CFLAGS = @WARNING_CFLAGS@ @STRICT_WARNING_CFLAGS@ @ERROR_CFLAGS@ `pkg-config --cflags-only-other alsa` \
            @EXTRA_CFLAGS@
