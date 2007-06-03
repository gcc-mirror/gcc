# Qt AWT backend for Classpath
#

## GCJ LOCAL: don't install this library
noinst_LTLIBRARIES = libqtpeer.la

AM_LDFLAGS = @CLASSPATH_MODULE@ @QT_LIBS@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@

## GCJ LOCAL: add libstdc++-v3 include directories
AM_CXXFLAGS = @QT_CFLAGS@ \
	-I$(top_builddir)/../../libstdc++-v3/include/$(target_alias) \
	-I$(top_builddir)/../../libstdc++-v3/include \
	-I$(top_srcdir)/../../libstdc++-v3/include \
	-I$(top_srcdir)/../../libstdc++-v3/libsupc++

libqtpeer_la_MOC = \
	slotcallbacks.moc.h

slotcallbacks.moc.h: slotcallbacks.cpp
	$(MOC) -o slotcallbacks.moc.h $(srcdir)/slotcallbacks.cpp

nodist_libqtpeer_la_SOURCES = \
	$(libqtpeer_la_MOC) 

libqtpeer_la_SOURCES = \
        buttonevent.h \
        componentevent.cpp \
        componentevent.h \
        containers.h \
        eventmethods.h \
        keybindings.cpp \
        keybindings.h \
        mainqtthread.cpp \
        mainthreadinterface.cpp \
        mainthreadinterface.h \
        nativewrapper.cpp \
        nativewrapper.h \
        qmatrix.cpp \
        qpainterpath.cpp \
        qpen.cpp \
        qtaudioclip.cpp \
        qtbuttonpeer.cpp \
        qtcanvaspeer.cpp \
        qtcheckboxpeer.cpp \
        qtchoicepeer.cpp \
        qtcomponent.cpp \
        qtcomponent.h \
        qtcomponentpeer.cpp \
        qtdialogpeer.cpp \
	qtembeddedwindowpeer.cpp \
        qtfiledialogpeer.cpp \
        qtfont.h \
        qtfontmetrics.cpp \
        qtfontpeer.cpp \
        qtframepeer.cpp \
        qtgraphics.cpp \
        qtgraphics.h \
        qtimage.cpp \
        qtimage.h \
        qtlabelpeer.cpp \
        qtlistpeer.cpp \
        qtmenubarpeer.cpp \
        qtmenucomponentpeer.cpp \
        qtmenuitempeer.cpp \
        qtmenupeer.cpp \
        qtpanelpeer.cpp \
        qtpopupmenupeer.cpp \
        qtscreendevice.cpp \
        qtscrollbarpeer.cpp \
        qtscrollpanepeer.cpp \
        qtstrings.cpp \
        qtstrings.h \
        qttextareapeer.cpp \
        qttextfieldpeer.cpp \
        qttoolkit.cpp \
        qtvolatileimage.cpp \
        qtwindowpeer.cpp \
        slotcallbacks.cpp \
        slotcallbacks.h 
libqtpeer_la_LDFLAGS = $(AM_LDFLAGS) -avoid-version

BUILT_SOURCES = $(libqtpeer_la_MOC)

CLEANFILES = so_locations $(BUILT_SOURCES)
