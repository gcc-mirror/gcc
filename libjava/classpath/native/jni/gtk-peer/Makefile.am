nativeexeclib_LTLIBRARIES = libgtkpeer.la

# GTK JNI sources.
libgtkpeer_la_SOURCES = gnu_java_awt_peer_gtk_CairoSurface.c \
			gnu_java_awt_peer_gtk_CairoGraphics2D.c \
			gnu_java_awt_peer_gtk_ComponentGraphics.c \
			gnu_java_awt_peer_gtk_ComponentGraphicsCopy.c \
			gnu_java_awt_peer_gtk_FreetypeGlyphVector.c \
			gnu_java_awt_peer_gtk_GdkFontPeer.c \
			gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.c \
			gnu_java_awt_peer_gtk_GdkPixbufDecoder.c \
			gnu_java_awt_peer_gtk_GdkRobotPeer.c \
			gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice.c \
			gnu_java_awt_peer_gtk_GtkButtonPeer.c \
			gnu_java_awt_peer_gtk_GtkCanvasPeer.c \
			gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.c \
			gnu_java_awt_peer_gtk_GtkCheckboxPeer.c \
			gnu_java_awt_peer_gtk_GtkChoicePeer.c \
			gnu_java_awt_peer_gtk_GtkClipboard.c \
			gnu_java_awt_peer_gtk_GtkComponentPeer.c \
			gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.c \
			gnu_java_awt_peer_gtk_GtkFileDialogPeer.c \
			gnu_java_awt_peer_gtk_GtkFramePeer.c \
			gnu_java_awt_peer_gtk_GtkGenericPeer.c \
			gnu_java_awt_peer_gtk_GtkImage.c \
			gnu_java_awt_peer_gtk_GtkLabelPeer.c \
			gnu_java_awt_peer_gtk_GtkListPeer.c \
			gnu_java_awt_peer_gtk_GtkMenuBarPeer.c \
			gnu_java_awt_peer_gtk_GtkMenuComponentPeer.c \
			gnu_java_awt_peer_gtk_GtkMenuItemPeer.c \
			gnu_java_awt_peer_gtk_GtkMenuPeer.c \
			gnu_java_awt_peer_gtk_GtkPanelPeer.c \
			gnu_java_awt_peer_gtk_GtkPopupMenuPeer.c \
			gnu_java_awt_peer_gtk_GtkScrollbarPeer.c \
			gnu_java_awt_peer_gtk_GtkScrollPanePeer.c \
			gnu_java_awt_peer_gtk_GtkSelection.c \
			gnu_java_awt_peer_gtk_GtkTextAreaPeer.c \
			gnu_java_awt_peer_gtk_GtkTextFieldPeer.c \
			gnu_java_awt_peer_gtk_GtkToolkit.c \
			gnu_java_awt_peer_gtk_GtkWindowPeer.c \
			gnu_java_awt_peer_gtk_GtkVolatileImage.c \
			GtkDragSourceContextPeer.c \
			cairographics2d.h \
			gdkdisplay.h \
			gdkfont.h \
			gtk_jawt.c \
			gtkpeer.c \
			gtkpeer.h

libgtkpeer_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo
libgtkpeer_la_LDFLAGS = $(AM_LDFLAGS) -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@ @GTK_LIBS@ @FREETYPE2_LIBS@ \
             @PANGOFT2_LIBS@ @X_PRE_LIBS@ @X_LIBS@ @X_EXTRA_LIBS@ @XTEST_LIBS@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@

# Just the WARNING_CFLAGS. We cannot use the strict flags since the gtk
# headers contain broken prototypes (by design, see gtkitemfactory.h).
AM_CFLAGS = @WARNING_CFLAGS@ @ERROR_CFLAGS@ \
            @GTK_CFLAGS@ @FREETYPE2_CFLAGS@ @PANGOFT2_CFLAGS@ \
            @X_CFLAGS@ @EXTRA_CFLAGS@
