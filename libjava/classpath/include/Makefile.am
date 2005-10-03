## GCJ LOCAL: headers are installed specially.
##include_HEADERS = jni.h jni_md.h jawt.h jawt_md.h

DISTCLEANFILES = jni_md.h

ARG_JNI_JAVAH = -jni
ARG_CLASSPATH_JAVAH = -bootclasspath
JAVAH = $(USER_JAVAH) $(ARG_JNI_JAVAH) $(ARG_CLASSPATH_JAVAH) ../lib:$(USER_CLASSLIB)

XMLJ_H_FILES = \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocument.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathNodeList.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocumentType.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeProcessingInstruction.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathNSResolver.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeTypeInfo.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNodeList.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNotation.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathResult.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeElement.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeEntity.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNode.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathExpression.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNamedNodeMap.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocumentBuilder.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeAttr.h \
$(top_srcdir)/include/gnu_xml_libxmlj_sax_GnomeLocator.h \
$(top_srcdir)/include/gnu_xml_libxmlj_sax_GnomeXMLReader.h \
$(top_srcdir)/include/gnu_xml_libxmlj_transform_GnomeTransformer.h \
$(top_srcdir)/include/gnu_xml_libxmlj_transform_GnomeTransformerFactory.h

GTKPEER_H_FILES = \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkFontPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkGraphics.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkGraphics2D.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkPixbufDecoder.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkRobotPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkTextLayout.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkButtonPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCanvasPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCheckboxPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkChoicePeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkClipboard.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkComponentPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkFileDialogPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkFramePeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkGenericPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkImage.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkLabelPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkListPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuBarPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuComponentPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuItemPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkPanelPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkPopupMenuPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkScrollPanePeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkScrollbarPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkSelection.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkTextAreaPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkTextFieldPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkToolkit.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkWindowPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GThreadNativeMethodRunner.h

QTPEER_H_FILES = \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtCheckboxPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtToolkit.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtAudioClip.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtScreenDevice.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtTextFieldPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuItemPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtButtonPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtWindowPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtDialogPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtImage.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuBarPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtVolatileImage.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFontPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtChoicePeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtGraphics.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtComponentPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtTextAreaPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QPen.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtPanelPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QPainterPath.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFontMetrics.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtScrollPanePeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtGraphicsEnvironment.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QMatrix.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtLabelPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtPopupMenuPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFramePeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtListPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtScrollbarPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtCanvasPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuComponentPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_MainQtThread.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFileDialogPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtContainerPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtEmbeddedWindowPeer.h 

H_FILES = \
$(XMLJ_H_FILES) \
$(GTKPEER_H_FILES) \
$(QTPEER_H_FILES) \
$(top_srcdir)/include/gnu_java_net_PlainDatagramSocketImpl.h \
$(top_srcdir)/include/gnu_java_net_PlainSocketImpl.h \
$(top_srcdir)/include/gnu_java_nio_VMPipe.h \
$(top_srcdir)/include/gnu_java_nio_VMSelector.h \
$(top_srcdir)/include/gnu_java_nio_channels_FileChannelImpl.h \
$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvEncoder.h \
$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvDecoder.h \
$(top_srcdir)/include/java_io_VMFile.h \
$(top_srcdir)/include/java_io_VMObjectInputStream.h \
$(top_srcdir)/include/java_io_VMObjectStreamClass.h \
$(top_srcdir)/include/java_lang_Math.h \
$(top_srcdir)/include/java_lang_VMDouble.h \
$(top_srcdir)/include/java_lang_VMFloat.h \
$(top_srcdir)/include/java_lang_VMProcess.h \
$(top_srcdir)/include/java_lang_VMSystem.h \
$(top_srcdir)/include/java_lang_reflect_Array.h \
$(top_srcdir)/include/java_net_VMInetAddress.h \
$(top_srcdir)/include/java_net_VMNetworkInterface.h \
$(top_srcdir)/include/java_nio_VMDirectByteBuffer.h \
$(top_srcdir)/include/java_nio_MappedByteBufferImpl.h \
$(top_srcdir)/include/java_util_VMTimeZone.h

if CREATE_JNI_HEADERS

all-local: $(H_FILES)

$(top_srcdir)/include/gnu_xml_libxmlj_dom_%.h: $(top_builddir)/lib/gnu/xml/libxmlj/dom/%.class
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.$*

$(top_srcdir)/include/gnu_xml_libxmlj_sax_%.h: $(top_builddir)/lib/gnu/xml/libxmlj/sax/%.class
	$(JAVAH) -o $@ gnu.xml.libxmlj.sax.$*

$(top_srcdir)/include/gnu_xml_libxmlj_transform_%.h: $(top_builddir)/lib/gnu/xml/libxmlj/transform/%.class
	$(JAVAH) -o $@ gnu.xml.libxmlj.transform.$*

$(top_srcdir)/include/gnu_java_awt_peer_gtk_%.h: $(top_builddir)/lib/gnu/java/awt/peer/gtk/%.class
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.$*

$(top_srcdir)/include/gnu_java_awt_peer_qt_%.h: $(top_builddir)/lib/gnu/java/awt/peer/qt/%.class
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.$*

$(top_srcdir)/include/gnu_java_net_PlainDatagramSocketImpl.h: $(top_srcdir)/gnu/java/net/PlainDatagramSocketImpl.java
	$(JAVAH) -o $@ gnu.java.net.PlainDatagramSocketImpl
$(top_srcdir)/include/gnu_java_net_PlainSocketImpl.h: $(top_srcdir)/gnu/java/net/PlainSocketImpl.java
	$(JAVAH) -o $@ gnu.java.net.PlainSocketImpl
$(top_srcdir)/include/gnu_java_nio_VMPipe.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMPipe.java
	$(JAVAH) -o $@ gnu.java.nio.VMPipe
$(top_srcdir)/include/gnu_java_nio_VMSelector.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMSelector.java
	$(JAVAH) -o $@ gnu.java.nio.VMSelector
$(top_srcdir)/include/java_io_VMFile.h: $(top_srcdir)/vm/reference/java/io/VMFile.java
	$(JAVAH) -o $@ java.io.VMFile
$(top_srcdir)/include/java_io_VMObjectInputStream.h: $(top_srcdir)/vm/reference/java/io/VMObjectInputStream.java
	$(JAVAH) -o $@ java.io.VMObjectInputStream
$(top_srcdir)/include/java_io_VMObjectStreamClass.h: $(top_srcdir)/vm/reference/java/io/VMObjectStreamClass.java
	$(JAVAH) -o $@ java.io.VMObjectStreamClass
$(top_srcdir)/include/java_lang_Math.h: $(top_srcdir)/java/lang/Math.java
	$(JAVAH) -o $@ java.lang.Math
$(top_srcdir)/include/java_lang_VMDouble.h: $(top_srcdir)/vm/reference/java/lang/VMDouble.java
	$(JAVAH) -o $@ java.lang.VMDouble
$(top_srcdir)/include/java_lang_VMFloat.h: $(top_srcdir)/vm/reference/java/lang/VMFloat.java
	$(JAVAH) -o $@ java.lang.VMFloat
$(top_srcdir)/include/java_lang_VMProcess.h: $(top_srcdir)/vm/reference/java/lang/VMProcess.java
	$(JAVAH) -o $@ java.lang.VMProcess
$(top_srcdir)/include/java_lang_VMSystem.h: $(top_srcdir)/vm/reference/java/lang/VMSystem.java
	$(JAVAH) -o $@ java.lang.VMSystem
$(top_srcdir)/include/java_lang_reflect_Array.h: $(top_srcdir)/java/lang/reflect/Array.java
	$(JAVAH) -o $@ java.lang.reflect.Array
$(top_srcdir)/include/java_net_VMInetAddress.h: $(top_srcdir)/vm/reference/java/net/VMInetAddress.java
	$(JAVAH) -o $@ java.net.VMInetAddress
$(top_srcdir)/include/java_net_VMNetworkInterface.h: $(top_srcdir)/vm/reference/java/net/VMNetworkInterface.java
	$(JAVAH) -o $@ java.net.VMNetworkInterface
$(top_srcdir)/include/java_nio_VMDirectByteBuffer.h: $(top_srcdir)/vm/reference/java/nio/VMDirectByteBuffer.java
	$(JAVAH) -o $@ java.nio.VMDirectByteBuffer
$(top_srcdir)/include/java_nio_MappedByteBufferImpl.h: $(top_srcdir)/java/nio/MappedByteBufferImpl.java
	$(JAVAH) -o $@ java.nio.MappedByteBufferImpl
$(top_srcdir)/include/gnu_java_nio_channels_FileChannelImpl.h: $(top_srcdir)/gnu/java/nio/channels/FileChannelImpl.java
	$(JAVAH) -o $@ gnu.java.nio.channels.FileChannelImpl
$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvDecoder.h: $(top_srcdir)/gnu/java/nio/charset/iconv/IconvDecoder.java
	$(JAVAH) -o $@ gnu.java.nio.charset.iconv.IconvDecoder
$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvEncoder.h: $(top_srcdir)/gnu/java/nio/charset/iconv/IconvEncoder.java
	$(JAVAH) -o $@ gnu.java.nio.charset.iconv.IconvEncoder
$(top_srcdir)/include/java_util_VMTimeZone.h: $(top_srcdir)/vm/reference/java/util/VMTimeZone.java
	$(JAVAH) -o $@ java.util.VMTimeZone

endif # CREATE_JNI_HEADERS
