## GCJ LOCAL: headers are installed specially.
##include_HEADERS = jni.h jni_md.h jawt.h jawt_md.h

if CREATE_JNI_HEADERS
DISTCLEANFILES = jni_md.h config-int.h $(H_FILES)
else
DISTCLEANFILES = jni_md.h config-int.h
endif

ARG_JNI_JAVAH = -jni
ARG_CLASSPATH_JAVAH = -bootclasspath
JAVAH = $(USER_JAVAH) $(ARG_JNI_JAVAH) $(ARG_CLASSPATH_JAVAH) ../lib:$(PATH_TO_GLIBJ_ZIP)
CLASSDIR = lib

SOUND_H_FILES = \
gnu_javax_sound_midi_alsa_AlsaPortDevice.h \
gnu_javax_sound_midi_alsa_AlsaMidiSequencerDevice.h \
gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider.h \
gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.h \
gnu_javax_sound_midi_dssi_DSSISynthesizer.h

GST_PEER_H_FILES = \
gnu_javax_sound_sampled_gstreamer_io_GstAudioFileReaderNativePeer.h \
gnu_javax_sound_sampled_gstreamer_io_GstInputStream.h \
gnu_javax_sound_sampled_gstreamer_lines_GstNativeDataLine.h \
gnu_javax_sound_sampled_gstreamer_lines_GstPipeline.h

XMLJ_H_FILES = \
gnu_xml_libxmlj_dom_GnomeDocument.h \
gnu_xml_libxmlj_dom_GnomeXPathNodeList.h \
gnu_xml_libxmlj_dom_GnomeDocumentType.h \
gnu_xml_libxmlj_dom_GnomeProcessingInstruction.h \
gnu_xml_libxmlj_dom_GnomeTypeInfo.h \
gnu_xml_libxmlj_dom_GnomeNodeList.h \
gnu_xml_libxmlj_dom_GnomeNotation.h \
gnu_xml_libxmlj_dom_GnomeXPathResult.h \
gnu_xml_libxmlj_dom_GnomeElement.h \
gnu_xml_libxmlj_dom_GnomeEntity.h \
gnu_xml_libxmlj_dom_GnomeNode.h \
gnu_xml_libxmlj_dom_GnomeXPathExpression.h \
gnu_xml_libxmlj_dom_GnomeNamedNodeMap.h \
gnu_xml_libxmlj_dom_GnomeDocumentBuilder.h \
gnu_xml_libxmlj_dom_GnomeAttr.h \
gnu_xml_libxmlj_sax_GnomeLocator.h \
gnu_xml_libxmlj_sax_GnomeXMLReader.h \
gnu_xml_libxmlj_transform_GnomeTransformer.h \
gnu_xml_libxmlj_transform_GnomeTransformerFactory.h

GTKPEER_H_FILES = \
gnu_java_awt_peer_gtk_CairoSurface.h \
gnu_java_awt_peer_gtk_CairoGraphics2D.h \
gnu_java_awt_peer_gtk_ComponentGraphics.h \
gnu_java_awt_peer_gtk_ComponentGraphicsCopy.h \
gnu_java_awt_peer_gtk_FreetypeGlyphVector.h \
gnu_java_awt_peer_gtk_GdkFontPeer.h \
gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h \
gnu_java_awt_peer_gtk_GdkPixbufDecoder.h \
gnu_java_awt_peer_gtk_GdkRobotPeer.h \
gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice.h \
gnu_java_awt_peer_gtk_GtkButtonPeer.h \
gnu_java_awt_peer_gtk_GtkCanvasPeer.h \
gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.h \
gnu_java_awt_peer_gtk_GtkCheckboxPeer.h \
gnu_java_awt_peer_gtk_GtkChoicePeer.h \
gnu_java_awt_peer_gtk_GtkClipboard.h \
gnu_java_awt_peer_gtk_GtkComponentPeer.h \
gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.h \
gnu_java_awt_peer_gtk_GtkFileDialogPeer.h \
gnu_java_awt_peer_gtk_GtkFramePeer.h \
gnu_java_awt_peer_gtk_GtkGenericPeer.h \
gnu_java_awt_peer_gtk_GtkImage.h \
gnu_java_awt_peer_gtk_GtkLabelPeer.h \
gnu_java_awt_peer_gtk_GtkListPeer.h \
gnu_java_awt_peer_gtk_GtkMenuBarPeer.h \
gnu_java_awt_peer_gtk_GtkMenuComponentPeer.h \
gnu_java_awt_peer_gtk_GtkMenuItemPeer.h \
gnu_java_awt_peer_gtk_GtkMenuPeer.h \
gnu_java_awt_peer_gtk_GtkPanelPeer.h \
gnu_java_awt_peer_gtk_GtkPopupMenuPeer.h \
gnu_java_awt_peer_gtk_GtkScrollPanePeer.h \
gnu_java_awt_peer_gtk_GtkScrollbarPeer.h \
gnu_java_awt_peer_gtk_GtkSelection.h \
gnu_java_awt_peer_gtk_GtkTextAreaPeer.h \
gnu_java_awt_peer_gtk_GtkTextFieldPeer.h \
gnu_java_awt_peer_gtk_GtkToolkit.h \
gnu_java_awt_peer_gtk_GtkWindowPeer.h \
gnu_java_awt_peer_gtk_GtkVolatileImage.h \
gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer.h

QTPEER_H_FILES = \
gnu_java_awt_peer_qt_QtCheckboxPeer.h \
gnu_java_awt_peer_qt_QtToolkit.h \
gnu_java_awt_peer_qt_QtAudioClip.h \
gnu_java_awt_peer_qt_QtScreenDevice.h \
gnu_java_awt_peer_qt_QtTextFieldPeer.h \
gnu_java_awt_peer_qt_QtMenuItemPeer.h \
gnu_java_awt_peer_qt_QtButtonPeer.h \
gnu_java_awt_peer_qt_QtWindowPeer.h \
gnu_java_awt_peer_qt_QtDialogPeer.h \
gnu_java_awt_peer_qt_QtImage.h \
gnu_java_awt_peer_qt_QtMenuBarPeer.h \
gnu_java_awt_peer_qt_QtVolatileImage.h \
gnu_java_awt_peer_qt_QtFontPeer.h \
gnu_java_awt_peer_qt_QtChoicePeer.h \
gnu_java_awt_peer_qt_QtGraphics.h \
gnu_java_awt_peer_qt_QtComponentPeer.h \
gnu_java_awt_peer_qt_QtMenuPeer.h \
gnu_java_awt_peer_qt_QtTextAreaPeer.h \
gnu_java_awt_peer_qt_QPen.h \
gnu_java_awt_peer_qt_QtPanelPeer.h \
gnu_java_awt_peer_qt_QPainterPath.h \
gnu_java_awt_peer_qt_QtFontMetrics.h \
gnu_java_awt_peer_qt_QtScrollPanePeer.h \
gnu_java_awt_peer_qt_QMatrix.h \
gnu_java_awt_peer_qt_QtLabelPeer.h \
gnu_java_awt_peer_qt_QtPopupMenuPeer.h \
gnu_java_awt_peer_qt_QtFramePeer.h \
gnu_java_awt_peer_qt_QtListPeer.h \
gnu_java_awt_peer_qt_QtScrollbarPeer.h \
gnu_java_awt_peer_qt_QtCanvasPeer.h \
gnu_java_awt_peer_qt_QtMenuComponentPeer.h \
gnu_java_awt_peer_qt_MainQtThread.h \
gnu_java_awt_peer_qt_QtFileDialogPeer.h \
gnu_java_awt_peer_qt_QtEmbeddedWindowPeer.h 

GCONF_PREFS_FILES = \
gnu_java_util_prefs_gconf_GConfNativePeer.h

H_FILES = \
$(SOUND_H_FILES) \
$(GST_PEER_H_FILES) \
$(XMLJ_H_FILES) \
$(GTKPEER_H_FILES) \
$(QTPEER_H_FILES) \
$(GCONF_PREFS_FILES) \
gnu_java_math_GMP.h \
gnu_java_net_VMPlainSocketImpl.h \
gnu_java_net_local_LocalSocketImpl.h \
gnu_java_nio_EpollSelectorImpl.h \
gnu_java_nio_FileChannelImpl.h \
gnu_java_nio_KqueueSelectorImpl.h \
gnu_java_nio_VMChannel.h \
gnu_java_nio_VMPipe.h \
gnu_java_nio_VMSelector.h \
gnu_java_nio_charset_iconv_IconvEncoder.h \
gnu_java_nio_charset_iconv_IconvDecoder.h \
gnu_java_lang_management_VMOperatingSystemMXBeanImpl.h \
java_io_VMConsole.h \
java_io_VMFile.h \
java_io_VMObjectInputStream.h \
java_io_VMObjectStreamClass.h \
java_lang_VMDouble.h \
java_lang_VMFloat.h \
java_lang_VMMath.h \
java_lang_VMProcess.h \
java_lang_VMSystem.h \
java_lang_reflect_VMArray.h \
java_net_VMInetAddress.h \
java_net_VMNetworkInterface.h \
java_net_VMURLConnection.h \
java_nio_VMDirectByteBuffer.h \
java_nio_MappedByteBufferImpl.h \
java_util_VMTimeZone.h

if CREATE_JNI_HEADERS

EXTRA_DIST=$(H_FILES)

all-local: $(H_FILES)

gnu_xml_libxmlj_dom_GnomeDocument.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeDocument.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeDocument
gnu_xml_libxmlj_dom_GnomeXPathNodeList.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeXPathNodeList.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeXPathNodeList
gnu_xml_libxmlj_dom_GnomeDocumentType.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeDocumentType.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeDocumentType
gnu_xml_libxmlj_dom_GnomeProcessingInstruction.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeProcessingInstruction.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeProcessingInstruction
gnu_xml_libxmlj_dom_GnomeTypeInfo.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeTypeInfo.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeTypeInfo
gnu_xml_libxmlj_dom_GnomeNodeList.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNodeList.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNodeList
gnu_xml_libxmlj_dom_GnomeNotation.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNotation.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNotation
gnu_xml_libxmlj_dom_GnomeXPathResult.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeXPathResult.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeXPathResult
gnu_xml_libxmlj_dom_GnomeElement.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeElement.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeElement
gnu_xml_libxmlj_dom_GnomeEntity.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeEntity.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeEntity
gnu_xml_libxmlj_dom_GnomeNode.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNode.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNode
gnu_xml_libxmlj_dom_GnomeXPathExpression.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeXPathExpression.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeXPathExpression
gnu_xml_libxmlj_dom_GnomeNamedNodeMap.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNamedNodeMap.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNamedNodeMap
gnu_xml_libxmlj_dom_GnomeDocumentBuilder.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeDocumentBuilder.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeDocumentBuilder
gnu_xml_libxmlj_dom_GnomeAttr.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeAttr.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeAttr
gnu_xml_libxmlj_sax_GnomeLocator.h: $(top_srcdir)/gnu/xml/libxmlj/sax/GnomeLocator.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.sax.GnomeLocator
gnu_xml_libxmlj_sax_GnomeXMLReader.h: $(top_srcdir)/gnu/xml/libxmlj/sax/GnomeXMLReader.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.sax.GnomeXMLReader
gnu_xml_libxmlj_transform_GnomeTransformer.h: $(top_srcdir)/gnu/xml/libxmlj/transform/GnomeTransformer.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.transform.GnomeTransformer
gnu_xml_libxmlj_transform_GnomeTransformerFactory.h: $(top_srcdir)/gnu/xml/libxmlj/transform/GnomeTransformerFactory.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.transform.GnomeTransformerFactory
gnu_java_awt_peer_gtk_CairoSurface.h: $(top_srcdir)/gnu/java/awt/peer/gtk/CairoSurface.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.CairoSurface
gnu_java_awt_peer_gtk_CairoGraphics2D.h: $(top_srcdir)/gnu/java/awt/peer/gtk/CairoGraphics2D.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.CairoGraphics2D
gnu_java_awt_peer_gtk_ComponentGraphics.h: $(top_srcdir)/gnu/java/awt/peer/gtk/ComponentGraphics.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.ComponentGraphics
gnu_java_awt_peer_gtk_ComponentGraphicsCopy.h: $(top_srcdir)/gnu/java/awt/peer/gtk/ComponentGraphicsCopy.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.ComponentGraphicsCopy
gnu_java_awt_peer_gtk_FreetypeGlyphVector.h: $(top_srcdir)/gnu/java/awt/peer/gtk/FreetypeGlyphVector.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.FreetypeGlyphVector
gnu_java_awt_peer_gtk_GdkFontPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkFontPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkFontPeer
gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkGraphicsEnvironment.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkGraphicsEnvironment
gnu_java_awt_peer_gtk_GdkPixbufDecoder.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkPixbufDecoder.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkPixbufDecoder
gnu_java_awt_peer_gtk_GdkRobotPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkRobotPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkRobotPeer
gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkScreenGraphicsDevice.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkScreenGraphicsDevice
gnu_java_awt_peer_gtk_GtkButtonPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkButtonPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkButtonPeer
gnu_java_awt_peer_gtk_GtkCanvasPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkCanvasPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkCanvasPeer
gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkCheckboxMenuItemPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkCheckboxMenuItemPeer
gnu_java_awt_peer_gtk_GtkCheckboxPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkCheckboxPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkCheckboxPeer
gnu_java_awt_peer_gtk_GtkChoicePeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkChoicePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkChoicePeer
gnu_java_awt_peer_gtk_GtkClipboard.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkClipboard.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkClipboard
gnu_java_awt_peer_gtk_GtkComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkComponentPeer
gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkEmbeddedWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkEmbeddedWindowPeer
gnu_java_awt_peer_gtk_GtkFileDialogPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkFileDialogPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkFileDialogPeer
gnu_java_awt_peer_gtk_GtkFramePeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkFramePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkFramePeer
gnu_java_awt_peer_gtk_GtkGenericPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkGenericPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkGenericPeer
gnu_java_awt_peer_gtk_GtkImage.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkImage
gnu_java_awt_peer_gtk_GtkLabelPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkLabelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkLabelPeer
gnu_java_awt_peer_gtk_GtkListPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkListPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkListPeer
gnu_java_awt_peer_gtk_GtkMenuBarPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuBarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuBarPeer
gnu_java_awt_peer_gtk_GtkMenuComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuComponentPeer
gnu_java_awt_peer_gtk_GtkMenuItemPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuItemPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuItemPeer
gnu_java_awt_peer_gtk_GtkMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuPeer
gnu_java_awt_peer_gtk_GtkPanelPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkPanelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkPanelPeer
gnu_java_awt_peer_gtk_GtkPopupMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkPopupMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkPopupMenuPeer
gnu_java_awt_peer_gtk_GtkScrollPanePeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkScrollPanePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkScrollPanePeer
gnu_java_awt_peer_gtk_GtkScrollbarPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkScrollbarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkScrollbarPeer
gnu_java_awt_peer_gtk_GtkSelection.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkSelection.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkSelection
gnu_java_awt_peer_gtk_GtkTextAreaPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkTextAreaPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkTextAreaPeer
gnu_java_awt_peer_gtk_GtkTextFieldPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkTextFieldPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkTextFieldPeer
gnu_java_awt_peer_gtk_GtkToolkit.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkToolkit.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkToolkit
gnu_java_awt_peer_gtk_GtkWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkWindowPeer
gnu_java_awt_peer_gtk_GtkVolatileImage.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkVolatileImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkVolatileImage
gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer.h: $(top_srcdir)/gnu/java/awt/dnd/peer/gtk/GtkDragSourceContextPeer.java
	$(JAVAH) -o $@ gnu.java.awt.dnd.peer.gtk.GtkDragSourceContextPeer
gnu_java_awt_peer_qt_QtCheckboxPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtCheckboxPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtCheckboxPeer
gnu_java_awt_peer_qt_QtToolkit.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtToolkit.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtToolkit
gnu_java_awt_peer_qt_QtAudioClip.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtAudioClip.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtAudioClip
gnu_java_awt_peer_qt_QtScreenDevice.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtScreenDevice.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtScreenDevice
gnu_java_awt_peer_qt_QtTextFieldPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtTextFieldPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtTextFieldPeer
gnu_java_awt_peer_qt_QtMenuItemPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuItemPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuItemPeer
gnu_java_awt_peer_qt_QtButtonPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtButtonPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtButtonPeer
gnu_java_awt_peer_qt_QtWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtWindowPeer
gnu_java_awt_peer_qt_QtDialogPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtDialogPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtDialogPeer
gnu_java_awt_peer_qt_QtImage.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtImage
gnu_java_awt_peer_qt_QtMenuBarPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuBarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuBarPeer
gnu_java_awt_peer_qt_QtVolatileImage.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtVolatileImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtVolatileImage
gnu_java_awt_peer_qt_QtFontPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFontPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFontPeer
gnu_java_awt_peer_qt_QtChoicePeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtChoicePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtChoicePeer
gnu_java_awt_peer_qt_QtGraphics.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtGraphics.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtGraphics
gnu_java_awt_peer_qt_QtComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtComponentPeer
gnu_java_awt_peer_qt_QtMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuPeer
gnu_java_awt_peer_qt_QtTextAreaPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtTextAreaPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtTextAreaPeer
gnu_java_awt_peer_qt_QPen.h: $(top_srcdir)/gnu/java/awt/peer/qt/QPen.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QPen
gnu_java_awt_peer_qt_QtPanelPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtPanelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtPanelPeer
gnu_java_awt_peer_qt_QPainterPath.h: $(top_srcdir)/gnu/java/awt/peer/qt/QPainterPath.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QPainterPath
gnu_java_awt_peer_qt_QtFontMetrics.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFontMetrics.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFontMetrics
gnu_java_awt_peer_qt_QtScrollPanePeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtScrollPanePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtScrollPanePeer
gnu_java_awt_peer_qt_QMatrix.h: $(top_srcdir)/gnu/java/awt/peer/qt/QMatrix.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QMatrix
gnu_java_awt_peer_qt_QtLabelPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtLabelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtLabelPeer
gnu_java_awt_peer_qt_QtPopupMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtPopupMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtPopupMenuPeer
gnu_java_awt_peer_qt_QtFramePeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFramePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFramePeer
gnu_java_awt_peer_qt_QtListPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtListPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtListPeer
gnu_java_awt_peer_qt_QtScrollbarPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtScrollbarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtScrollbarPeer
gnu_java_awt_peer_qt_QtCanvasPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtCanvasPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtCanvasPeer
gnu_java_awt_peer_qt_QtMenuComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuComponentPeer
gnu_java_awt_peer_qt_MainQtThread.h: $(top_srcdir)/gnu/java/awt/peer/qt/MainQtThread.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.MainQtThread
gnu_java_awt_peer_qt_QtFileDialogPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFileDialogPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFileDialogPeer
gnu_java_awt_peer_qt_QtEmbeddedWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtEmbeddedWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtEmbeddedWindowPeer
gnu_javax_sound_midi_alsa_AlsaPortDevice.h: $(top_srcdir)/gnu/javax/sound/midi/alsa/AlsaPortDevice.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.alsa.AlsaPortDevice
gnu_javax_sound_midi_alsa_AlsaMidiSequencerDevice.h: $(top_srcdir)/gnu/javax/sound/midi/alsa/AlsaMidiSequencerDevice.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.alsa.AlsaMidiSequencerDevice
gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider.h: $(top_srcdir)/gnu/javax/sound/midi/alsa/AlsaMidiDeviceProvider.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.alsa.AlsaMidiDeviceProvider
gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.h: $(top_srcdir)/gnu/javax/sound/midi/dssi/DSSIMidiDeviceProvider.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.dssi.DSSIMidiDeviceProvider
gnu_javax_sound_midi_dssi_DSSISynthesizer.h: $(top_srcdir)/gnu/javax/sound/midi/dssi/DSSISynthesizer.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.dssi.DSSISynthesizer
gnu_java_util_prefs_gconf_GConfNativePeer.h: $(top_srcdir)/gnu/java/util/prefs/gconf/GConfNativePeer.java
	$(JAVAH) -o $@ gnu.java.util.prefs.gconf.GConfNativePeer
gnu_javax_sound_sampled_gstreamer_io_GstAudioFileReaderNativePeer.h: $(top_builddir)/gnu/javax/sound/sampled/gstreamer/io/GstAudioFileReaderNativePeer.java
	$(JAVAH) -o $@ gnu.javax.sound.sampled.gstreamer.io.GstAudioFileReaderNativePeer
gnu_javax_sound_sampled_gstreamer_io_GstInputStream.h: $(top_builddir)/gnu/javax/sound/sampled/gstreamer/io/GstInputStream.java
	$(JAVAH) -o $@ gnu.javax.sound.sampled.gstreamer.io.GstInputStream
gnu_javax_sound_sampled_gstreamer_lines_GstNativeDataLine.h: $(top_builddir)/gnu/javax/sound/sampled/gstreamer/lines/GstNativeDataLine.java
	$(JAVAH) -o $@ gnu.javax.sound.sampled.gstreamer.lines.GstNativeDataLine
gnu_javax_sound_sampled_gstreamer_lines_GstPipeline.h: $(top_builddir)/gnu/javax/sound/sampled/gstreamer/lines/GstPipeline.java
	$(JAVAH) -o $@ gnu.javax.sound.sampled.gstreamer.lines.GstPipeline
gnu_java_math_GMP.h: $(top_srcdir)/gnu/java/math/GMP.java
	$(JAVAH) -o $@ gnu.java.math.GMP
gnu_java_net_VMPlainSocketImpl.h: $(top_srcdir)/vm/reference/gnu/java/net/VMPlainSocketImpl.java
	$(JAVAH) -o $@ gnu.java.net.VMPlainSocketImpl
gnu_java_net_local_LocalSocketImpl.h: $(top_srcdir)/gnu/java/net/local/LocalSocketImpl.java
	$(JAVAH) -o $@ gnu.java.net.local.LocalSocketImpl
gnu_java_nio_VMChannel.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMChannel.java
	$(JAVAH) -o $@ gnu.java.nio.VMChannel
gnu_java_nio_VMPipe.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMPipe.java
	$(JAVAH) -o $@ gnu.java.nio.VMPipe
gnu_java_nio_VMSelector.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMSelector.java
	$(JAVAH) -o $@ gnu.java.nio.VMSelector
gnu_java_lang_management_VMOperatingSystemMXBeanImpl.h: $(top_srcdir)/vm/reference/gnu/java/lang/management/VMOperatingSystemMXBeanImpl.java
	$(JAVAH) -o $@ gnu.java.lang.management.VMOperatingSystemMXBeanImpl
java_io_VMConsole.h: $(top_srcdir)/vm/reference/java/io/VMConsole.java
	$(JAVAH) -o $@ java.io.VMConsole
java_io_VMFile.h: $(top_srcdir)/vm/reference/java/io/VMFile.java
	$(JAVAH) -o $@ java.io.VMFile
java_io_VMObjectInputStream.h: $(top_srcdir)/vm/reference/java/io/VMObjectInputStream.java
	$(JAVAH) -o $@ java.io.VMObjectInputStream
java_io_VMObjectStreamClass.h: $(top_srcdir)/vm/reference/java/io/VMObjectStreamClass.java
	$(JAVAH) -o $@ java.io.VMObjectStreamClass
java_lang_VMMath.h: $(top_srcdir)/vm/reference/java/lang/VMMath.java
	$(JAVAH) -o $@ java.lang.VMMath
java_lang_VMDouble.h: $(top_srcdir)/vm/reference/java/lang/VMDouble.java
	$(JAVAH) -o $@ java.lang.VMDouble
java_lang_VMFloat.h: $(top_srcdir)/vm/reference/java/lang/VMFloat.java
	$(JAVAH) -o $@ java.lang.VMFloat
java_lang_VMProcess.h: $(top_srcdir)/vm/reference/java/lang/VMProcess.java
	$(JAVAH) -o $@ java.lang.VMProcess
java_lang_VMSystem.h: $(top_srcdir)/vm/reference/java/lang/VMSystem.java
	$(JAVAH) -o $@ java.lang.VMSystem
java_lang_reflect_VMArray.h: $(top_srcdir)/vm/reference/java/lang/reflect/VMArray.java
	$(JAVAH) -o $@ java.lang.reflect.VMArray
java_net_VMInetAddress.h: $(top_srcdir)/vm/reference/java/net/VMInetAddress.java
	$(JAVAH) -o $@ java.net.VMInetAddress
java_net_VMNetworkInterface.h: $(top_srcdir)/vm/reference/java/net/VMNetworkInterface.java
	$(JAVAH) -o $@ java.net.VMNetworkInterface
java_net_VMURLConnection.h: $(top_srcdir)/vm/reference/java/net/VMURLConnection.java
	$(JAVAH) -o $@ java.net.VMURLConnection

java_nio_VMDirectByteBuffer.h: $(top_srcdir)/vm/reference/java/nio/VMDirectByteBuffer.java
	$(JAVAH) -o $@ java.nio.VMDirectByteBuffer
java_nio_MappedByteBufferImpl.h: $(top_srcdir)/java/nio/MappedByteBufferImpl.java
	$(JAVAH) -o $@ java.nio.MappedByteBufferImpl

gnu_java_nio_FileChannelImpl.h: $(top_srcdir)/gnu/java/nio/FileChannelImpl.java
	$(JAVAH) -o $@ gnu.java.nio.FileChannelImpl

gnu_java_nio_KqueueSelectorImpl.h: $(top_srcdir)/gnu/java/nio/KqueueSelectorImpl.java
	$(JAVAH) -o $@ gnu.java.nio.KqueueSelectorImpl

gnu_java_nio_EpollSelectorImpl.h: $(top_srcdir)/gnu/java/nio/EpollSelectorImpl.java
	$(JAVAH) -o $@ gnu.java.nio.EpollSelectorImpl

gnu_java_nio_charset_iconv_IconvDecoder.h: $(top_srcdir)/gnu/java/nio/charset/iconv/IconvDecoder.java
	$(JAVAH) -o $@ gnu.java.nio.charset.iconv.IconvDecoder
gnu_java_nio_charset_iconv_IconvEncoder.h: $(top_srcdir)/gnu/java/nio/charset/iconv/IconvEncoder.java
	$(JAVAH) -o $@ gnu.java.nio.charset.iconv.IconvEncoder
java_util_VMTimeZone.h: $(top_srcdir)/vm/reference/java/util/VMTimeZone.java
	$(JAVAH) -o $@ java.util.VMTimeZone

endif # CREATE_JNI_HEADERS
