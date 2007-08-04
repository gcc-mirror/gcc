## GCJ LOCAL: headers are installed specially.
##include_HEADERS = jni.h jni_md.h jawt.h jawt_md.h

DISTCLEANFILES = jni_md.h config-int.h

ARG_JNI_JAVAH = -jni
ARG_CLASSPATH_JAVAH = -bootclasspath
JAVAH = $(USER_JAVAH) $(ARG_JNI_JAVAH) $(ARG_CLASSPATH_JAVAH) ../lib:$(USER_CLASSLIB)
CLASSDIR = lib

SOUND_H_FILES = \
$(top_srcdir)/include/gnu_javax_sound_midi_alsa_AlsaPortDevice.h \
$(top_srcdir)/include/gnu_javax_sound_midi_alsa_AlsaMidiSequencerDevice.h \
$(top_srcdir)/include/gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider.h \
$(top_srcdir)/include/gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.h \
$(top_srcdir)/include/gnu_javax_sound_midi_dssi_DSSISynthesizer.h

GST_PEER_H_FILES = \
$(top_srcdir)/include/gnu_javax_sound_sampled_gstreamer_io_GstAudioFileReaderNativePeer.h

XMLJ_H_FILES = \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocument.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathNodeList.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocumentType.h \
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeProcessingInstruction.h \
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
$(top_srcdir)/include/gnu_java_awt_peer_gtk_CairoSurface.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_CairoGraphics2D.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_ComponentGraphics.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_ComponentGraphicsCopy.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_FreetypeGlyphVector.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkFontPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkPixbufDecoder.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkRobotPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkButtonPeer.h \
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCanvasPeer.h \
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
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkVolatileImage.h \
$(top_srcdir)/include/gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer.h

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
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtEmbeddedWindowPeer.h 

GCONF_PREFS_FILES = \
$(top_srcdir)/include/gnu_java_util_prefs_gconf_GConfNativePeer.h

H_FILES = \
$(SOUND_H_FILES) \
$(GST_PEER_H_FILES) \
$(XMLJ_H_FILES) \
$(GTKPEER_H_FILES) \
$(QTPEER_H_FILES) \
$(GCONF_PREFS_FILES) \
$(top_srcdir)/include/gnu_java_net_VMPlainSocketImpl.h \
$(top_srcdir)/include/gnu_java_net_local_LocalSocketImpl.h \
$(top_srcdir)/include/gnu_java_nio_EpollSelectorImpl.h \
$(top_srcdir)/include/gnu_java_nio_FileChannelImpl.h \
$(top_srcdir)/include/gnu_java_nio_KqueueSelectorImpl.h \
$(top_srcdir)/include/gnu_java_nio_VMChannel.h \
$(top_srcdir)/include/gnu_java_nio_VMPipe.h \
$(top_srcdir)/include/gnu_java_nio_VMSelector.h \
$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvEncoder.h \
$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvDecoder.h \
$(top_srcdir)/include/gnu_java_lang_management_VMOperatingSystemMXBeanImpl.h \
$(top_srcdir)/include/java_io_VMFile.h \
$(top_srcdir)/include/java_io_VMObjectInputStream.h \
$(top_srcdir)/include/java_io_VMObjectStreamClass.h \
$(top_srcdir)/include/java_lang_VMDouble.h \
$(top_srcdir)/include/java_lang_VMFloat.h \
$(top_srcdir)/include/java_lang_VMMath.h \
$(top_srcdir)/include/java_lang_VMProcess.h \
$(top_srcdir)/include/java_lang_VMSystem.h \
$(top_srcdir)/include/java_lang_reflect_VMArray.h \
$(top_srcdir)/include/java_net_VMInetAddress.h \
$(top_srcdir)/include/java_net_VMNetworkInterface.h \
$(top_srcdir)/include/java_net_VMURLConnection.h \
$(top_srcdir)/include/java_nio_VMDirectByteBuffer.h \
$(top_srcdir)/include/java_nio_MappedByteBufferImpl.h \
$(top_srcdir)/include/java_util_VMTimeZone.h

if CREATE_JNI_HEADERS

all-local: $(H_FILES)

$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocument.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeDocument.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeDocument
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathNodeList.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeXPathNodeList.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeXPathNodeList
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocumentType.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeDocumentType.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeDocumentType
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeProcessingInstruction.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeProcessingInstruction.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeProcessingInstruction
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeTypeInfo.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeTypeInfo.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeTypeInfo
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNodeList.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNodeList.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNodeList
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNotation.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNotation.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNotation
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathResult.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeXPathResult.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeXPathResult
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeElement.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeElement.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeElement
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeEntity.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeEntity.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeEntity
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNode.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNode.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNode
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeXPathExpression.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeXPathExpression.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeXPathExpression
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeNamedNodeMap.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeNamedNodeMap.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeNamedNodeMap
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeDocumentBuilder.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeDocumentBuilder.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeDocumentBuilder
$(top_srcdir)/include/gnu_xml_libxmlj_dom_GnomeAttr.h: $(top_srcdir)/gnu/xml/libxmlj/dom/GnomeAttr.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.dom.GnomeAttr
$(top_srcdir)/include/gnu_xml_libxmlj_sax_GnomeLocator.h: $(top_srcdir)/gnu/xml/libxmlj/sax/GnomeLocator.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.sax.GnomeLocator
$(top_srcdir)/include/gnu_xml_libxmlj_sax_GnomeXMLReader.h: $(top_srcdir)/gnu/xml/libxmlj/sax/GnomeXMLReader.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.sax.GnomeXMLReader
$(top_srcdir)/include/gnu_xml_libxmlj_transform_GnomeTransformer.h: $(top_srcdir)/gnu/xml/libxmlj/transform/GnomeTransformer.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.transform.GnomeTransformer
$(top_srcdir)/include/gnu_xml_libxmlj_transform_GnomeTransformerFactory.h: $(top_srcdir)/gnu/xml/libxmlj/transform/GnomeTransformerFactory.java
	$(JAVAH) -o $@ gnu.xml.libxmlj.transform.GnomeTransformerFactory
$(top_srcdir)/include/gnu_java_awt_peer_gtk_CairoSurface.h: $(top_srcdir)/gnu/java/awt/peer/gtk/CairoSurface.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.CairoSurface
$(top_srcdir)/include/gnu_java_awt_peer_gtk_CairoGraphics2D.h: $(top_srcdir)/gnu/java/awt/peer/gtk/CairoGraphics2D.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.CairoGraphics2D
$(top_srcdir)/include/gnu_java_awt_peer_gtk_ComponentGraphics.h: $(top_srcdir)/gnu/java/awt/peer/gtk/ComponentGraphics.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.ComponentGraphics
$(top_srcdir)/include/gnu_java_awt_peer_gtk_ComponentGraphicsCopy.h: $(top_srcdir)/gnu/java/awt/peer/gtk/ComponentGraphicsCopy.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.ComponentGraphicsCopy
$(top_srcdir)/include/gnu_java_awt_peer_gtk_FreetypeGlyphVector.h: $(top_srcdir)/gnu/java/awt/peer/gtk/FreetypeGlyphVector.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.FreetypeGlyphVector
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkFontPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkFontPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkFontPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkGraphicsEnvironment.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkGraphicsEnvironment
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkPixbufDecoder.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkPixbufDecoder.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkPixbufDecoder
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkRobotPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkRobotPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkRobotPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GdkScreenGraphicsDevice.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GdkScreenGraphicsDevice
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkButtonPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkButtonPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkButtonPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCanvasPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkCanvasPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkCanvasPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCheckboxMenuItemPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkCheckboxMenuItemPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkCheckboxMenuItemPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkCheckboxPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkCheckboxPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkCheckboxPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkChoicePeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkChoicePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkChoicePeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkClipboard.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkClipboard.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkClipboard
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkComponentPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkEmbeddedWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkEmbeddedWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkEmbeddedWindowPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkFileDialogPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkFileDialogPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkFileDialogPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkFramePeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkFramePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkFramePeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkGenericPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkGenericPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkGenericPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkImage.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkImage
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkLabelPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkLabelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkLabelPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkListPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkListPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkListPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuBarPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuBarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuBarPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuComponentPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuItemPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuItemPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuItemPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkMenuPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkPanelPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkPanelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkPanelPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkPopupMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkPopupMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkPopupMenuPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkScrollPanePeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkScrollPanePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkScrollPanePeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkScrollbarPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkScrollbarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkScrollbarPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkSelection.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkSelection.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkSelection
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkTextAreaPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkTextAreaPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkTextAreaPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkTextFieldPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkTextFieldPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkTextFieldPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkToolkit.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkToolkit.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkToolkit
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkWindowPeer
$(top_srcdir)/include/gnu_java_awt_peer_gtk_GtkVolatileImage.h: $(top_srcdir)/gnu/java/awt/peer/gtk/GtkVolatileImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.gtk.GtkVolatileImage
$(top_srcdir)/include/gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer.h: $(top_srcdir)/gnu/java/awt/dnd/peer/gtk/GtkDragSourceContextPeer.java
	$(JAVAH) -o $@ gnu.java.awt.dnd.peer.gtk.GtkDragSourceContextPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtCheckboxPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtCheckboxPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtCheckboxPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtToolkit.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtToolkit.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtToolkit
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtAudioClip.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtAudioClip.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtAudioClip
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtScreenDevice.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtScreenDevice.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtScreenDevice
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtTextFieldPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtTextFieldPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtTextFieldPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuItemPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuItemPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuItemPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtButtonPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtButtonPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtButtonPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtWindowPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtDialogPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtDialogPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtDialogPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtImage.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtImage
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuBarPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuBarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuBarPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtVolatileImage.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtVolatileImage.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtVolatileImage
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFontPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFontPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFontPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtChoicePeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtChoicePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtChoicePeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtGraphics.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtGraphics.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtGraphics
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtComponentPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtTextAreaPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtTextAreaPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtTextAreaPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QPen.h: $(top_srcdir)/gnu/java/awt/peer/qt/QPen.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QPen
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtPanelPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtPanelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtPanelPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QPainterPath.h: $(top_srcdir)/gnu/java/awt/peer/qt/QPainterPath.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QPainterPath
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFontMetrics.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFontMetrics.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFontMetrics
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtScrollPanePeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtScrollPanePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtScrollPanePeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QMatrix.h: $(top_srcdir)/gnu/java/awt/peer/qt/QMatrix.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QMatrix
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtLabelPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtLabelPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtLabelPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtPopupMenuPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtPopupMenuPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtPopupMenuPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFramePeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFramePeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFramePeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtListPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtListPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtListPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtScrollbarPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtScrollbarPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtScrollbarPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtCanvasPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtCanvasPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtCanvasPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtMenuComponentPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtMenuComponentPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtMenuComponentPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_MainQtThread.h: $(top_srcdir)/gnu/java/awt/peer/qt/MainQtThread.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.MainQtThread
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtFileDialogPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtFileDialogPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtFileDialogPeer
$(top_srcdir)/include/gnu_java_awt_peer_qt_QtEmbeddedWindowPeer.h: $(top_srcdir)/gnu/java/awt/peer/qt/QtEmbeddedWindowPeer.java
	$(JAVAH) -o $@ gnu.java.awt.peer.qt.QtEmbeddedWindowPeer
$(top_srcdir)/include/gnu_javax_sound_midi_alsa_AlsaPortDevice.h: $(top_srcdir)/gnu/javax/sound/midi/alsa/AlsaPortDevice.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.alsa.AlsaPortDevice
$(top_srcdir)/include/gnu_javax_sound_midi_alsa_AlsaMidiSequencerDevice.h: $(top_srcdir)/gnu/javax/sound/midi/alsa/AlsaMidiSequencerDevice.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.alsa.AlsaMidiSequencerDevice
$(top_srcdir)/include/gnu_javax_sound_midi_alsa_AlsaMidiDeviceProvider.h: $(top_srcdir)/gnu/javax/sound/midi/alsa/AlsaMidiDeviceProvider.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.alsa.AlsaMidiDeviceProvider
$(top_srcdir)/include/gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.h: $(top_srcdir)/gnu/javax/sound/midi/dssi/DSSIMidiDeviceProvider.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.dssi.DSSIMidiDeviceProvider
$(top_srcdir)/include/gnu_javax_sound_midi_dssi_DSSISynthesizer.h: $(top_srcdir)/gnu/javax/sound/midi/dssi/DSSISynthesizer.java
	$(JAVAH) -o $@ gnu.javax.sound.midi.dssi.DSSISynthesizer
$(top_srcdir)/include/gnu_java_util_prefs_gconf_GConfNativePeer.h: $(top_srcdir)/gnu/java/util/prefs/gconf/GConfNativePeer.java
	$(JAVAH) -o $@ gnu.java.util.prefs.gconf.GConfNativePeer
$(top_srcdir)/include/gnu_javax_sound_sampled_gstreamer_io_GstAudioFileReaderNativePeer.h: $(top_builddir)/gnu/javax/sound/sampled/gstreamer/io/GstAudioFileReaderNativePeer.java
	$(JAVAH) -o $@ gnu.javax.sound.sampled.gstreamer.io.GstAudioFileReaderNativePeer
$(top_srcdir)/include/gnu_java_net_VMPlainSocketImpl.h: $(top_srcdir)/vm/reference/gnu/java/net/VMPlainSocketImpl.java
	$(JAVAH) -o $@ gnu.java.net.VMPlainSocketImpl
$(top_srcdir)/include/gnu_java_net_local_LocalSocketImpl.h: $(top_srcdir)/gnu/java/net/local/LocalSocketImpl.java
	$(JAVAH) -o $@ gnu.java.net.local.LocalSocketImpl
$(top_srcdir)/include/gnu_java_nio_VMChannel.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMChannel.java
	$(JAVAH) -o $@ gnu.java.nio.VMChannel
$(top_srcdir)/include/gnu_java_nio_VMPipe.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMPipe.java
	$(JAVAH) -o $@ gnu.java.nio.VMPipe
$(top_srcdir)/include/gnu_java_nio_VMSelector.h: $(top_srcdir)/vm/reference/gnu/java/nio/VMSelector.java
	$(JAVAH) -o $@ gnu.java.nio.VMSelector
$(top_srcdir)/include/gnu_java_lang_management_VMOperatingSystemMXBeanImpl.h: $(top_srcdir)/vm/reference/gnu/java/lang/management/VMOperatingSystemMXBeanImpl.java
	$(JAVAH) -o $@ gnu.java.lang.management.VMOperatingSystemMXBeanImpl
$(top_srcdir)/include/java_io_VMFile.h: $(top_srcdir)/vm/reference/java/io/VMFile.java
	$(JAVAH) -o $@ java.io.VMFile
$(top_srcdir)/include/java_io_VMObjectInputStream.h: $(top_srcdir)/vm/reference/java/io/VMObjectInputStream.java
	$(JAVAH) -o $@ java.io.VMObjectInputStream
$(top_srcdir)/include/java_io_VMObjectStreamClass.h: $(top_srcdir)/vm/reference/java/io/VMObjectStreamClass.java
	$(JAVAH) -o $@ java.io.VMObjectStreamClass
$(top_srcdir)/include/java_lang_VMMath.h: $(top_srcdir)/vm/reference/java/lang/VMMath.java
	$(JAVAH) -o $@ java.lang.VMMath
$(top_srcdir)/include/java_lang_VMDouble.h: $(top_srcdir)/vm/reference/java/lang/VMDouble.java
	$(JAVAH) -o $@ java.lang.VMDouble
$(top_srcdir)/include/java_lang_VMFloat.h: $(top_srcdir)/vm/reference/java/lang/VMFloat.java
	$(JAVAH) -o $@ java.lang.VMFloat
$(top_srcdir)/include/java_lang_VMProcess.h: $(top_srcdir)/vm/reference/java/lang/VMProcess.java
	$(JAVAH) -o $@ java.lang.VMProcess
$(top_srcdir)/include/java_lang_VMSystem.h: $(top_srcdir)/vm/reference/java/lang/VMSystem.java
	$(JAVAH) -o $@ java.lang.VMSystem
$(top_srcdir)/include/java_lang_reflect_VMArray.h: $(top_srcdir)/vm/reference/java/lang/reflect/VMArray.java
	$(JAVAH) -o $@ java.lang.reflect.VMArray
$(top_srcdir)/include/java_net_VMInetAddress.h: $(top_srcdir)/vm/reference/java/net/VMInetAddress.java
	$(JAVAH) -o $@ java.net.VMInetAddress
$(top_srcdir)/include/java_net_VMNetworkInterface.h: $(top_srcdir)/vm/reference/java/net/VMNetworkInterface.java
	$(JAVAH) -o $@ java.net.VMNetworkInterface
$(top_srcdir)/include/java_net_VMURLConnection.h: $(top_srcdir)/vm/reference/java/net/VMURLConnection.java
	$(JAVAH) -o $@ java.net.VMURLConnection

$(top_srcdir)/include/java_nio_VMDirectByteBuffer.h: $(top_srcdir)/vm/reference/java/nio/VMDirectByteBuffer.java
	$(JAVAH) -o $@ java.nio.VMDirectByteBuffer
$(top_srcdir)/include/java_nio_MappedByteBufferImpl.h: $(top_srcdir)/java/nio/MappedByteBufferImpl.java
	$(JAVAH) -o $@ java.nio.MappedByteBufferImpl

$(top_srcdir)/include/gnu_java_nio_FileChannelImpl.h: $(top_srcdir)/gnu/java/nio/FileChannelImpl.java
	$(JAVAH) -o $@ gnu.java.nio.FileChannelImpl

$(top_srcdir)/include/gnu_java_nio_KqueueSelectorImpl.h: $(top_srcdir)/gnu/java/nio/KqueueSelectorImpl.java
	$(JAVAH) -o $@ gnu.java.nio.KqueueSelectorImpl

$(top_srcdir)/include/gnu_java_nio_EpollSelectorImpl.h: $(top_srcdir)/gnu/java/nio/EpollSelectorImpl.java
	$(JAVAH) -o $@ gnu.java.nio.EpollSelectorImpl

$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvDecoder.h: $(top_srcdir)/gnu/java/nio/charset/iconv/IconvDecoder.java
	$(JAVAH) -o $@ gnu.java.nio.charset.iconv.IconvDecoder
$(top_srcdir)/include/gnu_java_nio_charset_iconv_IconvEncoder.h: $(top_srcdir)/gnu/java/nio/charset/iconv/IconvEncoder.java
	$(JAVAH) -o $@ gnu.java.nio.charset.iconv.IconvEncoder
$(top_srcdir)/include/java_util_VMTimeZone.h: $(top_srcdir)/vm/reference/java/util/VMTimeZone.java
	$(JAVAH) -o $@ java.util.VMTimeZone

endif # CREATE_JNI_HEADERS
