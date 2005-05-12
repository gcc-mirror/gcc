## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign subdir-objects

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

## The compiler with whatever flags we want for both -c and -C
## compiles.
GCJ_WITH_FLAGS = $(GCJ) --encoding=UTF-8 -Wno-deprecated -fclasspath= -fbootclasspath=$(BOOTCLASSPATH)

BOOTCLASSPATH = $(here)'$(CLASSPATH_SEPARATOR)'$(srcdir)'$(CLASSPATH_SEPARATOR)'$(top_srcdir)'$(CLASSPATH_SEPARATOR)'$(top_builddir)

AM_GCJFLAGS = \
	-B$(top_builddir) \
	@LIBGCJ_JAVAFLAGS@ \
	-fclasspath= -fbootclasspath=$(BOOTCLASSPATH) \
	--encoding=UTF-8 \
	-Wno-deprecated

BUILT_SOURCES = classes.stamp

w3c.jar: classes.stamp
	find org -name '*.class' -print | $(ZIP) -cfME@ $@

classes.stamp:	$(source_files)
	here=`pwd`; cd $(srcdir); \
	$(GCJ_WITH_FLAGS) -C -d $$here $(source_files)
	echo > classes.stamp

mostlyclean-local:
	-find . -name '*.class' | xargs rm
	-rm classes.stamp w3c.jar

noinst_LTLIBRARIES = libw3c-gcj.la

libw3c_gcj_la_SOURCES =	w3c.jar
libw3c_gcj_la_GCJFLAGS = -findirect-dispatch -fbootstrap-classes $(AM_GCJFLAGS)

source_files = \
org/w3c/dom/xpath/XPathNamespace.java \
org/w3c/dom/xpath/XPathResult.java \
org/w3c/dom/xpath/XPathException.java \
org/w3c/dom/xpath/XPathExpression.java \
org/w3c/dom/xpath/XPathNSResolver.java \
org/w3c/dom/xpath/XPathEvaluator.java \
org/w3c/dom/DOMException.java \
org/w3c/dom/Comment.java \
org/w3c/dom/stylesheets/DocumentStyle.java \
org/w3c/dom/stylesheets/MediaList.java \
org/w3c/dom/stylesheets/LinkStyle.java \
org/w3c/dom/stylesheets/StyleSheet.java	\
org/w3c/dom/stylesheets/StyleSheetList.java \
org/w3c/dom/Attr.java \
org/w3c/dom/Notation.java \
org/w3c/dom/TypeInfo.java \
org/w3c/dom/html2/HTMLIsIndexElement.java \
org/w3c/dom/html2/HTMLTableColElement.java \
org/w3c/dom/html2/HTMLLinkElement.java \
org/w3c/dom/html2/HTMLTitleElement.java	\
org/w3c/dom/html2/HTMLBRElement.java \
org/w3c/dom/html2/HTMLHeadElement.java \
org/w3c/dom/html2/HTMLHtmlElement.java \
org/w3c/dom/html2/HTMLScriptElement.java \
org/w3c/dom/html2/HTMLTableRowElement.java \
org/w3c/dom/html2/HTMLOptionsCollection.java \
org/w3c/dom/html2/HTMLTableCellElement.java \
org/w3c/dom/html2/HTMLDListElement.java	\
org/w3c/dom/html2/HTMLFieldSetElement.java \
org/w3c/dom/html2/HTMLImageElement.java	\
org/w3c/dom/html2/HTMLHRElement.java \
org/w3c/dom/html2/HTMLModElement.java \
org/w3c/dom/html2/HTMLHeadingElement.java \
org/w3c/dom/html2/HTMLTableElement.java	\
org/w3c/dom/html2/HTMLAreaElement.java \
org/w3c/dom/html2/HTMLOptGroupElement.java \
org/w3c/dom/html2/HTMLIFrameElement.java \
org/w3c/dom/html2/HTMLDirectoryElement.java \
org/w3c/dom/html2/HTMLParamElement.java	\
org/w3c/dom/html2/HTMLLegendElement.java \
org/w3c/dom/html2/HTMLFrameElement.java	\
org/w3c/dom/html2/HTMLFormElement.java \
org/w3c/dom/html2/HTMLStyleElement.java	\
org/w3c/dom/html2/HTMLFrameSetElement.java \
org/w3c/dom/html2/HTMLCollection.java \
org/w3c/dom/html2/HTMLAnchorElement.java \
org/w3c/dom/html2/HTMLLabelElement.java	\
org/w3c/dom/html2/HTMLBodyElement.java \
org/w3c/dom/html2/HTMLOptionElement.java \
org/w3c/dom/html2/HTMLTableSectionElement.java \
org/w3c/dom/html2/HTMLAppletElement.java \
org/w3c/dom/html2/HTMLSelectElement.java \
org/w3c/dom/html2/HTMLMetaElement.java \
org/w3c/dom/html2/HTMLMenuElement.java \
org/w3c/dom/html2/HTMLDivElement.java \
org/w3c/dom/html2/HTMLLIElement.java \
org/w3c/dom/html2/HTMLFontElement.java \
org/w3c/dom/html2/HTMLOListElement.java	\
org/w3c/dom/html2/HTMLBaseFontElement.java \
org/w3c/dom/html2/HTMLElement.java \
org/w3c/dom/html2/HTMLTableCaptionElement.java \
org/w3c/dom/html2/HTMLTextAreaElement.java \
org/w3c/dom/html2/HTMLPreElement.java \
org/w3c/dom/html2/HTMLObjectElement.java \
org/w3c/dom/html2/HTMLBaseElement.java \
org/w3c/dom/html2/HTMLUListElement.java	\
org/w3c/dom/html2/HTMLMapElement.java \
org/w3c/dom/html2/HTMLParagraphElement.java \
org/w3c/dom/html2/HTMLButtonElement.java \
org/w3c/dom/html2/HTMLQuoteElement.java	\
org/w3c/dom/html2/HTMLInputElement.java	\
org/w3c/dom/html2/HTMLDocument.java \
org/w3c/dom/ls/LSParser.java \
org/w3c/dom/ls/LSSerializerFilter.java \
org/w3c/dom/ls/LSLoadEvent.java	\
org/w3c/dom/ls/DOMImplementationLS.java	\
org/w3c/dom/ls/LSException.java	\
org/w3c/dom/ls/LSSerializer.java \
org/w3c/dom/ls/LSResourceResolver.java \
org/w3c/dom/ls/LSOutput.java \
org/w3c/dom/ls/LSInput.java \
org/w3c/dom/ls/LSProgressEvent.java \
org/w3c/dom/ls/LSParserFilter.java \
org/w3c/dom/DOMLocator.java \
org/w3c/dom/DOMStringList.java \
org/w3c/dom/DocumentFragment.java \
org/w3c/dom/events/MutationEvent.java \
org/w3c/dom/events/UIEvent.java	\
org/w3c/dom/events/EventListener.java \
org/w3c/dom/events/Event.java \
org/w3c/dom/events/MouseEvent.java \
org/w3c/dom/events/EventTarget.java \
org/w3c/dom/events/DocumentEvent.java \
org/w3c/dom/events/EventException.java \
org/w3c/dom/EntityReference.java \
org/w3c/dom/DOMErrorHandler.java \
org/w3c/dom/views/AbstractView.java \
org/w3c/dom/views/DocumentView.java \
org/w3c/dom/DOMError.java \
org/w3c/dom/bootstrap/DOMImplementationRegistry.java \
org/w3c/dom/traversal/TreeWalker.java \
org/w3c/dom/traversal/NodeFilter.java \
org/w3c/dom/traversal/DocumentTraversal.java \
org/w3c/dom/traversal/NodeIterator.java	\
org/w3c/dom/CharacterData.java \
org/w3c/dom/DOMConfiguration.java \
org/w3c/dom/css/CSSRule.java \
org/w3c/dom/css/Rect.java \
org/w3c/dom/css/CSSStyleSheet.java \
org/w3c/dom/css/CSSFontFaceRule.java \
org/w3c/dom/css/CSSPrimitiveValue.java \
org/w3c/dom/css/CSSRuleList.java \
org/w3c/dom/css/ViewCSS.java \
org/w3c/dom/css/CSSStyleRule.java \
org/w3c/dom/css/CSSUnknownRule.java \
org/w3c/dom/css/RGBColor.java \
org/w3c/dom/css/ElementCSSInlineStyle.java \
org/w3c/dom/css/CSSMediaRule.java \
org/w3c/dom/css/CSS2Properties.java \
org/w3c/dom/css/CSSValue.java \
org/w3c/dom/css/CSSStyleDeclaration.java \
org/w3c/dom/css/DOMImplementationCSS.java \
org/w3c/dom/css/CSSCharsetRule.java \
org/w3c/dom/css/Counter.java \
org/w3c/dom/css/CSSPageRule.java \
org/w3c/dom/css/DocumentCSS.java \
org/w3c/dom/css/CSSValueList.java \
org/w3c/dom/css/CSSImportRule.java \
org/w3c/dom/NameList.java \
org/w3c/dom/Element.java \
org/w3c/dom/Document.java \
org/w3c/dom/ranges/Range.java \
org/w3c/dom/ranges/RangeException.java \
org/w3c/dom/ranges/DocumentRange.java \
org/w3c/dom/DOMImplementationList.java \
org/w3c/dom/Entity.java	\
org/w3c/dom/UserDataHandler.java \
org/w3c/dom/DOMImplementation.java \
org/w3c/dom/CDATASection.java \
org/w3c/dom/ProcessingInstruction.java \
org/w3c/dom/Node.java \
org/w3c/dom/NamedNodeMap.java \
org/w3c/dom/NodeList.java \
org/w3c/dom/Text.java \
org/w3c/dom/DocumentType.java \
org/w3c/dom/DOMImplementationSource.java
