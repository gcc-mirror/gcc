/* XMLWriter.java -- 
   Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package gnu.xml.util;

import java.io.BufferedWriter;
import java.io.CharConversionException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Stack;

import org.xml.sax.*;
import org.xml.sax.ext.*;
import org.xml.sax.helpers.*;


/**
 * This class is a SAX handler which writes all its input as a well formed
 * XML or XHTML document.  If driven using SAX2 events, this output may
 * include a recreated document type declaration, subject to limitations
 * of SAX (no internal subset exposed) or DOM (the important declarations,
 * with their documentation, are discarded).
 *
 * <p> By default, text is generated "as-is", but some optional modes
 * are supported.  Pretty-printing is supported, to make life easier
 * for people reading the output.  XHTML (1.0) output has can be made
 * particularly pretty; all the built-in character entities are known.
 * Canonical XML can also be generated, assuming the input is properly
 * formed.
 *
 * <hr>
 *
 * <p> Some of the methods on this class are intended for applications to
 * use directly, rather than as pure SAX2 event callbacks.  Some of those
 * methods access the JavaBeans properties (used to tweak output formats,
 * for example canonicalization and pretty printing).  Subclasses
 * are expected to add new behaviors, not to modify current behavior, so
 * many such methods are final.</p>
 *
 * <p> The <em>write*()</em> methods may be slightly simpler for some
 * applications to use than direct callbacks.  For example, they support
 * a simple policy for encoding data items as the content of a single element.
 *
 * <p> To reuse an XMLWriter you must provide it with a new Writer, since
 * this handler closes the writer it was given as part of its endDocument()
 * handling.  (XML documents have an end of input, and the way to encode
 * that on a stream is to close it.) </p>
 *
 * <hr>
 *
 * <p> Note that any relative URIs in the source document, as found in
 * entity and notation declarations, ought to have been fully resolved by
 * the parser providing events to this handler.  This means that the
 * output text should only have fully resolved URIs, which may not be
 * the desired behavior in cases where later binding is desired. </p>
 *
 * <p> <em>Note that due to SAX2 defaults, you may need to manually
 * ensure that the input events are XML-conformant with respect to namespace
 * prefixes and declarations.  {@link gnu.xml.pipeline.NSFilter} is
 * one solution to this problem, in the context of processing pipelines.</em>
 * Something as simple as connecting this handler to a parser might not
 * generate the correct output.  Another workaround is to ensure that the
 * <em>namespace-prefixes</em> feature is always set to true, if you're
 * hooking this directly up to some XMLReader implementation.
 *
 * @see gnu.xml.pipeline.TextConsumer
 *
 * @author David Brownell
 */
public class XMLWriter
    implements ContentHandler, LexicalHandler, DTDHandler, DeclHandler
{
    // text prints/escapes differently depending on context
    //	CTX_ENTITY ... entity literal value
    //	CTX_ATTRIBUTE ... attribute literal value
    //	CTX_CONTENT ... content of an element
    //	CTX_UNPARSED ... CDATA, comment, PI, names, etc
    //  CTX_NAME ... name or nmtoken, no escapes possible
    private static final int	CTX_ENTITY = 1;
    private static final int	CTX_ATTRIBUTE = 2;
    private static final int	CTX_CONTENT = 3;
    private static final int	CTX_UNPARSED = 4;
    private static final int	CTX_NAME = 5;

// FIXME: names (element, attribute, PI, notation, etc) are not
// currently written out with range checks (escapeChars).
// In non-XHTML, some names can't be directly written; panic!

    private static String	sysEOL;

    static {
	try {
	    sysEOL = System.getProperty ("line.separator", "\n");

	    // don't use the system's EOL if it's illegal XML.
	    if (!isLineEnd (sysEOL))
		sysEOL = "\n";

	} catch (SecurityException e) {
	    sysEOL = "\n";
	}
    }

    private static boolean isLineEnd (String eol)
    {
	return "\n".equals (eol)
		    || "\r".equals (eol)
		    || "\r\n".equals (eol);
    }

    private Writer		out;
    private boolean		inCDATA;
    private int			elementNestLevel;
    private String		eol = sysEOL;

    private short		dangerMask;
    private StringBuffer	stringBuf;
    private Locator		locator;
    private ErrorHandler	errHandler;

    private boolean		expandingEntities = false;
    private int			entityNestLevel;
    private boolean		xhtml;
    private boolean		startedDoctype;
    private String		encoding;

    private boolean		canonical;
    private boolean		inDoctype;
    private boolean		inEpilogue;

    // pretty printing controls
    private boolean		prettyPrinting;
    private int			column;
    private boolean		noWrap;
    private Stack		space = new Stack ();

    // this is not a hard'n'fast rule -- longer lines are OK,
    // but are to be avoided.  Here, prettyprinting is more to
    // show structure "cleanly" than to be precise about it.
    // better to have ragged layout than one line 24Kb long.
    private static final int	lineLength = 75;


    /**
     * Constructs this handler with System.out used to write SAX events
     * using the UTF-8 encoding.  Avoid using this except when you know
     * it's safe to close System.out at the end of the document.
     */
    public XMLWriter () throws IOException
	{ this (System.out); }

    /**
     * Constructs a handler which writes all input to the output stream
     * in the UTF-8 encoding, and closes it when endDocument is called.
     * (Yes it's annoying that this throws an exception -- but there's
     * really no way around it, since it's barely possible a JDK may
     * exist somewhere that doesn't know how to emit UTF-8.)
     */
    public XMLWriter (OutputStream out) throws IOException
    {
	this (new OutputStreamWriter (out, "UTF8"));
    }

    /**
     * Constructs a handler which writes all input to the writer, and then
     * closes the writer when the document ends.  If an XML declaration is
     * written onto the output, and this class can determine the name of
     * the character encoding for this writer, that encoding name will be
     * included in the XML declaration.
     *
     * <P> See the description of the constructor which takes an encoding
     * name for imporant information about selection of encodings.
     *
     * @param writer XML text is written to this writer.
     */
    public XMLWriter (Writer writer)
    {
	this (writer, null);
    }

    /**
     * Constructs a handler which writes all input to the writer, and then
     * closes the writer when the document ends.  If an XML declaration is
     * written onto the output, this class will use the specified encoding
     * name in that declaration.  If no encoding name is specified, no
     * encoding name will be declared unless this class can otherwise
     * determine the name of the character encoding for this writer.
     *
     * <P> At this time, only the UTF-8 ("UTF8") and UTF-16 ("Unicode")
     * output encodings are fully lossless with respect to XML data.  If you
     * use any other encoding you risk having your data be silently mangled
     * on output, as the standard Java character encoding subsystem silently
     * maps non-encodable characters to a question mark ("?") and will not
     * report such errors to applications.
     *
     * <p> For a few other encodings the risk can be reduced. If the writer is
     * a java.io.OutputStreamWriter, and uses either the ISO-8859-1 ("8859_1",
     * "ISO8859_1", etc) or US-ASCII ("ASCII") encodings, content which
     * can't be encoded in those encodings will be written safely.  Where
     * relevant, the XHTML entity names will be used; otherwise, numeric
     * character references will be emitted.
     *
     * <P> However, there remain a number of cases where substituting such
     * entity or character references is not an option.  Such references are
     * not usable within a DTD, comment, PI, or CDATA section.  Neither may
     * they be used when element, attribute, entity, or notation names have
     * the problematic characters.
     *
     * @param writer XML text is written to this writer.
     * @param encoding if non-null, and an XML declaration is written,
     *	this is the name that will be used for the character encoding.
     */
    public XMLWriter (Writer writer, String encoding)
    {
	setWriter (writer, encoding);
    }
    
    private void setEncoding (String encoding)
    {
	if (encoding == null && out instanceof OutputStreamWriter)
	    encoding = ((OutputStreamWriter)out).getEncoding ();

	if (encoding != null) {
	    encoding = encoding.toUpperCase ();

	    // Use official encoding names where we know them,
	    // avoiding the Java-only names.  When using common
	    // encodings where we can easily tell if characters
	    // are out of range, we'll escape out-of-range
	    // characters using character refs for safety.

	    // I _think_ these are all the main synonyms for these!
	    if ("UTF8".equals (encoding)) {
		encoding = "UTF-8";
	    } else if ("US-ASCII".equals (encoding)
		    || "ASCII".equals (encoding)) {
		dangerMask = (short) 0xff80;
		encoding = "US-ASCII";
	    } else if ("ISO-8859-1".equals (encoding)
		    || "8859_1".equals (encoding)
		    || "ISO8859_1".equals (encoding)) {
		dangerMask = (short) 0xff00;
		encoding = "ISO-8859-1";
	    } else if ("UNICODE".equals (encoding)
		    || "UNICODE-BIG".equals (encoding)
		    || "UNICODE-LITTLE".equals (encoding)) {
		encoding = "UTF-16";

		// TODO: UTF-16BE, UTF-16LE ... no BOM; what
		// release of JDK supports those Unicode names?
	    }

	    if (dangerMask != 0)
		stringBuf = new StringBuffer ();
	}

	this.encoding = encoding;
    }


    /**
     * Resets the handler to write a new text document.
     *
     * @param writer XML text is written to this writer.
     * @param encoding if non-null, and an XML declaration is written,
     *	this is the name that will be used for the character encoding.
     *
     * @exception IllegalStateException if the current
     *	document hasn't yet ended (with {@link #endDocument})
     */
    final public void setWriter (Writer writer, String encoding)
    {
	if (out != null)
	    throw new IllegalStateException (
		"can't change stream in mid course");
	out = writer;
	if (out != null)
	    setEncoding (encoding);
	if (!(out instanceof BufferedWriter))
	    out = new BufferedWriter (out);
	space.push ("default");
    }

    /**
     * Assigns the line ending style to be used on output.
     * @param eolString null to use the system default; else
     *	"\n", "\r", or "\r\n".
     */
    final public void setEOL (String eolString)
    {
	if (eolString == null)
	    eol = sysEOL;
	else if (!isLineEnd (eolString))
	    eol = eolString;
	else
	    throw new IllegalArgumentException (eolString);
    }

    /**
     * Assigns the error handler to be used to present most fatal
     * errors.
     */
    public void setErrorHandler (ErrorHandler handler)
    {
	errHandler = handler;
    }

    /**
     * Used internally and by subclasses, this encapsulates the logic
     * involved in reporting fatal errors.  It uses locator information
     * for good diagnostics, if available, and gives the application's
     * ErrorHandler the opportunity to handle the error before throwing
     * an exception.
     */
    protected void fatal (String message, Exception e)
    throws SAXException
    {
	SAXParseException	x;

	if (locator == null)
	    x = new SAXParseException (message, null, null, -1, -1, e);
	else
	    x = new SAXParseException (message, locator, e);
	if (errHandler != null)
	    errHandler.fatalError (x);
	throw x;
    }


    // JavaBeans properties

    /**
     * Controls whether the output should attempt to follow the "transitional"
     * XHTML rules so that it meets the "HTML Compatibility Guidelines"
     * appendix in the XHTML specification.  A "transitional" Document Type
     * Declaration (DTD) is placed near the beginning of the output document,
     * instead of whatever DTD would otherwise have been placed there, and
     * XHTML empty elements are printed specially.  When writing text in
     * US-ASCII or ISO-8859-1 encodings, the predefined XHTML internal
     * entity names are used (in preference to character references) when
     * writing content characters which can't be expressed in those encodings.
     *
     * <p> When this option is enabled, it is the caller's responsibility
     * to ensure that the input is otherwise valid as XHTML.  Things to
     * be careful of in all cases, as described in the appendix referenced
     * above, include:  <ul>
     *
     *	<li> Element and attribute names must be in lower case, both
     *		in the document and in any CSS style sheet.
     *	<li> All XML constructs must be valid as defined by the XHTML
     *		"transitional" DTD (including all familiar constructs,
     *		even deprecated ones).
     *	<li> The root element must be "html".
     *	<li> Elements that must be empty (such as <em>&lt;br&gt;</em>
     *		must have no content.
     *	<li> Use both <em>lang</em> and <em>xml:lang</em> attributes
     *		when specifying language.
     *	<li> Similarly, use both <em>id</em> and <em>name</em> attributes
     *		when defining elements that may be referred to through
     *		URI fragment identifiers ... and make sure that the
     *		value is a legal NMTOKEN, since not all such HTML 4.0
     *		identifiers are valid in XML.
     *	<li> Be careful with character encodings; make sure you provide
     *		a <em>&lt;meta http-equiv="Content-type"
     *		content="text/xml;charset=..." /&gt;</em> element in
     *		the HTML "head" element, naming the same encoding
     *		used to create this handler.  Also, if that encoding
     *		is anything other than US-ASCII, make sure that if
     *		the document is given a MIME content type, it has
     *		a <em>charset=...</em> attribute with that encoding.
     *	</ul>
     *
     * <p> Additionally, some of the oldest browsers have additional
     * quirks, to address with guidelines such as: <ul>
     *
     *	<li> Processing instructions may be rendered, so avoid them.
     *		(Similarly for an XML declaration.)
     *	<li> Embedded style sheets and scripts should not contain XML
     *		markup delimiters:  &amp;, &lt;, and ]]&gt; are trouble.
     *	<li> Attribute values should not have line breaks or multiple
     *		consecutive white space characters.
     *	<li> Use no more than one of the deprecated (transitional)
     *		<em>&lt;isindex&gt;</em> elements.
     *	<li> Some boolean attributes (such as <em>compact, checked,
     *		disabled, readonly, selected,</em> and more) confuse
     *		some browsers, since they only understand minimized
     *		versions which are illegal in XML.
     *	</ul>
     *
     * <p> Also, some characteristics of the resulting output may be
     * a function of whether the document is later given a MIME
     * content type of <em>text/html</em> rather than one indicating
     * XML (<em>application/xml</em> or <em>text/xml</em>).  Worse,
     * some browsers ignore MIME content types and prefer to rely URI
     * name suffixes -- so an "index.xml" could always be XML, never
     * XHTML, no matter its MIME type.
     */
    final public void setXhtml (boolean value)
    {
	if (locator != null)
	    throw new IllegalStateException ("started parsing");
	xhtml = value;
	if (xhtml)
	    canonical = false;
    }

    /**
     * Returns true if the output attempts to echo the input following
     * "transitional" XHTML rules and matching the "HTML Compatibility
     * Guidelines" so that an HTML version 3 browser can read the output
     * as HTML; returns false (the default) othewise.
     */
    final public boolean isXhtml ()
    {
	return xhtml;
    }

    /**
     * Controls whether the output text contains references to
     * entities (the default), or instead contains the expanded
     * values of those entities.
     */
    final public void setExpandingEntities (boolean value)
    {
	if (locator != null)
	    throw new IllegalStateException ("started parsing");
	expandingEntities = value;
	if (!expandingEntities)
	    canonical = false;
    }

    /**
     * Returns true if the output will have no entity references;
     * returns false (the default) otherwise.
     */
    final public boolean isExpandingEntities ()
    {
	return expandingEntities;
    }

    /**
     * Controls pretty-printing, which by default is not enabled
     * (and currently is most useful for XHTML output).
     * Pretty printing enables structural indentation, sorting of attributes
     * by name, line wrapping, and potentially other mechanisms for making
     * output more or less readable.
     *
     * <p> At this writing, structural indentation and line wrapping are
     * enabled when pretty printing is enabled and the <em>xml:space</em>
     * attribute has the value <em>default</em> (its other legal value is
     * <em>preserve</em>, as defined in the XML specification).  The three
     * XHTML element types which use another value are recognized by their
     * names (namespaces are ignored).
     *
     * <p> Also, for the record, the "pretty" aspect of printing here
     * is more to provide basic structure on outputs that would otherwise
     * risk being a single long line of text.  For now, expect the
     * structure to be ragged ... unless you'd like to submit a patch
     * to make this be more strictly formatted!
     *
     * @exception IllegalStateException thrown if this method is invoked
     *	after output has begun.
     */
    final public void setPrettyPrinting (boolean value)
    {
	if (locator != null)
	    throw new IllegalStateException ("started parsing");
	prettyPrinting = value;
	if (prettyPrinting)
	    canonical = false;
    }

    /**
     * Returns value of flag controlling pretty printing.
     */
    final public boolean isPrettyPrinting ()
    {
	return prettyPrinting;
    }


    /**
     * Sets the output style to be canonicalized.  Input events must
     * meet requirements that are slightly more stringent than the
     * basic well-formedness ones, and include:  <ul>
     *
     *	<li> Namespace prefixes must not have been changed from those
     *	in the original document.  (This may only be ensured by setting
     *	the SAX2 XMLReader <em>namespace-prefixes</em> feature flag;
     *	by default, it is cleared.)
     *
     *	<li> Redundant namespace declaration attributes have been
     *	removed.  (If an ancestor element defines a namespace prefix
     *	and that declaration hasn't been overriden, an element must
     *	not redeclare it.)
     *
     *	<li> If comments are not to be included in the canonical output,
     *	they must first be removed from the input event stream; this
     *	<em>Canonical XML with comments</em> by default.
     *
     *	<li> If the input character encoding was not UCS-based, the
     *	character data must have been normalized using Unicode
     *	Normalization Form C.  (UTF-8 and UTF-16 are UCS-based.)
     *
     *	<li> Attribute values must have been normalized, as is done
     *	by any conformant XML processor which processes all external
     *	parameter entities.
     *
     *	<li> Similarly, attribute value defaulting has been performed.
     *
     *	</ul>
     *
     * <p> Note that fragments of XML documents, as specified by an XPath
     * node set, may be canonicalized.  In such cases, elements may need
     * some fixup (for <em>xml:*</em> attributes and application-specific
     * context).
     *
     * @exception IllegalArgumentException if the output encoding
     *	is anything other than UTF-8.
     */
    final public void setCanonical (boolean value)
    {
	if (value && !"UTF-8".equals (encoding))
	    throw new IllegalArgumentException ("encoding != UTF-8");
	canonical = value;
	if (canonical) {
	    prettyPrinting = xhtml = false;
	    expandingEntities = true;
	    eol = "\n";
	}
    }


    /**
     * Returns value of flag controlling canonical output.
     */
    final public boolean isCanonical ()
    {
	return canonical;
    }


    /**
     * Flushes the output stream.  When this handler is used in long lived
     * pipelines, it can be important to flush buffered state, for example
     * so that it can reach the disk as part of a state checkpoint.
     */
    final public void flush ()
    throws IOException
    {
	if (out != null)
	    out.flush ();
    }


    // convenience routines

// FIXME:  probably want a subclass that holds a lot of these...
// and maybe more!
    
    /**
     * Writes the string as if characters() had been called on the contents
     * of the string.  This is particularly useful when applications act as
     * producers and write data directly to event consumers.
     */
    final public void write (String data)
    throws SAXException
    {
	char	buf [] = data.toCharArray ();
	characters (buf, 0, buf.length);
    }


    /**
     * Writes an element that has content consisting of a single string.
     * @see #writeEmptyElement
     * @see #startElement
     */
    public void writeElement (
	String uri,
	String localName,
	String qName,
	Attributes atts,
	String content
    ) throws SAXException
    {
	if (content == null || content.length () == 0) {
	    writeEmptyElement (uri, localName, qName, atts);
	    return;
	}
	startElement (uri, localName, qName, atts);
	char chars [] = content.toCharArray ();
	characters (chars, 0, chars.length);
	endElement (uri, localName, qName);
    }


    /**
     * Writes an element that has content consisting of a single integer,
     * encoded as a decimal string.
     * @see #writeEmptyElement
     * @see #startElement
     */
    public void writeElement (
	String uri,
	String localName,
	String qName,
	Attributes atts,
	int content
    ) throws SAXException
    {
	writeElement (uri, localName, qName, atts, Integer.toString (content));
    }


    // SAX1 ContentHandler
    /** <b>SAX1</b>:  provides parser status information */
    final public void setDocumentLocator (Locator l)
    {
	locator = l;
    }


    // URL for dtd that validates against all normal HTML constructs
    private static final String xhtmlFullDTD =
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd";

    
    /**
     * <b>SAX1</b>:  indicates the beginning of a document parse.
     * If you're writing (well formed) fragments of XML, neither
     * this nor endDocument should be called.
     */
    // NOT final
    public void startDocument ()
    throws SAXException
    {
	try {
	    if (out == null)
		throw new IllegalStateException (
		    "null Writer given to XMLWriter");

	    // Not all parsers provide the locator we want; this also
	    // flags whether events are being sent to this object yet.
	    // We could only have this one call if we only printed whole
	    // documents ... but we also print fragments, so most of the
	    // callbacks here replicate this test.

	    if (locator == null)
		locator = new LocatorImpl ();
	    
	    // Unless the data is in US-ASCII or we're canonicalizing, write
	    // the XML declaration if we know the encoding.  US-ASCII won't
	    // normally get mangled by web server confusion about the
	    // character encodings used.  Plus, it's an easy way to
	    // ensure we can write ASCII that's unlikely to confuse
	    // elderly HTML parsers.

	    if (!canonical
		    && dangerMask != (short) 0xff80
		    && encoding != null) {
		rawWrite ("<?xml version='1.0'");
		rawWrite (" encoding='" + encoding + "'");
		rawWrite ("?>");
		newline ();
	    }

	    if (xhtml) {

		rawWrite ("<!DOCTYPE html PUBLIC");
		newline ();
		rawWrite ("  '-//W3C//DTD XHTML 1.0 Transitional//EN'");
		newline ();
		rawWrite ("  '");
		    // NOTE:  URL (above) matches the REC
		rawWrite (xhtmlFullDTD);
		rawWrite ("'>");
		newline ();
		newline ();

		// fake the rest of the handler into ignoring
		// everything until the root element, so any
		// XHTML DTD comments, PIs, etc are ignored
		startedDoctype = true;
	    }

	    entityNestLevel = 0;

	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /**
     * <b>SAX1</b>:  indicates the completion of a parse.
     * Note that all complete SAX event streams make this call, even
     * if an error is reported during a parse.
     */
    // NOT final
    public void endDocument ()
    throws SAXException
    {
	try {
	    if (!canonical) {
		newline ();
		newline ();
	    }
	    out.close ();
	    out = null;
	    locator = null;
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    // XHTML elements declared as EMPTY print differently
    final private static boolean isEmptyElementTag (String tag)
    {
	switch (tag.charAt (0)) {
	  case 'a':	return "area".equals (tag);
	  case 'b':	return "base".equals (tag)
			    || "basefont".equals (tag)
			    || "br".equals (tag);
	  case 'c':	return "col".equals (tag);
	  case 'f':	return "frame".equals (tag);
	  case 'h':	return "hr".equals (tag);
	  case 'i':	return "img".equals (tag)
			    || "input".equals (tag)
			    || "isindex".equals (tag);
	  case 'l':	return "link".equals (tag);
	  case 'm':	return "meta".equals (tag);
	  case 'p':	return "param".equals (tag);
	}
	return false;
    }

    private static boolean indentBefore (String tag)
    {
	// basically indent before block content
	// and within structure like tables, lists
	switch (tag.charAt (0)) {
	  case 'a':	return "applet".equals (tag);
	  case 'b':	return "body".equals (tag)
			    || "blockquote".equals (tag);
	  case 'c':	return "center".equals (tag);
	  case 'f':	return "frame".equals (tag)
			    || "frameset".equals (tag);
	  case 'h':	return "head".equals (tag);
	  case 'm':	return "meta".equals (tag);
	  case 'o':	return "object".equals (tag);
	  case 'p':	return "param".equals (tag)
			    || "pre".equals (tag);
	  case 's':	return "style".equals (tag);
	  case 't':	return "title".equals (tag)
			    || "td".equals (tag)
			    || "th".equals (tag);
	}
	// ... but not inline elements like "em", "b", "font"
	return false;
    }

    private static boolean spaceBefore (String tag)
    {
	// blank line AND INDENT before certain structural content
	switch (tag.charAt (0)) {
	  case 'h':	return "h1".equals (tag)
			    || "h2".equals (tag)
			    || "h3".equals (tag)
			    || "h4".equals (tag)
			    || "h5".equals (tag)
			    || "h6".equals (tag)
			    || "hr".equals (tag);
	  case 'l':	return "li".equals (tag);
	  case 'o':	return "ol".equals (tag);
	  case 'p':	return "p".equals (tag);
	  case 't':	return "table".equals (tag)
			    || "tr".equals (tag);
	  case 'u':	return "ul".equals (tag);
	}
	return false;
    }

    // XHTML DTDs say these three have xml:space="preserve"
    private static boolean spacePreserve (String tag)
    {
	return "pre".equals (tag)
		|| "style".equals (tag)
		|| "script".equals (tag);
    }

    /**
     * <b>SAX2</b>:  ignored.
     */
    final public void startPrefixMapping (String prefix, String uri)
	{}

    /**
     * <b>SAX2</b>:  ignored.
     */
    final public void endPrefixMapping (String prefix)
	{}

    private void writeStartTag (
	String name,
	Attributes atts,
	boolean isEmpty
    ) throws SAXException, IOException
    {
	rawWrite ('<');
	rawWrite (name);

	// write out attributes ... sorting is particularly useful
	// with output that's been heavily defaulted.
	if (atts != null && atts.getLength () != 0) {

	    // Set up to write, with optional sorting
	    int 	indices [] = new int [atts.getLength ()];

	    for (int i= 0; i < indices.length; i++)
		indices [i] = i;
	    
	    // optionally sort

// FIXME:  canon xml demands xmlns nodes go first,
// and sorting by URI first (empty first) then localname
// it should maybe use a different sort

	    if (canonical || prettyPrinting) {

		// insertion sort by attribute name
		for (int i = 1; i < indices.length; i++) {
		    int	n = indices [i], j;
		    String	s = atts.getQName (n);

		    for (j = i - 1; j >= 0; j--) {
			if (s.compareTo (atts.getQName (indices [j]))
				>= 0)
			    break;
			indices [j + 1] = indices [j];
		    }
		    indices [j + 1] = n;
		}
	    }

	    // write, sorted or no
	    for (int i= 0; i < indices.length; i++) {
		String	s = atts.getQName (indices [i]);

		    if (s == null || "".equals (s))
			throw new IllegalArgumentException ("no XML name");
		rawWrite (" ");
		rawWrite (s);
		rawWrite ("=");
		writeQuotedValue (atts.getValue (indices [i]),
		    CTX_ATTRIBUTE);
	    }
	}
	if (isEmpty)
	    rawWrite (" /");
	rawWrite ('>');
    }

    /**
     * <b>SAX2</b>:  indicates the start of an element.
     * When XHTML is in use, avoid attribute values with
     * line breaks or multiple whitespace characters, since
     * not all user agents handle them correctly.
     */
    final public void startElement (
	String uri,
	String localName,
	String qName,
	Attributes atts
    ) throws SAXException
    {
	startedDoctype = false;

	if (locator == null)
	    locator = new LocatorImpl ();
	    
	if (qName == null || "".equals (qName))
	    throw new IllegalArgumentException ("no XML name");

	try {
	    if (entityNestLevel != 0)
		return;
	    if (prettyPrinting) {
		String whitespace = null;

		if (xhtml && spacePreserve (qName))
		    whitespace = "preserve";
		else if (atts != null)
		    whitespace = atts.getValue ("xml:space");
		if (whitespace == null)
		    whitespace = (String) space.peek ();
		space.push (whitespace);

		if ("default".equals (whitespace)) {
		    if (xhtml) {
			if (spaceBefore (qName)) {
			    newline ();
			    doIndent ();
			} else if (indentBefore (qName))
			    doIndent ();
			// else it's inlined, modulo line length
			// FIXME: incrementing element nest level
			// for inlined elements causes ugliness
		    } else
			doIndent ();
		}
	    }
	    elementNestLevel++;
	    writeStartTag (qName, atts, xhtml && isEmptyElementTag (qName));

	    if (xhtml) {
// FIXME: if this is an XHTML "pre" element, turn
// off automatic wrapping.
	    }

	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /**
     * Writes an empty element.
     * @see #startElement
     */
    public void writeEmptyElement (
	String uri,
	String localName,
	String qName,
	Attributes atts
    ) throws SAXException
    {
	if (canonical) {
	    startElement (uri, localName, qName, atts);
	    endElement (uri, localName, qName);
	} else {
	    try {
		writeStartTag (qName, atts, true);
	    } catch (IOException e) {
		fatal ("can't write", e);
	    }
	}
    }


    /** <b>SAX2</b>:  indicates the end of an element */
    final public void endElement (String uri, String localName, String qName)
    throws SAXException
    {
	if (qName == null || "".equals (qName))
	    throw new IllegalArgumentException ("no XML name");

	try {
	    elementNestLevel--;
	    if (entityNestLevel != 0)
		return;
	    if (xhtml && isEmptyElementTag (qName))
		return;
	    rawWrite ("</");
	    rawWrite (qName);
	    rawWrite ('>');

	    if (prettyPrinting) {
		if (!space.empty ())
		    space.pop ();
		else
		    fatal ("stack discipline", null);
	    }
	    if (elementNestLevel == 0)
		inEpilogue = true;

	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX1</b>:  reports content characters */
    final public void characters (char ch [], int start, int length)
    throws SAXException
    {
	if (locator == null)
	    locator = new LocatorImpl ();

	try {
	    if (entityNestLevel != 0)
		return;
	    if (inCDATA) {
		escapeChars (ch, start, length, CTX_UNPARSED);
	    } else {
		escapeChars (ch, start, length, CTX_CONTENT);
	    }
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX1</b>:  reports ignorable whitespace */
    final public void ignorableWhitespace (char ch [], int start, int length)
    throws SAXException
    {
	if (locator == null)
	    locator = new LocatorImpl ();

	try {
	    if (entityNestLevel != 0)
		return;
	    // don't forget to map NL to CRLF, CR, etc
	    escapeChars (ch, start, length, CTX_CONTENT);
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /**
     * <b>SAX1</b>:  reports a PI.
     * This doesn't check for illegal target names, such as "xml" or "XML",
     * or namespace-incompatible ones like "big:dog"; the caller is
     * responsible for ensuring those names are legal.
     */
    final public void processingInstruction (String target, String data)
    throws SAXException
    {
	if (locator == null)
	    locator = new LocatorImpl ();

	// don't print internal subset for XHTML
	if (xhtml && startedDoctype)
	    return;

	// ancient HTML browsers might render these ... their loss.
	// to prevent:  "if (xhtml) return;".

	try {
	    if (entityNestLevel != 0)
		return;
	    if (canonical && inEpilogue)
		newline ();
	    rawWrite ("<?");
	    rawWrite (target);
	    rawWrite (' ');
	    escapeChars (data.toCharArray (), -1, -1, CTX_UNPARSED);
	    rawWrite ("?>");
	    if (elementNestLevel == 0 && !(canonical && inEpilogue))
		newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX1</b>: indicates a non-expanded entity reference */
    public void skippedEntity (String name)
    throws SAXException
    {
	try {
	    rawWrite ("&");
	    rawWrite (name);
	    rawWrite (";");
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    // SAX2 LexicalHandler

    /** <b>SAX2</b>:  called before parsing CDATA characters */
    final public void startCDATA ()
    throws SAXException
    {
	if (locator == null)
	    locator = new LocatorImpl ();
	
	if (canonical)
	    return;

	try {
	    inCDATA = true;
	    if (entityNestLevel == 0)
		rawWrite ("<![CDATA[");
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX2</b>:  called after parsing CDATA characters */
    final public void endCDATA ()
    throws SAXException
    {
	if (canonical)
	    return;

	try {
	    inCDATA = false;
	    if (entityNestLevel == 0)
		rawWrite ("]]>");
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /**
     * <b>SAX2</b>:  called when the doctype is partially parsed
     * Note that this, like other doctype related calls, is ignored
     * when XHTML is in use.
     */
    final public void startDTD (String name, String publicId, String systemId)
    throws SAXException
    {
	if (locator == null)
	    locator = new LocatorImpl ();
	if (xhtml)
	    return;
	try {
	    inDoctype = startedDoctype = true;
	    if (canonical)
		return;
	    rawWrite ("<!DOCTYPE ");
	    rawWrite (name);
	    rawWrite (' ');

	    if (!expandingEntities) {
		if (publicId != null)
		    rawWrite ("PUBLIC '" + publicId + "' '" + systemId + "' ");
		else if (systemId != null)
		    rawWrite ("SYSTEM '" + systemId + "' ");
	    }

	    rawWrite ('[');
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX2</b>:  called after the doctype is parsed */
    final public void endDTD ()
    throws SAXException
    {
	inDoctype = false;
	if (canonical || xhtml)
	    return;
	try {
	    rawWrite ("]>");
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /**
     * <b>SAX2</b>:  called before parsing a general entity in content
     */
    final public void startEntity (String name)
    throws SAXException
    {
	try {
	    boolean	writeEOL = true;

	    // Predefined XHTML entities (for characters) will get
	    // mapped back later.
	    if (xhtml || expandingEntities)
		return;

	    entityNestLevel++;
	    if (name.equals ("[dtd]"))
		return;
	    if (entityNestLevel != 1)
		return;
	    if (!name.startsWith ("%")) {
		writeEOL = false;
		rawWrite ('&');
	    }
	    rawWrite (name);
	    rawWrite (';');
	    if (writeEOL)
		newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /**
     * <b>SAX2</b>:  called after parsing a general entity in content
     */
    final public void endEntity (String name)
    throws SAXException
    {
	if (xhtml || expandingEntities)
	    return;
	entityNestLevel--;
    }

    /**
     * <b>SAX2</b>:  called when comments are parsed.
     * When XHTML is used, the old HTML tradition of using comments
     * to for inline CSS, or for JavaScript code is  discouraged.
     * This is because XML processors are encouraged to discard, on
     * the grounds that comments are for users (and perhaps text
     * editors) not programs.  Instead, use external scripts
     */
    final public void comment (char ch [], int start, int length)
    throws SAXException
    {
	if (locator == null)
	    locator = new LocatorImpl ();

	// don't print internal subset for XHTML
	if (xhtml && startedDoctype)
	    return;
	// don't print comment in doctype for canon xml
	if (canonical && inDoctype)
	    return;

	try {
	    boolean indent;

	    if (prettyPrinting && space.empty ())
		fatal ("stack discipline", null);
	    indent = prettyPrinting && "default".equals (space.peek ());
	    if (entityNestLevel != 0)
		return;
	    if (indent)
		doIndent ();
	    if (canonical && inEpilogue)
		newline ();
	    rawWrite ("<!--");
	    escapeChars (ch, start, length, CTX_UNPARSED);
	    rawWrite ("-->");
	    if (indent)
		doIndent ();
	    if (elementNestLevel == 0 && !(canonical && inEpilogue))
		newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    // SAX1 DTDHandler

    /** <b>SAX1</b>:  called on notation declarations */
    final public void notationDecl (String name,
    	String publicId, String systemId)
    throws SAXException
    {
	if (xhtml)
	    return;
	try {
	    // At this time, only SAX2 callbacks start these.
	    if (!startedDoctype)
		return;

	    if (entityNestLevel != 0)
		return;
	    rawWrite ("<!NOTATION " + name + " ");
	    if (publicId != null)
		rawWrite ("PUBLIC \"" + publicId + '"');
	    else
		rawWrite ("SYSTEM ");
	    if (systemId != null)
		rawWrite ('"' + systemId + '"');
	    rawWrite (">");
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX1</b>:  called on unparsed entity declarations */
    final public void unparsedEntityDecl (String name,
	String publicId, String systemId,
	String notationName)
    throws SAXException
    {
	if (xhtml)
	    return;
	try {
	    // At this time, only SAX2 callbacks start these.
	    if (!startedDoctype)  {
		// FIXME: write to temporary buffer, and make the start
		// of the root element write these declarations.
		return;
	    }

	    if (entityNestLevel != 0)
		return;
	    rawWrite ("<!ENTITY " + name + " ");
	    if (publicId != null)
		rawWrite ("PUBLIC \"" + publicId + '"');
	    else
		rawWrite ("SYSTEM ");
	    rawWrite ('"' + systemId + '"');
	    rawWrite (" NDATA " + notationName + ">");
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    // SAX2 DeclHandler

    /** <b>SAX2</b>:  called on attribute declarations */
    final public void attributeDecl (String eName, String aName,
	    String type, String mode, String value)
    throws SAXException
    {
	if (xhtml)
	    return;
	try {
	    // At this time, only SAX2 callbacks start these.
	    if (!startedDoctype)
		return;
	    if (entityNestLevel != 0)
		return;
	    rawWrite ("<!ATTLIST " + eName + ' ' + aName + ' ');
	    rawWrite (type);
	    rawWrite (' ');
	    if (mode != null)
		rawWrite (mode + ' ');
	    if (value != null) 
		writeQuotedValue (value, CTX_ATTRIBUTE);
	    rawWrite ('>');
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX2</b>:  called on element declarations */
    final public void elementDecl (String name, String model)
    throws SAXException
    {
	if (xhtml)
	    return;
	try {
	    // At this time, only SAX2 callbacks start these.
	    if (!startedDoctype)
		return;
	    if (entityNestLevel != 0)
		return;
	    rawWrite ("<!ELEMENT " + name + ' ' + model + '>');
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX2</b>:  called on external entity declarations */
    final public void externalEntityDecl (
	String name,
	String publicId,
	String systemId)
    throws SAXException
    {
	if (xhtml)
	    return;
	try {
	    // At this time, only SAX2 callbacks start these.
	    if (!startedDoctype)
		return;
	    if (entityNestLevel != 0)
		return;
	    rawWrite ("<!ENTITY ");
	    if (name.startsWith ("%")) {
		rawWrite ("% ");
		rawWrite (name.substring (1));
	    } else
		rawWrite (name);
	    if (publicId != null)
		rawWrite (" PUBLIC \"" + publicId + '"');
	    else
		rawWrite (" SYSTEM ");
	    rawWrite ('"' + systemId + "\">");
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    /** <b>SAX2</b>:  called on internal entity declarations */
    final public void internalEntityDecl (String name, String value)
    throws SAXException
    {
	if (xhtml)
	    return;
	try {
	    // At this time, only SAX2 callbacks start these.
	    if (!startedDoctype)
		return;
	    if (entityNestLevel != 0)
		return;
	    rawWrite ("<!ENTITY ");
	    if (name.startsWith ("%")) {
		rawWrite ("% ");
		rawWrite (name.substring (1));
	    } else
		rawWrite (name);
	    rawWrite (' ');
	    writeQuotedValue (value, CTX_ENTITY);
	    rawWrite ('>');
	    newline ();
	} catch (IOException e) {
	    fatal ("can't write", e);
	}
    }

    private void writeQuotedValue (String value, int code)
    throws SAXException, IOException
    {
	char	buf [] = value.toCharArray ();
	int	off = 0, len = buf.length;

	// we can't add line breaks to attribute/entity/... values
	noWrap = true;
	rawWrite ('"');
	escapeChars (buf, off, len, code);
	rawWrite ('"');
	noWrap = false;
    }
    
    // From "HTMLlat1x.ent" ... names of entities for ISO-8859-1
    // (Latin/1) characters, all codes:  160-255 (0xA0-0xFF).
    // Codes 128-159 have no assigned values.
    private static final String HTMLlat1x [] = {
	// 160
	"nbsp", "iexcl", "cent", "pound", "curren",
	"yen", "brvbar", "sect", "uml", "copy",

	// 170
	"ordf", "laquo", "not", "shy", "reg",
	"macr", "deg", "plusmn", "sup2", "sup3",

	// 180
	"acute", "micro", "para", "middot", "cedil",
	"sup1", "ordm", "raquo", "frac14", "frac12",

	// 190
	"frac34", "iquest", "Agrave", "Aacute", "Acirc",
	"Atilde", "Auml", "Aring", "AElig", "Ccedil",

	// 200
	"Egrave", "Eacute", "Ecirc", "Euml", "Igrave",
	"Iacute", "Icirc", "Iuml", "ETH", "Ntilde",

	// 210
	"Ograve", "Oacute", "Ocirc", "Otilde", "Ouml",
	"times", "Oslash", "Ugrave", "Uacute", "Ucirc",

	// 220
	"Uuml", "Yacute", "THORN", "szlig", "agrave",
	"aacute", "acirc", "atilde", "auml", "aring",

	// 230
	"aelig", "ccedil", "egrave", "eacute", "ecirc",
	"euml", "igrave", "iacute", "icirc", "iuml",

	// 240
	"eth", "ntilde", "ograve", "oacute", "ocirc",
	"otilde", "ouml", "divide", "oslash", "ugrave",

	// 250
	"uacute", "ucirc", "uuml", "yacute", "thorn",
	"yuml"
    };

    // From "HTMLsymbolx.ent" ... some of the symbols that
    // we can conveniently handle.  Entities for the Greek.
    // alphabet (upper and lower cases) are compact.
    private static final String HTMLsymbolx_GR [] = {
	// 913
	"Alpha", "Beta", "Gamma", "Delta", "Epsilon",
	"Zeta", "Eta", "Theta", "Iota", "Kappa",

	// 923
	"Lambda", "Mu", "Nu", "Xi", "Omicron",
	"Pi", "Rho", null, "Sigma", "Tau",

	// 933
	"Upsilon", "Phi", "Chi", "Psi", "Omega"
    };

    private static final String HTMLsymbolx_gr [] = {
	// 945
	"alpha", "beta", "gamma", "delta", "epsilon",
	"zeta", "eta", "theta", "iota", "kappa",

	// 955
	"lambda", "mu", "nu", "xi", "omicron",
	"pi", "rho", "sigmaf", "sigma", "tau",

	// 965
	"upsilon", "phi", "chi", "psi", "omega"
    };


    // General routine to write text and substitute predefined
    // entities (XML, and a special case for XHTML) as needed.
    private void escapeChars (char buf [], int off, int len, int code)
    throws SAXException, IOException
    {
	int	first = 0;

	if (off < 0) {
	    off = 0;
	    len = buf.length;
	}
	for (int i = 0; i < len; i++) {
	    String	esc;
	    char 	c = buf [off + i];

	    switch (c) {
	      // Note that CTX_ATTRIBUTE isn't explicitly tested here;
	      // all syntax delimiters are escaped in CTX_ATTRIBUTE,
	      // otherwise it's similar to CTX_CONTENT

	      // ampersand flags entity references; entity replacement
	      // text has unexpanded references, other text doesn't.
	      case '&':
		if (code == CTX_ENTITY || code == CTX_UNPARSED)
		    continue;
		esc = "amp";
		break;

	      // attributes and text may NOT have literal '<', but
	      // entities may have markup constructs
	      case '<':
		if (code == CTX_ENTITY || code == CTX_UNPARSED)
		    continue;
		esc = "lt";
		break;

	      // as above re markup constructs; but otherwise
	      // except when canonicalizing, this is for consistency
	      case '>':
		if (code == CTX_ENTITY || code == CTX_UNPARSED)
		    continue;
	        esc = "gt";
		break;
	      case '\'':
		if (code == CTX_CONTENT || code == CTX_UNPARSED)
		    continue;
		if (canonical)
		    continue;
		esc = "apos";
		break;

	      // needed when printing quoted attribute/entity values
	      case '"':
		if (code == CTX_CONTENT || code == CTX_UNPARSED)
		    continue;
		esc = "quot";
		break;

	      // make line ends work per host OS convention
	      case '\n':
		esc = eol;
		break;

	      //
	      // No other characters NEED special treatment ... except
	      // for encoding-specific issues, like whether the character
	      // can really be represented in that encoding.
	      //
	      default:
		//
		// There are characters we can never write safely; getting
		// them is an error.
		//
		//   (a) They're never legal in XML ... detected by range 
		//	checks, and (eventually) by remerging surrogate
		//	pairs on output.  (Easy error for apps to prevent.)
		//
		//   (b) This encoding can't represent them, and we
		//	can't make reference substitution (e.g. inside
		//	CDATA sections, names, PI data, etc).  (Hard for
		//	apps to prevent, except by using UTF-8 or UTF-16
		//	as their output encoding.)
		//
		// We know a very little bit about what characters
		// the US-ASCII and ISO-8859-1 encodings support.  For
		// other encodings we can't detect the second type of
		// error at all.  (Never an issue for UTF-8 or UTF-16.)
		//

// FIXME:  CR in CDATA is an error; in text, turn to a char ref

// FIXME:  CR/LF/TAB in attributes should become char refs

		if ((c > 0xfffd)
			|| ((c < 0x0020) && !((c == 0x0009)
				|| (c == 0x000A) || (c == 0x000D)))
			|| (((c & dangerMask) != 0)
			    && (code == CTX_UNPARSED))) {

		    // if case (b) in CDATA, we might end the section,
		    // write a reference, then restart ... possible
		    // in one DOM L3 draft.

		    throw new CharConversionException (
			    "Illegal or non-writable character: U+"
			    + Integer.toHexString (c));
		}

		//
		// If the output encoding represents the character
		// directly, let it do so!  Else we'll escape it.
		//
		if ((c & dangerMask) == 0)
		    continue;
		esc = null;

		// Avoid numeric refs where symbolic ones exist, as
		// symbolic ones make more sense to humans reading!
		if (xhtml) {
		    // all the HTMLlat1x.ent entities
		    // (all the "ISO-8859-1" characters)
		    if (c >= 160 && c <= 255)
			esc = HTMLlat1x [c - 160];

		    // not quite half the HTMLsymbolx.ent entities
		    else if (c >= 913 && c <= 937)
			esc = HTMLsymbolx_GR [c - 913];
		    else if (c >= 945 && c <= 969)
			esc = HTMLsymbolx_gr [c - 945];

		    else switch (c) {
			// all of the HTMLspecialx.ent entities
			case  338: esc = "OElig";	break;
			case  339: esc = "oelig";	break;
			case  352: esc = "Scaron";	break;
			case  353: esc = "scaron";	break;
			case  376: esc = "Yuml";	break;
			case  710: esc = "circ";	break;
			case  732: esc = "tilde";	break;
			case 8194: esc = "ensp";	break;
			case 8195: esc = "emsp";	break;
			case 8201: esc = "thinsp";	break;
			case 8204: esc = "zwnj";	break;
			case 8205: esc = "zwj";		break;
			case 8206: esc = "lrm";		break;
			case 8207: esc = "rlm";		break;
			case 8211: esc = "ndash";	break;
			case 8212: esc = "mdash";	break;
			case 8216: esc = "lsquo";	break;
			case 8217: esc = "rsquo";	break;
			case 8218: esc = "sbquo";	break;
			case 8220: esc = "ldquo";	break;
			case 8221: esc = "rdquo";	break;
			case 8222: esc = "bdquo";	break;
			case 8224: esc = "dagger";	break;
			case 8225: esc = "Dagger";	break;
			case 8240: esc = "permil";	break;
			case 8249: esc = "lsaquo";	break;
			case 8250: esc = "rsaquo";	break;
			case 8364: esc = "euro";	break;

			// the other HTMLsymbox.ent entities
			case  402: esc = "fnof";	break;
			case  977: esc = "thetasym";	break;
			case  978: esc = "upsih";	break;
			case  982: esc = "piv";		break;
			case 8226: esc = "bull";	break;
			case 8230: esc = "hellip";	break;
			case 8242: esc = "prime";	break;
			case 8243: esc = "Prime";	break;
			case 8254: esc = "oline";	break;
			case 8260: esc = "frasl";	break;
			case 8472: esc = "weierp";	break;
			case 8465: esc = "image";	break;
			case 8476: esc = "real";	break;
			case 8482: esc = "trade";	break;
			case 8501: esc = "alefsym";	break;
			case 8592: esc = "larr";	break;
			case 8593: esc = "uarr";	break;
			case 8594: esc = "rarr";	break;
			case 8595: esc = "darr";	break;
			case 8596: esc = "harr";	break;
			case 8629: esc = "crarr";	break;
			case 8656: esc = "lArr";	break;
			case 8657: esc = "uArr";	break;
			case 8658: esc = "rArr";	break;
			case 8659: esc = "dArr";	break;
			case 8660: esc = "hArr";	break;
			case 8704: esc = "forall";	break;
			case 8706: esc = "part";	break;
			case 8707: esc = "exist";	break;
			case 8709: esc = "empty";	break;
			case 8711: esc = "nabla";	break;
			case 8712: esc = "isin";	break;
			case 8713: esc = "notin";	break;
			case 8715: esc = "ni";		break;
			case 8719: esc = "prod";	break;
			case 8721: esc = "sum";		break;
			case 8722: esc = "minus";	break;
			case 8727: esc = "lowast";	break;
			case 8730: esc = "radic";	break;
			case 8733: esc = "prop";	break;
			case 8734: esc = "infin";	break;
			case 8736: esc = "ang";		break;
			case 8743: esc = "and";		break;
			case 8744: esc = "or";		break;
			case 8745: esc = "cap";		break;
			case 8746: esc = "cup";		break;
			case 8747: esc = "int";		break;
			case 8756: esc = "there4";	break;
			case 8764: esc = "sim";		break;
			case 8773: esc = "cong";	break;
			case 8776: esc = "asymp";	break;
			case 8800: esc = "ne";		break;
			case 8801: esc = "equiv";	break;
			case 8804: esc = "le";		break;
			case 8805: esc = "ge";		break;
			case 8834: esc = "sub";		break;
			case 8835: esc = "sup";		break;
			case 8836: esc = "nsub";	break;
			case 8838: esc = "sube";	break;
			case 8839: esc = "supe";	break;
			case 8853: esc = "oplus";	break;
			case 8855: esc = "otimes";	break;
			case 8869: esc = "perp";	break;
			case 8901: esc = "sdot";	break;
			case 8968: esc = "lceil";	break;
			case 8969: esc = "rceil";	break;
			case 8970: esc = "lfloor";	break;
			case 8971: esc = "rfloor";	break;
			case 9001: esc = "lang";	break;
			case 9002: esc = "rang";	break;
			case 9674: esc = "loz";		break;
			case 9824: esc = "spades";	break;
			case 9827: esc = "clubs";	break;
			case 9829: esc = "hearts";	break;
			case 9830: esc = "diams";	break;
		    }
		}

		// else escape with numeric char refs
		if (esc == null) {
		    stringBuf.setLength (0);
		    stringBuf.append ("#x");
		    stringBuf.append (Integer.toHexString (c).toUpperCase ());
		    esc = stringBuf.toString ();

		    // FIXME:  We don't write surrogate pairs correctly.
		    // They should work as one ref per character, since
		    // each pair is one character.  For reading back into
		    // Unicode, it matters beginning in Unicode 3.1 ...
		}
		break;
	    }
	    if (i != first)
		rawWrite (buf, off + first, i - first);
	    first = i + 1;
	    if (esc == eol)
		newline ();
	    else {
		rawWrite ('&');
		rawWrite (esc);
		rawWrite (';');
	    }
	}
	if (first < len)
	    rawWrite (buf, off + first, len - first);
    }



    private void newline ()
    throws SAXException, IOException
    {
	out.write (eol);
	column = 0;
    }

    private void doIndent ()
    throws SAXException, IOException
    {
	int	space = elementNestLevel * 2;

	newline ();
	column = space;
	// track tabs only at line starts
	while (space > 8) {
	    out.write ("\t");
	    space -= 8;
	}
	while (space > 0) {
	    out.write ("  ");
	    space -= 2;
	}
    }

    private void rawWrite (char c)
    throws IOException
    {
	out.write (c);
	column++;
    }

    private void rawWrite (String s)
    throws SAXException, IOException
    {
	if (prettyPrinting && "default".equals (space.peek ())) {
	    char data [] = s.toCharArray ();
	    rawWrite (data, 0, data.length);
	} else {
	    out.write (s);
	    column += s.length ();
	}
    }

    // NOTE:  if xhtml, the REC gives some rules about whitespace
    // which we could follow ... notably, many places where conformant
    // agents "must" consolidate/normalize whitespace.  Line ends can
    // be removed there, etc.  This may not be the right place to do
    // such mappings though.

    // Line buffering may help clarify algorithms and improve results.

    // It's likely xml:space needs more attention.

    private void rawWrite (char buf [], int offset, int length)
    throws SAXException, IOException
    {
	boolean		wrap;

	if (prettyPrinting && space.empty ())
	    fatal ("stack discipline", null);

	wrap = prettyPrinting && "default".equals (space.peek ());
	if (!wrap) {
	    out.write (buf, offset, length);
	    column += length;
	    return;
	}

	// we're pretty printing and want to fill lines out only
	// to the desired line length.
	while (length > 0) {
	    int		target = lineLength - column;
	    boolean	wrote = false;

	    // Do we even have a problem?
	    if (target > length || noWrap) {
		out.write (buf, offset, length);
		column += length;
		return;
	    }

	    // break the line at a space character, trying to fill
	    // as much of the line as possible.
	    char	c;

	    for (int i = target - 1; i >= 0; i--) {
		if ((c = buf [offset + i]) == ' ' || c == '\t') {
		    i++;
		    out.write (buf, offset, i);
		    doIndent ();
		    offset += i;
		    length -= i;
		    wrote = true;
		    break;
		}
	    }
	    if (wrote)
		continue;
	    
	    // no space character permitting break before target
	    // line length is filled.  So, take the next one.
	    if (target < 0)
		target = 0;
	    for (int i = target; i < length; i++)
		if ((c = buf [offset + i]) == ' ' || c == '\t') {
		    i++;
		    out.write (buf, offset, i);
		    doIndent ();
		    offset += i;
		    length -= i;
		    wrote = true;
		    break;
		}
	    if (wrote)
		continue;
	    
	    // no such luck.
	    out.write (buf, offset, length);
	    column += length;
	    break;
	}
    }
}
