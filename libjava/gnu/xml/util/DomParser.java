/* DomParser.java -- 
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

import java.util.Enumeration;
import java.util.Locale;

import org.xml.sax.*;
import org.xml.sax.helpers.AttributesImpl;
import org.xml.sax.helpers.NamespaceSupport;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.DefaultHandler2;
import org.xml.sax.ext.LexicalHandler;

import org.w3c.dom.*;


/**
 * This parser emits SAX2 parsing events as it traverses a DOM tree, using
 * any conformant implementation of DOM.  It exposes all SAX1 features,
 * and the following SAX2 features and properties (as
 * identified by standard URIs which are not fully provided here).  Note
 * that if a Level 1 DOM implementation is given, then this behaves as if
 * namespaces were disabled, and namespace prefixes were enabled.  </p>
 *
 * <table border="1" width='100%' cellpadding='3' cellspacing='0'>
 * <tr bgcolor='#ccccff'>
 *	<th><font size='+1'>Name</font></th>
 *	<th><font size='+1'>Notes</font></th></tr>
 *
 * <tr><td colspan=2><center><em>Features ... URL prefix is
 * <b>http://xml.org/sax/features/</b></em></center></td></tr>
 *
 * <tr><td>(URL)/external-general-entities</td>
 *	<td>false (does no parsing)</td></tr>
 * <tr><td>(URL)/external-parameter-entities</td>
 *	<td>false (does no parsing)</td></tr>
 * <tr><td>(URL)/namespaces</td>
 *	<td>Value is fixed at <em>true</em></td></tr>
 * <tr><td>(URL)/namespace-prefixes</td>
 *	<td>Value is settable, defaulting to <em>false</em>
 *	(<code>xmlns</code> attributes hidden, and names aren't prefixed)
 *	</td></tr>
 * <tr><td>(URL)/string-interning</td>
 *	<td>Value is fixed at <em>false</em> (DOM provides no
 *	guarantees as to interning)</td></tr>
 * <tr><td>(URL)/validation</td>
 *	<td>false (does no parsing)</td></tr>
 * <tr><td>(URL)/lexical-handler/parameter-entities</td>
 *	<td>false (DOM doesn't do parameter entities)</td></tr>
 *
 * <tr><td colspan=2><center><em>Properties ... URL prefix is
 * <b>http://xml.org/sax/properties/</b></em></center></td></tr>
 *
 *
 * <tr><td>(URL)/dom-node</td>
 *	<td>This property may be set before parsing to hold a DOM
 *	<em>Document</em> node; any arguments given to <em>parse</em>
 *	methods are ignored.  When retrieved
 *	during a parse, this value contains the "current" DOM node.
 *	</td></tr>
 * <tr><td>(URL)/declaration-handler</td>
 *	<td>A declaration handler may be provided.  Declaration of external
 *	general entities is exposed, but not parameter entities; none of the
 *	entity names reported here will begin with "%". </td></tr>
 * <tr><td>(URL)/lexical-handler</td>
 *	<td>A lexical handler may be provided.  While the start and end of
 *	any external subset are reported, expansion of other parameter
 *	entities (e.g. inside attribute list declarations) is not exposed.
 *	Expansion of general entities within attributes is also not exposed
 *	(see below).</td></tr>
 * </table>
 *
 * <P> The consequences of modifying a DOM document tree as it is being walked
 * by this "parser" are unspecified; don't do it! </P>
 *
 * @author David Brownell
 */
final public class DomParser implements XMLReader
{
    // Stuff used internally to route events correctly
    private DefaultHandler2	defaultHandler = new DefaultHandler2 ();

    // per-parse SAX stuff
    private ContentHandler	contentHandler = defaultHandler;
    private DTDHandler		dtdHandler = defaultHandler;
    private DeclHandler		declHandler = defaultHandler;
    private LexicalHandler	lexicalHandler = defaultHandler;

    // shared context
    private ErrorHandler	errHandler = defaultHandler;
    private EntityResolver	resolver = defaultHandler;
    private Locale		locale = Locale.getDefault ();

    // parser state
    private Node		start;
    private Node		current;
    private boolean		isL2;
    private boolean		showNamespaces = true;
    private boolean		showXML1_0 = false;
    private NamespaceSupport	prefixStack = new NamespaceSupport ();
    private boolean		isDocument;


    /**
     * Constructs an unitialized <b>SAX2</b> parser.
     */
    public DomParser () {
    } 

    /**
     * Constructs an <b>SAX2</b> parser initialized to traverse the specified
     * DOM tree.  If the node is a document, the startDocument() and
     * endDocument() calls bracket the calls exposing children.
     */
    public DomParser (Node node) {
	setStart (node);
    } 


    // stuff that most components in an application should be sharing:
    // resolver and error locale.

    /**
     * <b>SAX2</b>: Returns the object used when resolving external
     * entities during parsing (both general and parameter entities).
     */
    public EntityResolver getEntityResolver ()
    {
	return resolver;
    }

    /**
     * <b>SAX1</b>: Provides an object which may be used when resolving external
     * entities during parsing (both general and parameter entities).
     */
    public void setEntityResolver (EntityResolver resolver)
    {
	if (resolver == null)
	    resolver = defaultHandler;
	this.resolver = resolver;
    }

    /**
     * <b>SAX1</b>: Identifies the locale which the parser should use for the
     * diagnostics it provides.
     *
     * @exception SAXException as defined in the specification for
     *	<em>org.xml.sax.Parser.setLocale()</em>
     */
    public void setLocale (Locale locale)
    throws SAXException
    {
	if (locale == null)
	    locale = Locale.getDefault ();
	this.locale = locale;
    }

    
    // different modules will tend to handle error handling the same,
    // but it may not be the same through the whole app

    /**
     * <b>SAX2</b>: Returns the object used to receive callbacks for XML
     * errors of all levels (fatal, nonfatal, warning).
     */
    public ErrorHandler getErrorHandler ()
    {
	return errHandler;
    }

    /**
     * <b>SAX1</b>: Provides an object which receives callbacks for XML errors
     * of all levels (fatal, nonfatal, warning).
     */
    public void setErrorHandler (ErrorHandler handler)
    {
	if (handler == null)
	    handler = defaultHandler;
	errHandler = handler;
    }


    // stuff different parts of a module will handle differently

    /**
     * <b>SAX2</b>: Returns the object used to report the logical
     * content of an XML document.
     */
    public ContentHandler getContentHandler ()
    {
	return contentHandler;
    }

    /**
     * <b>SAX2</b>: Assigns the object used to report the logical
     * content of an XML document.
     */
    public void setContentHandler (ContentHandler handler)
    {
	if (handler == null)
	    handler = defaultHandler;
	contentHandler = handler;
    }

    /**
     * <b>SAX2</b>: Returns the object used to process declarations related
     * to notations and unparsed entities.
     */
    public DTDHandler getDTDHandler ()
    {
	return dtdHandler;
    }

    /**
     * <b>SAX1</b>: Provides an object which may be used to intercept
     * declarations related to notations and unparsed entities.
     */
    public void setDTDHandler (DTDHandler handler)
    {
	if (handler == null)
	    handler = defaultHandler;
	dtdHandler = handler;
    }


    /**
     * <b>SAX1</b>:  Parses the previously provided DOM document (the
     * input parameter is ignored).  When this returns, that same
     * document may be parsed again without needing a "reset".
     *
     * @param uri ignored (pass an empty string)
     * @exception SAXException as defined in the specification for
     *	<em>org.xml.sax.Parser.parse()</em>
     */
    public void parse (String uri) throws SAXException
    {
	parse ();
    }

    /**
     * <b>SAX1</b>:  Parses the previously provided DOM document (the
     * input parameter is ignored).  When this returns, that same
     * document may be parsed again without needing a "reset".
     *
     * @param input ignored
     * @exception SAXException as defined in the specification for
     *	<em>org.xml.sax.Parser.parse()</em>
     */
    public void parse (InputSource input) throws SAXException
    {
	parse ();
    }

    private void parse () throws SAXException
    {
	try {
	    walk ();
	} finally {
	    if (isDocument)
		contentHandler.endDocument ();
	    current = null;
	    prefixStack.reset ();
	}
    }

    private boolean getIsL2 (Node node)
    {
	DOMImplementation	impl;
	Document		doc;

	if (node instanceof Document)
	    doc = (Document) node;
	else
	    doc = node.getOwnerDocument ();
	if (doc == null)
	    throw new RuntimeException ("? unowned node - L2 DTD ?");
	impl = doc.getImplementation ();
	return impl.hasFeature ("XML", "2.0");
    }


    private static final String FEATURES = "http://xml.org/sax/features/";
    private static final String HANDLERS = "http://xml.org/sax/properties/";

    /**
     * <b>SAX2</b>: Tells whether this parser supports the specified feature.
     */
    public boolean getFeature (String name)
    throws SAXNotRecognizedException, SAXNotSupportedException
    {
	// basically, none are relevant -- they relate more to
	// parsing than to walking a "parse tree".

		// FIXME: DOM feature to expose interning?

	if ((FEATURES + "validation").equals (name)
		|| (FEATURES + "external-general-entities")
		    .equals (name)
		|| (FEATURES + "external-parameter-entities")
		    .equals (name)
		|| (FEATURES + "string-interning").equals (name)
		)
	    return false;
    
	if ((FEATURES + "namespaces").equals (name))
	    return showNamespaces;
	if ((FEATURES + "namespace-prefixes").equals (name))
	    return showXML1_0;

	throw new SAXNotRecognizedException (name);
    }

    /**
     * <b>SAX2</b>:  Returns the specified property.  At this time only
     * the declaration and lexical handlers, and current the "DOM" node,
     * are supported.
     */
    public Object getProperty (String name)
    throws SAXNotRecognizedException, SAXNotSupportedException
    {
	if ((HANDLERS + "declaration-handler").equals (name))
	    return declHandler == defaultHandler ? null : declHandler;
	if ((HANDLERS + "lexical-handler").equals (name))
	    return lexicalHandler == defaultHandler ? null : lexicalHandler;

	if ((HANDLERS + "dom-node").equals (name))
	    return current;

	// unknown properties
	throw new SAXNotRecognizedException (name);
    }

    /**
     * <b>SAX2</b>:  Sets the state of features supported in this parser.
     * Only the namespace support features are mutable.
     */
    public void setFeature (String name, boolean state)
    throws SAXNotRecognizedException, SAXNotSupportedException
    {
	if (current != null)
	    throw new IllegalStateException ("feature change midparse");

	boolean value = getFeature (name);

	if (value == state)
	    return;

	if ((FEATURES + "namespaces").equals (name)) {
	    if (!showXML1_0 && state == false)
		throw new SAXNotSupportedException ("Illegal namespace "
			+ "processing configuration");
	    showNamespaces = state;
	    return;
	}
	if ((FEATURES + "namespace-prefixes").equals (name)) {
	    if (!showNamespaces && state == false)
		throw new SAXNotSupportedException ("Illegal namespace "
			+ "processing configuration");
	    showXML1_0 = state;
	    return;
	}

	throw new SAXNotSupportedException (name);
    }

    /**
     * <b>SAX2</b>:  Assigns the specified property.  At this time only
     * declaration and lexical handlers, and the initial DOM document, are
     * supported.  These must not be changed to values of the wrong type.
     * Like SAX1 handlers, these handlers may be changed at any time.
     * Like SAX1 input source or document URI, the initial DOM document
     * may not be changed during a parse.
     */
    public void setProperty (String name, Object state)
    throws SAXNotRecognizedException, SAXNotSupportedException
    {
	if ((HANDLERS + "declaration-handler").equals (name)) {
	    if (!(state instanceof DeclHandler || state == null))
		throw new SAXNotSupportedException (name);
	    declHandler = (DeclHandler) state;
	    return;
	}

	if ((HANDLERS + "lexical-handler").equals (name)) {
	    if (!(state instanceof LexicalHandler || state == null))
		throw new SAXNotSupportedException (name);
	    lexicalHandler = (LexicalHandler) state;
	    return;
	}

	if ((HANDLERS + "dom-node").equals (name)) {
	    if (state == null || state instanceof Node) {
		if (current != null)
		    throw new SAXNotSupportedException (
			"property is readonly during parse:  " + name);
		setStart ((Node) state);
		return;
	    }
	    throw new SAXNotSupportedException ("not a DOM Node");
	}

	// unknown properties
	throw new SAXNotRecognizedException (name);
    }

    private void setStart (Node property)
    {
	start = property;
	if (start != null) {
	    isL2 = getIsL2 (start);
	    isDocument = (start instanceof Document);
	}
    }

    //
    // Non-recursive walk, using DOM state when backtracking is needed
    //
    private void walk ()
    throws SAXException
    {
	int			type;
	NamedNodeMap		nodes;
	int			length;
	AttributesImpl		attrs = new AttributesImpl ();
	char			chars [];
	String			ns, local;

	synchronized (this) {
	    if (current != null)
		throw new IllegalStateException ("already walking tree");

	    // JVM guarantees assignments are atomic; so no other
	    // thread could get this far till this walk's done.
	    current = start;
	}
    
	for (;;) {
	    type = current.getNodeType ();

	    //
	    // First, visit the current node, including any "start" calls
	    //
	    switch (type) {

	      case Node.DOCUMENT_NODE:
	        contentHandler.startDocument ();
		break;

	      case Node.ELEMENT_NODE:
		nodes = current.getAttributes ();
		length = nodes.getLength ();
		prefixStack.pushContext ();
		for (int i = 0; i < length; i++) {
		    Attr	attr = (Attr) nodes.item (i);
		    String	name = attr.getNodeName ();

		    if (showNamespaces && name.startsWith ("xmlns")) {
			String	prefix;
			String	uri;
			
			// NOTE: DOM L2 (CR2+ and REC) violate the
			// Namespaces REC, treat "xmlns" like a strange
			// attribute instead of a magic token
			if ("xmlns".equals (name))
			    prefix = "";
			else
			    prefix = name.substring (6);
			uri = attr.getNodeValue ();

			prefixStack.declarePrefix (prefix, uri);
			contentHandler.startPrefixMapping (prefix, uri);
			
			if (!showXML1_0)
			    continue;
		    }

		    //
		    // NOTE:  DOM doesn't record the attribute type info
		    // which SAX exposes; so this always reports CDATA.
		    //
		    // NOTE:  SAX doesn't expose the isSpecified info which
		    // DOM exposes; that's discarded here.  Similarly with
		    // the information DOM hides inside itself about what
		    // the default values for an attribute are.
		    //
		    if (showNamespaces) {
			if (isL2) {
			    if ((ns = attr.getNamespaceURI ()) == null)
				ns = "";
			    // Note:  SAX2 and DOM handle "local" names
			    // differently
			    if ((local = attr.getLocalName ()) == null)
				local = name;
			} else {
// XXX
			    throw new RuntimeException (
				"NYI, ns lookup when parsing L1 DOM");
			}
		    } else
			ns = local = "";
		    attrs.addAttribute (ns, local, name,
			"CDATA", attr.getNodeValue ());
		}
		if (showNamespaces) {
		    if (isL2) {
			if ((ns = current.getNamespaceURI ()) == null)
			    ns = "";
			// Note:  SAX2 and DOM handle "local" names differently
			if ((local = current.getLocalName ()) == null)
			    local = current.getNodeName ();
		    } else {
// XXX
			throw new RuntimeException (
			    "NYI, ns lookup when parsing L1 DOM");
		    }
		} else
		    ns = local = "";
		contentHandler.startElement  (ns, local,
		    current.getNodeName (), attrs);
		if (length != 0)
		    attrs.clear ();
		break;

	      case Node.CDATA_SECTION_NODE:
		lexicalHandler.startCDATA ();
		chars = current.getNodeValue ().toCharArray ();
		contentHandler.characters (chars, 0, chars.length);
		lexicalHandler.endCDATA ();
		break;

	      case Node.COMMENT_NODE:
		chars = current.getNodeValue ().toCharArray ();
		lexicalHandler.comment (chars, 0, chars.length);
		break;

	      case Node.DOCUMENT_TYPE_NODE:
		{
		    DocumentType	doctype = (DocumentType) current;

		    //
		    // Only DOM L2 supports recreating even some DTDs in full.
		    //
		    if (isL2) {
			lexicalHandler.startDTD (doctype.getName (),
				doctype.getPublicId (),
				doctype.getSystemId ());
		    } else
			lexicalHandler.startDTD (doctype.getName (),
				null, null);
		    
		    //
		    // The only sure way to recreate is to provide both the
		    // internal and external subsets.  Otherwise, only part
		    // of the job can be done ... because from the DTD, DOM
		    // discards both the critical data, like the attribute and
		    // element declarations, as well as the PIs and comments
		    // that are used to hold their documentation.
		    //
		    // Even the entity and notation declarations that it can
		    // expose can't be recorded without proprietary extensions.
		    //
		    // We construct a comment to tell what we know about how
		    // (in)complete this particular really DTD is.
		    //
		    {
			String message;
			char buf [];

			//
			// Though DOM L2 lets the whole doctype be recreated,
			// SAX2 can't represent it (input or output).
			// So this will be the typical case.
			//
			if (isL2 && doctype.getInternalSubset () != null)
			    message =
		    " Full DTD known; can't be shown using SAX2. ";

			//
			// Otherwise, we'll concoct a partial DTD.  If there's
			// any more data here at all, it was provided using a
			// (proprietary) extension to DOM.
			//
			else
			    message =
	    " This DTD was was recreated using incomplete DOM L2 records. ";

			buf = message.toCharArray ();
			lexicalHandler.comment (buf, 0, buf.length);
		    }

		    // report notations first
		    nodes = doctype.getNotations ();
		    length = nodes.getLength ();
		    for (int i = 0; i < length; i++) {
			Notation notation = (Notation) nodes.item (i);
			    dtdHandler.notationDecl (
				notation.getNodeName (),
				notation.getPublicId (),
				notation.getSystemId ());
		    }

		    // then parsed and unparsed external general entities
		    nodes = doctype.getEntities ();
		    length = nodes.getLength ();
		    for (int i = 0; i < length; i++) {
			Entity	entity = (Entity) nodes.item (i);
			String	notation = entity.getNotationName ();

			if (notation != null)
			    dtdHandler.unparsedEntityDecl (
				entity.getNodeName (),
				entity.getPublicId (),
				entity.getSystemId (),
				notation);
			else if (entity.getSystemId () != null)
			    declHandler.externalEntityDecl (
				entity.getNodeName (),
				entity.getPublicId (),
				entity.getSystemId ());
			
			//
			// NOTE:  DOM doesn't clearly provide internal
			// entity support; but in case someone tries to
			// fudge such support, we defend ourselves above.
			//
			// NOTE:  DOM doesn't expose parameter entities
			// (thank you thank you thank you thank you)
			//
		    }

		    //
		    // NOTE:  DOM (levels 1 and 2) doesn't expose real
		    // typing information (element or attribute decls),
		    // as exposed by SAX2 declaration handlers.
		    //
		    lexicalHandler.endDTD ();
		}
		break;

	      case Node.ENTITY_REFERENCE_NODE:
		// this isn't done except (a) in content, and
		// (b) not within a start tag (att value)
		lexicalHandler.startEntity (current.getNodeName ());
		break;

	      case Node.PROCESSING_INSTRUCTION_NODE:
	        contentHandler.processingInstruction (
		    current.getNodeName (), current.getNodeValue ());
		break;

	      case Node.TEXT_NODE:
		chars = current.getNodeValue ().toCharArray ();
		contentHandler.characters (chars, 0, chars.length);
		break;

	      default:
		// e.g. fragments, entities, notations, attributes
		throw new SAXException ("Illegal DOM Node type in Document:  "
		    +  current.getNodeType ());
	    }

	    //
	    // Then, pick the next node to visit.  If the next node isn't
	    // a child, an "end" call may be needed before moving on.
	    // If there's no next node, we're done.
	    //
	    Node		next;

	    switch (type) {
	      case Node.DOCUMENT_NODE:
	      case Node.ELEMENT_NODE:
	      case Node.ENTITY_REFERENCE_NODE:
		//
		// For elements that can have children, visit those
		// children before any siblings (i.e. depth first)
		// and after visiting this node (i.e. preorder)
		//
		next = current.getFirstChild ();
		if (next != null) {
		    current = next;
		    break;
		}
		//
		// Else treat this like other childless nodes, but
		// handle this node's "end" immediately.
		//
		callEnd (current);

		// FALLTHROUGH

	      case Node.CDATA_SECTION_NODE:
	      case Node.COMMENT_NODE:
	      case Node.DOCUMENT_TYPE_NODE:
	      case Node.ENTITY_NODE:
	      case Node.PROCESSING_INSTRUCTION_NODE:
	      case Node.TEXT_NODE:
		//
		// Use next sibling, if there is one.
		// Else, climb up a level (calling "end")
		//	until we find an ancestral sibling
		//	or until we we climb off the top (FINISH)
		//
		for (;;) {
		    if ((next = current.getNextSibling ()) != null)
			break;
		    current = current.getParentNode ();
		    if (current == null || current == start)
			return;
		    callEnd (current);
		}
		current = next;
		break;

	      default:
		throw new SAXException (
		    "Illegal DOM Node type found:  " +  current.getNodeType ());
	    }
	}
    }

    private void callEnd (Node node) throws SAXException
    {
	switch (node.getNodeType ()) {
	  // only these three container types may ever be found
	  // directly inside a Document.
	  case Node.DOCUMENT_NODE:
	    // for SAX conformance, endDocument must always
	    // be called ... it's done in a "finally" clause)
	    return;

	  case Node.ELEMENT_NODE:
	    if (showNamespaces) {
		if (isL2)
		    contentHandler.endElement (
			node.getNamespaceURI (),
			node.getLocalName (),
			node.getNodeName ());
		else
// XXX
		    throw new RuntimeException (
			"NYI, ns lookup when parsing L1 DOM");
		for (Enumeration e = prefixStack.getDeclaredPrefixes ();
			e.hasMoreElements ();
			) {
		    contentHandler.endPrefixMapping ((String) e.nextElement ());
		}
	    } else
		contentHandler.endElement ("", "", node.getNodeName ());
	    prefixStack.popContext ();
	    return;

	  case Node.ENTITY_REFERENCE_NODE:
	    // see above -- in content, outside start tags.
	    lexicalHandler.endEntity (node.getNodeName ());
	    return;

	  // these can be given at the top level
	  case Node.DOCUMENT_FRAGMENT_NODE:
	  case Node.ATTRIBUTE_NODE:
	    return;

	  default:
	    throw new SAXException (
		"Illegal DOM container type found:  "
			+  current.getNodeType ());
	}
    }
}
