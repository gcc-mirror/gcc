/* DomConsumer.java -- 
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

package gnu.xml.pipeline;

import gnu.xml.util.DomParser;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.AttributesImpl;
import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Element;
import org.w3c.dom.EntityReference;
import org.w3c.dom.Node;
import org.w3c.dom.ProcessingInstruction;
import org.w3c.dom.Text;

/**
 * This consumer builds a DOM Document from its input, acting either as a
 * pipeline terminus or as an intermediate buffer.  When a document's worth
 * of events has been delivered to this consumer, that document is read with
 * a {@link DomParser} and sent to the next consumer.  It is also available
 * as a read-once property.
 *
 * <p>The DOM tree is constructed as faithfully as possible.  There are some
 * complications since a DOM should expose behaviors that can't be implemented
 * without API backdoors into that DOM, and because some SAX parsers don't
 * report all the information that DOM permits to be exposed.  The general
 * problem areas involve information from the Document Type Declaration (DTD).
 * DOM only represents a limited subset, but has some behaviors that depend
 * on much deeper knowledge of a document's DTD.  You shouldn't have much to
 * worry about unless you change handling of "noise" nodes from its default
 * setting (which ignores them all); note if you use JAXP to populate your
 * DOM trees, it wants to save "noise" nodes by default.  (Such nodes include
 * ignorable whitespace, comments, entity references and CDATA boundaries.)
 * Otherwise, your
 * main worry will be if you use a SAX parser that doesn't flag ignorable
 * whitespace unless it's validating (few don't).
 *
 * <p> The SAX2 events used as input must contain XML Names for elements
 * and attributes, with original prefixes.  In SAX2,
 * this is optional unless the "namespace-prefixes" parser feature is set.
 * Moreover, many application components won't provide completely correct
 * structures anyway.  <em>Before you convert a DOM to an output document,
 * you should plan to postprocess it to create or repair such namespace
 * information.</em> The {@link NSFilter} pipeline stage does such work.
 *
 * <p> <em>Note:  changes late in DOM L2 process made it impractical to
 * attempt to create the DocumentType node in any implementation-neutral way,
 * much less to populate it (L1 didn't support even creating such nodes).
 * To create and populate such a node, subclass the inner
 * {@link DomConsumer.Handler} class and teach it about the backdoors into
 * whatever DOM implementation you want.  It's possible that some revised
 * DOM API (L3?) will make this problem solvable again. </em>
 *
 * @see DomParser
 *
 * @author David Brownell
 */
public class DomConsumer implements EventConsumer
{
    private Class		domImpl;

    private boolean		hidingCDATA = true;
    private boolean		hidingComments = true;
    private boolean		hidingWhitespace = true;
    private boolean		hidingReferences = true;

    private Handler		handler;
    private ErrorHandler	errHandler;

    private EventConsumer	next;

    // FIXME:  this can't be a generic pipeline stage just now,
    // since its input became a Class not a String (to be turned
    // into a class, using the right class loader)


    /**
     * Configures this pipeline terminus to use the specified implementation
     * of DOM when constructing its result value.
     *
     * @param impl class implementing {@link org.w3c.dom.Document Document}
     *	which publicly exposes a default constructor
     *
     * @exception SAXException when there is a problem creating an
     *	empty DOM document using the specified implementation
     */
    public DomConsumer (Class impl)
    throws SAXException
    {
	domImpl = impl;
	handler = new Handler (this);
    }

    /**
     * This is the hook through which a subclass provides a handler
     * which knows how to access DOM extensions, specific to some
     * implementation, to record additional data in a DOM.
     * Treat this as part of construction; don't call it except
     * before (or between) parses.
     */
    protected void setHandler (Handler h)
    {
	handler = h;
    }


    private Document emptyDocument ()
    throws SAXException
    {
	try {
	    return (Document) domImpl.newInstance ();
	} catch (IllegalAccessException e) {
	    throw new SAXException ("can't access constructor: "
		    + e.getMessage ());
	} catch (InstantiationException e) {
	    throw new SAXException ("can't instantiate Document: "
		    + e.getMessage ());
	}
    }


    /**
     * Configures this consumer as a buffer/filter, using the specified
     * DOM implementation when constructing its result value.
     *
     * <p> This event consumer acts as a buffer and filter, in that it
     * builds a DOM tree and then writes it out when <em>endDocument</em>
     * is invoked.  Because of the limitations of DOM, much information
     * will as a rule not be seen in that replay.  To get a full fidelity
     * copy of the input event stream, use a {@link TeeConsumer}.
     *
     * @param impl class implementing {@link org.w3c.dom.Document Document}
     *	which publicly exposes a default constructor
     * @param next receives a "replayed" sequence of parse events when
     *	the <em>endDocument</em> method is invoked.
     *
     * @exception SAXException when there is a problem creating an
     *	empty DOM document using the specified DOM implementation
     */
    public DomConsumer (Class impl, EventConsumer n)
    throws SAXException
    {
	this (impl);
	next = n;
    }


    /**
     * Returns the document constructed from the preceding
     * sequence of events.  This method should not be
     * used again until another sequence of events has been
     * given to this EventConsumer.  
     */
    final public Document getDocument ()
    {
	return handler.clearDocument ();
    }

    public void setErrorHandler (ErrorHandler handler)
    {
	errHandler = handler;
    }


    /**
     * Returns true if the consumer is hiding entity references nodes
     * (the default), and false if EntityReference nodes should
     * instead be created.  Such EntityReference nodes will normally be
     * empty, unless an implementation arranges to populate them and then
     * turn them back into readonly objects.
     *
     * @see #setHidingReferences
     */
    final public boolean	isHidingReferences ()
	{ return hidingReferences; }

    /**
     * Controls whether the consumer will hide entity expansions,
     * or will instead mark them with entity reference nodes.
     *
     * @see #isHidingReferences
     * @param flag False if entity reference nodes will appear
     */
    final public void		setHidingReferences (boolean flag)
	{ hidingReferences = flag; }
    

    /**
     * Returns true if the consumer is hiding comments (the default),
     * and false if they should be placed into the output document.
     *
     * @see #setHidingComments
     */
    public final boolean isHidingComments ()
	{ return hidingComments; }

    /**
     * Controls whether the consumer is hiding comments.
     *
     * @see #isHidingComments
     */
    public final void setHidingComments (boolean flag)
	{ hidingComments = flag; }


    /**
     * Returns true if the consumer is hiding ignorable whitespace
     * (the default), and false if such whitespace should be placed
     * into the output document as children of element nodes.
     *
     * @see #setHidingWhitespace
     */
    public final boolean isHidingWhitespace ()
	{ return hidingWhitespace; }

    /**
     * Controls whether the consumer hides ignorable whitespace
     *
     * @see #isHidingComments
     */
    public final void setHidingWhitespace (boolean flag)
	{ hidingWhitespace = flag; }


    /**
     * Returns true if the consumer is saving CDATA boundaries, or
     * false (the default) otherwise.
     *
     * @see #setHidingCDATA
     */
    final public boolean	isHidingCDATA ()
	{ return hidingCDATA; }

    /**
     * Controls whether the consumer will save CDATA boundaries.
     *
     * @see #isHidingCDATA
     * @param flag True to treat CDATA text differently from other
     *	text nodes
     */
    final public void		setHidingCDATA (boolean flag)
	{ hidingCDATA = flag; }
    


    /** Returns the document handler being used. */
    final public ContentHandler getContentHandler ()
	{ return handler; }

    /** Returns the DTD handler being used. */
    final public DTDHandler getDTDHandler ()
	{ return handler; }

    /**
     * Returns the lexical handler being used.
     * (DOM construction can't really use declaration handlers.)
     */
    final public Object getProperty (String id)
    throws SAXNotRecognizedException
    {
	if ("http://xml.org/sax/properties/lexical-handler".equals (id))
	    return handler;
	if ("http://xml.org/sax/properties/declaration-handler".equals (id))
	    return handler;
	throw new SAXNotRecognizedException (id);
    }

    EventConsumer getNext () { return next; }

    ErrorHandler getErrorHandler () { return errHandler; }

    /**
     * Class used to intercept various parsing events and use them to
     * populate a DOM document.  Subclasses would typically know and use
     * backdoors into specific DOM implementations, used to implement 
     * DTD-related functionality.
     *
     * <p> Note that if this ever throws a DOMException (runtime exception)
     * that will indicate a bug in the DOM (e.g. doesn't support something
     * per specification) or the parser (e.g. emitted an illegal name, or
     * accepted illegal input data). </p>
     */
    public static class Handler
	implements ContentHandler, LexicalHandler,
	    DTDHandler, DeclHandler
    {
	protected DomConsumer		consumer;

	private DOMImplementation	impl;
	private Document 		document;
	private boolean		isL2;

	private Locator		locator;
	private Node		top;
	private boolean		inCDATA;
	private boolean		mergeCDATA;
	private boolean		inDTD;
	private String		currentEntity;

	private boolean		recreatedAttrs;
	private AttributesImpl	attributes = new AttributesImpl ();

	/**
	 * Subclasses may use SAX2 events to provide additional
	 * behaviors in the resulting DOM.
	 */
	protected Handler (DomConsumer consumer)
	throws SAXException
	{
	    this.consumer = consumer;
	    document = consumer.emptyDocument ();
	    impl = document.getImplementation ();
	    isL2 = impl.hasFeature ("XML", "2.0");
	}

	private void fatal (String message, Exception x)
	throws SAXException
	{
	    SAXParseException	e;
	    ErrorHandler	errHandler = consumer.getErrorHandler ();

	    if (locator == null)
		e = new SAXParseException (message, null, null, -1, -1, x);
	    else
		e = new SAXParseException (message, locator, x);
	    if (errHandler != null)
		errHandler.fatalError (e);
	    throw e;
	}

	/**
	 * Returns and forgets the document produced.  If the handler is
	 * reused, a new document may be created.
	 */
	Document clearDocument ()
	{
	    Document retval = document;
	    document = null;
	    locator = null;
	    return retval;
	}

	/**
	 * Returns the document under construction.
	 */
	protected Document getDocument ()
	    { return document; }
	
	/**
	 * Returns the current node being populated.  This is usually
	 * an Element or Document, but it might be an EntityReference
	 * node if some implementation-specific code knows how to put
	 * those into the result tree and later mark them as readonly.
	 */
	protected Node getTop ()
	    { return top; }


	// SAX1
	public void setDocumentLocator (Locator locator)
	{
	    this.locator = locator;
	}

	// SAX1
	public void startDocument ()
	throws SAXException
	{
	    if (document == null)
		try {
		    if (isL2) {
			// couple to original implementation
			document = impl.createDocument (null, "foo", null);
			document.removeChild (document.getFirstChild ());
		    } else {
			document = consumer.emptyDocument ();
		    }
		} catch (Exception e) {
		    fatal ("DOM create document", e);
		}
	    top = document;
	}

	// SAX1
	public void endDocument ()
	throws SAXException
	{
	    try {
		if (consumer.getNext () != null && document != null) {
		    DomParser	parser = new DomParser (document);

		    EventFilter.bind (parser, consumer.getNext ());
		    parser.parse ("ignored");
		}
	    } finally {
		top = null;
	    }
	}

	// SAX1
	public void processingInstruction (String target, String data)
	throws SAXException
	{
	    // we can't create populated entity ref nodes using
	    // only public DOM APIs (they've got to be readonly)
	    if (currentEntity != null)
		return;

	    ProcessingInstruction	pi;

	    if (isL2
		    // && consumer.isUsingNamespaces ()
		    && target.indexOf (':') != -1)
		namespaceError (
		    "PI target name is namespace nonconformant: "
			+ target);
	    if (inDTD)
		return;
	    pi = document.createProcessingInstruction (target, data);
	    top.appendChild (pi);
	}

	/**
	 * Subclasses may overrride this method to provide a more efficient
	 * way to construct text nodes.
	 * Typically, copying the text into a single character array will
	 * be more efficient than doing that as well as allocating other
	 * needed for a String, including an internal StringBuffer.
	 * Those additional memory and CPU costs can be incurred later,
	 * if ever needed.
	 * Unfortunately the standard DOM factory APIs encourage those costs
	 * to be incurred early.
	 */
	protected Text createText (
	    boolean	isCDATA,
	    char	ch [],
	    int		start,
	    int		length
	) {
	    String	value = new String (ch, start, length);

	    if (isCDATA)
		return document.createCDATASection (value);
	    else
		return document.createTextNode (value);
	}

	// SAX1
	public void characters (char ch [], int start, int length)
	throws SAXException
	{
	    // we can't create populated entity ref nodes using
	    // only public DOM APIs (they've got to be readonly
	    // at creation time)
	    if (currentEntity != null)
		return;

	    Node	lastChild = top.getLastChild ();

	    // merge consecutive text or CDATA nodes if appropriate.
	    if (lastChild instanceof Text) {
		if (consumer.isHidingCDATA ()
			// consecutive Text content ... always merge
			|| (!inCDATA
			    && !(lastChild instanceof CDATASection))
			// consecutive CDATASection content ... don't
			// merge between sections, only within them
			|| (inCDATA && mergeCDATA
			    && lastChild instanceof CDATASection)
			    ) {
		    CharacterData	last = (CharacterData) lastChild;
		    String		value = new String (ch, start, length);
		    
		    last.appendData (value);
		    return;
		}
	    }
	    if (inCDATA && !consumer.isHidingCDATA ()) {
		top.appendChild (createText (true, ch, start, length));
		mergeCDATA = true;
	    } else
		top.appendChild (createText (false, ch, start, length));
	}

	// SAX2
	public void skippedEntity (String name)
	throws SAXException
	{
	    // this callback is useless except to report errors, since
	    // we can't know if the ref was in content, within an
	    // attribute, within a declaration ... only one of those
	    // cases supports more intelligent action than a panic.
	    fatal ("skipped entity: " + name, null);
	}

	// SAX2
	public void startPrefixMapping (String prefix, String uri)
	throws SAXException
	{
	    // reconstruct "xmlns" attributes deleted by all
	    // SAX2 parsers without "namespace-prefixes" = true
	    if ("".equals (prefix))
		attributes.addAttribute ("", "", "xmlns",
			"CDATA", uri);
	    else
		attributes.addAttribute ("", "", "xmlns:" + prefix,
			"CDATA", uri);
	    recreatedAttrs = true;
	}

	// SAX2
	public void endPrefixMapping (String prefix)
	throws SAXException
	    { }

	// SAX2
	public void startElement (
	    String uri,
	    String localName,
	    String qName,
	    Attributes atts
	) throws SAXException
	{
	    // we can't create populated entity ref nodes using
	    // only public DOM APIs (they've got to be readonly)
	    if (currentEntity != null)
		return;

	    // parser discarded basic information; DOM tree isn't writable
	    // without massaging to assign prefixes to all nodes.
	    // the "NSFilter" class does that massaging.
	    if (qName.length () == 0)
		qName = localName;


	    Element	element;
	    int		length = atts.getLength ();

	    if (!isL2) {
		element = document.createElement (qName);

		// first the explicit attributes ...
		length = atts.getLength ();
		for (int i = 0; i < length; i++)
		    element.setAttribute (atts.getQName (i),
					    atts.getValue (i));
		// ... then any recreated ones (DOM deletes duplicates)
		if (recreatedAttrs) {
		    recreatedAttrs = false;
		    length = attributes.getLength ();
		    for (int i = 0; i < length; i++)
			element.setAttribute (attributes.getQName (i),
						attributes.getValue (i));
		    attributes.clear ();
		}

		top.appendChild (element);
		top = element;
		return;
	    }

	    // For an L2 DOM when namespace use is enabled, use
	    // createElementNS/createAttributeNS except when
	    // (a) it's an element in the default namespace, or
	    // (b) it's an attribute with no prefix
	    String	namespace;
	    
	    if (localName.length () != 0)
		namespace = (uri.length () == 0) ? null : uri;
	    else
		namespace = getNamespace (getPrefix (qName), atts);

	    if (namespace == null)
		element = document.createElement (qName);
	    else
		element = document.createElementNS (namespace, qName);

	    populateAttributes (element, atts);
	    if (recreatedAttrs) {
		recreatedAttrs = false;
		// ... DOM deletes any duplicates
		populateAttributes (element, attributes);
		attributes.clear ();
	    }

	    top.appendChild (element);
	    top = element;
	}

	final static String	xmlnsURI = "http://www.w3.org/2000/xmlns/";

	private void populateAttributes (Element element, Attributes attrs)
	throws SAXParseException
	{
	    int		length = attrs.getLength ();

	    for (int i = 0; i < length; i++) {
		String	type = attrs.getType (i);
		String	value = attrs.getValue (i);
		String	name = attrs.getQName (i);
		String	local = attrs.getLocalName (i);
		String	uri = attrs.getURI (i);

		// parser discarded basic information, DOM tree isn't writable
		if (name.length () == 0)
		    name = local;

		// all attribute types other than these three may not
		// contain scoped names... enumerated attributes get
		// reported as NMTOKEN, except for NOTATION values
		if (!("CDATA".equals (type)
			|| "NMTOKEN".equals (type)
			|| "NMTOKENS".equals (type))) {
		    if (value.indexOf (':') != -1) {
			namespaceError (
				"namespace nonconformant attribute value: "
				    + "<" + element.getNodeName ()
				    + " " + name + "='" + value + "' ...>");
		    }
		}

		// xmlns="" is legal (undoes default NS)
		// xmlns:foo="" is illegal
		String prefix = getPrefix (name);
		String namespace;

		if ("xmlns".equals (prefix)) {
		    if ("".equals (value))
			namespaceError ("illegal null namespace decl, " + name);
		    namespace = xmlnsURI;
		} else if ("xmlns".equals (name))
		    namespace = xmlnsURI;

		else if (prefix == null)
		    namespace = null;
		else if (!"".equals(uri) && uri.length () != 0)
		    namespace = uri;
		else
		    namespace = getNamespace (prefix, attrs);

		if (namespace == null)
		    element.setAttribute (name, value);
		else
		    element.setAttributeNS (namespace, name, value);
	    }
	}

	private String getPrefix (String name)
	{
	    int		temp;

	    if ((temp = name.indexOf (':')) > 0)
		return name.substring (0, temp);
	    return null;
	}

	// used with SAX1-level parser output 
	private String getNamespace (String prefix, Attributes attrs)
	throws SAXParseException
	{
	    String namespace;
	    String decl;

	    // defaulting 
	    if (prefix == null) {
		decl = "xmlns";
		namespace = attrs.getValue (decl);
		if ("".equals (namespace))
		    return null;
		else if (namespace != null)
		    return namespace;

	    // "xmlns" is like a keyword
	    // ... according to the Namespace REC, but DOM L2 CR2+
	    // and Infoset violate that by assigning a namespace.
	    // that conflict is resolved elsewhere.
	    } else if ("xmlns".equals (prefix))
		return null;

	    // "xml" prefix is fixed
	    else if ("xml".equals (prefix))
		return "http://www.w3.org/XML/1998/namespace";

	    // otherwise, expect a declaration
	    else {
		decl = "xmlns:" + prefix;
		namespace = attrs.getValue (decl);
	    }
	    
	    // if we found a local declaration, great
	    if (namespace != null)
		return namespace;


	    // ELSE ... search up the tree we've been building
	    for (Node n = top;
		    n != null && n.getNodeType () != Node.DOCUMENT_NODE;
		    n = n.getParentNode ()) {
		if (n.getNodeType () == Node.ENTITY_REFERENCE_NODE)
		    continue;
		Element e = (Element) n;
		Attr attr = e.getAttributeNode (decl);
		if (attr != null)
		    return attr.getNodeValue ();
	    }
	    // see above re "xmlns" as keyword
	    if ("xmlns".equals (decl))
		return null;

	    namespaceError ("Undeclared namespace prefix: " + prefix);
	    return null;
	}

	// SAX2
	public void endElement (String uri, String localName, String qName)
	throws SAXException
	{
	    // we can't create populated entity ref nodes using
	    // only public DOM APIs (they've got to be readonly)
	    if (currentEntity != null)
		return;

	    top = top.getParentNode ();
	}

	// SAX1 (mandatory reporting if validating)
	public void ignorableWhitespace (char ch [], int start, int length)
	throws SAXException
	{
	    if (consumer.isHidingWhitespace ())
		return;
	    characters (ch, start, length);
	}

	// SAX2 lexical event
	public void startCDATA ()
	throws SAXException
	{
	    inCDATA = true;
	    // true except for the first fragment of a cdata section
	    mergeCDATA = false;
	}
	
	// SAX2 lexical event
	public void endCDATA ()
	throws SAXException
	{
	    inCDATA = false;
	}
	
	// SAX2 lexical event
	//
	// this SAX2 callback merges two unrelated things:
	//	- Declaration of the root element type ... belongs with
	//    the other DTD declaration methods, NOT HERE.
	//	- IDs for the optional external subset ... belongs here
	//    with other lexical information.
	//
	// ...and it doesn't include the internal DTD subset, desired
	// both to support DOM L2 and to enable "pass through" processing
	//
	public void startDTD (String name, String publicId, String SystemId)
	throws SAXException
	{
	    // need to filter out comments and PIs within the DTD
	    inDTD = true;
	}
	
	// SAX2 lexical event
	public void endDTD ()
	throws SAXException
	{
	    inDTD = false;
	}
	
	// SAX2 lexical event
	public void comment (char ch [], int start, int length)
	throws SAXException
	{
	    Node	comment;

	    // we can't create populated entity ref nodes using
	    // only public DOM APIs (they've got to be readonly)
	    if (consumer.isHidingComments ()
		    || inDTD
		    || currentEntity != null)
		return;
	    comment = document.createComment (new String (ch, start, length));
	    top.appendChild (comment);
	}

	/**
	 * May be overridden by subclasses to return true, indicating
	 * that entity reference nodes can be populated and then made
	 * read-only.
	 */
	public boolean canPopulateEntityRefs ()
	    { return false; }

	// SAX2 lexical event
	public void startEntity (String name)
	throws SAXException
	{
	    // are we ignoring what would be contents of an
	    // entity ref, since we can't populate it?
	    if (currentEntity != null)
		return;

	    // Are we hiding all entity boundaries?
	    if (consumer.isHidingReferences ())
		return;

	    // SAX2 shows parameter entities; DOM hides them
	    if (name.charAt (0) == '%' || "[dtd]".equals (name))
		return;

	    // Since we can't create a populated entity ref node in any
	    // standard way, we create an unpopulated one.
	    EntityReference ref = document.createEntityReference (name);
	    top.appendChild (ref);
	    top = ref;

	    // ... allowing subclasses to populate them
	    if (!canPopulateEntityRefs ())
		currentEntity = name;
	}

	// SAX2 lexical event
	public void endEntity (String name)
	throws SAXException
	{
	    if (name.charAt (0) == '%' || "[dtd]".equals (name))
		return;
	    if (name.equals (currentEntity))
		currentEntity = null;
	    if (!consumer.isHidingReferences ())
		top = top.getParentNode ();
	}


	// SAX1 DTD event
	public void notationDecl (
	    String name,
	    String publicId, String SystemId
	) throws SAXException
	{
	    /* IGNORE -- no public DOM API lets us store these
	     * into the doctype node
	     */
	}

	// SAX1 DTD event
	public void unparsedEntityDecl (
	    String name,
	    String publicId, String SystemId,
	    String notationName
	) throws SAXException
	{
	    /* IGNORE -- no public DOM API lets us store these
	     * into the doctype node
	     */
	}

	// SAX2 declaration event
	public void elementDecl (String name, String model)
	throws SAXException
	{
	    /* IGNORE -- no content model support in DOM L2 */
	}

	// SAX2 declaration event
	public void attributeDecl (
	    String eName,
	    String aName,
	    String type,
	    String mode,
	    String value
	) throws SAXException
	{
	    /* IGNORE -- no attribute model support in DOM L2 */
	}

	// SAX2 declaration event
	public void internalEntityDecl (String name, String value)
	throws SAXException
	{
	    /* IGNORE -- no public DOM API lets us store these
	     * into the doctype node
	     */
	}

	// SAX2 declaration event
	public void externalEntityDecl (
	    String name,
	    String publicId,
	    String SystemId
	) throws SAXException
	{
	    /* IGNORE -- no public DOM API lets us store these
	     * into the doctype node
	     */
	}

	//
	// These really should offer the option of nonfatal handling,
	// like other validity errors, though that would cause major
	// chaos in the DOM data structures.  DOM is already spec'd
	// to treat many of these as fatal, so this is consistent.
	//
	private void namespaceError (String description)
	throws SAXParseException
	{
	    SAXParseException err;
	    
	    err = new SAXParseException (description, locator);
	    throw err;
	}
    }
}
