// ParserAdapter.java - adapt a SAX1 Parser to a SAX2 XMLReader.
// Written by David Megginson, sax@megginson.com
// NO WARRANTY!  This class is in the public domain.

// $Id: ParserAdapter.java,v 1.1 2000/10/02 02:43:20 sboag Exp $

package org.xml.sax.helpers;

import java.io.IOException;
import java.util.Enumeration;

import org.xml.sax.Parser;	// deprecated
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.AttributeList; // deprecated
import org.xml.sax.EntityResolver;
import org.xml.sax.DTDHandler;
import org.xml.sax.DocumentHandler; // deprecated
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import org.xml.sax.XMLReader;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;


/**
 * Adapt a SAX1 Parser as a SAX2 XMLReader.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * </blockquote>
 *
 * <p>This class wraps a SAX1 {@link org.xml.sax.Parser Parser}
 * and makes it act as a SAX2 {@link org.xml.sax.XMLReader XMLReader},
 * with feature, property, and Namespace support.  Note
 * that it is not possible to report {@link org.xml.sax.ContentHandler#skippedEntity
 * skippedEntity} events, since SAX1 does not make that information available.</p>
 *
 * <p>This adapter does not test for duplicate Namespace-qualified
 * attribute names.</p>
 *
 * @since SAX 2.0
 * @author David Megginson, 
 *         <a href="mailto:sax@megginson.com">sax@megginson.com</a>
 * @version 2.0
 * @see org.xml.sax.helpers.XMLReaderAdapter
 * @see org.xml.sax.XMLReader
 * @see org.xml.sax.Parser
 */
public class ParserAdapter implements XMLReader, DocumentHandler
{


    ////////////////////////////////////////////////////////////////////
    // Constructors.
    ////////////////////////////////////////////////////////////////////


    /**
     * Construct a new parser adapter.
     *
     * <p>Use the "org.xml.sax.parser" property to locate the
     * embedded SAX1 driver.</p>
     *
     * @exception org.xml.sax.SAXException If the embedded driver
     *            cannot be instantiated or if the
     *            org.xml.sax.parser property is not specified.
     */
    public ParserAdapter ()
      throws SAXException
    {
	super();

	String driver = System.getProperty("org.xml.sax.parser");

	try {
	    setup(ParserFactory.makeParser());
	} catch (ClassNotFoundException e1) {
	    throw new
		SAXException("Cannot find SAX1 driver class " +
			     driver, e1);
	} catch (IllegalAccessException e2) {
	    throw new
		SAXException("SAX1 driver class " +
			     driver +
			     " found but cannot be loaded", e2);
	} catch (InstantiationException e3) {
	    throw new
		SAXException("SAX1 driver class " +
			     driver +
			     " loaded but cannot be instantiated", e3);
	} catch (ClassCastException e4) {
	    throw new
		SAXException("SAX1 driver class " +
			     driver +
			     " does not implement org.xml.sax.Parser");
	} catch (NullPointerException e5) {
	    throw new 
		SAXException("System property org.xml.sax.parser not specified");
	}
    }


    /**
     * Construct a new parser adapter.
     *
     * <p>Note that the embedded parser cannot be changed once the
     * adapter is created; to embed a different parser, allocate
     * a new ParserAdapter.</p>
     *
     * @param parser The SAX1 parser to embed.
     * @exception java.lang.NullPointerException If the parser parameter
     *            is null.
     */
    public ParserAdapter (Parser parser)
    {
	super();
	setup(parser);
    }


    /**
     * Internal setup method.
     *
     * @param parser The embedded parser.
     * @exception java.lang.NullPointerException If the parser parameter
     *            is null.
     */
    private void setup (Parser parser)
    {
	if (parser == null) {
	    throw new
		NullPointerException("Parser argument must not be null");
	}
	this.parser = parser;
	atts = new AttributesImpl();
	nsSupport = new NamespaceSupport();
	attAdapter = new AttributeListAdapter();
    }



    ////////////////////////////////////////////////////////////////////
    // Implementation of org.xml.sax.XMLReader.
    ////////////////////////////////////////////////////////////////////


    //
    // Internal constants for the sake of convenience.
    //
    private final static String FEATURES = "http://xml.org/sax/features/";
    private final static String NAMESPACES = FEATURES + "namespaces";
    private final static String NAMESPACE_PREFIXES = FEATURES + "namespace-prefixes";
    private final static String VALIDATION = FEATURES + "validation";
    private final static String EXTERNAL_GENERAL =
	FEATURES + "external-general-entities";
    private final static String EXTERNAL_PARAMETER =
	FEATURES + "external-parameter-entities";


    /**
     * Set a feature for the parser.
     *
     * <p>The only features supported are namespaces and 
     * namespace-prefixes.</p>
     *
     * @param name The feature name, as a complete URI.
     * @param state The requested feature state.
     * @exception org.xml.sax.SAXNotRecognizedException If the feature
     *            name is not known.
     * @exception org.xml.sax.SAXNotSupportedException If the feature
     *            state is not supported.
     * @see org.xml.sax.XMLReader#setFeature
     */
    public void setFeature (String name, boolean state)
	throws SAXNotRecognizedException, SAXNotSupportedException
    {
	if (name.equals(NAMESPACES)) {
	    checkNotParsing("feature", name);
	    namespaces = state;
	    if (!namespaces && !prefixes) {
		prefixes = true;
	    }
	} else if (name.equals(NAMESPACE_PREFIXES)) {
	    checkNotParsing("feature", name);
	    prefixes = state;
	    if (!prefixes && !namespaces) {
		namespaces = true;
	    }
	} else if (name.equals(VALIDATION) ||
		   name.equals(EXTERNAL_GENERAL) ||
		   name.equals(EXTERNAL_PARAMETER)) {
	    throw new SAXNotSupportedException("Feature: " + name);
	} else {
	    throw new SAXNotRecognizedException("Feature: " + name);
	}
    }


    /**
     * Check a parser feature.
     *
     * <p>The only features supported are namespaces and 
     * namespace-prefixes.</p>
     *
     * @param name The feature name, as a complete URI.
     * @return The current feature state.
     * @exception org.xml.sax.SAXNotRecognizedException If the feature
     *            name is not known.
     * @exception org.xml.sax.SAXNotSupportedException If querying the
     *            feature state is not supported.
     * @see org.xml.sax.XMLReader#setFeature
     */
    public boolean getFeature (String name)
	throws SAXNotRecognizedException, SAXNotSupportedException
    {
	if (name.equals(NAMESPACES)) {
	    return namespaces;
	} else if (name.equals(NAMESPACE_PREFIXES)) {
	    return prefixes;
	} else if (name.equals(VALIDATION) ||
		   name.equals(EXTERNAL_GENERAL) ||
		   name.equals(EXTERNAL_PARAMETER)) {
	    throw new SAXNotSupportedException("Feature: " + name);
	} else {
	    throw new SAXNotRecognizedException("Feature: " + name);
	}
    }


    /**
     * Set a parser property.
     *
     * <p>No special properties are currently supported.</p>
     *
     * @param name The property name.
     * @param value The property value.
     * @exception org.xml.sax.SAXNotRecognizedException If the feature
     *            name is not known.
     * @exception org.xml.sax.SAXNotSupportedException If the feature
     *            state is not supported.
     * @see org.xml.sax.XMLReader#setProperty
     */
    public void setProperty (String name, Object value)
	throws SAXNotRecognizedException, SAXNotSupportedException
    {
	throw new SAXNotRecognizedException("Property: " + name);
    }


    /**
     * Get a parser property.
     *
     * <p>No special properties are currently supported.</p>
     *
     * @param name The property name.
     * @return The property value.
     * @exception org.xml.sax.SAXNotRecognizedException If the feature
     *            name is not known.
     * @exception org.xml.sax.SAXNotSupportedException If the feature
     *            state is not supported.
     * @see org.xml.sax.XMLReader#getProperty
     */
    public Object getProperty (String name)
	throws SAXNotRecognizedException, SAXNotSupportedException
    {
	throw new SAXNotRecognizedException("Property: " + name);
    }


    /**
     * Set the entity resolver.
     *
     * @param resolver The new entity resolver.
     * @exception java.lang.NullPointerException If the entity resolver
     *            parameter is null.
     * @see org.xml.sax.XMLReader#setEntityResolver
     */
    public void setEntityResolver (EntityResolver resolver)
    {
	if (resolver == null) {
	    throw new NullPointerException("Null entity resolver");
	}
	entityResolver = resolver;
    }


    /**
     * Return the current entity resolver.
     *
     * @return The current entity resolver, or null if none was supplied.
     * @see org.xml.sax.XMLReader#getEntityResolver
     */
    public EntityResolver getEntityResolver ()
    {
	return entityResolver;
    }


    /**
     * Set the DTD handler.
     *
     * @param resolver The new DTD handler.
     * @exception java.lang.NullPointerException If the DTD handler
     *            parameter is null.
     * @see org.xml.sax.XMLReader#setEntityResolver
     */
    public void setDTDHandler (DTDHandler handler)
    {
	if (handler == null) {
	    throw new NullPointerException("Null DTD handler");
	}
	dtdHandler = handler;
    }


    /**
     * Return the current DTD handler.
     *
     * @return The current DTD handler, or null if none was supplied.
     * @see org.xml.sax.XMLReader#getEntityResolver
     */
    public DTDHandler getDTDHandler ()
    {
	return dtdHandler;
    }


    /**
     * Set the content handler.
     *
     * @param resolver The new content handler.
     * @exception java.lang.NullPointerException If the content handler
     *            parameter is null.
     * @see org.xml.sax.XMLReader#setEntityResolver
     */
    public void setContentHandler (ContentHandler handler)
    {
	if (handler == null) {
	    throw new NullPointerException("Null content handler");
	}
	contentHandler = handler;
    }


    /**
     * Return the current content handler.
     *
     * @return The current content handler, or null if none was supplied.
     * @see org.xml.sax.XMLReader#getEntityResolver
     */
    public ContentHandler getContentHandler ()
    {
	return contentHandler;
    }


    /**
     * Set the error handler.
     *
     * @param resolver The new error handler.
     * @exception java.lang.NullPointerException If the error handler
     *            parameter is null.
     * @see org.xml.sax.XMLReader#setEntityResolver
     */
    public void setErrorHandler (ErrorHandler handler)
    {
	if (handler == null) {
	    throw new NullPointerException("Null error handler");
	}
	errorHandler = handler;
    }


    /**
     * Return the current error handler.
     *
     * @return The current error handler, or null if none was supplied.
     * @see org.xml.sax.XMLReader#getEntityResolver
     */
    public ErrorHandler getErrorHandler ()
    {
	return errorHandler;
    }


    /**
     * Parse an XML document.
     *
     * @param systemId The absolute URL of the document.
     * @exception java.io.IOException If there is a problem reading
     *            the raw content of the document.
     * @exception org.xml.sax.SAXException If there is a problem
     *            processing the document.
     * @see #parse(org.xml.sax.InputSource)
     * @see org.xml.sax.Parser#parse(java.lang.String)
     */
    public void parse (String systemId)
	throws IOException, SAXException
    {
	parse(new InputSource(systemId));
    }


    /**
     * Parse an XML document.
     *
     * @param input An input source for the document.
     * @exception java.io.IOException If there is a problem reading
     *            the raw content of the document.
     * @exception org.xml.sax.SAXException If there is a problem
     *            processing the document.
     * @see #parse(java.lang.String)
     * @see org.xml.sax.Parser#parse(org.xml.sax.InputSource)
     */
    public void parse (InputSource input)
	throws IOException, SAXException
    {
	if (parsing) {
	    throw new SAXException("Parser is already in use");
	}
	setupParser();
	parsing = true;
	try {
	    parser.parse(input);
	} finally {
	    parsing = false;
	}
	parsing = false;
    }



    ////////////////////////////////////////////////////////////////////
    // Implementation of org.xml.sax.DocumentHandler.
    ////////////////////////////////////////////////////////////////////


    /**
     * Adapt a SAX1 document locator event.
     *
     * @param locator A document locator.
     * @see org.xml.sax.ContentHandler#setDocumentLocator
     */
    public void setDocumentLocator (Locator locator)
    {
	this.locator = locator;
	if (contentHandler != null) {
	    contentHandler.setDocumentLocator(locator);
	}
    }


    /**
     * Adapt a SAX1 start document event.
     *
     * @exception org.xml.sax.SAXException The client may raise a
     *            processing exception.
     * @see org.xml.sax.DocumentHandler#startDocument
     */
    public void startDocument ()
	throws SAXException
    {
	if (contentHandler != null) {
	    contentHandler.startDocument();
	}
    }


    /**
     * Adapt a SAX1 end document event.
     *
     * @exception org.xml.sax.SAXException The client may raise a
     *            processing exception.
     * @see org.xml.sax.DocumentHandler#endDocument
     */
    public void endDocument ()
	throws SAXException
    {
	if (contentHandler != null) {
	    contentHandler.endDocument();
	}
    }


    /**
     * Adapt a SAX1 startElement event.
     *
     * <p>If necessary, perform Namespace processing.</p>
     *
     * @param qName The qualified (prefixed) name.
     * @param qAtts The XML 1.0 attribute list (with qnames).
     */
    public void startElement (String qName, AttributeList qAtts)
	throws SAXException
    {
				// If we're not doing Namespace
				// processing, dispatch this quickly.
	if (!namespaces) {
	    if (contentHandler != null) {
		attAdapter.setAttributeList(qAtts);
		contentHandler.startElement("", "", qName.intern(),
					    attAdapter);
	    }
	    return;
	}


				// OK, we're doing Namespace processing.
	nsSupport.pushContext();
	boolean seenDecl = false;
	atts.clear();
	
				// Take a first pass and copy all
				// attributes into the SAX2 attribute
				// list, noting any Namespace 
				// declarations.
	int length = qAtts.getLength();
	for (int i = 0; i < length; i++) {
	    String attQName = qAtts.getName(i);
	    String type = qAtts.getType(i);
	    String value = qAtts.getValue(i);

				// Found a declaration...
	    if (attQName.startsWith("xmlns")) {
		String prefix;
		int n = attQName.indexOf(':');
		if (n == -1) {
		    prefix = "";
		} else {
		    prefix = attQName.substring(n+1);
		}
		if (!nsSupport.declarePrefix(prefix, value)) {
		    reportError("Illegal Namespace prefix: " + prefix);
		}
		if (contentHandler != null) {
		    contentHandler.startPrefixMapping(prefix, value);
		}
				// We may still have to add this to
				// the list.
		if (prefixes) {
		    atts.addAttribute("", "", attQName.intern(),
				      type, value);
		}
		seenDecl = true;

				// This isn't a declaration.
	    } else {
		String attName[] = processName(attQName, true);
		atts.addAttribute(attName[0], attName[1], attName[2],
				  type, value);
	    }
	}
	
				// If there was a Namespace declaration,
				// we have to make a second pass just
				// to be safe -- this will happen very
				// rarely, possibly only once for each
				// document.
	if (seenDecl) {
	    length = atts.getLength();
	    for (int i = 0; i < length; i++) {
		String attQName = atts.getQName(i);
		if (!attQName.startsWith("xmlns")) {
		    String attName[] = processName(attQName, true);
		    atts.setURI(i, attName[0]);
		    atts.setLocalName(i, attName[1]);
		}
	    }
	}

				// OK, finally report the event.
	if (contentHandler != null) {
	    String name[] = processName(qName, false);
	    contentHandler.startElement(name[0], name[1], name[2], atts);
	}
    }


    /**
     * Adapt a SAX1 end element event.
     *
     * @param qName The qualified (prefixed) name.
     * @exception org.xml.sax.SAXException The client may raise a
     *            processing exception.
     * @see org.xml.sax.DocumentHandler#endElement
     */
    public void endElement (String qName)
	throws SAXException
    {
				// If we're not doing Namespace
				// processing, dispatch this quickly.
	if (!namespaces) {
	    if (contentHandler != null) {
		contentHandler.endElement("", "", qName.intern());
	    }
	    return;
	}

				// Split the name.
	String names[] = processName(qName, false);
	if (contentHandler != null) {
	    contentHandler.endElement(names[0], names[1], names[2]);
	    Enumeration prefixes = nsSupport.getDeclaredPrefixes();
	    while (prefixes.hasMoreElements()) {
		String prefix = (String)prefixes.nextElement();
		contentHandler.endPrefixMapping(prefix);
	    }
	}
	nsSupport.popContext();
    }


    /**
     * Adapt a SAX1 characters event.
     *
     * @param ch An array of characters.
     * @param start The starting position in the array.
     * @param length The number of characters to use.
     * @exception org.xml.sax.SAXException The client may raise a
     *            processing exception.
     * @see org.xml.sax.DocumentHandler#characters
     */
    public void characters (char ch[], int start, int length)
	throws SAXException
    {
	if (contentHandler != null) {
	    contentHandler.characters(ch, start, length);
	}
    }


    /**
     * Adapt a SAX1 ignorable whitespace event.
     *
     * @param ch An array of characters.
     * @param start The starting position in the array.
     * @param length The number of characters to use.
     * @exception org.xml.sax.SAXException The client may raise a
     *            processing exception.
     * @see org.xml.sax.DocumentHandler#ignorableWhitespace
     */
    public void ignorableWhitespace (char ch[], int start, int length)
	throws SAXException
    {
	if (contentHandler != null) {
	    contentHandler.ignorableWhitespace(ch, start, length);
	}
    }


    /**
     * Adapt a SAX1 processing instruction event.
     *
     * @param target The processing instruction target.
     * @param data The remainder of the processing instruction
     * @exception org.xml.sax.SAXException The client may raise a
     *            processing exception.
     * @see org.xml.sax.DocumentHandler#processingInstruction
     */
    public void processingInstruction (String target, String data)
	throws SAXException
    {
	if (contentHandler != null) {
	    contentHandler.processingInstruction(target, data);
	}
    }



    ////////////////////////////////////////////////////////////////////
    // Internal utility methods.
    ////////////////////////////////////////////////////////////////////


    /**
     * Initialize the parser before each run.
     */
    private void setupParser ()
    {
	nsSupport.reset();

	if (entityResolver != null) {
	    parser.setEntityResolver(entityResolver);
	}
	if (dtdHandler != null) {
	    parser.setDTDHandler(dtdHandler);
	}
	if (errorHandler != null) {
	    parser.setErrorHandler(errorHandler);
	}
	parser.setDocumentHandler(this);
	locator = null;
    }


    /**
     * Process a qualified (prefixed) name.
     *
     * <p>If the name has an undeclared prefix, use only the qname
     * and make an ErrorHandler.error callback in case the app is
     * interested.</p>
     *
     * @param qName The qualified (prefixed) name.
     * @param isAttribute true if this is an attribute name.
     * @return The name split into three parts.
     * @exception org.xml.sax.SAXException The client may throw
     *            an exception if there is an error callback.
     */
    private String [] processName (String qName, boolean isAttribute)
	throws SAXException
    {
	String parts[] = nsSupport.processName(qName, nameParts,
					       isAttribute);
	if (parts == null) {
	    parts = new String[3];
	    parts[2] = qName.intern();
	    reportError("Undeclared prefix: " + qName);
	}
	return parts;
    }


    /**
     * Report a non-fatal error.
     *
     * @param message The error message.
     * @exception org.xml.sax.SAXException The client may throw
     *            an exception.
     */
    void reportError (String message)
	throws SAXException
    {
	if (errorHandler == null) {
	    return;
	}

	SAXParseException e;
	if (locator != null) {
	    e = new SAXParseException(message, locator);
	} else {
	    e = new SAXParseException(message, null, null, -1, -1);
	}
	errorHandler.error(e);
    }


    /**
     * Throw an exception if we are parsing.
     *
     * <p>Use this method to detect illegal feature or
     * property changes.</p>
     *
     * @param type The type of thing (feature or property).
     * @param name The feature or property name.
     * @exception org.xml.sax.SAXNotSupportedException If a
     *            document is currently being parsed.
     */
    private void checkNotParsing (String type, String name)
	throws SAXNotSupportedException
    {
	if (parsing) {
	    throw new SAXNotSupportedException("Cannot change " +
					       type + ' ' +
					       name + " while parsing");
					       
	}
    }



    ////////////////////////////////////////////////////////////////////
    // Internal state.
    ////////////////////////////////////////////////////////////////////

    private NamespaceSupport nsSupport;
    private AttributeListAdapter attAdapter;

    private boolean parsing = false;
    private String nameParts[] = new String[3];

    private Parser parser = null;

    private AttributesImpl atts = null;

				// Features
    private boolean namespaces = true;
    private boolean prefixes = false;

				// Properties

				// Handlers
    Locator locator;

    EntityResolver entityResolver = null;
    DTDHandler dtdHandler = null;
    ContentHandler contentHandler = null;
    ErrorHandler errorHandler = null;



    ////////////////////////////////////////////////////////////////////
    // Inner class to wrap an AttributeList when not doing NS proc.
    ////////////////////////////////////////////////////////////////////


    /**
     * Adapt a SAX1 AttributeList as a SAX2 Attributes object.
     *
     * <p>This class is in the Public Domain, and comes with NO
     * WARRANTY of any kind.</p>
     *
     * <p>This wrapper class is used only when Namespace support
     * is disabled -- it provides pretty much a direct mapping
     * from SAX1 to SAX2, except that names and types are 
     * interned whenever requested.</p>
     */
    final class AttributeListAdapter implements Attributes
    {

	/**
	 * Construct a new adapter.
	 */
	AttributeListAdapter ()
	{
	}


	/**
	 * Set the embedded AttributeList.
	 *
	 * <p>This method must be invoked before any of the others
	 * can be used.</p>
	 *
	 * @param The SAX1 attribute list (with qnames).
	 */
	void setAttributeList (AttributeList qAtts)
	{
	    this.qAtts = qAtts;
	}


	/**
	 * Return the length of the attribute list.
	 *
	 * @return The number of attributes in the list.
	 * @see org.xml.sax.Attributes#getLength
	 */
	public int getLength ()
	{
	    return qAtts.getLength();
	}


	/**
	 * Return the Namespace URI of the specified attribute.
	 *
	 * @param The attribute's index.
	 * @return Always the empty string.
	 * @see org.xml.sax.Attributes#getURI
	 */
	public String getURI (int i)
	{
	    return "";
	}


	/**
	 * Return the local name of the specified attribute.
	 *
	 * @param The attribute's index.
	 * @return Always the empty string.
	 * @see org.xml.sax.Attributes#getLocalName
	 */
	public String getLocalName (int i)
	{
	    return "";
	}


	/**
	 * Return the qualified (prefixed) name of the specified attribute.
	 *
	 * @param The attribute's index.
	 * @return The attribute's qualified name, internalized.
	 */
	public String getQName (int i)
	{
	    return qAtts.getName(i).intern();
	}


	/**
	 * Return the type of the specified attribute.
	 *
	 * @param The attribute's index.
	 * @return The attribute's type as an internalized string.
	 */
	public String getType (int i)
	{
	    return qAtts.getType(i).intern();
	}


	/**
	 * Return the value of the specified attribute.
	 *
	 * @param The attribute's index.
	 * @return The attribute's value.
	 */
	public String getValue (int i)
	{
	    return qAtts.getValue(i);
	}


	/**
	 * Look up an attribute index by Namespace name.
	 *
	 * @param uri The Namespace URI or the empty string.
	 * @param localName The local name.
	 * @return The attributes index, or -1 if none was found.
	 * @see org.xml.sax.Attributes#getIndex(java.lang.String,java.lang.String)
	 */
	public int getIndex (String uri, String localName)
	{
	    return -1;
	}


	/**
	 * Look up an attribute index by qualified (prefixed) name.
	 *
	 * @param qName The qualified name.
	 * @return The attributes index, or -1 if none was found.
	 * @see org.xml.sax.Attributes#getIndex(java.lang.String)
	 */
	public int getIndex (String qName)
	{
	    int max = atts.getLength();
	    for (int i = 0; i < max; i++) {
		if (qAtts.getName(i).equals(qName)) {
		    return i;
		}
	    }
	    return -1;
	}


	/**
	 * Look up the type of an attribute by Namespace name.
	 *
	 * @param uri The Namespace URI
	 * @param localName The local name.
	 * @return The attribute's type as an internalized string.
	 */
	public String getType (String uri, String localName)
	{
	    return null;
	}


	/**
	 * Look up the type of an attribute by qualified (prefixed) name.
	 *
	 * @param qName The qualified name.
	 * @return The attribute's type as an internalized string.
	 */
	public String getType (String qName)
	{
	    return qAtts.getType(qName).intern();
	}


	/**
	 * Look up the value of an attribute by Namespace name.
	 *
	 * @param uri The Namespace URI
	 * @param localName The local name.
	 * @return The attribute's value.
	 */
	public String getValue (String uri, String localName)
	{
	    return null;
	}


	/**
	 * Look up the value of an attribute by qualified (prefixed) name.
	 *
	 * @param qName The qualified name.
	 * @return The attribute's value.
	 */
	public String getValue (String qName)
	{
	    return qAtts.getValue(qName);
	}

	private AttributeList qAtts;
    }
}

// end of ParserAdapter.java
