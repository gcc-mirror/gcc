// SAX exception class.
// http://www.saxproject.org
// No warranty; no copyright -- use this as you will.
// $Id: SAXParseException.java,v 1.3.2.5 2002/01/29 21:34:14 dbrownell Exp $

package org.xml.sax;

/**
 * Encapsulate an XML parse error or warning.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>This exception may include information for locating the error
 * in the original XML document, as if it came from a {@link Locator}
 * object.  Note that although the application
 * will receive a SAXParseException as the argument to the handlers
 * in the {@link org.xml.sax.ErrorHandler ErrorHandler} interface, 
 * the application is not actually required to throw the exception; 
 * instead, it can simply read the information in it and take a 
 * different action.</p>
 *
 * <p>Since this exception is a subclass of {@link org.xml.sax.SAXException 
 * SAXException}, it inherits the ability to wrap another exception.</p>
 *
 * @since SAX 1.0
 * @author David Megginson
 * @version 2.0.1 (sax2r2)
 * @see org.xml.sax.SAXException
 * @see org.xml.sax.Locator
 * @see org.xml.sax.ErrorHandler
 */
public class SAXParseException extends SAXException {
    
    
    //////////////////////////////////////////////////////////////////////
    // Constructors.
    //////////////////////////////////////////////////////////////////////


    /**
     * Create a new SAXParseException from a message and a Locator.
     *
     * <p>This constructor is especially useful when an application is
     * creating its own exception from within a {@link org.xml.sax.ContentHandler
     * ContentHandler} callback.</p>
     *
     * @param message The error or warning message.
     * @param locator The locator object for the error or warning (may be
     *        null).
     * @see org.xml.sax.Locator
     */
    public SAXParseException (String message, Locator locator) {
	super(message);
	if (locator != null) {
	    init(locator.getPublicId(), locator.getSystemId(),
		 locator.getLineNumber(), locator.getColumnNumber());
	} else {
	    init(null, null, -1, -1);
	}
    }
    
    
    /**
     * Wrap an existing exception in a SAXParseException.
     *
     * <p>This constructor is especially useful when an application is
     * creating its own exception from within a {@link org.xml.sax.ContentHandler
     * ContentHandler} callback, and needs to wrap an existing exception that is not a
     * subclass of {@link org.xml.sax.SAXException SAXException}.</p>
     *
     * @param message The error or warning message, or null to
     *                use the message from the embedded exception.
     * @param locator The locator object for the error or warning (may be
     *        null).
     * @param e Any exception.
     * @see org.xml.sax.Locator
     */
    public SAXParseException (String message, Locator locator,
			      Exception e) {
	super(message, e);
	if (locator != null) {
	    init(locator.getPublicId(), locator.getSystemId(),
		 locator.getLineNumber(), locator.getColumnNumber());
	} else {
	    init(null, null, -1, -1);
	}
    }
    
    
    /**
     * Create a new SAXParseException.
     *
     * <p>This constructor is most useful for parser writers.</p>
     *
     * <p>All parameters except the message are as if
     * they were provided by a {@link Locator}.  For example, if the
     * system identifier is a URL (including relative filename), the
     * caller must resolve it fully before creating the exception.</p>
     *
     *
     * @param message The error or warning message.
     * @param publicId The public identifer of the entity that generated
     *                 the error or warning.
     * @param systemId The system identifer of the entity that generated
     *                 the error or warning.
     * @param lineNumber The line number of the end of the text that
     *                   caused the error or warning.
     * @param columnNumber The column number of the end of the text that
     *                     cause the error or warning.
     */
    public SAXParseException (String message, String publicId, String systemId,
			      int lineNumber, int columnNumber)
    {
	super(message);
	init(publicId, systemId, lineNumber, columnNumber);
    }
    
    
    /**
     * Create a new SAXParseException with an embedded exception.
     *
     * <p>This constructor is most useful for parser writers who
     * need to wrap an exception that is not a subclass of
     * {@link org.xml.sax.SAXException SAXException}.</p>
     *
     * <p>All parameters except the message and exception are as if
     * they were provided by a {@link Locator}.  For example, if the
     * system identifier is a URL (including relative filename), the
     * caller must resolve it fully before creating the exception.</p>
     *
     * @param message The error or warning message, or null to use
     *                the message from the embedded exception.
     * @param publicId The public identifer of the entity that generated
     *                 the error or warning.
     * @param systemId The system identifer of the entity that generated
     *                 the error or warning.
     * @param lineNumber The line number of the end of the text that
     *                   caused the error or warning.
     * @param columnNumber The column number of the end of the text that
     *                     cause the error or warning.
     * @param e Another exception to embed in this one.
     */
    public SAXParseException (String message, String publicId, String systemId,
			      int lineNumber, int columnNumber, Exception e)
    {
	super(message, e);
	init(publicId, systemId, lineNumber, columnNumber);
    }


    /**
     * Internal initialization method.
     *
     * @param publicId The public identifier of the entity which generated the exception,
     *        or null.
     * @param systemId The system identifier of the entity which generated the exception,
     *        or null.
     * @param lineNumber The line number of the error, or -1.
     * @param columnNumber The column number of the error, or -1.
     */
    private void init (String publicId, String systemId,
		       int lineNumber, int columnNumber)
    {
	this.publicId = publicId;
	this.systemId = systemId;
	this.lineNumber = lineNumber;
	this.columnNumber = columnNumber;
    }
    
    
    /**
     * Get the public identifier of the entity where the exception occurred.
     *
     * @return A string containing the public identifier, or null
     *         if none is available.
     * @see org.xml.sax.Locator#getPublicId
     */
    public String getPublicId ()
    {
	return this.publicId;
    }
    
    
    /**
     * Get the system identifier of the entity where the exception occurred.
     *
     * <p>If the system identifier is a URL, it will have been resolved
     * fully.</p>
     *
     * @return A string containing the system identifier, or null
     *         if none is available.
     * @see org.xml.sax.Locator#getSystemId
     */
    public String getSystemId ()
    {
	return this.systemId;
    }
    
    
    /**
     * The line number of the end of the text where the exception occurred.
     *
     * <p>The first line is line 1.</p>
     *
     * @return An integer representing the line number, or -1
     *         if none is available.
     * @see org.xml.sax.Locator#getLineNumber
     */
    public int getLineNumber ()
    {
	return this.lineNumber;
    }
    
    
    /**
     * The column number of the end of the text where the exception occurred.
     *
     * <p>The first column in a line is position 1.</p>
     *
     * @return An integer representing the column number, or -1
     *         if none is available.
     * @see org.xml.sax.Locator#getColumnNumber
     */
    public int getColumnNumber ()
    {
	return this.columnNumber;
    }
    
    
    
    //////////////////////////////////////////////////////////////////////
    // Internal state.
    //////////////////////////////////////////////////////////////////////


    /**
     * @serial The public identifier, or null.
     * @see #getPublicId
     */    
    private String publicId;


    /**
     * @serial The system identifier, or null.
     * @see #getSystemId
     */
    private String systemId;


    /**
     * @serial The line number, or -1.
     * @see #getLineNumber
     */
    private int lineNumber;


    /**
     * @serial The column number, or -1.
     * @see #getColumnNumber
     */
    private int columnNumber;
    
}

// end of SAXParseException.java
