// XMLReaderFactory.java - factory for creating a new reader.
// Written by David Megginson, sax@megginson.com
// NO WARRANTY!  This class is in the Public Domain.

// $Id: XMLReaderFactory.java,v 1.1 2000/10/02 02:43:20 sboag Exp $

package org.xml.sax.helpers;
import org.xml.sax.Parser;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;


/**
 * Factory for creating an XML reader.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * </blockquote>
 *
 * <p>This class contains static methods for creating an XML reader
 * from an explicit class name, or for creating an XML reader based
 * on the value of the <code>org.xml.sax.driver</code> system 
 * property:</p>
 *
 * <pre>
 * try {
 *   XMLReader myReader = XMLReaderFactory.createXMLReader();
 * } catch (SAXException e) {
 *   System.err.println(e.getMessage());
 * }
 * </pre>
 *
 * <p>Note that these methods will not be usable in environments where
 * system properties are not accessible or where the application or
 * applet is not permitted to load classes dynamically.</p>
 *
 * <p><strong>Note to implementors:</strong> SAX implementations in specialized
 * environments may replace this class with a different one optimized for the
 * environment, as long as its method signatures remain the same.</p>
 *
 * @since SAX 2.0
 * @author David Megginson, 
 *         <a href="mailto:sax@megginson.com">sax@megginson.com</a>
 * @version 2.0
 * @see org.xml.sax.XMLReader
 */
final public class XMLReaderFactory
{

    /**
     * Private constructor.
     *
     * <p>This constructor prevents the class from being instantiated.</p>
     */
    private XMLReaderFactory ()
    {
    }


    /**
     * Attempt to create an XML reader from a system property.
     *
     * <p>This method uses the value of the system property
     * "org.xml.sax.driver" as the full name of a Java class
     * and tries to instantiate that class as a SAX2 
     * XMLReader.</p>
     *
     * <p>Note that many Java interpreters allow system properties
     * to be specified on the command line.</p>
     *
     * @return A new XMLReader.
     * @exception org.xml.sax.SAXException If the value of the
     *            "org.xml.sax.driver" system property is null,
     *            or if the class cannot be loaded and instantiated.
     * @see #createXMLReader(java.lang.String)
     */
    public static XMLReader createXMLReader ()
	throws SAXException
    {
	String className = System.getProperty("org.xml.sax.driver");
	if (className == null) {
	    Parser parser;
	    try {
		parser = ParserFactory.makeParser();
	    } catch (Exception e) {
		parser = null;
	    }
	    if (parser == null) {
		throw new
		    SAXException("System property org.xml.sax.driver not specified");
	    } else {
		return new ParserAdapter(parser);
	    }
	} else {
	    return createXMLReader(className);
	}
    }


    /**
     * Attempt to create an XML reader from a class name.
     *
     * <p>Given a class name, this method attempts to load
     * and instantiate the class as an XML reader.</p>
     *
     * @return A new XML reader.
     * @exception org.xml.sax.SAXException If the class cannot be
     *            loaded, instantiated, and cast to XMLReader.
     * @see #createXMLReader()
     */
    public static XMLReader createXMLReader (String className)
	throws SAXException
    {
	try {
	    return (XMLReader)(Class.forName(className).newInstance());
	} catch (ClassNotFoundException e1) {
	    throw new SAXException("SAX2 driver class " + className +
				   " not found", e1);
	} catch (IllegalAccessException e2) {
	    throw new SAXException("SAX2 driver class " + className +
				   " found but cannot be loaded", e2);
	} catch (InstantiationException e3) {
	    throw new SAXException("SAX2 driver class " + className +
				   " loaded but cannot be instantiated (no empty public constructor?)",
				   e3);
	} catch (ClassCastException e4) {
	    throw new SAXException("SAX2 driver class " + className +
				   " does not implement XMLReader", e4);
	}
				   
    }

}

// end of XMLReaderFactory.java
