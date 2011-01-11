/* DocumentBuilder.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

package javax.xml.parsers;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import javax.xml.validation.Schema;
import org.w3c.dom.Document;
import org.w3c.dom.DOMImplementation;
import org.xml.sax.InputSource;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;

/**
 * Convenience class for parsing an XML document into a W3C DOM object
 * graph.
 * Instances of this class are <em>not</em> guaranteed to be thread safe.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public abstract class DocumentBuilder
{

  protected DocumentBuilder()
  {
  }

  /**
   * Parse the specified input stream and return a DOM Document.
   * Prefer the version of this method that specifies a system ID, in order
   * to resolve external references correctly.
   * @param is an XML input stream
   * @exception IllegalArgumentException if the input stream is null
   */
  public Document parse(InputStream is)
    throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input stream is null");
      }
    return parse(new InputSource(is));
  }

  /**
   * Parse the specified input stream and return a DOM Document.
   * @param is an XML input stream
   * @param systemId the system ID of the XML document
   * @exception IllegalArgumentException if the input stream is null
   */
  public Document parse(InputStream is, String systemId)
    throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input stream is null");
      }
    InputSource  source = new InputSource(is);
    source.setSystemId(systemId);
    return parse(source);
  }

  /**
   * Parse the content of the specified URI and return a DOM Document.
   * @param uri an XML system ID
   * @exception IllegalArgumentException if the URI is null
   */
  public Document parse(String uri)
    throws SAXException, IOException
  {
    if (uri == null)
      {
        throw new IllegalArgumentException("URI is null");
      }
    return parse(new InputSource(uri));
  }

  /**
   * Parse the specified file and return a DOM Document.
   * @param f the XML file
   * @exception IllegalArgumentException if the file is null
   */
  public Document parse(File f)
    throws SAXException, IOException
  {
    if (f == null)
      {
        throw new IllegalArgumentException("file is null");
      }
    InputSource  source = new InputSource(new FileInputStream(f));
    source.setSystemId(f.toURL().toString());
    return parse(source);
  }

  /**
   * Parse the specified input source and return a DOM Document.
   * @param source the input source
   * @exception IllegalArgumentException if the input source is null
   */
  public abstract Document parse(InputSource source)
    throws SAXException, IOException;

  /**
   * Indicates whether this document builder is XML Namespace aware.
   */
  public abstract boolean isNamespaceAware();

  /**
   * Indicates whether this document builder will validate its input.
   */
  public abstract boolean isValidating();

  /**
   * Sets the SAX entity resolver callback used to resolve external entities
   * in the XML document(s) to parse.
   * @param er an entity resolver
   */
  public abstract void setEntityResolver(EntityResolver er);

  /**
   * Sets the SAX error handler callback used to report parsing errors.
   * @param eh the error handler
   */
  public abstract void setErrorHandler(ErrorHandler eh);

  /**
   * Creates a new, empty DOM Document.
   * To create a document with a root element and optional doctype, use the
   * <code>DOMImplementation</code> instead.
   * @see org.w3c.dom.DOMImplementation#createDocument
   */
  public abstract Document newDocument();

  /**
   * Returns the DOM implementation.
   */
  public abstract DOMImplementation getDOMImplementation();

  // -- JAXP 1.3 methods --

  /**
   * Reset this document builder to its original configuration.
   * @since 1.3
   */
  public void reset()
  {
  }

  /**
   * Returns the schema in use by the XML processor.
   */
  public Schema getSchema()
  {
    return null;
  }

  /**
   * Returns the XInclude processing mode in use by the parser.
   */
  public boolean isXIncludeAware()
  {
    return false;
  }

}
