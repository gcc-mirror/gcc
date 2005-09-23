/* SAXParser.java -- 
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
import org.xml.sax.HandlerBase;
import org.xml.sax.InputSource;
import org.xml.sax.Parser;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Convenience class for using or accessing a SAX version 1 or 2 parser.
 * Instances of this class are <em>not</em> guaranteed to be thread safe.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public abstract class SAXParser
{
  
  protected SAXParser()
  {
  }

  /**
   * Parse the specifed input stream, reporting SAX1 events to the given
   * handler.
   * Prefer the SAX2 version of this method, since the HandlerBase class is
   * now deprecated.
   * Also prefer the version of this method that specifies a system ID, in
   * order to resolve external references correctly.
   * @param is an XML input stream
   * @param hb the SAX1 handler
   * @exception IllegalArgumentException if the input stream is null
   * @see #parse(java.io.InputStream,org.xml.sax.helpers.DefaultHandler)
   */
  public void parse(InputStream is, HandlerBase hb) 
    throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input stream is null");
      }
    parse(new InputSource(is), hb);
  }

  /**
   * Parse the specified input stream, reporting SAX1 events to the given
   * handler.
   * Prefer the SAX2 version of this method, since the HandlerBase class is
   * now deprecated.
   * @param is an XML input stream
   * @param hb the SAX1 handler
   * @param systemId the system ID of the XML document
   * @exception IllegalArgumentException if the input stream is null
   * @see #parse(java.io.InputStream,org.xml.sax.helpers.DefaultHandler,java.lang.String)
   */
  public void parse(InputStream is, HandlerBase hb, String systemId)
     throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input stream is null");
      }
    InputSource  source = new InputSource(is);
    source.setSystemId(systemId);
    parse(source, hb);
  }

  /**
   * Parse the specified input stream, reporting SAX2 events to the given
   * handler.
   * Prefer the version of this method that specifies a system ID, in
   * order to resolve external references correctly.
   * @param is an XML input stream
   * @param dh the SAX2 handler
   * @exception IllegalArgumentException if the input stream is null
   */
  public void parse(InputStream is, DefaultHandler dh) 
    throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input stream is null");
      }
    parse(new InputSource(is), dh);
  }

  /**
   * Parse the specified input stream, reporting SAX2 events to the given
   * handler.
   * @param is an XML input stream
   * @param dh the SAX2 handler
   * @param systemId the system ID of the XML document
   * @exception IllegalArgumentException if the input stream is null
   */
  public void parse (InputStream is, DefaultHandler dh, String systemId)
     throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input stream is null");
      }
    InputSource  source = new InputSource(is);
    source.setSystemId(systemId);
    parse(source, dh);
  }

  /**
   * Parse the content of the specified URI, reporting SAX1 events to the
   * given handler.
   * Prefer the SAX2 version of this method, since the HandlerBase class is
   * now deprecated.
   * @param uri an XML system ID
   * @param hb the SAX1 handler
   * @exception IllegalArgumentException if the URI is null
   * @see #parse(java.lang.String,org.xml.sax.helpers.DefaultHandler)
   */
  public void parse(String uri, HandlerBase hb) 
    throws SAXException, IOException
  {
    if (uri == null)
      {
        throw new IllegalArgumentException("URI is null");
      }
    parse(new InputSource(uri), hb);
  }

  /**
   * Parse the content of the specified URI, reporting SAX2 events to the
   * given handler.
   * @param uri an XML system ID
   * @param dh the SAX2 handler
   * @exception IllegalArgumentException if the URI is null
   */
  public void parse(String uri, DefaultHandler dh) 
    throws SAXException, IOException
  {
    if (uri == null)
      {
        throw new IllegalArgumentException("URI is null");
      }
    parse(new InputSource(uri), dh);
  }

  /**
   * Parse the content of the specified file, reporting SAX1 events to the
   * given handler.
   * Prefer the SAX2 version of this method, since the HandlerBase class is
   * now deprecated.
   * @param f an XML file
   * @param hb the SAX1 handler
   * @exception IllegalArgumentException if the file is null
   * @see #parse(java.io.File,org.xml.sax.helpers.DefaultHandler)
   */
  public void parse(File f, HandlerBase hb) 
    throws SAXException, IOException
  {
    if (f == null)
      {
        throw new IllegalArgumentException("file is null");
      }
    InputSource source = new InputSource(new FileInputStream(f));
    source.setSystemId(f.toURL().toString());
    parse(source, hb);
  }

  /**
   * Parse the content of the specified file, reporting SAX2 events to the
   * given handler.
   * @param f an XML file
   * @param dh the SAX2 handler
   * @exception IllegalArgumentException if the file is null
   */
  public void parse(File f, DefaultHandler dh) 
    throws SAXException, IOException
  {
    if (f == null)
      {
        throw new IllegalArgumentException("file is null");
      }
    InputSource source = new InputSource(new FileInputStream(f));
    source.setSystemId(f.toURL().toString());
    parse(source, dh);
  }

  /**
   * Parse the specified input source, reporting SAX1 events to the
   * given handler.
   * Prefer the SAX2 version of this method, since the HandlerBase class is
   * now deprecated.
   * @param is the SAX input source
   * @param hb the SAX1 handler
   * @exception IllegalArgumentException if the input source is null
   * @see #parse(org.xml.sax.InputSource,org.xml.sax.helpers.DefaultHandler)
   */
  public void parse(InputSource is, HandlerBase hb) 
    throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input source is null");
      }
    Parser parser = getParser();
    parser.setDocumentHandler(hb);
    parser.setDTDHandler(hb);
    parser.setEntityResolver(hb);
    parser.setErrorHandler(hb);
    parser.parse(is);
  }

  /**
   * Parse the specified input source, reporting SAX2 events to the
   * given handler.
   * @param is an XML file
   * @param dh the SAX2 handler
   * @exception IllegalArgumentException if the input source is null
   */
  public void parse(InputSource is, DefaultHandler dh) 
    throws SAXException, IOException
  {
    if (is == null)
      {
        throw new IllegalArgumentException("input source is null");
      }
    XMLReader reader = getXMLReader();
    reader.setContentHandler(dh);
    reader.setDTDHandler(dh);
    reader.setEntityResolver(dh);
    reader.setErrorHandler(dh);
    reader.parse(is);
  }

  /**
   * Returns the underlying SAX1 parser.
   */
  public abstract Parser getParser() throws SAXException;

  /**
   * Returns the underlying SAX2 parser.
   * @since 1.1
   */
  public abstract XMLReader getXMLReader() throws SAXException;

  /**
   * Indicates whether this parser is XML Namespace aware.
   */
  public abstract boolean isNamespaceAware();

  /**
   * Indicates whether this parser will validate its input.
   */
  public abstract boolean isValidating();

  /**
   * Sets the specified SAX2 parser property.
   * @param name the name of the property
   * @param value the value of the property
   */
  public abstract void setProperty(String name, Object value) 
    throws SAXNotRecognizedException, SAXNotSupportedException;

  /**
   * Returns the value of the specified SAX2 parser property.
   * @param name the name of the property
   */
  public abstract Object getProperty(String name) 
    throws SAXNotRecognizedException, SAXNotSupportedException;

  // -- JAXP 1.3 methods --

  /**
   * Resets this parser to its original configuration.
   * @since 1.3
   */
  public void reset()
  {
  }

  /**
   * Returns the schema in use by this parser.
   * @since 1.3
   */
  public Schema getSchema()
  {
    return null;
  }

  /**
   * Indicates whether this parser is XInclude-aware.
   * @since 1.3
   */
  public boolean isXIncludeAware()
  {
    return false;
  }
  
}
