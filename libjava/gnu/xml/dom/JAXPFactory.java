/* JAXPFactory.java -- 
   Copyright (C) 2001 Free Software Foundation, Inc.

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

package gnu.xml.dom;

import java.io.IOException;

import org.w3c.dom.Document;
import org.w3c.dom.DOMImplementation;

import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;


/**
 * DOM bootstrapping API, for use with JAXP.
 *
 * @see Consumer
 *
 * @author David Brownell
 */
public final class JAXPFactory
  extends DocumentBuilderFactory
{
  
  private static final String	PROPERTY = "http://xml.org/sax/properties/";
  private static final String	FEATURE = "http://xml.org/sax/features/";
  
  private SAXParserFactory	pf;
  
  /**
   * Default constructor.
   */
  public JAXPFactory()
  {
  }
  
  /**
   * Constructs a JAXP document builder which uses the default
   * JAXP SAX2 parser and the DOM implementation in this package.
   */
  public DocumentBuilder newDocumentBuilder()
    throws ParserConfigurationException
  {
    if (pf == null)
      {
        // Force use of AElfred2 since not all JAXP parsers
        // conform very well to the SAX2 API spec ...
        pf = new gnu.xml.aelfred2.JAXPFactory();
        // pf = SAXParserFactory.newInstance ();
      }
    
    // JAXP default: false
    pf.setValidating(isValidating());

    // FIXME:  this namespace setup may cause errors in some
    // conformant SAX2 parsers, which we CAN patch up by
    // splicing a "NSFilter" stage up front ...
    
    // JAXP default: false
    pf.setNamespaceAware(isNamespaceAware());

    try
      {
        // undo rude "namespace-prefixes=false" default
        pf.setFeature(FEATURE + "namespace-prefixes", true);

        return new JAXPBuilder(pf.newSAXParser().getXMLReader(), this);
      }
    catch (SAXException e)
      {
        String msg = "can't create JAXP DocumentBuilder: " + e.getMessage();
        throw new ParserConfigurationException(msg);
      }
  }
  
  /** There seems to be no useful specification for attribute names */
  public void setAttribute(String name, Object value)
    throws IllegalArgumentException
  {
    if ("http://java.sun.com/xml/jaxp/properties/schemaLanguage".equals(name))
      {
        // TODO
      }
    else
      {
        throw new IllegalArgumentException(name);
      }
  }

  /** There seems to be no useful specification for attribute names */
  public Object getAttribute(String name)
    throws IllegalArgumentException
  {
    throw new IllegalArgumentException(name);
  }

  static final class JAXPBuilder
    extends DocumentBuilder
    implements ErrorHandler
  {

    private Consumer	consumer;
    private XMLReader	producer;
    private DomImpl		impl;
    
    JAXPBuilder(XMLReader parser, JAXPFactory factory)
      throws ParserConfigurationException
    {
      impl = new DomImpl();
      
      // set up consumer side
      try
        {
          consumer = new Consumer();
        }
      catch (SAXException e)
        {
          throw new ParserConfigurationException(e.getMessage());
        }

      // JAXP defaults: true, noise nodes are good (bleech)
      consumer.setHidingReferences(factory.isExpandEntityReferences());
      consumer.setHidingComments(factory.isIgnoringComments());
      consumer.setHidingWhitespace(factory.isIgnoringElementContentWhitespace());
      consumer.setHidingCDATA(factory.isCoalescing());

      // set up producer side
      producer = parser;
      producer.setContentHandler(consumer.getContentHandler());
      producer.setDTDHandler(consumer.getDTDHandler());

      try
        {
          String	id;
          
          // if validating, report validity errors, and default
          // to treating them as fatal
          if (factory.isValidating ())
            {
              producer.setFeature(FEATURE + "validation", true);
              producer.setErrorHandler(this);
            }
          
          // always save prefix info, maybe do namespace processing
          producer.setFeature(FEATURE + "namespace-prefixes", true);
          producer.setFeature(FEATURE + "namespaces",
                              factory.isNamespaceAware());

          // set important handlers
          id = PROPERTY + "lexical-handler";
          producer.setProperty(id, consumer.getProperty(id));

          id = PROPERTY + "declaration-handler";
          producer.setProperty(id, consumer.getProperty(id));
          
        }
      catch (SAXException e)
        {
          throw new ParserConfigurationException(e.getMessage());
        }
    }
    
    public Document parse(InputSource source) 
      throws SAXException, IOException
    {
      producer.parse(source);
      Document doc = consumer.getDocument();
      // TODO inputEncoding
      doc.setDocumentURI(source.getSystemId());
      return doc;
    }

    public boolean isNamespaceAware()
    {
      try
        {
          return producer.getFeature(FEATURE + "namespaces");
        }
      catch (SAXException e)
        {
          // "can't happen"
          throw new RuntimeException(e.getMessage());
        }
    }

    public boolean isValidating()
    {
      try
        {
          return producer.getFeature(FEATURE + "validation");
        }
      catch (SAXException e)
        {
          // "can't happen"
          throw new RuntimeException(e.getMessage());
        }
    }

    public void setEntityResolver(EntityResolver resolver)
    {
      producer.setEntityResolver(resolver);
    }

    public void setErrorHandler(ErrorHandler handler)
    {
      producer.setErrorHandler(handler);
      consumer.setErrorHandler(handler);
    }

    public DOMImplementation getDOMImplementation()
    {
      return impl;
    }

    public Document newDocument()
    {
      return new DomDocument();
    }
	
    // implementation of error handler that's used when validating
    public void fatalError(SAXParseException e)
      throws SAXException
    {
      throw e;
    }
    
    public void error(SAXParseException e)
      throws SAXException
    {
      throw e;
    }
    
    public void warning(SAXParseException e)
      throws SAXException
    {
      /* ignore */
    }
 
  }

}

