/* XmlReader.java -- 
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package gnu.xml.aelfred2;

import java.io.IOException;
import java.util.Locale;

import org.xml.sax.*;
import org.xml.sax.ext.*;

import gnu.xml.pipeline.EventFilter;
import gnu.xml.pipeline.ValidationConsumer;


/**
 * This SAX2 parser optionally layers a validator over the &AElig;lfred2
 * SAX2 parser.  While this will not evaluate every XML validity constraint,
 * it does support all the validity constraints that are of any real utility
 * outside the strict SGML-compatible world.  See the documentation for the
 * SAXDriver class for information about the SAX2 features and properties
 * that are supported, and documentation for the ValidationConsumer for
 * information about what validity constraints may not be supported.
 * (&AElig;lfred2 tests some of those, even in non-validating mode, to
 * achieve better conformance.)
 *
 * <p> Note that due to its internal construction, you can't change most
 * handlers until parse() returns.  This diverges slightly from SAX, which
 * expects later binding to be supported.  Early binding involves less
 * runtime overhead, which is an issue for event pipelines as used inside
 * this parser.  Rather than relying on the parser to handle late binding
 * to your own handlers, do it yourself.
 *
 * @see SAXDriver
 * @see gnu.xml.pipeline.ValidationConsumer
 *
 * @author David Brownell
 */
public final class XmlReader
  implements XMLReader
{

  static class FatalErrorHandler
    extends DefaultHandler2
  {
    
    public void error(SAXParseException e)
      throws SAXException
    {
      throw e;
    }
    
  }
  
  private SAXDriver aelfred2 = new SAXDriver();
  private EventFilter filter = new EventFilter();
  private boolean isValidating;
  private boolean active;

  /**
   * Constructs a SAX Parser.
   */
  public XmlReader()
  {
  }

  /**
   * Constructs a SAX Parser, optionally treating validity errors
   * as if they were fatal errors.
   */
  public XmlReader(boolean invalidIsFatal)
  {
    if (invalidIsFatal)
      {
        setErrorHandler(new FatalErrorHandler());
      }
  }
  
  /**
   * <b>SAX2</b>: Returns the object used to report the logical
   * content of an XML document.
   */
  public ContentHandler getContentHandler()
  {
    return filter.getContentHandler();
  }

  /**
   * <b>SAX2</b>: Assigns the object used to report the logical
   * content of an XML document.
   * @exception IllegalStateException if called mid-parse
   */
  public void setContentHandler(ContentHandler handler)
  {
    if (active)
      {
        throw new IllegalStateException("already parsing");
      }
    filter.setContentHandler(handler);
  }

  /**
   * <b>SAX2</b>: Returns the object used to process declarations related
   * to notations and unparsed entities.
   */
  public DTDHandler getDTDHandler()
  {
    return filter.getDTDHandler();
  }

  /**
   * <b>SAX1</b> Assigns DTD handler
   * @exception IllegalStateException if called mid-parse
   */
  public void setDTDHandler(DTDHandler handler)
  {
    if (active)
      {
        throw new IllegalStateException("already parsing");
      }
    filter.setDTDHandler(handler);
  }
  
  /**
   * <b>SAX2</b>: Returns the object used when resolving external
   * entities during parsing (both general and parameter entities).
   */
  public EntityResolver getEntityResolver()
  {
    return aelfred2.getEntityResolver();
  }

  /**
   * <b>SAX1</b> Assigns parser's entity resolver
   */
  public void setEntityResolver(EntityResolver handler)
  {
    aelfred2.setEntityResolver(handler);
  }

  /**
   * <b>SAX2</b>: Returns the object used to receive callbacks for XML
   * errors of all levels (fatal, nonfatal, warning); this is never null;
   */
  public ErrorHandler getErrorHandler()
  {
    return aelfred2.getErrorHandler();
  }

  /**
   * <b>SAX1</b> Assigns error handler
   * @exception IllegalStateException if called mid-parse
   */
  public void setErrorHandler(ErrorHandler handler)
  {
    if (active)
      {
        throw new IllegalStateException("already parsing");
      }
    aelfred2.setErrorHandler(handler);
  }

  /**
   * <b>SAX2</b>:  Assigns the specified property.
   * @exception IllegalStateException if called mid-parse
   */
  public void setProperty(String propertyId, Object value)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    if (active)
      {
        throw new IllegalStateException("already parsing");
      }
    if (getProperty(propertyId) != value)
      {
        filter.setProperty(propertyId, value);
      }
  }

  /**
   * <b>SAX2</b>:  Returns the specified property.
   */
  public Object getProperty(String propertyId)
    throws SAXNotRecognizedException
  {
    if ((SAXDriver.PROPERTY + "declaration-handler").equals(propertyId)
        || (SAXDriver.PROPERTY + "lexical-handler").equals(propertyId))
      {
        return filter.getProperty(propertyId);
      }
    throw new SAXNotRecognizedException(propertyId);
  }
  
  private void forceValidating()
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    aelfred2.setFeature(SAXDriver.FEATURE + "namespace-prefixes",
                        true);
    aelfred2.setFeature(SAXDriver.FEATURE + "external-general-entities",
                        true);
    aelfred2.setFeature(SAXDriver.FEATURE + "external-parameter-entities",
                        true);
  }

  /**
   * <b>SAX2</b>:  Sets the state of features supported in this parser.
   * Note that this parser requires reporting of namespace prefixes when
   * validating.
   */
  public void setFeature(String featureId, boolean state)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    boolean value = getFeature(featureId);

    if (state == value)
      {
        return;
      }

    if ((SAXDriver.FEATURE + "validation").equals(featureId))
      {
        if (active)
          {
            throw new SAXNotSupportedException("already parsing");
          }
        if (state)
          {
            forceValidating();
          }
        isValidating = state;
      }
    else
      {
        aelfred2.setFeature(featureId, state);
      }
  }

  /**
   * <b>SAX2</b>: Tells whether this parser supports the specified feature.
   * At this time, this directly parallels the underlying SAXDriver,
   * except that validation is optionally supported.
   *
   * @see SAXDriver
   */
  public boolean getFeature(String featureId)
    throws SAXNotRecognizedException, SAXNotSupportedException
  {
    if ((SAXDriver.FEATURE + "validation").equals(featureId))
      {
        return isValidating;
      }

    return aelfred2.getFeature(featureId);
  }

  /**
   * <b>SAX1</b>: Sets the locale used for diagnostics; currently,
   * only locales using the English language are supported.
   * @param locale The locale for which diagnostics will be generated
   */
  public void setLocale(Locale locale)
    throws SAXException
  {
    aelfred2.setLocale(locale);
  }

  /**
   * <b>SAX1</b>: Preferred API to parse an XML document, using a
   * system identifier (URI).
     */
  public void parse(String systemId)
    throws SAXException, IOException
  {
    parse(new InputSource(systemId));
  }

  /**
   * <b>SAX1</b>: Underlying API to parse an XML document, used
   * directly when no URI is available.  When this is invoked,
   * and the parser is set to validate, some features will be
   * automatically reset to appropriate values:  for reporting
   * namespace prefixes, and incorporating external entities.
   *
   * @param source The XML input source.
   *
   * @exception IllegalStateException if called mid-parse
   * @exception SAXException The handlers may throw any SAXException,
   *  and the parser normally throws SAXParseException objects.
   * @exception IOException IOExceptions are normally through through
   *  the parser if there are problems reading the source document.
   */
  public void parse(InputSource source)
    throws SAXException, IOException
  {
    EventFilter next;
    boolean nsdecls;

    synchronized (aelfred2)
      {
        if (active)
          {
            throw new IllegalStateException("already parsing");
          }
        active = true;
      }

    // set up the output pipeline
    if (isValidating)
      {
        forceValidating();
        next = new ValidationConsumer(filter);
      }
    else
      {
        next = filter;
      }

    // connect pipeline and error handler
    // don't let _this_ call to bind() affect xmlns* attributes
    nsdecls = aelfred2.getFeature(SAXDriver.FEATURE + "namespace-prefixes");
    EventFilter.bind(aelfred2, next);
    if (!nsdecls)
      {
        aelfred2.setFeature(SAXDriver.FEATURE + "namespace-prefixes",
                            false);
      }

    // parse, clean up
    try
      {
        aelfred2.parse(source);
      }
    finally
      {
        active = false;
      }
  }

}

