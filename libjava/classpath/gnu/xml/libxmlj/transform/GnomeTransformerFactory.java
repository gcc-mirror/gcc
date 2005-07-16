/* GnomeTransformerFactory.java - 
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.xml.libxmlj.transform;

import java.io.InputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.URIResolver;

import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import gnu.xml.libxmlj.util.XMLJ;

/**
 *  An implementation of <code>TransformerFactory</code> producing
 *  <code>Transformer</code> objects which use <code>libxslt</code>
 *  for transformation.
 *
 *  @author Julian Scheid
 *  @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class GnomeTransformerFactory
  extends TransformerFactory
{

  static
  {
    XMLJ.init ();
  }
  
  /**
   *  URIResolver set by user, or default implementation.
   */
  private URIResolver uriResolver;

  /**
   *  ErrorListener set by user, or default implementation.
   */
  private ErrorListener errorListener;

  /**
   *  Attributes set by user.
   */
  private Map attributes = new HashMap ();

  //--- Implementation of javax.xml.transform.TransformerFactory
  //--- follows.
  
  // -- begin getAssociatedStylesheet implementation --

  /**
   * Returns the stylesheet associated with the specified XML source, or
   * <code>null</code> if no associated stylesheet could be found.
   */
  public Source getAssociatedStylesheet(Source source, String media,
                                        String title, String charset) 
    throws TransformerConfigurationException
  {
    String href= null;
    String base = source.getSystemId();
    if (source instanceof DOMSource)
      {
        Node node = ((DOMSource) source).getNode();
        Document doc = (node.getNodeType() == Node.DOCUMENT_NODE) ?
          (Document) node : node.getOwnerDocument();
        if (base == null)
          {
            base = doc.getDocumentURI();
          }
        for (node = doc.getFirstChild(); node != null;
             node = node.getNextSibling())
          {
            if (node.getNodeType() == Node.PROCESSING_INSTRUCTION_NODE &&
                "xml-stylesheet".equals(node.getNodeName()))
              {
                String data = node.getNodeValue();
                if (media != null &&
                    !media.equals(parseParameter(data, "type")))
                  {
                    continue;
                  }
                if (title != null &&
                    !title.equals(parseParameter(data, "title")))
                  {
                    continue;
                  }
                href = parseParameter(data, "href");
              }
          }
      }
    else
      {
        InputSource input;
        XMLReader parser = null;
        try
          {
            if (source instanceof SAXSource)
              {
                SAXSource sax = (SAXSource) source;
                input = sax.getInputSource();
                parser = sax.getXMLReader();
              }
            else
              {
                StreamSource stream = (StreamSource) source;
                InputStream in = stream.getInputStream();
                input = new InputSource(in);
              }
            input.setSystemId(base);
            if (parser == null)
              {
                parser = createXMLReader();
              }
            AssociatedStylesheetHandler ash =
              new AssociatedStylesheetHandler();
            ash.media = media;
            ash.title = title;
            parser.setContentHandler(ash);
            parser.parse(input);
            href = ash.href;
          }
        catch (SAXException e)
          {
            throw new TransformerConfigurationException(e);
          }
        catch (IOException e)
          {
            throw new TransformerConfigurationException(e);
          }
      }
    if (href == null)
      {
        return null;
      }
    if (base != null)
      {
        base = XMLJ.getBaseURI(base);
      }
    href = XMLJ.getAbsoluteURI(base, href);
    return new StreamSource(href);
  }

  private XMLReader createXMLReader()
    throws TransformerConfigurationException
  {
    try
      {
        SAXParserFactory factory = SAXParserFactory.newInstance();
        SAXParser parser = factory.newSAXParser();
        return parser.getXMLReader();
      }
    catch (FactoryConfigurationError e)
      {
        throw new TransformerConfigurationException(e);
      }
    catch (ParserConfigurationException e)
      {
        throw new TransformerConfigurationException(e);
      }
    catch (SAXException e)
      {
        throw new TransformerConfigurationException(e);
      }
  }

  class AssociatedStylesheetHandler
    extends DefaultHandler
  {
    
    String media;
    String title;
    String href;

    public void processingInstruction(String target, String data)
      throws SAXException
    {
      if ("xml-stylesheet".equals(target))
        {
          if (media != null && !media.equals(parseParameter(data, "type")))
            {
              return;
            }
          if (title != null && !title.equals(parseParameter(data, "title")))
            {
              return;
            }
          href = parseParameter(data, "href");
        }
    }
    
  }

  String parseParameter(String data, String name)
  {
    int start = data.indexOf(name + "=");
    if (start != -1)
      {
        start += name.length() + 2;
        char delim = data.charAt(start - 1);
        int end = data.indexOf(delim, start);
        if (end != -1)
          {
            return data.substring(start, end);
          }
      }
    return null;
  }

  // -- end getAssociatedStylesheet implementation --

  public synchronized void setAttribute (String name, Object value)
  {
    this.attributes.put (name, value);
  } 

  public synchronized Object getAttribute (String name)
  {
    return attributes.get (name);
  }
  
  public void setErrorListener (ErrorListener errorListener)
  {
    this.errorListener = errorListener;
  } 

  public ErrorListener getErrorListener ()
  {
    return errorListener;
  }
  
  public void setURIResolver (URIResolver uriResolver)
  {
    this.uriResolver = uriResolver;
  } 

  public URIResolver getURIResolver ()
  {
    return uriResolver;
  }
  
  public boolean getFeature (String name)
  {
    return (StreamSource.FEATURE.equals (name) ||
            StreamResult.FEATURE.equals (name) ||
            DOMSource.FEATURE.equals (name) ||
            DOMResult.FEATURE.equals (name));
  }

  public void setFeature(String name, boolean value)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException(name);
  }

  /**
   *  Returns a new instance of class {@link Transformer} for a
   *  null souce.
   */
  public Transformer newTransformer ()
    throws TransformerConfigurationException
  {
    return newTransformer (null);
  }

  /**
   *  Returns a new instance of class {@link Transformer} for
   *  the given souce.
   */
  public Transformer newTransformer (Source source)
    throws TransformerConfigurationException
  {
    return new GnomeTransformer (source, uriResolver, errorListener);
  }

  /**
   *  Returns a new instance of class {@link Templates} for
   *  the given souce.
   */
  public Templates newTemplates (Source source) 
    throws TransformerConfigurationException
  {
    return new GnomeTransformer (source, uriResolver, errorListener);
  }

  /**
   *  Perform native cleanup.
   */
  public static native void freeLibxsltGlobal ();

}
