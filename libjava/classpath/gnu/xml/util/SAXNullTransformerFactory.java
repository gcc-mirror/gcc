/* SAXNullTransformerFactory.java -- 
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

package gnu.xml.util;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Hashtable;
import java.util.Properties;

import gnu.xml.dom.Consumer;
import gnu.xml.dom.DomDocument;
import gnu.xml.pipeline.DomConsumer;
import gnu.xml.pipeline.EventFilter;

import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.sax.*;
import javax.xml.transform.stream.*;

import org.xml.sax.*;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.helpers.LocatorImpl;


/**
 * Implements null transforms. XSLT stylesheets are not supported.
 * This class provides a way to translate three representations of
 * XML data (SAX event stream, DOM tree, and XML text) into each other.
 * In essence it's a thinnish wrapper around basic SAX event
 * <a href="../pipeline/package-summary.html">pipeline</a> facilities, which
 * exposes only limited functionality.  The <em>javax.xml.transform</em>
 * functionality is implemented as follows: <ul>
 *
 * <li>The {@link javax.xml.transform.sax.SAXSource SAXSource} class
 * just wraps an {@link XMLReader} and {@link InputSource}, while the
 * {@link javax.xml.transform.sax.SAXResult SAXResult} class is less
 * functional than a {@link gnu.xml.pipeline.EventConsumer EventConsumer}.
 * (Notably, it drops all but one declaration from any DTD.)</li>
 *
 * <li>The {@link javax.xml.transform.dom.DOMSource DOMSource} class
 * corresponds to special SAX parsers like {@link DomParser}, and the
 * {@link javax.xml.transform.dom.DOMResult DOMResult} class corresponds
 * to a {@link gnu.xml.pipeline.DomConsumer DomConsumer}.</li>
 *
 * <li>The {@link javax.xml.transform.stream.StreamSource StreamSource}
 * class corresponds to a SAX {@link InputSource}, and the
 * {@link javax.xml.transform.stream.StreamResult StreamResult} class
 * corresponds to a {@link gnu.xml.pipeline.TextConsumer TextConsumer}.</li>
 *
 * </ul>
 *
 * <p><em>This implementation is preliminary.</em>
 *
 * @see gnu.xml.pipeline.XsltFilter
 *
 * @author David Brownell
 */
public class SAXNullTransformerFactory extends SAXTransformerFactory
{
  
  private ErrorListener	errListener;
  private URIResolver		uriResolver;
  
  /** Default constructor */
  public SAXNullTransformerFactory () { }
  
  //
  // only has stuff that makes sense with null transforms
  //
  
  /**
   * Returns true if the requested feature is supported.
   * All three kinds of input and output are accepted:
   * XML text, SAX events, and DOM nodes.
   */
  public boolean getFeature (String feature)
  {
    return SAXTransformerFactory.FEATURE.equals (feature)
      || SAXResult.FEATURE.equals (feature)
      || SAXSource.FEATURE.equals (feature)
      || DOMResult.FEATURE.equals (feature)
      || DOMSource.FEATURE.equals (feature)
      || StreamResult.FEATURE.equals (feature)
      || StreamSource.FEATURE.equals (feature)
      ;
  }

  public void setFeature(String name, boolean value)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException(name);
  }


  /** Throws an exception (no implementation attributes are supported) */
  public void setAttribute (String key, Object value)
  {
    throw new IllegalArgumentException ();
  }

  /** Throws an exception (no implementation attributes are supported) */
  public Object getAttribute (String key)
  {
    throw new IllegalArgumentException ();
  }

  /** (not yet implemented) */
  public Source getAssociatedStylesheet (Source source,
                                         String media,
                                         String title,
                                         String charset)
    throws TransformerConfigurationException
  {
    // parse, and find the appropriate xsl-stylesheet PI contents
    throw new IllegalArgumentException ();
  }

  public Transformer newTransformer ()
    throws TransformerConfigurationException
  {
    return new NullTransformer ();
  }

  /**
   * Returns a TransformerHandler that knows how to generate output
   * in all three standard formats.  Output text is generated using
   * {@link XMLWriter}, and the GNU implementation of
   * {@link DomDocument DOM} is used.
   *
   * @see SAXResult
   * @see StreamResult
   * @see DOMResult
   */
  public TransformerHandler newTransformerHandler ()
    throws TransformerConfigurationException
  {
    NullTransformer	transformer = new NullTransformer ();
    return transformer.handler;
  }

  //
  // Stuff that depends on XSLT support, which we don't provide
  //
  private static final String		noXSLT = "No XSLT support";

  /** Throws an exception (XSLT is not supported). */
  public Transformer newTransformer (Source stylesheet)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException (noXSLT);
  }

  /** Throws an exception (XSLT is not supported). */
  public Templates newTemplates (Source stylesheet)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException (noXSLT);
  }

  /** Throws an exception (XSLT is not supported). */
  public TemplatesHandler newTemplatesHandler ()
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException (noXSLT);
  }

  /** Throws an exception (XSLT is not supported). */
  public TransformerHandler newTransformerHandler (Source stylesheet)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException (noXSLT);
  }

  /** Throws an exception (XSLT is not supported). */
  public TransformerHandler newTransformerHandler (Templates stylesheet)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException (noXSLT);
  }

  /** Throws an exception (XSLT is not supported). */
  public XMLFilter newXMLFilter (Source stylesheet)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException (noXSLT);
  }

  /** Throws an exception (XSLT is not supported). */
  public XMLFilter newXMLFilter (Templates stylesheet)
    throws TransformerConfigurationException
  {
    throw new TransformerConfigurationException (noXSLT);
  }

  /** Returns the value assigned by {@link #setErrorListener}.  */
  public ErrorListener getErrorListener ()
  {
    return errListener;
  }

  /** Assigns a value that would be used when parsing stylesheets */
  public void setErrorListener (ErrorListener e)
  {
    errListener = e;
  }

  /** Returns the value assigned by {@link #setURIResolver}.  */
  public URIResolver getURIResolver ()
  {
    return uriResolver;
  }

  /** Assigns a value that would be used when parsing stylesheets */
  public void setURIResolver (URIResolver u)
  {
    uriResolver = u;
  }


  //
  // Helper classes.  These might in theory be subclassed
  // by an XSLT implementation, if they were exported.
  //

  static class DomTerminus
    extends DomConsumer
  {

    DomTerminus (DOMResult result)
      throws SAXException
    {
      // won't really throw SAXException
      super (DomDocument.class);
      setHandler (new DomHandler (this, result));
    }
    
  }

  static class DomHandler
    extends Consumer.Backdoor
  {
  
    private DOMResult	result;
    
    DomHandler (DomConsumer c, DOMResult r)
      throws SAXException
    {
      // won't really throw SAXException
      super (c);
      result = r;
    }

    public void endDocument ()
      throws SAXException
    {
      super.endDocument ();
      result.setNode (getDocument ());
    }
    
  }

  private static OutputStream getOutputStream (String uri)
    throws IOException
  {
    // JDK stupidity:  file "protocol does not support output" ... 
    if (uri.startsWith ("file:"))
      return new FileOutputStream (uri.substring (5));
    
    // Otherwise ...
    URL		url = new URL (uri);
    URLConnection	conn = url.openConnection ();
    
    conn.setDoOutput (true);
    return conn.getOutputStream ();
  }
  

  static class NullHandler
    extends EventFilter
    implements TransformerHandler
  {
   
    private String		systemId;
    private Transformer	transformer;
    
    NullHandler (Transformer t)
    {
      transformer = t;
    }

    public Transformer getTransformer ()
    {
      return transformer;
    }

    public String getSystemId ()
    {
      return systemId;
    }

    public void setSystemId (String id)
    {
      systemId = id;
    }

    public void setResult (Result result)
    {
      if (result.getSystemId () != null)
        systemId = result.getSystemId ();
      
      try
        {
          
          // output to partial SAX event stream?
          if (result instanceof SAXResult)
            {
              SAXResult 	r = (SAXResult) result;
              
              setContentHandler (r.getHandler ());
              setProperty (LEXICAL_HANDLER, r.getLexicalHandler ());
              // DTD info is filtered out by javax.transform
              
              // output to DOM tree?
            }
          else if (result instanceof DOMResult)
            {
              DomTerminus	out = new DomTerminus ((DOMResult) result);

              setContentHandler (out.getContentHandler ());
              setProperty (LEXICAL_HANDLER,
                           out.getProperty (LEXICAL_HANDLER));
              // save DTD-derived info, if any.
              setDTDHandler (out.getDTDHandler ());
              setProperty (DECL_HANDLER,
                           out.getProperty (DECL_HANDLER));

              // node is saved into result on endDocument()

              // output to (XML) text?
            }
          else if (result instanceof StreamResult)
            {
              StreamResult	r = (StreamResult) result;
              XMLWriter		out;

              // FIXME:  when do output properties take effect?
              // encoding, standalone decl, xml/xhtml/... ...

              // FIXME:  maybe put nsfix filter up front

              try
                {
                  if (r.getWriter () != null)
                    out = new XMLWriter (r.getWriter ());
                  else if (r.getOutputStream () != null)
                    out = new XMLWriter (r.getOutputStream ());
                  else if (r.getSystemId () != null)
                    out = new XMLWriter (
                                         getOutputStream (r.getSystemId ()));
                  else
                    throw new IllegalArgumentException (
                                                        "bad StreamResult");
                }
              catch (IOException e)
                {
                  e.printStackTrace ();
                  // on jdk 1.4, pass the root cause ...
                  throw new IllegalArgumentException (e.getMessage ());
                }

              // out.setExpandingEntities (true);
              // out.setPrettyPrinting (true);
              // out.setXhtml (true);

              setContentHandler (out);
              setProperty (LEXICAL_HANDLER, out);
              // save DTD info, if any; why not?
              setDTDHandler (out);
              setProperty (DECL_HANDLER, out);
            }
          
        }
      catch (SAXException e)
        {
          // SAXNotSupportedException or SAXNotRecognizedException:
          // "can't happen" ... but SAXException for DOM build probs
          // could happen, so ...
          // on jdk 1.4, pass the root cause ...
          throw new IllegalArgumentException (e.getMessage ());
        }
    }
  }

  // an interface that adds no value
  static class LocatorAdapter
    extends LocatorImpl
    implements SourceLocator
  {
  
    LocatorAdapter (SAXParseException e)
    {
      setSystemId (e.getSystemId ());
      setPublicId (e.getPublicId ());
      setLineNumber (e.getLineNumber ());
      setColumnNumber (e.getColumnNumber ());
    }
    
  }

  // another interface that adds no value
  static class ListenerAdapter
    implements ErrorHandler
  {
    
    NullTransformer	transformer;
    
    ListenerAdapter (NullTransformer t)
    {
      transformer = t;
    }
    
    private TransformerException map (SAXParseException e)
    {
      return new TransformerException (
                                       e.getMessage (),
                                       new LocatorAdapter (e),
                                       e);
    }

    public void error (SAXParseException e)
      throws SAXParseException
    {
      try
        {
          if (transformer.errListener != null)
            transformer.errListener.error (map (e));
        }
      catch (TransformerException ex)
        {
          transformer.ex = ex;
          throw e;
        }
    }

    public void fatalError (SAXParseException e)
      throws SAXParseException
    {
      try
        {
          if (transformer.errListener != null)
            transformer.errListener.fatalError (map (e));
          else
            throw map (e);
        } catch (TransformerException ex) {
          transformer.ex = ex;
          throw e;
        }
    }

    public void warning (SAXParseException e)
      throws SAXParseException
    {
      try
        {
          if (transformer.errListener != null)
            transformer.errListener.warning (map (e));
        }
      catch (TransformerException ex)
        {
          transformer.ex = ex;
          throw e;
        }
    }
  }

  static class NullTransformer
    extends Transformer
  {
  
    private URIResolver		uriResolver;
    private Properties		props = new Properties ();
    private Hashtable		params = new Hashtable (7);
    
    ErrorListener			errListener = null;
    TransformerException		ex = null;
    NullHandler			handler;
    
    NullTransformer ()
    {
      super ();
      handler = new NullHandler (this);
    }
    
    public ErrorListener getErrorListener ()
    {
      return errListener;
    }
    
    public void setErrorListener (ErrorListener e)
    {
      errListener = e;
    }
    
    public URIResolver getURIResolver ()
    {
      return uriResolver;
    }
    
    public void setURIResolver (URIResolver u)
    {
      uriResolver = u;
    }

    public void setOutputProperties (Properties p)
    {
      props = (Properties) p.clone ();
    }
    
    public Properties getOutputProperties ()
    {
      return (Properties) props.clone ();
    }

    public void setOutputProperty (String name, String value)
    {
      props.setProperty (name, value);
    }
    
    public String getOutputProperty (String name)
    {
      return props.getProperty (name);
    }

    public void clearParameters ()
    {
      params.clear ();
    }
    
    public void setParameter (String name, Object value)
    {
      props.put (name, value);
    }
    
    public Object getParameter (String name)
    {
      return props.get (name);
    }

    public void transform (Source in, Result out)
      throws TransformerException
    {
      try
        {
          XMLReader		producer;
          InputSource		input;
          
          // Input from DOM?
          if (in instanceof DOMSource)
            {
              DOMSource	source = (DOMSource) in;
              
              if (source.getNode () == null)
                throw new IllegalArgumentException ("no DOM node");
              producer = new DomParser (source.getNode ());
              input = null;
              
              // Input from SAX?
            }
          else if (in instanceof SAXSource)
            {
              SAXSource	source = (SAXSource) in;
              
              producer = source.getXMLReader ();
              if (producer == null)
                producer = XMLReaderFactory.createXMLReader ();
              
              input = source.getInputSource ();
              if (input == null)
                {
                  if (source.getSystemId () != null)
                    input = new InputSource (source.getSystemId ());
                  else
                    throw new IllegalArgumentException (
                                                        "missing SAX input");
                }
              
              // Input from a stream or something?
            }
          else
            {
              producer = XMLReaderFactory.createXMLReader ();
              input = SAXSource.sourceToInputSource (in);
              if (input == null)
                throw new IllegalArgumentException ("missing input");
            }
          
          // preserve original namespace prefixes
          try
            {
              producer.setFeature(EventFilter.FEATURE_URI +
                                  "namespace-prefixes",
                                  true);
            }
          catch (Exception e)
            {
              /* ignore */
              // FIXME if we couldn't, "NsFix" stage before the output ..
            }
          
          // arrange the output
          handler.setResult (out);
          EventFilter.bind (producer, handler);
          
          // then parse ... single element pipeline
          producer.parse (input);
          
        }
      catch (IOException e)
        {
          throw new TransformerException ("transform failed", e);
          
        }
      catch (SAXException e)
        {
          if (ex == null && ex.getCause () == e)
            throw ex;
          else
            throw new TransformerException ("transform failed", e);
          
        }
      finally
        {
          ex = null;
        }
    }
  }

}
