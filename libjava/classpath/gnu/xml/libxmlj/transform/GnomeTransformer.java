/* GnomeTransformer.java -
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
import java.io.OutputStream;

import java.net.URL;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.SourceLocator;
import javax.xml.transform.Result;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.URIResolver;

import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Node;

import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;

import gnu.xml.libxmlj.dom.GnomeDocument;
import gnu.xml.libxmlj.sax.GnomeXMLReader;
import gnu.xml.libxmlj.util.NamedInputStream;
import gnu.xml.libxmlj.util.StandaloneLocator;
import gnu.xml.libxmlj.util.XMLJ;

/**
 * An implementation of {@link javax.xml.transform.Transformer} which
 * performs XSLT transformation using <code>libxslt</code>.
 *
 * @author Julian Scheid
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class GnomeTransformer
  extends Transformer
  implements Templates
{

  /**
   * The parameters added by the user via {@link setParameter()}.
   */
  private Map parameters;

  /**
   * The output properties set by the user.
   */
  private Properties outputProperties;

  /**
   * The URI resolver to use during transformation.
   */
  private URIResolver resolver;

  /**
   * The error listener for transformation errors.
   */
  private ErrorListener errorListener;

  /**
   * Handle to the source stylesheet.
   * This is a native pointer of type xsltStylesheetPtr.
   */
  private Object stylesheet;

  /**
   * Constructor.
   * @param source the XSLT stylesheet document source
   * @param resolver the resolver to use during transformation
   * @param errorListener the error listener for transformation errors
   */
  GnomeTransformer (Source source,
                    URIResolver resolver,
                    ErrorListener errorListener)
    throws TransformerConfigurationException
  {
    this.resolver = resolver;
    this.errorListener = errorListener;
    parameters = new HashMap ();
    outputProperties = new Properties ();

    if (source == null)
      {
        stylesheet = newStylesheet ();
      }
    else if (source instanceof StreamSource)
      {
        try
          {
            StreamSource ss = (StreamSource) source;
            NamedInputStream in = XMLJ.getInputStream (ss);
            String systemId = ss.getSystemId ();
            String publicId = ss.getPublicId ();
            String base = XMLJ.getBaseURI (systemId);
            byte[] detectBuffer = in.getDetectBuffer ();
            if (detectBuffer == null)
              {
                String msg = "No document element";
                throw new TransformerConfigurationException (msg);
              }
            stylesheet = newStylesheetFromStream (in, detectBuffer, publicId,
                                                  systemId, base,
                                                  (resolver != null),
                                                  (errorListener != null));
          }
        catch (IOException e)
          {
            throw new TransformerConfigurationException (e);
          }
      }
    else if (source instanceof DOMSource)
      {
        DOMSource ds = (DOMSource) source;
        Node node = ds.getNode ();
        if (!(node instanceof GnomeDocument))
          {
            String msg = "Node is not a GnomeDocument";
            throw new TransformerConfigurationException (msg);
          }
        GnomeDocument doc = (GnomeDocument) node;
        stylesheet = newStylesheetFromDoc (doc);
      }
    else
      {
        String msg = "Source type not supported (" + source + ")";
        throw new TransformerConfigurationException (msg);
      }
  }

  /**
   * Copy constructor.
   */
  private GnomeTransformer (Object stylesheet,
                            URIResolver resolver,
                            ErrorListener errorListener,
                            Map parameters,
                            Properties outputProperties)
  {
    this.stylesheet = stylesheet;
    this.resolver = resolver;
    this.errorListener = errorListener;
    this.parameters = parameters;
    this.outputProperties = outputProperties;
  }

  private native Object newStylesheet ()
    throws TransformerConfigurationException;

  private native Object newStylesheetFromStream (InputStream in,
                                                 byte[] detectBuffer,
                                                 String publicId,
                                                 String systemId,
                                                 String base,
                                                 boolean entityResolver,
                                                 boolean errorHandler)
    throws TransformerConfigurationException;

  private native Object newStylesheetFromDoc (GnomeDocument doc)
    throws TransformerConfigurationException;

  //--- Implementation of javax.xml.transform.Transformer follows.

  // Set, get and clear the parameters to use on transformation

  public synchronized void setParameter (String parameter, Object value)
  {
    parameters.put (parameter, value);
  }

  public synchronized Object getParameter (String name)
  {
    return parameters.get (name);
  }

  public synchronized void clearParameters ()
  {
    parameters.clear ();
  }

  // Set and get the ErrorListener to use on transformation

  public void setErrorListener (ErrorListener listener)
  {
    this.errorListener = listener;
  }

  public ErrorListener getErrorListener ()
  {
    return errorListener;
  }

  // Set and get the URIResolver to use on transformation

  public void setURIResolver (URIResolver resolver)
  {
    this.resolver = resolver;
  }

  public URIResolver getURIResolver ()
  {
    return resolver;
  }

  // Set the output properties to use on transformation; get default
  // output properties and output properties specified in the
  // stylesheet or by the user.

  public void setOutputProperties (Properties outputProperties)
  {
    // Note: defensive copying
    this.outputProperties = new Properties (outputProperties);
  }

  public void setOutputProperty (String name, String value)
  {
    outputProperties.setProperty (name, value);
  }

  public Properties getOutputProperties ()
  {
    // Note: defensive copying
    return new Properties (this.outputProperties);
  }

  public String getOutputProperty (String name)
  {
    return outputProperties.getProperty (name);
  }

  // -- Templates --

  public Transformer newTransformer ()
  {
    return new GnomeTransformer (stylesheet, resolver, errorListener,
                                 new HashMap (parameters),
                                 new Properties (outputProperties));
  }

  // -- transform --

  /**
   * Transforms the given source and writes the result to the
   * given target.
   */
  public void transform (Source source, Result result)
    throws TransformerException
  {
    if (source instanceof StreamSource)
      {
        try
          {
            StreamSource ss = (StreamSource) source;
            NamedInputStream in = XMLJ.getInputStream (ss);
            String publicId = ss.getPublicId ();
            String systemId = ss.getSystemId ();
            String base = XMLJ.getBaseURI (systemId);
            byte[] detectBuffer = in.getDetectBuffer ();
            if (detectBuffer == null)
              {
                throw new TransformerException ("No document element");
              }
            if (result instanceof StreamResult)
              {
                OutputStream out = XMLJ.getOutputStream ((StreamResult) result);
                transformStreamToStream (in, detectBuffer, publicId, systemId,
                                         base, (resolver != null),
                                         (errorListener != null), out);
              }
            else if (result instanceof DOMResult)
              {
                DOMResult dr = (DOMResult) result;
                GnomeDocument ret =
                  transformStreamToDoc (in, detectBuffer, publicId, systemId,
                                        base, (resolver != null),
                                        (errorListener != null));
                dr.setNode (ret);
                dr.setSystemId (null);
              }
            else if (result instanceof SAXResult)
              {
                SAXResult sr = (SAXResult) result;
                transformStreamToSAX (in, detectBuffer, publicId, systemId,
                                      base, (resolver != null),
                                      (errorListener != null),
                                      getSAXContext (sr));
              }
            else
              {
                String msg = "Result type not supported (" + result + ")";
                throw new TransformerConfigurationException (msg);
              }
          }
        catch (IOException e)
          {
            throw new TransformerException (e);
          }
      }
    else if (source instanceof DOMSource)
      {
        DOMSource ds = (DOMSource) source;
        Node node = ds.getNode ();
        if (!(node instanceof GnomeDocument))
          {
            String msg = "Node is not a GnomeDocument (" + node + ")";
            throw new TransformerException (msg);
          }
        GnomeDocument doc = (GnomeDocument) node;
        if (result instanceof StreamResult)
          {
            try
              {
                OutputStream out = XMLJ.getOutputStream ((StreamResult) result);
                transformDocToStream (doc, out);
              }
            catch (IOException e)
              {
                throw new TransformerException (e);
              }
          }
        else if (result instanceof DOMResult)
          {
            DOMResult dr = (DOMResult) result;
            GnomeDocument ret = transformDocToDoc (doc);
            dr.setNode (ret);
            dr.setSystemId (null);
          }
        else if (result instanceof SAXResult)
          {
            SAXResult sr = (SAXResult) result;
            transformDocToSAX (doc, getSAXContext (sr));
          }
        else
          {
            String msg = "Result type not supported";
            throw new TransformerConfigurationException (msg);
          }
      }
    else
      {
        String msg = "Source type not supported";
        throw new TransformerConfigurationException (msg);
      }
  }

  private GnomeXMLReader getSAXContext (SAXResult result)
  {
    GnomeXMLReader ctx = new GnomeXMLReader ();
    ctx.setContentHandler (result.getHandler ());
    ctx.setLexicalHandler (result.getLexicalHandler ());
    if (errorListener != null)
      {
        ErrorHandler errorHandler =
          new ErrorListenerErrorHandler (errorListener);
        ctx.setErrorHandler (errorHandler);
      }
    if (resolver != null)
      {
        EntityResolver entityResolver =
          new URIResolverEntityResolver (resolver);
        ctx.setEntityResolver (entityResolver);
      }
    return ctx;
  }

  private native void transformStreamToStream (InputStream in,
                                               byte[] detectBuffer,
                                               String publicId,
                                               String systemId,
                                               String base,
                                               boolean entityResolver,
                                               boolean errorHandler,
                                               OutputStream out)
    throws TransformerException;

  private native GnomeDocument transformStreamToDoc (InputStream in,
                                                     byte[] detectBuffer,
                                                     String publicId,
                                                     String systemId,
                                                     String base,
                                                     boolean entityResolver,
                                                     boolean errorHandler)
    throws TransformerException;

  private native void transformStreamToSAX (InputStream in,
                                            byte[] detectBuffer,
                                            String publicId,
                                            String systemId,
                                            String base,
                                            boolean entityResolver,
                                            boolean errorHandler,
                                            GnomeXMLReader out)
    throws TransformerException;

  private native void transformDocToStream (GnomeDocument in,
                                            OutputStream out)
    throws TransformerException;

  private native GnomeDocument transformDocToDoc (GnomeDocument in)
    throws TransformerException;

  private native void transformDocToSAX (GnomeDocument in,
                                         GnomeXMLReader out)
    throws TransformerException;

  /*
   * Retrieve parameters as a string array.
   * This is a convenience method called from native code.
   */
  private String[] getParameterArray ()
  {
    String[] parameterArray = new String[parameters.size () * 2];
    int index = 0;
    for (Iterator it = parameters.keySet ().iterator ();
         it.hasNext ();
         ++index)
      {
        String parameterKey = (String) it.next ();
        String parameterValue = (String) parameters.get (parameterKey);
        parameterArray[index * 2 + 0] = parameterKey;
        parameterArray[index * 2 + 1] =
          "'" + ((parameterValue != null) ? parameterValue : "") + "'";
        // FIXME encode parameter value correctly for XPath
      }
    return parameterArray;
  }

  // -- Free xsltStylesheet handle --

  public void finalize ()
  {
    if (stylesheet != null)
      {
        free ();
        stylesheet = null;
      }
  }

  private native void free ();

  // -- Callbacks --

  private InputStream resolveEntity (String publicId, String systemId)
    throws TransformerException
  {
    if (resolver != null)
      {
        systemId = resolver.resolve (null, systemId).getSystemId ();
      }
    if (systemId == null)
      {
        return null;
      }
    try
      {
        URL url = new URL (systemId);
        return XMLJ.getInputStream (url);
      }
    catch (IOException e)
      {
        throw new TransformerException (e);
      }
  }

  private void setDocumentLocator (Object ctx, Object loc)
  {
  }

  private void warning (String message,
                        int lineNumber,
                        int columnNumber,
                        String publicId,
                        String systemId)
    throws TransformerException
  {
    if (errorListener == null)
      {
        return;
      }
    SourceLocator l = new StandaloneLocator (lineNumber,
                                             columnNumber,
                                             publicId,
                                             systemId);
    errorListener.warning (new TransformerException (message, l));
  }

  private void error (String message,
                      int lineNumber,
                      int columnNumber,
                      String publicId,
                      String systemId)
    throws TransformerException
  {
    if (errorListener == null)
      {
        return;
      }
    SourceLocator l = new StandaloneLocator (lineNumber,
                                             columnNumber,
                                             publicId,
                                             systemId);
    errorListener.error (new TransformerException (message, l));
  }

  private void fatalError (String message,
                           int lineNumber,
                           int columnNumber,
                           String publicId,
                           String systemId)
    throws TransformerException
  {
    if (errorListener == null)
      {
        return;
      }
    SourceLocator l = new StandaloneLocator (lineNumber,
                                             columnNumber,
                                             publicId,
                                             systemId);
    errorListener.fatalError (new TransformerException (message, l));
  }

}
