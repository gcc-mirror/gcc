/* TransformerFactory.java -- 
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

package javax.xml.transform;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.Properties;

/**
 * Factory for obtaining transformation contexts.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public abstract class TransformerFactory
{

  protected TransformerFactory()
  {
  }

  /**
   * Creates a new factory instance.
   * The implementation class to load is the first found in the following
   * locations:
   * <ol>
   * <li>the <code>javax.xml.transform.TransformerFactory</code> system
   * property</li>
   * <li>the above named property value in the
   * <code><i>$JAVA_HOME</i>/lib/jaxp.properties</code> file</li>
   * <li>the class name specified in the
   * <code>META-INF/services/javax.xml.parsers.DocumentBuilderFactory</code>
   * system resource</li>
   * <li>the default factory class</li>
   * </ol>
   */
  public static TransformerFactory newInstance() 
    throws TransformerFactoryConfigurationError
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      {
        loader = TransformerFactory.class.getClassLoader();
      }
    String className = null;
    int count = 0;
    do
      {
        className = getFactoryClassName(loader, count++);
        if (className != null)
          {
            try
              {
                Class t = (loader != null) ? loader.loadClass(className) :
                  Class.forName(className);
                return (TransformerFactory) t.newInstance();
              }
            catch (ClassNotFoundException e)
              {
                className = null;
              }
            catch (Exception e)
              { 
                throw new TransformerFactoryConfigurationError(e,
                    "error instantiating class " + className);
              } 
          }
      }
    while (className == null && count < 3);
    try
      {
        Class t =
          Class.forName("gnu.xml.transform.TransformerFactoryImpl");
        return (TransformerFactory) t.newInstance();
      }
    catch (Exception e)
      {
        throw new TransformerFactoryConfigurationError(e);
      }
  }
  
  private static String getFactoryClassName(ClassLoader loader, int attempt)
  {
    final String propertyName = "javax.xml.transform.TransformerFactory";
    switch (attempt)
      {
        case 0:
          return System.getProperty(propertyName);
        case 1:
          try
            {
              File file = new File(System.getProperty("java.home"));
              file = new File(file, "lib");
              file = new File(file, "jaxp.properties");
              InputStream in = new FileInputStream(file);
              Properties props = new Properties();
              props.load(in);
              in.close();
              return props.getProperty(propertyName);
            }
          catch (IOException e)
            {
              return null;
            }
        case 2: 
          try
            {
              String serviceKey = "/META-INF/services/" + propertyName;
              InputStream in = (loader != null) ?
                loader.getResourceAsStream(serviceKey) :
                TransformerFactory.class.getResourceAsStream(serviceKey);
              if (in != null)
                {
                  BufferedReader r =
                    new BufferedReader(new InputStreamReader(in));
                  String ret = r.readLine();
                  r.close();
                  return ret;
                }
            }
          catch (IOException e)
            {
            }
          return null;
        default:
          return null;
      }
  }
  
  /**
   * Creates a new transformer using the specified stylesheet.
   * @param source the source of an <a href='http://www.w3.org/TR/xslt'>XSLT
   * stylesheet</a> specifying the transformation to apply
   */
  public abstract Transformer newTransformer(Source source) 
    throws TransformerConfigurationException;

  /**
   * Creates a new transformer that applies the identity transform.
   */
  public abstract Transformer newTransformer() 
    throws TransformerConfigurationException;

  /**
   * Creates a new compiled transformation using the specified stylesheet.
   * @param source the source of an <a href='http://www.w3.org/TR/xslt'>XSLT
   * stylesheet</a> specifying the transformation to apply
   */
  public abstract Templates newTemplates(Source source) 
    throws TransformerConfigurationException;

  /**
   * Returns a source object representing the XML resource specified by the
   * <a href='http://www.w3.org/TR/xml-stylesheet/'>xml-stylesheet</a>
   * processing instruction and matching the given criteria.
   * Note that if multiple stylesheets are selected, the source represents a
   * stylesheet composed of a list of imports.
   * @param source the source XML document
   * @param media the media attribute to match, or <code>null</code> to match
   * the preferred templates
   * @param title the title attribute to match, or <code>null</code> to match
   * any
   * @param charset the charset attribute to match, or <code>null</code> to
   * match any
   */
  public abstract Source getAssociatedStylesheet(Source source, 
                                                 String media,
                                                 String title,
                                                 String charset) 
    throws TransformerConfigurationException;

  /**
   * Set the resolver callback to be used by transformers obtained from
   * this factory.
   */
  public abstract void setURIResolver(URIResolver resolver);

  /**
   * Returns the resolver callback to be used by transformers obtained from
   * this factory.
   */
  public abstract URIResolver getURIResolver();

  /**
   * Sets a feature of transformers and templates obtained from this
   * factory.
   * Feature names are fully qualified URIs, and may depend on the factory
   * implementation.
   * @param name the name of the feature
   * @param value the feature state
   * @exception TransformerConfigurationException if the feature is
   * unsupported
   */
  public abstract void setFeature(String name, boolean value)
    throws TransformerConfigurationException;

  /**
   * Returns the state of a feature in the factory implementation.
   * Feature names are fully qualified URIs, and may depend on the factory
   * implementation. JAXP also predefines several features, including the
   * constants in {@link javax.xml.XMLConstants} and
   * <ul>
   * <li>{@link javax.xml.transform.dom.DOMSource#FEATURE}</li>
   * <li>{@link javax.xml.transform.dom.DOMResult#FEATURE}</li>
   * <li>{@link javax.xml.transform.sax.SAXSource#FEATURE}</li>
   * <li>{@link javax.xml.transform.sax.SAXResult#FEATURE}</li>
   * <li>{@link javax.xml.transform.sax.SAXTransformerFactory#FEATURE}</li>
   * <li>{@link javax.xml.transform.sax.SAXTransformerFactory#FEATURE_XMLFILTER}</li>
   * <li>{@link javax.xml.transform.stream.StreamSource#FEATURE}</li>
   * <li>{@link javax.xml.transform.stream.StreamResult#FEATURE}</li>
   * </ul>
   * The latter expose various capabilities of the factory implementation.
   */
  public abstract boolean getFeature(String name);

  /**
   * Set a named attribute on the underlying implementation.
   * @param name the attribute name
   * @param value the value to assign
   * @exception IllegalArgumentException if the attribute is not supported
   */
  public abstract void setAttribute(String name, Object value)
    throws IllegalArgumentException;

  /**
   * Retrieve the specified named attribute value.
   * @param name the attribute name
   * @exception IllegalArgumentException if the attribute is not supported
   */
  public abstract Object getAttribute(String name) 
    throws IllegalArgumentException;

  /**
   * Sets the callback to be used by transformers obtained from this factory
   * to report transformation errors.
   */
  public abstract void setErrorListener(ErrorListener listener) 
    throws IllegalArgumentException;

  /**
   * Returns the callback to be used by transformers obtained from this
   * factory to report transformation errors.
   */
  public abstract ErrorListener getErrorListener();

}
