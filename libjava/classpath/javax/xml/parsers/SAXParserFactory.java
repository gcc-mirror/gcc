/* SAXParserFactory.java -- 
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.Properties;
import javax.xml.validation.Schema;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;

/**
 * Factory for obtaining SAX parsers.
 * Instances of this class are <em>not</em> guaranteed to be thread safe.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public abstract class SAXParserFactory
{

  private boolean validating;
  private boolean namespaceAware;
  private Schema schema;
  private boolean xIncludeAware;

  protected SAXParserFactory()
  {
  }

  /**
   * Creates a new factory instance.
   * The implementation class to load is the first found in the following
   * locations:
   * <ol>
   * <li>the <code>javax.xml.parsers.SAXParserFactory</code> system
   * property</li>
   * <li>the above named property value in the
   * <code><i>$JAVA_HOME</i>/lib/jaxp.properties</code> file</li>
   * <li>the class name specified in the
   * <code>META-INF/services/javax.xml.parsers.SAXParserFactory</code>
   * system resource</li>
   * <li>the default factory class</li>
   * </ol>
   */
  public static SAXParserFactory newInstance()
    throws FactoryConfigurationError
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      {
        loader = SAXParserFactory.class.getClassLoader();
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
                return (SAXParserFactory) t.newInstance();
              }
            catch (ClassNotFoundException e)
              {
                className = null;
              }
            catch (Exception e)
              {
                throw new FactoryConfigurationError(e,
                     "error instantiating class " + className);
              }
          }
      }
    while (className == null && count < 3);
    return new gnu.xml.stream.SAXParserFactory();
  }

  private static String getFactoryClassName(ClassLoader loader, int attempt)
  {
    final String propertyName = "javax.xml.parsers.SAXParserFactory";
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
                SAXParserFactory.class.getResourceAsStream(serviceKey);
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
   * Creates a new parser instance using the currently specified factory
   * configuration.
   * @exception ParserConfigurationException if the specified configuration
   * is not supported
   */
  public abstract SAXParser newSAXParser()
    throws ParserConfigurationException, SAXException;

  /**
   * Sets whether parsers obtained from this factory will be XML Namespace
   * aware.
   */
  public void setNamespaceAware(boolean awareness)
  {
    namespaceAware = awareness;
  }

  /**
   * Sets whether parsers obtained from this factory will validate their
   * input.
   */
  public void setValidating(boolean validating)
  {
    this.validating = validating;
  }

  /**
   * Indicates whether parsers obtained from this factory will be XML
   * Namespace aware.
   */
  public boolean isNamespaceAware()
  {
    return namespaceAware;
  }

  /**
   * Indicates whether parsers obtained from this factory will validate
   * their input.
   */
  public boolean isValidating()
  {
    return validating;
  }

  /**
   * Sets the specified feature for SAX2 parsers obtained from this factory.
   * @param name the feature name
   * @param value the featurevalue
   */
  public abstract void setFeature(String name, boolean value) 
    throws ParserConfigurationException, SAXNotRecognizedException, 
           SAXNotSupportedException;

  /**
   * Returns the specified feature for SAX2 parsers obtained from this
   * factory.
   * @param name the feature name
   */
  public abstract boolean getFeature(String name) 
    throws ParserConfigurationException, SAXNotRecognizedException, 
           SAXNotSupportedException;

  // -- JAXP 1.3 methods --

  /**
   * Returns the schema.
   * @since 1.3
   * @see #setSchema
   */
  public Schema getSchema()
  {
    return schema;
  }

  /**
   * Sets the schema.
   * @since 1.3
   * @see #getSchema
   */
  public void setSchema(Schema schema)
  {
    this.schema = schema;
  }

  /**
   * Indicates whether parsers obtained from this factory will be XInclude
   * aware.
   * @since 1.3
   */
  public boolean isXIncludeAware()
  {
    return xIncludeAware;
  }

  /**
   * Sets whether parsers obtained from this factory will be XInclude aware.
   * @since 1.3
   */
  public void setXIncludeAware(boolean state)
  {
    xIncludeAware = state;
  }

}
