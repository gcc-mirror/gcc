/* DocumentBuilderFactory.java --
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

/**
 * Factory for obtaining document builders.
 * Instances of this class are <em>not</em> guaranteed to be thread safe.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class DocumentBuilderFactory
{

  private boolean namespaceAware;
  private boolean validating;
  private boolean ignoringElementContentWhitespace;
  private boolean expandEntityReferences = true;
  private boolean ignoringComments;
  private boolean coalescing;
  private Schema schema;
  private boolean xIncludeAware;

  protected DocumentBuilderFactory()
  {
  }

  /**
   * Creates a new factory instance.
   * The implementation class to load is the first found in the following
   * locations:
   * <ol>
   * <li>the <code>javax.xml.parsers.DocumentBuilderFactory</code> system
   * property</li>
   * <li>the above named property value in the
   * <code><i>$JAVA_HOME</i>/lib/jaxp.properties</code> file</li>
   * <li>the class name specified in the
   * <code>META-INF/services/javax.xml.parsers.DocumentBuilderFactory</code>
   * system resource</li>
   * <li>the default factory class</li>
   * </ol>
   */
  public static DocumentBuilderFactory newInstance()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      {
        loader = DocumentBuilderFactory.class.getClassLoader();
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
                Class<?> t = (loader != null) ? loader.loadClass(className) :
                  Class.forName(className);
                return (DocumentBuilderFactory) t.newInstance();
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
    return new gnu.xml.dom.DomDocumentBuilderFactory();
  }

  private static String getFactoryClassName(ClassLoader loader, int attempt)
  {
    final String propertyName = "javax.xml.parsers.DocumentBuilderFactory";
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
                DocumentBuilderFactory.class.getResourceAsStream(serviceKey);
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
   * Creates a new document builder instance using the currently specified
   * factory configuration.
   * @exception ParserConfigurationException if the specified configuration
   * is not supported
   */
  public abstract DocumentBuilder newDocumentBuilder()
    throws ParserConfigurationException;

  /**
   * Sets whether document builders obtained from this factory will be XML
   * Namespace aware.
   */
  public void setNamespaceAware(boolean awareness)
  {
    namespaceAware = awareness;
  }

  /**
   * Sets whether document builders obtained from this factory will validate
   * their input.
   */
  public void setValidating(boolean validating)
  {
    this.validating = validating;
  }

  /**
   * Sets whether document builders obtained from this factory will
   * eliminate whitespace within elements that have an element-only content
   * model.
   */
  public void setIgnoringElementContentWhitespace(boolean whitespace)
  {
    ignoringElementContentWhitespace = whitespace;
  }

  /**
   * Sets whether document builders obtained from this factory will expand
   * entity reference nodes.
   */
  public void setExpandEntityReferences(boolean expandEntityRef)
  {
    expandEntityReferences = expandEntityRef;
  }

  /**
   * Sets whether document builders obtained from this factory will discard
   * comment nodes.
   */
  public void setIgnoringComments(boolean ignoreComments)
  {
    ignoringComments = ignoreComments;
  }

  /**
   * Sets whether document builders obtained from this factory will convert
   * CDATA sections to text nodes and normalize adjacent text nodes into a
   * single text node.
   */
  public void setCoalescing(boolean coalescing)
  {
    this.coalescing = coalescing;
  }

  /**
   * Indicates whether document builders obtained from this factory will be
   * XML Namespace aware.
   */
  public boolean isNamespaceAware()
  {
    return namespaceAware;
  }

  /**
   * Indicates whether document builders obtained from this factory will
   * validate their input.
   */
  public boolean isValidating()
  {
    return validating;
  }

  /**
   * Indicates whether document builders obtained from this factory will
   * eliminate whitespace within elements that have an element-only content
   * model.
   */
  public boolean isIgnoringElementContentWhitespace()
  {
    return ignoringElementContentWhitespace;
  }

  /**
   * Indicates whether document builders obtained from this factory will
   * expand entity reference nodes.
   */
  public boolean isExpandEntityReferences()
  {
    return expandEntityReferences;
  }

  /**
   * Indicates whether document builders obtained from this factory will
   * discard comment nodes.
   */
  public boolean isIgnoringComments()
  {
    return ignoringComments;
  }

  /**
   * Indicates whether document builders obtained from this factory will
   * convert CDATA sections to text nodes and normalize adjacent text nodes
   * into a single text node.
   */
  public boolean isCoalescing()
  {
    return coalescing;
  }

  /**
   * Set the named attribute on the underlying implementation.
   * @param name the name of the attribute
   * @param value the new value
   * @exception IllegalArgumentException if the attribute is not recognized
   */
  public abstract void setAttribute(String name, Object value)
    throws IllegalArgumentException;

  /**
   * Retrieves the named attribute value from the underlying implementation.
   * @param name the name of the attribute
   * @exception IllegalArgumentException if the attribute is not recognized
   */
  public abstract Object getAttribute(String name)
    throws IllegalArgumentException;

  // -- JAXP 1.3 methods --

  /**
   * Returns the schema.
   * @see #setSchema
   * @since 1.5
   */
  public Schema getSchema()
  {
    return schema;
  }

  /**
   * Sets the schema.
   * @see #getSchema
   * @since 1.5
   */
  public void setSchema(Schema schema)
  {
    this.schema = schema;
  }

  /**
   * Indicates whether parsers obtained from this factory will be XInclude
   * aware.
   * @since 1.5
   */
  public boolean isXIncludeAware()
  {
    return xIncludeAware;
  }

  /**
   * Sets whether parsers obtained from this factory will be XInclude aware.
   * @since 1.5
   */
  public void setXIncludeAware(boolean state)
  {
    xIncludeAware = state;
  }

  /**
   * Sets the value of the specified feature.
   * @param name the feature name (URI)
   * @param value whether to enable the feature or not
   * @exception ParserConfigurationException if the feature is not
   * supported.
   * @since 1.5
   */
  public abstract void setFeature(String name, boolean value)
    throws ParserConfigurationException;

  /**
   * Returns the value of the specified feature.
   * @param name the feature name (URI)
   * @exception ParserConfigurationException if the feature is not
   * supported.
   * @since 1.5
   */
  public abstract boolean getFeature(String name)
    throws ParserConfigurationException;

}
