/* XMLOutputFactory.java -- 
   Copyright (C) 2005,2006  Free Software Foundation, Inc.

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

package javax.xml.stream;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Properties;
import javax.xml.transform.Result;

/**
 * Factory for obtaining XML stream and event writers for various kinds of
 * output sink.
 * <h3>Configuration</h3>
 * <table>
 * <tr>
 * <th>Name</th>
 * <th>Description</th>
 * <th>Type</th>
 * <th>Default</th>
 * <th>Required</th>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.isRepairingNamespaces</td>
 * <td>default namespace prefixes</td>
 * <td>Boolean</td>
 * <td>Boolean.FALSE</td>
 * <td>yes</td>
 * </tr>
 * </table>
 */
public abstract class XMLOutputFactory
{

  /**
   * Property used to control whether to default namespace prefixes.
   * If true, the writer will create a namespace declaration for any
   * attribute that doesn't have a namespace declaration in scope.
   */
  public static final java.lang.String IS_REPAIRING_NAMESPACES = 
    "javax.xml.stream.isRepairingNamespaces";

  protected XMLOutputFactory()
  {
  }

  /**
   * Creates a new <b>output</b> factory.
   * The implementation class to load is the first found in the following
   * locations:
   * <ol>
   * <li>the <code>javax.xml.stream.XMLOutputFactory</code> system
   * property</li>
   * <li>the above named property value in the
   * <code><i>$JAVA_HOME</i>/lib/stax.properties</code> file</li>
   * <li>the class name specified in the
   * <code>META-INF/services/javax.xml.stream.XMLOutputFactory</code>
   * system resource</li>
   * <li>the default factory class</li>
   * </ol>
   */
  public static XMLOutputFactory newInstance()
    throws FactoryConfigurationError
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      {
        loader = XMLOutputFactory.class.getClassLoader();
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
                return (XMLOutputFactory) t.newInstance();
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
    return new gnu.xml.stream.XMLOutputFactoryImpl();
  }

  private static String getFactoryClassName(ClassLoader loader, int attempt)
  {
    final String propertyName = "javax.xml.stream.XMLOutputFactory";
    switch (attempt)
      {
        case 0:
          return System.getProperty(propertyName);
        case 1:
          try
            {
              File file = new File(System.getProperty("java.home"));
              file = new File(file, "lib");
              file = new File(file, "stax.properties");
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
                XMLOutputFactory.class.getResourceAsStream(serviceKey);
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
   * Creates a new <b>input</b> factory.
   * This appears to be an API design bug.
   * @see javax.xml.stream.XMLInputFactory.newInstance(String,ClassLoader)
   */
  public static XMLInputFactory newInstance(String factoryId,
                                            ClassLoader classLoader)
    throws FactoryConfigurationError
  {
    return XMLInputFactory.newInstance(factoryId, classLoader);
  }

  /**
   * Creates a new stream writer.
   */
  public abstract XMLStreamWriter createXMLStreamWriter(Writer stream)
    throws XMLStreamException;

  /**
   * Creates a new stream writer.
   */
  public abstract XMLStreamWriter createXMLStreamWriter(OutputStream stream)
    throws XMLStreamException;

  /**
   * Creates a new stream writer.
   */
  public abstract XMLStreamWriter createXMLStreamWriter(OutputStream stream,
                                                        String encoding)
    throws XMLStreamException;

  /**
   * Creates a new stream writer.
   * @exception UnsupportedOperationException if this method is not
   * supported
   */
  public abstract XMLStreamWriter createXMLStreamWriter(Result result)
    throws XMLStreamException;
  
  /**
   * Creates a new event writer.
   * @exception UnsupportedOperationException if this method is not
   * supported
   */
  public abstract XMLEventWriter createXMLEventWriter(Result result)
    throws XMLStreamException;

  /**
   * Creates a new event writer.
   */
  public abstract XMLEventWriter createXMLEventWriter(OutputStream stream)
    throws XMLStreamException;

  /**
   * Creates a new event writer.
   */
  public abstract XMLEventWriter createXMLEventWriter(OutputStream stream,
                                                      String encoding)
    throws XMLStreamException;

  /**
   * Creates a new event writer.
   */
  public abstract XMLEventWriter createXMLEventWriter(Writer stream)
    throws XMLStreamException;

  /**
   * Sets the implementation-specific property of the given name.
   * @exception IllegalArgumentException if the property is not supported
   */
  public abstract void setProperty(String name, Object value)
    throws IllegalArgumentException;

  /**
   * Returns the implementation-specific property of the given name.
   * @exception IllegalArgumentException if the property is not supported
   */
  public abstract Object getProperty(String name)
    throws IllegalArgumentException;

  /**
   * Indicates whether the specified property is supported.
   */
  public abstract boolean isPropertySupported(String name);

}

