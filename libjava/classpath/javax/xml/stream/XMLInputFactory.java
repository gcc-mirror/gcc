/* XMLInputFactory.java --
   Copyright (C) 2005,2006,2009  Free Software Foundation, Inc.

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
import java.io.Reader;
import java.util.Properties;
import javax.xml.stream.util.XMLEventAllocator;
import javax.xml.transform.Source;

/**
 * Factory for creating stream and event readers from various kinds of input
 * source.
 * <h3>Parameters</h3>
 * <table>
 * <tr>
 * <th>Name</th>
 * <th>Description</th>
 * <th>Type</th>
 * <th>Default</th>
 * <th>Required</th>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.isValidating</td>
 * <td>Controls DTD validation</td>
 * <td>Boolean</td>
 * <td>Boolean.FALSE</td>
 * <td>no</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.isNamespaceAware</td>
 * <td>Controls namespace processing for XML 1.0</td>
 * <td>Boolean</td>
 * <td>Boolean.TRUE</td>
 * <td>true is required, false is optional</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.isCoalescing</td>
 * <td>Controls coalescing (normalization of adjacent character data)</td>
 * <td>Boolean</td>
 * <td>Boolean.FALSE</td>
 * <td>yes</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.isReplacingEntityReferences</td>
 * <td>Controls replacement of entity references with their replacement
 * text</td>
 * <td>Boolean</td>
 * <td>Boolean.TRUE</td>
 * <td>yes</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.isSupportingExternalEntities</td>
 * <td>Controls whether to resolve external entities</td>
 * <td>Boolean</td>
 * <td>not specified</td>
 * <td>yes</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.supportDTD</td>
 * <td>Controls whether to support DTDs</td>
 * <td>Boolean</td>
 * <td>Boolean.TRUE</td>
 * <td>yes</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.reporter</td>
 * <td></td>
 * <td>javax.xml.stream.XMLReporter</td>
 * <td></td>
 * <td>yes</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.resolver</td>
 * <td></td>
 * <td>javax.xml.stream.XMLResolver</td>
 * <td></td>
 * <td>yes</td>
 * </tr>
 * <tr>
 * <td>javax.xml.stream.allocator</td>
 * <td></td>
 * <td>javax.xml.stream.util.XMLEventAllocator</td>
 * <td></td>
 * <td>yes</td>
 * </tr>
 * </table>
 */
public abstract class XMLInputFactory
{

  /**
   * Property used to control namespace support.
   */
  public static final String IS_NAMESPACE_AWARE =
    "javax.xml.stream.isNamespaceAware";

  /**
   * Property used to control DTD validation.
   */
  public static final String IS_VALIDATING = "javax.xml.stream.isValidating";

  /**
   * Property used to control whether to coalesce adjacent text events.
   */
  public static final String IS_COALESCING = "javax.xml.stream.isCoalescing";

  /**
   * Property used to control whether to replace entity references with
   * their replacement text.
   */
  public static final String IS_REPLACING_ENTITY_REFERENCES =
    "javax.xml.stream.isReplacingEntityReferences";

  /**
   * Property used to control whether to resolve external entities.
   */
  public static final String IS_SUPPORTING_EXTERNAL_ENTITIES =
    "javax.xml.stream.isSupportingExternalEntities";

  /**
   * Property used to indicate whether to support DTDs.
   */
  public static final String SUPPORT_DTD = "javax.xml.stream.supportDTD";

  /**
   * Property used to control the error reporter implementation.
   */
  public static final String REPORTER = "javax.xml.stream.reporter";

  /**
   * Property used to control the entity resolver implementation.
   */
  public static final String RESOLVER = "javax.xml.stream.resolver";

  /**
   * Property used to control the event allocator implementation.
   */
  public static final String ALLOCATOR = "javax.xml.stream.allocator";

  protected XMLInputFactory()
  {
  }

  /**
   * Creates a new factory instance.
   * @see #newInstance(String,ClassLoader)
   */
  public static XMLInputFactory newInstance()
    throws FactoryConfigurationError
  {
    return newInstance(null, null);
  }

  /**
   * Creates a new factory instance.
   * The implementation class to load is the first found in the following
   * locations:
   * <ol>
   * <li>the <code>javax.xml.stream.XMLInputFactory</code> system
   * property</li>
   * <li>the above named property value in the
   * <code><i>$JAVA_HOME</i>/lib/stax.properties</code> file</li>
   * <li>the class name specified in the
   * <code>META-INF/services/javax.xml.stream.XMLInputFactory</code>
   * system resource</li>
   * <li>the default factory class</li>
   * </ol>
   * @param factoryId name of the factory, same as a property name
   * @param classLoader the class loader to use
   * @return the factory implementation
   * @exception FactoryConfigurationError if an instance of this factory
   * cannot be loaded
   */
  public static XMLInputFactory newInstance(String factoryId,
                                            ClassLoader classLoader)
    throws FactoryConfigurationError
  {
    ClassLoader loader = classLoader;
    if (loader == null)
      {
        loader = Thread.currentThread().getContextClassLoader();
      }
    if (loader == null)
      {
        loader = XMLInputFactory.class.getClassLoader();
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
                return (XMLInputFactory) t.newInstance();
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
    return new gnu.xml.stream.XMLInputFactoryImpl();
  }

  private static String getFactoryClassName(ClassLoader loader, int attempt)
  {
    final String propertyName = "javax.xml.stream.XMLInputFactory";
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
                XMLInputFactory.class.getResourceAsStream(serviceKey);
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
   * Creates a new stream reader.
   */
  public abstract XMLStreamReader createXMLStreamReader(Reader reader)
    throws XMLStreamException;

  /**
   * Creates a new stream reader.
   */
  public abstract XMLStreamReader createXMLStreamReader(Source source)
    throws XMLStreamException;

  /**
   * Creates a new stream reader.
   */
  public abstract XMLStreamReader createXMLStreamReader(InputStream stream)
    throws XMLStreamException;

  /**
   * Creates a new stream reader.
   */
  public abstract XMLStreamReader createXMLStreamReader(InputStream stream,
                                                        String encoding)
    throws XMLStreamException;

  /**
   * Creates a new stream reader.
   */
  public abstract XMLStreamReader createXMLStreamReader(String systemId,
                                                        InputStream stream)
    throws XMLStreamException;

  /**
   * Creates a new stream reader.
   */
  public abstract XMLStreamReader createXMLStreamReader(String systemId,
                                                        Reader reader)
    throws XMLStreamException;

  /**
   * Creates a new event reader.
   */
  public abstract XMLEventReader createXMLEventReader(Reader reader)
    throws XMLStreamException;

  /**
   * Creates a new event reader.
   */
  public abstract XMLEventReader createXMLEventReader(String systemId,
                                                      Reader reader)
    throws XMLStreamException;

  /**
   * Creates a new event reader.
   */
  public abstract XMLEventReader createXMLEventReader(XMLStreamReader reader)
    throws XMLStreamException;

  /**
   * Creates a new event reader.
   */
  public abstract XMLEventReader createXMLEventReader(Source source)
    throws XMLStreamException;

  /**
   * Creates a new event reader.
   */
  public abstract XMLEventReader createXMLEventReader(InputStream stream)
    throws XMLStreamException;

  /**
   * Creates a new event reader.
   */
  public abstract XMLEventReader createXMLEventReader(InputStream stream,
                                                      String encoding)
    throws XMLStreamException;

  /**
   * Creates a new event reader.
   */
  public abstract XMLEventReader createXMLEventReader(String systemId,
                                                      InputStream stream)
    throws XMLStreamException;

  /**
   * Create a new filtered reader.
   */
  public abstract XMLStreamReader createFilteredReader(XMLStreamReader reader,
                                                       StreamFilter filter)
    throws XMLStreamException;

  /**
   * Create a new filtered reader.
   */
  public abstract XMLEventReader createFilteredReader(XMLEventReader reader,
                                                      EventFilter filter)
    throws XMLStreamException;

  /**
   * Returns the entity resolver.
   */
  public abstract XMLResolver getXMLResolver();

  /**
   * Sets the entity resolver.
   */
  public abstract void setXMLResolver(XMLResolver resolver);

  /**
   * Returns the error reporter.
   */
  public abstract XMLReporter getXMLReporter();

  /**
   * Sets the error reporter.
   */
  public abstract void setXMLReporter(XMLReporter reporter);

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

  /**
   * Sets the event allocator.
   */
  public abstract void setEventAllocator(XMLEventAllocator allocator);

  /**
   * Returns the event allocator.
   */
  public abstract XMLEventAllocator getEventAllocator();

}
