/* XMLEventFactory.java -- 
   Copyright (C) 2005  Free Software Foundation, Inc.

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
import java.util.Iterator;
import java.util.Properties;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.Characters;
import javax.xml.stream.events.Comment;
import javax.xml.stream.events.DTD;
import javax.xml.stream.events.EndDocument;
import javax.xml.stream.events.EndElement;
import javax.xml.stream.events.EntityDeclaration;
import javax.xml.stream.events.EntityReference;
import javax.xml.stream.events.Namespace;
import javax.xml.stream.events.ProcessingInstruction;
import javax.xml.stream.events.StartDocument;
import javax.xml.stream.events.StartElement;

/**
 * Factory for XML events.
 */
public abstract class XMLEventFactory
{

  protected XMLEventFactory()
  {
  }

  /**
   * Create a new factory instance.
   * @see #newInstance(String,ClassLoader)
   */
  public static XMLEventFactory newInstance()
    throws FactoryConfigurationError
  {
    return newInstance(null, null);
  }

  /**
   * Create a new factory instance.
   * The implementation class to load is the first found in the following
   * locations:
   * <ol>
   * <li>the <code>javax.xml.stream.XMLEventFactory</code> system
   * property</li>
   * <li>the above named property value in the
   * <code><i>$JAVA_HOME</i>/lib/stax.properties</code> file</li>
   * <li>the class name specified in the
   * <code>META-INF/services/javax.xml.stream.XMLEventFactory</code>
   * system resource</li>
   * <li>the default factory class</li>
   * </ol>
   */
  static XMLEventFactory newInstance(String factoryId, ClassLoader classLoader)
    throws FactoryConfigurationError
  {
    ClassLoader loader = classLoader;
    if (loader == null)
      {
        loader = Thread.currentThread().getContextClassLoader();
      }
    if (loader == null)
      {
        loader = XMLEventFactory.class.getClassLoader();
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
                return (XMLEventFactory) t.newInstance();
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
    return new gnu.xml.stream.XMLEventFactoryImpl();
  }

  private static String getFactoryClassName(ClassLoader loader, int attempt)
  {
    final String propertyName = "javax.xml.stream.XMLEventFactory";
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
                XMLEventFactory.class.getResourceAsStream(serviceKey);
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
   * Sets the location for each event created by this factory.
   */
  public abstract void setLocation(Location location);

  /**
   * Create an attribute event.
   */
  public abstract Attribute createAttribute(String prefix, String namespaceURI,
                                            String localName, String value);
  
  /**
   * Create an attribute event.
   */
  public abstract Attribute createAttribute(String localName, String value);

  /**
   * Create an attribute event.
   */
  public abstract Attribute createAttribute(QName name, String value);

  /**
   * Create a namespace declaration event.
   */
  public abstract Namespace createNamespace(String namespaceURI);

  /**
   * Create a namespace declaration event.
   */
  public abstract Namespace createNamespace(String prefix, String namespaceUri);

  /**
   * Create a start-element event.
   */
  public abstract StartElement createStartElement(QName name,
                                                  Iterator attributes,
                                                  Iterator namespaces);

  /**
   * Create a start-element event.
   */
  public abstract StartElement createStartElement(String prefix,
                                                  String namespaceUri,
                                                  String localName);

  /**
   * Create a start-element event.
   */
  public abstract StartElement createStartElement(String prefix,
                                                  String namespaceUri,
                                                  String localName,
                                                  Iterator attributes,
                                                  Iterator namespaces);

  /**
   * Create a start-element event.
   */
  public abstract StartElement createStartElement(String prefix,
                                                  String namespaceUri,
                                                  String localName,
                                                  Iterator attributes,
                                                  Iterator namespaces,
                                                  NamespaceContext context);
  
  /**
   * Create an end-element event.
   */
  public abstract EndElement createEndElement(QName name,
                                              Iterator namespaces);

  /**
   * Create an end-element event.
   */
  public abstract EndElement createEndElement(String prefix,
                                              String namespaceUri,
                                              String localName);

  /**
   * Create an end-element event.
   */
  public abstract EndElement createEndElement(String prefix,
                                              String namespaceUri,
                                              String localName,
                                              Iterator namespaces);

  /**
   * Create a text event.
   */
  public abstract Characters createCharacters(String content);

  /**
   * Create a text event of type CDATA section.
   */
  public abstract Characters createCData(String content);

  /**
   * Create a text event of type whitespace.
   */
  public abstract Characters createSpace(String content);

  /**
   * Create a text event of type ignorable whitespace.
   */
  public abstract Characters createIgnorableSpace(String content);

  /**
   * Create a start-document event.
   */
  public abstract StartDocument createStartDocument();

  /**
   * Create a start-document event.
   */
  public abstract StartDocument createStartDocument(String encoding,
                                                    String version,
                                                    boolean standalone);

  /**
   * Create a start-document event.
   */
  public abstract StartDocument createStartDocument(String encoding,
                                                    String version);

  /**
   * Create a start-document event.
   */
  public abstract StartDocument createStartDocument(String encoding);

  /**
   * Create an end-document event.
   */
  public abstract EndDocument createEndDocument();

  /**
   * Create an entity reference event.
   */
  //public abstract EntityReference createEntityReference(String name,
  //                                                      EntityDeclaration declaration);
  public abstract EntityReference createEntityReference(String name,
                                                        String replacementText);

  /**
   * Create a comment event.
   */
  public abstract Comment createComment(String text);

  /**
   * Create a processing instruction event.
   */
  public abstract ProcessingInstruction createProcessingInstruction(String target,
                                                                    String data);

  /**
   * Create a DOCTYPE declaration event.
   */
  public abstract DTD createDTD(String dtd);
  
}

