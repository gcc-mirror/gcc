/* XMLEventFactoryImpl.java --
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

package gnu.xml.stream;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLEventFactory;
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
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLEventFactoryImpl
  extends XMLEventFactory
{

  protected Location location;

  public void setLocation(Location location)
  {
    this.location = location;
  }

  public Attribute createAttribute(String prefix, String namespaceURI,
                                   String localName, String value)
  {
    return new AttributeImpl(location,
                             new QName(namespaceURI, localName, prefix),
                             value, "CDATA", true);
  }

  public Attribute createAttribute(String localName, String value)
  {
    return new AttributeImpl(location,
                             new QName(localName),
                             value, "CDATA", true);
  }

  public Attribute createAttribute(QName name, String value)
  {
    return new AttributeImpl(location, name, value,
                             "CDATA", true);
  }

  public Namespace createNamespace(String namespaceURI)
  {
    return new NamespaceImpl(location,
                             XMLConstants.DEFAULT_NS_PREFIX,
                             namespaceURI,
                             true);
  }

  public Namespace createNamespace(String prefix, String namespaceUri)
  {
     return new NamespaceImpl(location, prefix, namespaceUri, true);
  }

  public StartElement createStartElement(QName name,
                                         Iterator attributes,
                                         Iterator namespaces)
  {
    return new StartElementImpl(location, name,
                                createLinkedList(attributes),
                                createLinkedList(namespaces),
                                null);
  }

  public StartElement createStartElement(String prefix,
                                         String namespaceUri,
                                         String localName)
  {
    return new StartElementImpl(location,
                                new QName(namespaceUri, localName, prefix),
                                Collections.EMPTY_LIST,
                                Collections.EMPTY_LIST,
                                null);
  }

  public StartElement createStartElement(String prefix,
                                         String namespaceUri,
                                         String localName,
                                         Iterator attributes,
                                         Iterator namespaces)
  {
    return new StartElementImpl(location,
                                new QName(namespaceUri, localName, prefix),
                                createLinkedList(attributes),
                                createLinkedList(namespaces),
                                null);
  }

  public StartElement createStartElement(String prefix,
                                         String namespaceUri,
                                         String localName,
                                         Iterator attributes,
                                         Iterator namespaces,
                                         NamespaceContext context)
  {
    return new StartElementImpl(location,
                                new QName(namespaceUri, localName, prefix),
                                createLinkedList(attributes),
                                createLinkedList(namespaces),
                                context);
  }

  public EndElement createEndElement(QName name,
                                     Iterator namespaces)
  {
    return new EndElementImpl(location, name,
                              createLinkedList(namespaces));
  }

  public EndElement createEndElement(String prefix,
                                     String namespaceUri,
                                     String localName)
  {
    return new EndElementImpl(location,
                              new QName(namespaceUri, localName, prefix),
                              Collections.EMPTY_LIST);
  }

  public EndElement createEndElement(String prefix,
                                     String namespaceUri,
                                     String localName,
                                     Iterator namespaces)
  {
    return new EndElementImpl(location,
                              new QName(namespaceUri, localName, prefix),
                              createLinkedList(namespaces));
  }

  public Characters createCharacters(String content)
  {
    return new CharactersImpl(location, content, false, false, false);
  }

  public Characters createCData(String content)
  {
    return new CharactersImpl(location, content, false, true, false);
  }

  public Characters createSpace(String content)
  {
    return new CharactersImpl(location, content, true, false, false);
  }

  public Characters createIgnorableSpace(String content)
  {
    return new CharactersImpl(location, content, true, false, true);
  }

  public StartDocument createStartDocument()
  {
    return new StartDocumentImpl(location, null, "UTF-8", "1.0",
                                 false, false, false);
  }

  public StartDocument createStartDocument(String encoding,
                                           String version,
                                           boolean standalone)
  {
    return new StartDocumentImpl(location, null, encoding, version,
                                 standalone, true, true);
  }

  public StartDocument createStartDocument(String encoding,
                                           String version)
  {
    return new StartDocumentImpl(location, null, encoding, version,
                                 false, false, true);
  }

  public StartDocument createStartDocument(String encoding)
  {
    return new StartDocumentImpl(location, null, encoding, "1.0",
                                 false, false, true);
  }

  public EndDocument createEndDocument()
  {
    return new EndDocumentImpl(location);
  }

  public EntityReference createEntityReference(String name,
                                               EntityDeclaration declaration)
  {
    return new EntityReferenceImpl(location, declaration, name);
  }

  public Comment createComment(String text)
  {
    return new CommentImpl(location, text);
  }

  public ProcessingInstruction createProcessingInstruction(String target,
                                                           String data)
  {
    return new ProcessingInstructionImpl(location, target, data);
  }

  public DTD createDTD(String dtd)
  {
    return new DTDImpl(location, dtd, null,
                       Collections.EMPTY_LIST,
                       Collections.EMPTY_LIST);
  }

  LinkedList createLinkedList(Iterator i)
  {
    LinkedList ret = new LinkedList();
    while (i.hasNext())
      ret.add(i.next());
    return ret;
  }

}
