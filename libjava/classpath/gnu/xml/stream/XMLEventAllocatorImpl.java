/* XMLEventAllocatorImpl.java -- 
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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.events.EntityDeclaration;
import javax.xml.stream.events.XMLEvent;
import javax.xml.stream.util.XMLEventAllocator;
import javax.xml.stream.util.XMLEventConsumer;

/**
 * Allocator for creating XML events based on a reader state.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLEventAllocatorImpl
  implements XMLEventAllocator
{

  protected Map entityDeclarations;

  protected XMLEventAllocatorImpl()
  {
    entityDeclarations = new HashMap();
  }

  public XMLEvent allocate(XMLStreamReader reader)
    throws XMLStreamException
  {
    String text;
    boolean whitespace;
    boolean ignorableWhitespace;
    int len;
    List namespaces;
    int eventType = reader.getEventType();
    Location location = reader.getLocation();
    switch (eventType)
      {
      case XMLStreamConstants.CDATA:
        text = reader.getText();
        whitespace = isWhitespace(text);
        // TODO ignorableWhitespace
        ignorableWhitespace = whitespace && false;
        return new CharactersImpl(location, text,
                                  whitespace, true, ignorableWhitespace);
      case XMLStreamConstants.CHARACTERS:
        text = reader.getText();
        whitespace = false;
        // TODO ignorableWhitespace
        ignorableWhitespace = whitespace && false;
        return new CharactersImpl(location, text,
                                  whitespace, false, ignorableWhitespace);
      case XMLStreamConstants.COMMENT:
        text = reader.getText();
        return new CommentImpl(location, text);
      case XMLStreamConstants.DTD:
        text = reader.getText();
        List notations = new LinkedList();
        List entities = new LinkedList();
        // TODO readDTDBody(notations, entities);
        return new DTDImpl(location, text, null, notations, entities);
      case XMLStreamConstants.END_DOCUMENT:
        return new EndDocumentImpl(location);
      case XMLStreamConstants.END_ELEMENT:
        len = reader.getNamespaceCount();
        namespaces = new LinkedList();
        for (int i = 0; i < len; i++)
          namespaces.add(new NamespaceImpl(location,
                                           reader.getNamespacePrefix(i),
                                           reader.getNamespaceURI(i)));
        return new EndElementImpl(location,
                                  reader.getName(),
                                  namespaces);
      case XMLStreamConstants.ENTITY_REFERENCE:
        String name = reader.getLocalName();
        EntityDeclaration decl =
          (EntityDeclaration) entityDeclarations.get(name);
        return new EntityReferenceImpl(location, decl, name);
      case XMLStreamConstants.PROCESSING_INSTRUCTION:
        return new ProcessingInstructionImpl(location,
                                             reader.getPITarget(),
                                             reader.getPIData());
      case XMLStreamConstants.SPACE:
        text = reader.getText();
        whitespace = true;
        // TODO ignorableWhitespace
        ignorableWhitespace = whitespace && false;
        return new CharactersImpl(location, text,
                                  whitespace, false, ignorableWhitespace);
      case XMLStreamConstants.START_DOCUMENT:
        String systemId = location.getSystemId();
        String encoding = reader.getCharacterEncodingScheme();
        boolean encodingDeclared = encoding != null;
        if (encoding == null)
          {
            encoding = reader.getEncoding();
            if (encoding == null)
              encoding = "UTF-8";
          }
        String xmlVersion = reader.getVersion();
        if (xmlVersion == null)
          xmlVersion = "1.0";
        boolean xmlStandalone = reader.isStandalone();
        boolean standaloneDeclared = reader.standaloneSet();
        return new StartDocumentImpl(location,
                                     systemId,
                                     encoding,
                                     xmlVersion,
                                     xmlStandalone,
                                     standaloneDeclared,
                                     encodingDeclared);
      case XMLStreamConstants.START_ELEMENT:
        len = reader.getNamespaceCount();
        namespaces = new LinkedList();
        for (int i = 0; i < len; i++)
          namespaces.add(new NamespaceImpl(location,
                                           reader.getNamespacePrefix(i),
                                           reader.getNamespaceURI(i)));
        len = reader.getAttributeCount();
        List attributes = new LinkedList();
        for (int i = 0; i < len; i++)
          attributes.add(new AttributeImpl(location,
                                           reader.getAttributeName(i),
                                           reader.getAttributeValue(i),
                                           QName.valueOf(reader.getAttributeType(i)),
                                           reader.isAttributeSpecified(i)));
        return new StartElementImpl(location,
                                    reader.getName(),
                                    attributes, namespaces,
                                    reader.getNamespaceContext());
      default:
        throw new XMLStreamException("Unknown event type: " + eventType);
      }
  }

  public void allocate(XMLStreamReader reader, XMLEventConsumer consumer)
    throws XMLStreamException
  {
    consumer.add(allocate(reader));
  }

  public XMLEventAllocator newInstance()
  {
    return new XMLEventAllocatorImpl();
  }
  
  protected boolean isWhitespace(String text)
  {
    int len = text.length();
    for (int i = 0; i < len; i++)
      {
        char c = text.charAt(i);
        if (c != 0x20 && c != 0x09 && c != 0x0a && c != 0x0d)
          return false;
      }
    return true;
  }
  
}

