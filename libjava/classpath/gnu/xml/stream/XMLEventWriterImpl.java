/* XMLEventWriterImpl.java --
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

package gnu.xml.stream;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLEventWriter;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.Characters;
import javax.xml.stream.events.Comment;
import javax.xml.stream.events.DTD;
import javax.xml.stream.events.Namespace;
import javax.xml.stream.events.ProcessingInstruction;
import javax.xml.stream.events.StartDocument;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;

/**
 * Writer to write events to an underlying XML stream writer.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLEventWriterImpl
  implements XMLEventWriter
{

  protected final XMLStreamWriter writer;

  protected XMLEventWriterImpl(XMLStreamWriter writer)
  {
    this.writer = writer;
  }

  public void flush()
    throws XMLStreamException
  {
    writer.flush();
  }

  public void close()
    throws XMLStreamException
  {
    writer.close();
  }

  public void add(XMLEvent event)
    throws XMLStreamException
  {
    QName name;
    String uri;
    switch (event.getEventType())
      {
      case XMLStreamConstants.START_ELEMENT:
        StartElement startElement = event.asStartElement();
        name = startElement.getName();
        uri = name.getNamespaceURI();
        if (uri != null && !"".equals(uri))
          writer.writeStartElement(name.getPrefix(), name.getLocalPart(), uri);
        else
          writer.writeStartElement(name.getLocalPart());
        break;
      case XMLStreamConstants.END_ELEMENT:
        writer.writeEndElement();
        break;
      case XMLStreamConstants.ATTRIBUTE:
        Attribute attribute = (Attribute) event;
        name = attribute.getName();
        uri = name.getNamespaceURI();
        if (uri != null && !"".equals(uri))
          writer.writeAttribute(name.getPrefix(), uri, name.getLocalPart(),
                                attribute.getValue());
        else
          writer.writeAttribute(name.getLocalPart(), attribute.getValue());
        break;
      case XMLStreamConstants.NAMESPACE:
        Namespace namespace = (Namespace) event;
        uri = namespace.getNamespaceURI();
        writer.writeNamespace(namespace.getPrefix(), uri);
        break;
      case XMLStreamConstants.PROCESSING_INSTRUCTION:
        ProcessingInstruction pi = (ProcessingInstruction) event;
        String data = pi.getData();
        if (data == null)
          writer.writeProcessingInstruction(pi.getTarget());
        else
          writer.writeProcessingInstruction(pi.getTarget(), data);
        break;
      case XMLStreamConstants.COMMENT:
        Comment comment = (Comment) event;
        writer.writeComment(comment.getText());
        break;
      case XMLStreamConstants.START_DOCUMENT:
        StartDocument startDocument = (StartDocument) event;
        writer.writeStartDocument(startDocument.getVersion());
        break;
      case XMLStreamConstants.END_DOCUMENT:
        writer.writeEndDocument();
        break;
      case XMLStreamConstants.DTD:
        DTD dtd = (DTD) event;
        writer.writeDTD(dtd.getDocumentTypeDeclaration());
        break;
      case XMLStreamConstants.CHARACTERS:
      case XMLStreamConstants.SPACE:
        Characters characters = event.asCharacters();
        writer.writeCharacters(characters.getData());
        break;
      case XMLStreamConstants.CDATA:
        Characters cdata = event.asCharacters();
        writer.writeCData(cdata.getData());
        break;
      }
  }

  public void add(XMLEventReader reader)
    throws XMLStreamException
  {
    while (reader.hasNext())
      add(reader.nextEvent());
  }

  public String getPrefix(String uri)
    throws XMLStreamException
  {
    return writer.getPrefix(uri);
  }

  public void setPrefix(String prefix, String uri)
    throws XMLStreamException
  {
    writer.setPrefix(prefix, uri);
  }

  public void setDefaultNamespace(String uri)
    throws XMLStreamException
  {
    writer.setDefaultNamespace(uri);
  }

  public void setNamespaceContext(NamespaceContext context)
    throws XMLStreamException
  {
    writer.setNamespaceContext(context);
  }

  public NamespaceContext getNamespaceContext()
  {
    return writer.getNamespaceContext();
  }

}
