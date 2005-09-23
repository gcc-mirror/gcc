/* XMLStreamWriter.java -- 
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

import javax.xml.namespace.NamespaceContext;

/**
 * Interface for writing XML to a stream.
 */
public interface XMLStreamWriter
{
  
  /**
   * Write the start of a tag.
   */
  void writeStartElement(String localName)
    throws XMLStreamException;

  /**
   * Write the start of a tag.
   */
  void writeStartElement(String namespaceURI, String localName)
    throws XMLStreamException;

  /**
   * Write the start of a tag.
   */
  void writeStartElement(String prefix, String localName, String namespaceURI)
    throws XMLStreamException;

  /**
   * Write an empty tag.
   */
  void writeEmptyElement(String namespaceURI, String localName)
    throws XMLStreamException;

  /**
   * Write an empty tag.
   */
  void writeEmptyElement(String prefix, String localName, String namespaceURI)
    throws XMLStreamException;

  /**
   * Write an empty tag.
   */
  void writeEmptyElement(String localName)
    throws XMLStreamException;

  /**
   * Closes the currently open tag.
   */
  void writeEndElement()
    throws XMLStreamException;

  /**
   * Closes any currently open tags.
   */
  void writeEndDocument()
    throws XMLStreamException;

  /**
   * Frees any resources used by this writer.
   * This will not close the underlying output sink.
   */
  void close()
    throws XMLStreamException;

  /**
   * Flushes any cached information to the underlying output sink.
   */
  void flush()
    throws XMLStreamException;

  /**
   * Write an attribute.
   */
  void writeAttribute(String localName, String value)
    throws XMLStreamException;

  /**
   * Write an attribute.
   */
  void writeAttribute(String prefix, String namespaceURI,
                      String localName, String value)
    throws XMLStreamException;

  /**
   * Write an attribute.
   */
  void writeAttribute(String namespaceURI, String localName, String value)
    throws XMLStreamException;

  /**
   * Write a namespace declaration.
   */
  void writeNamespace(String prefix, String namespaceURI)
    throws XMLStreamException;

  /**
   * Write a default namespace declaration.
   */
  void writeDefaultNamespace(String namespaceURI)
    throws XMLStreamException;

  /**
   * Write a comment.
   */
  void writeComment(String data)
    throws XMLStreamException;

  /**
   * Write a processing instruction.
   */
  void writeProcessingInstruction(String target)
    throws XMLStreamException;

  /**
   * Write a processing instruction.
   */
  void writeProcessingInstruction(String target, String data)
    throws XMLStreamException;

  /**
   * Write a CDATA section.
   */
  void writeCData(String data)
    throws XMLStreamException;

  /**
   * Write a DOCTYPE declaration.
   */
  void writeDTD(String dtd)
    throws XMLStreamException;

  /**
   * Write an entity reference.
   */
  void writeEntityRef(String name)
    throws XMLStreamException;

  /**
   * Write an XML declaration.
   */
  void writeStartDocument()
    throws XMLStreamException;
  
  /**
   * Write an XML declaration with the specified XML version.
   */
  void writeStartDocument(String version)
    throws XMLStreamException;

  /**
   * Write an XML declaration with the specifed XML version and encoding.
   */
  void writeStartDocument(String encoding, String version)
    throws XMLStreamException;

  /**
   * Write the specified text.
   */
  void writeCharacters(String text)
    throws XMLStreamException;

  /**
   * Write the specified text.
   */
  void writeCharacters(char[] text, int start, int len)
    throws XMLStreamException;

  /**
   * Returns the prefix associated with the given namespace URI.
   */
  String getPrefix(String uri)
    throws XMLStreamException;

  /**
   * Sets the prefix for the given namespace URI.
   */
  void setPrefix(String prefix, String uri)
    throws XMLStreamException;

  /**
   * Sets the URI for the default namespace.
   */
  void setDefaultNamespace(String uri)
    throws XMLStreamException;

  /**
   * Sets the namespace context for namespace resolution.
   */
  void setNamespaceContext(NamespaceContext context)
    throws XMLStreamException;

  /**
   * Returns the current namespace context.
   */
  NamespaceContext getNamespaceContext();

  /**
   * Returns the implementation-specific feature or property of the given
   * name.
   * @exception IllegalArgumentException if the property is not supported
   */
  Object getProperty(String name)
    throws IllegalArgumentException;
  
}

