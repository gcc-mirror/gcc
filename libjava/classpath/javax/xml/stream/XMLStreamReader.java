/* XMLStreamReader.java --
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

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

/**
 * Interface implemented by an XML parser.
 */
public interface XMLStreamReader
  extends XMLStreamConstants
{

  /**
   * Returns the implementation-specific feature or property of the given
   * name.
   */
  Object getProperty(String name)
    throws IllegalArgumentException;

  /**
   * Returns the next parsing event.
   */
  int next()
    throws XMLStreamException;

  /**
   * Tests whether the current event is of the given type and namespace.
   * @exception XMLStreamException if the test fails
   */
  void require(int type, String namespaceURI, String localName)
    throws XMLStreamException;

  /**
   * Returns the text content of a text-only element.
   * When invoked, the current event must be START_ELEMENT.
   * On completion, the current event will be END_ELEMENT.
   */
  String getElementText()
    throws XMLStreamException;

  /**
   * Skips any ignorable whitespace, comments, and processing instructions
   * until a START_ELEMENT or END_ELEMENT event is encountered.
   * @exception XMLStreamException if an event of any other type is
   * encountered
   */
  int nextTag()
    throws XMLStreamException;

  /**
   * Indicates whether there are any remaining events to be read.
   */
  boolean hasNext()
    throws XMLStreamException;

  /**
   * Frees any resources used by this parser.
   * This method will not close the underlying input source.
   */
  void close()
    throws XMLStreamException;

  /**
   * Returns the namespace URI for the given prefix.
   */
  String getNamespaceURI(String prefix);

  /**
   * Indicates whether the current event is START_ELEMENT.
   */
  boolean isStartElement();

  /**
   * Indicates whether the current event is END_ELEMENT.
   */
  boolean isEndElement();

  /**
   * Indicates whether the current event is character data.
   */
  boolean isCharacters();

  /**
   * Indicates whether the current event is ignorable whitespace.
   */
  boolean isWhiteSpace();

  /**
   * Returns the normalized attribute value for the given attribute.
   */
  String getAttributeValue(String namespaceURI, String localName);

  /**
   * Returns the number of attributes on this element.
   * This method can only be invoked on a START_ELEMENT event.
   */
  int getAttributeCount();

  /**
   * Returns the QName of the attribute at the given index.
   */
  QName getAttributeName(int index);

  /**
   * Returns the namespace URI of the attribute at the given index.
   */
  String getAttributeNamespace(int index);

  /**
   * Returns the local-name of the attribute at the given index.
   */
  String getAttributeLocalName(int index);

  /**
   * Returns the namespace prefix of the attribute at the given index.
   */
  String getAttributePrefix(int index);

  /**
   * Returns the type of the attribute at the specified index.
   */
  String getAttributeType(int index);

  /**
   * Returns the normalized value of the attribute at the given index.
   */
  String getAttributeValue(int index);

  /**
   * Indicates whether the attribute at the given index was specified in the
   * underlying XML source or created by default.
   */
  boolean isAttributeSpecified(int index);

  /**
   * Returns the number of namespaces declared on this event.
   * This method is only valid on a START_ELEMENT, END_ELEMENT, or NAMESPACE
   * event.
   */
  int getNamespaceCount();

  /**
   * Returns the prefix of the namespace at the given index, or null if this
   * is the default namespace declaration.
   */
  String getNamespacePrefix(int index);

  /**
   * Returns the URI of the namespace at the given index.
   */
  String getNamespaceURI(int index);

  /**
   * Returns the namespace context for the current position.
   */
  NamespaceContext getNamespaceContext();

  /**
   * Returns the type of the current event.
   */
  int getEventType();

  /**
   * Returns the string value of the current event.
   */
  String getText();

  /**
   * Returns the string value of the current event as a character array.
   */
  char[] getTextCharacters();

  /**
   * Copies the string value of the current event into the specified
   * character array.
   */
  int getTextCharacters(int sourceStart, char[] target,
                        int targetStart, int length)
    throws XMLStreamException;

  /**
   * Returns the offset of the first character in the text character array.
   */
  int getTextStart();

  /**
   * Returns the length of the characters in the text character array.
   */
  int getTextLength();

  /**
   * Returns the input encoding.
   */
  String getEncoding();

  /**
   * Indicates whether the current event has text.
   */
  boolean hasText();

  /**
   * Returns the current location of the parser cursor in the underlying
   * input source.
   */
  Location getLocation();

  /**
   * Returns the QName of the current element.
   * This method is only valid on a START_ELEMENT or END_ELEMENT event.
   */
  QName getName();

  /**
   * Returns the local-name of the current element.
   */
  String getLocalName();

  /**
   * Indicates whether the current event has a name.
   */
  boolean hasName();

  /**
   * Returns the namespace URI of the current element.
   */
  String getNamespaceURI();

  /**
   * Returns the namespace prefix of the current element.
   */
  String getPrefix();

  /**
   * Returns the XML version declared in the XML declaration.
   */
  String getVersion();

  /**
   * Returns the standalone flag declared in the XML declaration.
   */
  boolean isStandalone();

  /**
   * Indicates whether the standalone flag was set in the document.
   */
  boolean standaloneSet();

  /**
   * Returns the encoding declared in the XML declaration.
   */
  String getCharacterEncodingScheme();

  /**
   * Returns the target of the current processing instruction event.
   */
  String getPITarget();

  /**
   * Returns the data of the current processing instruction event.
   */
  String getPIData();

}
