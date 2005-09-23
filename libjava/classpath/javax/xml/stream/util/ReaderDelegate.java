/* ReaderDelegate.java -- 
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

package javax.xml.stream.util;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

/**
 * Base class for XML stream reader filters.
 */
public class ReaderDelegate
  implements XMLStreamReader, XMLStreamConstants
{

  private XMLStreamReader parent;

  /**
   * Constructs an empty filter with no parent set.
   */
  public ReaderDelegate()
  {
  }

  /**
   * Constructs an empty filter with the specfied parent.
   */
  public ReaderDelegate(XMLStreamReader reader)
  {
    parent = reader;
  }

  /**
   * Sets the parent.
   */
  public void setParent(XMLStreamReader reader)
  {
    parent = reader;
  }

  /**
   * Returns the parent.
   */
  public XMLStreamReader getParent()
  {
    return parent;
  }

  public int next()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.next();
    throw new XMLStreamException();
  }

  public int nextTag()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.nextTag();
    throw new XMLStreamException();
  }

  public String getElementText()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.getElementText();
    throw new XMLStreamException();
  }

  public void require(int type, String namespaceURI, String localName)
    throws XMLStreamException
  {
    if (parent != null)
      parent.require(type, namespaceURI, localName);
  }

  public boolean hasNext()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.hasNext();
    return false;
  }

  public void close()
    throws XMLStreamException
  {
    if (parent != null)
      parent.close();
  }

  public String getNamespaceURI(String prefix)
  {
    if (parent != null)
      return parent.getNamespaceURI(prefix);
    return null;
  }

  public NamespaceContext getNamespaceContext()
  {
    if (parent != null)
      return parent.getNamespaceContext();
    return null;
  }

  public boolean isStartElement()
  {
    if (parent != null)
      return parent.isStartElement();
    return false;
  }

  public boolean isEndElement()
  {
    if (parent != null)
      return parent.isEndElement();
    return false;
  }

  public boolean isCharacters()
  {
    if (parent != null)
      return parent.isCharacters();
    return false;
  }

  public boolean isWhiteSpace()
  {
    if (parent != null)
      return parent.isWhiteSpace();
    return false;
  }

  public String getAttributeValue(String namespaceUri, String localName)
  {
    if (parent != null)
      return parent.getAttributeValue(namespaceUri, localName);
    return null;
  }

  public int getAttributeCount()
  {
    if (parent != null)
      return parent.getAttributeCount();
    return 0;
  }

  public QName getAttributeQName(int index)
  {
    if (parent != null)
      return parent.getAttributeQName(index);
    return null;
  }

  public String getAttributePrefix(int index)
  {
    if (parent != null)
      return parent.getAttributePrefix(index);
    return null;
  }

  public String getAttributeNamespace(int index)
  {
    if (parent != null)
      return parent.getAttributeNamespace(index);
    return null;
  }

  public String getAttributeName(int index)
  {
    if (parent != null)
      return parent.getAttributeName(index);
    return null;
  }

  public String getAttributeType(int index)
  {
    if (parent != null)
      return parent.getAttributeType(index);
    return null;
  }

  public String getAttributeValue(int index)
  {
    if (parent != null)
      return parent.getAttributeValue(index);
    return null;
  }

  public boolean isAttributeSpecified(int index)
  {
    if (parent != null)
      return parent.isAttributeSpecified(index);
    return false;
  }

  public int getNamespaceCount()
  {
    if (parent != null)
      return parent.getNamespaceCount();
    return 0;
  }

  public String getNamespacePrefix(int index)
  {
    if (parent != null)
      return parent.getNamespacePrefix(index);
    return null;
  }

  public String getNamespaceURI(int index)
  {
    if (parent != null)
      return parent.getNamespaceURI(index);
    return null;
  }

  public int getEventType()
  {
    if (parent != null)
      return parent.getEventType();
    return 0;
  }

  public String getText()
  {
    if (parent != null)
      return parent.getText();
    return null;
  }

  public int getTextCharacters(int sourceStart, char[] target,
                               int targetStart, int length)
    throws XMLStreamException
  {
    if (parent != null)
      return parent.getTextCharacters(sourceStart, target, targetStart, length);
    return 0;
  }

  public char[] getTextCharacters()
  {
    if (parent != null)
      return parent.getTextCharacters();
    return null;
  }

  public int getTextStart()
  {
    if (parent != null)
      return parent.getTextStart();
    return 0;
  }

  public int getTextLength()
  {
    if (parent != null)
      return parent.getTextLength();
    return 0;
  }

  public String getEncoding()
  {
    if (parent != null)
      return parent.getEncoding();
    return null;
  }

  public boolean hasText()
  {
    if (parent != null)
      return parent.hasText();
    return false;
  }

  public Location getLocation()
  {
    if (parent != null)
      return parent.getLocation();
    return null;
  }

  public QName getName()
  {
    if (parent != null)
      return parent.getName();
    return null;
  }

  public String getLocalName()
  {
    if (parent != null)
      return parent.getLocalName();
    return null;
  }

  public boolean hasName()
  {
    if (parent != null)
      return parent.hasName();
    return false;
  }

  public String getNamespaceURI()
  {
    if (parent != null)
      return parent.getNamespaceURI();
    return null;
  }

  public String getPrefix()
  {
    if (parent != null)
      return parent.getPrefix();
    return null;
  }

  public String getVersion()
  {
    if (parent != null)
      return parent.getVersion();
    return null;
  }

  public boolean isStandalone()
  {
    if (parent != null)
      return parent.isStandalone();
    return false;
  }

  public boolean standaloneSet()
  {
    if (parent != null)
      return parent.standaloneSet();
    return false;
  }

  public String getCharacterEncodingScheme()
  {
    if (parent != null)
      return parent.getCharacterEncodingScheme();
    return null;
  }

  public String getPITarget()
  {
    if (parent != null)
      return parent.getPITarget();
    return null;
  }

  public String getPIData()
  {
    if (parent != null)
      return parent.getPIData();
    return null;
  }

  public Object getProperty(String name)
  {
    if (parent != null)
      return parent.getProperty(name);
    throw new IllegalArgumentException();
  }
  
}

