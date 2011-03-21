/* EventReaderDelegate.java --
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

package javax.xml.stream.util;

import java.util.NoSuchElementException;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.events.XMLEvent;

/**
 * Base class for event reader filters.
 */
public class EventReaderDelegate
  implements XMLEventReader
{

  private XMLEventReader parent;

  /**
   * Constructs an empty filter with no parent set.
   */
  public EventReaderDelegate()
  {
  }

  /**
   * Constructs an empty filter with the given parent.
   */
  public EventReaderDelegate(XMLEventReader reader)
  {
    parent = reader;
  }

  /**
   * Sets the parent.
   */
  public void setParent(XMLEventReader reader)
  {
    parent = reader;
  }

  /**
   * Returns the parent.
   */
  public XMLEventReader getParent()
  {
    return parent;
  }

  public XMLEvent nextEvent()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.nextEvent();
    throw new NoSuchElementException();
  }

  public Object next()
  {
    if (parent != null)
      return parent.next();
    throw new NoSuchElementException();
  }

  public boolean hasNext()
  {
    if (parent != null)
      return parent.hasNext();
    return false;
  }

  public XMLEvent peek()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.peek();
    return null;
  }

  public String getElementText()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.getElementText();
    throw new XMLStreamException();
  }

  public XMLEvent nextTag()
    throws XMLStreamException
  {
    if (parent != null)
      return parent.nextTag();
    throw new XMLStreamException();
  }

  public Object getProperty(String name)
    throws IllegalArgumentException
  {
    if (parent != null)
      return parent.getProperty(name);
    throw new IllegalArgumentException(name);
  }

  public void close()
    throws XMLStreamException
  {
    if (parent != null)
      parent.close();
  }

  public void remove()
  {
    throw new UnsupportedOperationException();
  }

}
