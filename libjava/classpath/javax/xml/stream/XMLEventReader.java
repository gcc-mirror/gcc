/* XMLEventReader.java -- 
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

import java.util.Iterator;
import javax.xml.stream.events.XMLEvent;

/**
 * An XML parser.
 */
@SuppressWarnings("unchecked")
public interface XMLEventReader
  extends Iterator
{

  /**
   * Returns the next XML event.
   */
  XMLEvent nextEvent()
    throws XMLStreamException;

  /**
   * Indicates whether there are more XML events to be read.
   */
  boolean hasNext();
  
  /**
   * Looks at the next XML event without advancing the cursor in the stream.
   * Returns <code>null</code> if there are no more events to read.
   */
  XMLEvent peek()
    throws XMLStreamException;

  /**
   * Reads the text context of an element.
   * When invoked, the current event must be START_ELEMENT.
   * On completion, the current event will be END_ELEMENT.
   */
  String getElementText()
    throws XMLStreamException;

  /**
   * Returns the next element event.
   * This method skips insignificant space until a START_ELEMENT or
   * END_ELEMENT event is found.
   * @exception XMLStreamException if an event that was not an insignificant
   * space event was encountered
   */
  XMLEvent nextTag()
    throws XMLStreamException;

  /**
   * Returns the implementation-specific feature or property of the given
   * name.
   * @exception IllegalArgumentException if the property is not supported
   */
  Object getProperty(String name)
    throws IllegalArgumentException;

  /**
   * Free any resources associated with this parser.
   * This method will not close the underlying input source.
   */
  void close()
    throws XMLStreamException;

}

