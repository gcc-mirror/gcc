/* XMLEventReaderImpl.java --
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

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.events.XMLEvent;
import javax.xml.stream.util.XMLEventAllocator;

/**
 * Parser using XML events.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class XMLEventReaderImpl
  implements XMLEventReader
{

  protected final XMLStreamReader reader;
  protected final XMLEventAllocator allocator;
  protected final String systemId;
  protected XMLEvent peekEvent;

  protected XMLEventReaderImpl(XMLStreamReader reader,
                               XMLEventAllocator allocator,
                               String systemId)
  {
    this.reader = reader;
    this.allocator = allocator;
    this.systemId = systemId;
  }

  public XMLEvent nextEvent()
    throws XMLStreamException
  {
    XMLEvent ret = peek();
    peekEvent = null;
    return ret;
  }

  public Object next()
  {
    try
      {
        return nextEvent();
      }
    catch (XMLStreamException e)
      {
        RuntimeException e2 = new RuntimeException();
        e2.initCause(e);
        throw e2;
      }
  }

  public boolean hasNext()
  {
    if (peekEvent != null)
      return true;
    try
      {
        return reader.hasNext();
      }
    catch (XMLStreamException e)
      {
        return false;
      }
  }

  public XMLEvent peek()
    throws XMLStreamException
  {
    if (peekEvent != null)
      return peekEvent;
    if (!reader.hasNext())
      return null;
    reader.next();
    peekEvent = allocator.allocate(reader);
    return peekEvent;
  }

  public String getElementText()
    throws XMLStreamException
  {
    return reader.getElementText();
  }

  public XMLEvent nextTag()
    throws XMLStreamException
  {
    if (peekEvent != null)
      {
        int eventType = peekEvent.getEventType();
        if (eventType == XMLStreamConstants.START_ELEMENT ||
            eventType == XMLStreamConstants.END_ELEMENT)
          return peekEvent;
        else
          peekEvent = null;
      }
    reader.nextTag();
    return allocator.allocate(reader);
  }

  public Object getProperty(String name)
    throws IllegalArgumentException
  {
    return reader.getProperty(name);
  }

  public void close()
    throws XMLStreamException
  {
    reader.close();
  }

  public void remove()
  {
    throw new UnsupportedOperationException();
  }

}
