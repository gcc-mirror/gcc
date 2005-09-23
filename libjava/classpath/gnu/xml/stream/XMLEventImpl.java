/* XMLEventImpl.java -- 
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

import java.io.Writer;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.Characters;
import javax.xml.stream.events.EndElement;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;

/**
 * An XML stream event.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class XMLEventImpl
  implements XMLEvent
{

  protected final Location location;

  protected XMLEventImpl(Location location)
  {
    this.location = location;
  }

  public abstract int getEventType();

  public Location getLocation()
  {
    return location;
  }

  public boolean isStartElement()
  {
    return getEventType() == START_ELEMENT;
  }

  public boolean isAttribute()
  {
    return getEventType() == ATTRIBUTE;
  }

  public boolean isNamespace()
  {
    return getEventType() == NAMESPACE;
  }

  public boolean isEndElement()
  {
    return getEventType() == END_ELEMENT;
  }

  public boolean isEntityReference()
  {
    return getEventType() == ENTITY_REFERENCE;
  }

  public boolean isProcessingInstruction()
  {
    return getEventType() == PROCESSING_INSTRUCTION;
  }

  public boolean isCharacters()
  {
    int et = getEventType();
    return et == CHARACTERS || et == CDATA;
  }

  public boolean isStartDocument()
  {
    return getEventType() == START_DOCUMENT;
  }

  public boolean isEndDocument()
  {
    return getEventType() == END_DOCUMENT;
  }

  public boolean isStartEntity()
  {
    return getEventType() == START_ENTITY;
  }

  public boolean isEndEntity()
  {
    return getEventType() == END_ENTITY;
  }

  public StartElement asStartElement()
  {
    return (StartElement) this;
  }

  public EndElement asEndElement()
  {
    return (EndElement) this;
  }

  public Characters asCharacters()
  {
    return (Characters) this;
  }

  public QName getSchemaType()
  {
    return null;
  }
  
  public abstract void writeAsEncodedUnicode(Writer writer)
    throws XMLStreamException;

  protected String encode(String text, boolean inAttr)
  {
    int len = text.length();
    StringBuffer buf = null;
    for (int i = 0; i < len; i++)
      {
        char c = text.charAt(i);
        if (c == '<')
          {
            if (buf == null)
              {
                buf = new StringBuffer(text.substring(0, i));
              }
            buf.append("&lt;");
          }
        else if (c == '>')
          {
            if (buf == null)
              {
                buf = new StringBuffer(text.substring(0, i));
              }
            buf.append("&gt;");
          }
        else if (c == '&')
          {
            if (buf == null)
              {
                buf = new StringBuffer(text.substring(0, i));
              }
            buf.append("&amp;");
          }
        else if (c == '\'' && inAttr)
          {
            if (buf == null)
              {
                buf = new StringBuffer(text.substring(0, i));
              }
            buf.append("&apos;");
          }
        else if (c == '"' && inAttr)
          {
            if (buf == null)
              {
                buf = new StringBuffer(text.substring(0, i));
              }
            buf.append("&quot;");
          }
        else if (buf != null)
          {
            buf.append(c);
          }
      }
    return (buf == null) ? text : buf.toString(); 
  }

}

