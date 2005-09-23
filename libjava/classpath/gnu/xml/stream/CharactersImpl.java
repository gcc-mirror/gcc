/* CharactersImpl.java -- 
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

import java.io.IOException;
import java.io.Writer;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.Characters;

/**
 * A character data (text) event.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class CharactersImpl
  extends XMLEventImpl
  implements Characters
{

  protected final String data;
  protected final boolean whitespace;
  protected final boolean cdata;
  protected final boolean ignorableWhitespace;

  protected CharactersImpl(Location location,
                           String data, boolean whitespace, boolean cdata,
                           boolean ignorableWhitespace)
  {
    super(location);
    this.data = data;
    this.whitespace = whitespace;
    this.cdata = cdata;
    this.ignorableWhitespace = ignorableWhitespace;
  }

  public int getEventType()
  {
    return cdata ? CDATA : whitespace ? SPACE : CHARACTERS;
  }

  public String getData()
  {
    return data;
  }

  public boolean isWhiteSpace()
  {
    return whitespace;
  }

  public boolean isCData()
  {
    return cdata;
  }
  
  public boolean isIgnorableWhiteSpace()
  {
    return ignorableWhitespace;
  }
  
  public void writeAsEncodedUnicode(Writer writer)
    throws XMLStreamException
  {
    try
      {
        if (cdata)
          {
            writer.write("<![CDATA[");
            writer.write(data);
            writer.write("]]>");
          }
        else   
          writer.write(encode(data, false));
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e.getMessage());
        e2.initCause(e);
        throw e2;
      }
  }
  
}

