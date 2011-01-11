/* DTDImpl.java --
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
import java.util.List;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.DTD;

/**
 * A DOCTYPE declaration event.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DTDImpl
  extends XMLEventImpl
  implements DTD
{

  protected final String body;
  protected final Object impl;
  protected final List notations;
  protected final List entities;

  protected DTDImpl(Location location,
                    String body, Object impl, List notations, List entities)
  {
    super(location);
    this.body = body;
    this.impl = impl;
    this.notations = notations;
    this.entities = entities;
  }

  public int getEventType()
  {
    return DTD;
  }

  public String getDocumentTypeDeclaration()
  {
    return body;
  }

  public Object getProcessedDTD()
  {
    return impl;
  }

  public List getNotations()
  {
    return notations;
  }

  public List getEntities()
  {
    return entities;
  }

  public void writeAsEncodedUnicode(Writer writer)
    throws XMLStreamException
  {
    try
      {
        writer.write("<!DOCTYPE ");
        writer.write(body);
        writer.write(">");
      }
    catch (IOException e)
      {
        XMLStreamException e2 = new XMLStreamException(e.getMessage());
        e2.initCause(e);
        throw e2;
      }
  }

}
