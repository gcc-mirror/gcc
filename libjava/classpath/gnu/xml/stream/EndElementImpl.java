/* EndElementImpl.java --
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
import java.util.Iterator;
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.EndElement;

/**
 * An end-element event.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class EndElementImpl
  extends XMLEventImpl
  implements EndElement
{

  protected final QName name;
  protected final List namespaces;

  protected EndElementImpl(Location location, QName name, List namespaces)
  {
    super(location);
    this.name = name;
    this.namespaces = namespaces;
  }

  public int getEventType()
  {
    return END_ELEMENT;
  }

  public QName getName()
  {
    return name;
  }

  public Iterator getNamespaces()
  {
    return namespaces.iterator();
  }

  public void writeAsEncodedUnicode(Writer writer)
    throws XMLStreamException
  {
    try
      {
        writer.write("</");
        String prefix = name.getPrefix();
        if (prefix != null && !"".equals(prefix))
          {
            writer.write(prefix);
            writer.write(':');
          }
        writer.write(name.getLocalPart());
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
