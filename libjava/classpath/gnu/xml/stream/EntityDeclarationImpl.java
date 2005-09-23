/* EntityDeclarationImpl.java -- 
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
import javax.xml.stream.events.EntityDeclaration;

/**
 * An entity declaration event.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class EntityDeclarationImpl
  extends XMLEventImpl
  implements EntityDeclaration
{

  protected final String publicId;
  protected final String systemId;
  protected final String name;
  protected final String notationName;
  protected final String replacementText;
  protected final String baseUri;

  protected EntityDeclarationImpl(Location location,
                                  String publicId, String systemId,
                                  String name, String notationName,
                                  String replacementText, String baseUri)
  {
    super(location);
    this.publicId = publicId;
    this.systemId = systemId;
    this.name = name;
    this.notationName = notationName;
    this.replacementText = replacementText;
    this.baseUri = baseUri;
  }

  public int getEventType()
  {
    return ENTITY_DECLARATION;
  }

  public String getPublicId()
  {
    return publicId;
  }

  public String getSystemId()
  {
    return systemId;
  }

  public String getName()
  {
    return name;
  }

  public String getNotationName()
  {
    return notationName;
  }

  public String getReplacementText()
  {
    return replacementText;
  }

  public String getBaseURI()
  {
    return baseUri;
  }

  public void writeAsEncodedUnicode(Writer writer)
    throws XMLStreamException
  {
    try
      {
        writer.write("<!ENTITY ");
        writer.write(name);
        writer.write(' ');
        if (systemId != null)
          {
            if (publicId != null)
              {
                writer.write(" PUBLIC ");
                writer.write('"');
                writer.write(publicId);
                writer.write('"');
                writer.write(' ');
                writer.write('"');
                writer.write(systemId);
                writer.write('"');
              }
            else
              {
                writer.write(" SYSTEM ");
                writer.write('"');
                writer.write(systemId);
                writer.write('"');
              }
            if (notationName != null)
              {
                writer.write(" NDATA ");
                writer.write(notationName);
              }
          }
        else
          {
            writer.write('"');
            if (replacementText != null)
              writer.write(replacementText);
            writer.write('"');
          }
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

