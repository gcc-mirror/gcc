/* StringArrayAttributes.java -
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.xml.libxmlj.sax;

import org.xml.sax.Attributes;

/**
 * An implementation of Attributes that reads values from an array of
 * strings, supplied by libxml2.
 * Each pair of elements in the array represents a key followed by a value.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class StringArrayAttributes
implements Attributes
{

  private int len;
  private XMLName[] keys;
  private String[] values;

  StringArrayAttributes (GnomeXMLReader parser, String[] pairs)
  {
    len = (pairs == null) ? 0 : pairs.length / 2;
    keys = new XMLName[len];
    values = new String[len];
    for (int i = 0; i < len; i++)
    {
      int pairIndex = i * 2;
      keys[i] = new XMLName (parser, pairs[pairIndex]);
      values[i] = pairs[pairIndex + 1];
    }
  }

  public int getLength ()
  {
    return len;
  }

  public String getURI (int index)
  {
    if (index < 0 || index >= len)
      {
        return null;
      }
    return keys[index].uri;
  }

  public String getLocalName (int index)
  {
    if (index < 0 || index >= len)
      {
        return null;
      }
    return keys[index].localName;
  }

  public String getQName (int index)
  {
    if (index < 0 || index >= len)
      {
        return null;
      }
    return keys[index].qName;
  }

  public String getType (int index)
  {
    if (index < 0 || index >= len)
      {
        return null;
      }
    // TODO can we get this information from libxml2?
    return "CDATA";
  }

  public String getValue (int index)
  {
    if (index < 0 || index >= len)
      {
        return null;
      }
    return values[index];
  }

  public int getIndex (String uri, String localName)
  {
    for (int i = 0; i < len; i++)
    {
      XMLName key = keys[i];
      if (key.localName.equals (localName))
      {
        if ((key.uri == null && uri == null) ||
            (key.uri != null && key.uri.equals(uri)))
          {
            return i;
          }
      }
    }
    return -1;
  }

  public int getIndex (String qName)
  {
    for (int i = 0; i < len; i++)
    {
      if (keys[i].qName.equals (qName))
        {
          return i;
        }
    }
    return -1;
  }

  public String getType (String uri, String localName)
  {
    return getType (getIndex (uri, localName));
  }

  public String getType (String qName)
  {
    return getType (getIndex (qName));
  }

  public String getValue (String uri, String localName)
  {
    return getValue (getIndex (uri, localName));
  }

  public String getValue (String qName)
  {
    return getValue (getIndex (qName));
  }

}
