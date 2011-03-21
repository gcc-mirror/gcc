/* GDayType.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.xml.validation.datatype;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.ValidationContext;

/**
 * The XML Schema gDay type.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class GDayType
  extends AtomicSimpleType
{

  static class GDay
    implements Comparable
  {

    int day;

    public int hashCode()
    {
      return day;
    }

    public boolean equals(Object other)
    {
      if (other instanceof GDay)
        return ((GDay) other).day == day;
      return false;
    }

    public int compareTo(Object other)
    {
      if (other instanceof GDay)
        {
          GDay gd = (GDay) other;
          if (gd.day == day)
            return 0;
          return (day < gd.day) ? -1 : 1;
        }
      return 0;
    }

  }

  static final int[] CONSTRAINING_FACETS = {
    Facet.PATTERN,
    Facet.ENUMERATION,
    Facet.WHITESPACE,
    Facet.MAX_INCLUSIVE,
    Facet.MAX_EXCLUSIVE,
    Facet.MIN_INCLUSIVE,
    Facet.MIN_EXCLUSIVE
  };

  GDayType()
  {
    super(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "gDay"),
          TypeLibrary.ANY_SIMPLE_TYPE);
  }

  public int[] getConstrainingFacets()
  {
    return CONSTRAINING_FACETS;
  }

  public void checkValid(String value, ValidationContext context)
    throws DatatypeException
  {
    super.checkValid(value, context);
    int len = value.length();
    int state = 0;
    int start = 0;
    for (int i = 0; i < len; i++)
      {
        char c = value.charAt(i);
        if (c >= 0x30 && c <= 0x39)
          continue;
        switch (state)
          {
          case 0: // year
            if (c == '-')
              {
                switch (i)
                  {
                  case 0:
                    continue;
                  case 1:
                    state = 1;
                    start = i + 1;
                    continue;
                  default:
                    throw new DatatypeException(i, "invalid GDay value");
                  }
              }
            break;
          case 1: // month
            if (c == '-')
              {
                if (i - start != 0)
                  throw new DatatypeException(i, "invalid GDay value");
                state = 2;
                start = i + 1;
                continue;
              }
            break;
          }
        throw new DatatypeException(i, "invalid GDay value");
      }
    switch (state)
      {
      case 2: // day
        if (len - start != 2)
          throw new DatatypeException("invalid GDay value");
        break;
      default:
        throw new DatatypeException("invalid GDay value");
      }
  }

  public Object createValue(String literal, ValidationContext context) {
    try
      {
        GDay ret = new GDay();
        ret.day = Integer.parseInt(literal.substring(3));
        return ret;
      }
    catch (Exception e)
      {
        return null;
      }
  }

}
