/* UnsignedLongType.java -- 
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
 * The XML Schema unsignedLong type.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class UnsignedLongType
  extends AtomicSimpleType
{

  static final int[] CONSTRAINING_FACETS = {
    Facet.TOTAL_DIGITS,
    Facet.FRACTION_DIGITS,
    Facet.PATTERN,
    Facet.WHITESPACE,
    Facet.ENUMERATION,
    Facet.MAX_INCLUSIVE,
    Facet.MAX_EXCLUSIVE,
    Facet.MIN_INCLUSIVE,
    Facet.MIN_EXCLUSIVE
  };

  static final String MAX_VALUE = "18446744073709551615";
  static final int LENGTH = MAX_VALUE.length();

  UnsignedLongType()
  {
    super(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedLong"),
          TypeLibrary.NON_NEGATIVE_INTEGER);
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
    if (len == 0)
      throw new DatatypeException(0, "invalid unsigned long value");
    boolean compare = false;
    for (int i = 0; i < len; i++)
      {
        if (len - i > LENGTH)
          throw new DatatypeException(i, "invalid unsigned long value");
        else if (len - i == LENGTH)
          compare = true;
        char c = value.charAt(i);
        if (c >= 0x30 && c <= 0x39)
          {
            if (compare)
              {
                char d = MAX_VALUE.charAt(i);
                if (Character.digit(c, 10) > Character.digit(d, 10))
                  throw new DatatypeException(i, "invalid unsigned long value");
              }
            continue;
          }
        throw new DatatypeException(i, "invalid unsigned long value");
      }
  }
  
  public Object createValue(String literal, ValidationContext context) {
    try
      {
        return new Long(literal);
      }
    catch (NumberFormatException e)
      {
        return null;
      }
  }
  
}

