/* Base64BinaryType.java -- 
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
 * The XML Schema base64Binary type.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class Base64BinaryType
  extends AtomicSimpleType
{

  static final String B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    "abcdefghijklmnopqrstuvwxyz0123456789+/";
  static final String B16 = "AEIMQUYcgkosw048";
  static final String B04 = "AQgw";

  static final int[] CONSTRAINING_FACETS = {
    Facet.LENGTH,
    Facet.MIN_LENGTH,
    Facet.MAX_LENGTH,
    Facet.PATTERN,
    Facet.ENUMERATION,
    Facet.WHITESPACE
  };

  Base64BinaryType()
  {
    super(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "base64Binary"),
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
    // TODO value = collapseWhitespace(value);
    int len = value.length();
    try
      {
        for (int i = len - 1; i >= 0; )
          {
            char c4 = value.charAt(i--);
            if (c4 == ' ')
              c4 = value.charAt(i--);
            char c3 = value.charAt(i--);
            if (c3 == ' ')
              c3 = value.charAt(i--);
            char c2 = value.charAt(i--);
            if (c2 == ' ')
              c2 = value.charAt(i--);
            char c1 = value.charAt(i--);
            if (c1 == ' ')
              c1 = value.charAt(i--);
            
            if (c4 == '=')
              {
                if (c3 == '=')
                  {
                    if (B04.indexOf(c2) != -1 &&
                        B64.indexOf(c1) != -1)
                      continue;
                  }
                else if (B16.indexOf(c3) != -1)
                  {
                    if (B64.indexOf(c2) != -1 &&
                        B64.indexOf(c1) != -1)
                      continue;
                  }
              }
            else if (B64.indexOf(c4) != -1)
              continue;
            throw new DatatypeException(i, "illegal BASE64");
          }
      }
    catch (IndexOutOfBoundsException e)
      {
        throw new DatatypeException("illegal BASE64");
      }
  }
  
}

