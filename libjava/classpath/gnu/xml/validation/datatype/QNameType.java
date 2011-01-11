/* QNameType.java --
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

import java.io.IOException;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.ValidationContext;
import gnu.xml.stream.UnicodeReader;
import gnu.xml.stream.XMLParser;

/**
 * The XML Schema QName type.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class QNameType
  extends AtomicSimpleType
{

  static final int[] CONSTRAINING_FACETS = {
    Facet.LENGTH,
    Facet.MIN_LENGTH,
    Facet.MAX_LENGTH,
    Facet.PATTERN,
    Facet.ENUMERATION,
    Facet.WHITESPACE
  };

  QNameType()
  {
    super(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "QName"),
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
    int ci = -1;
    try
      {
        int[] cp = UnicodeReader.toCodePointArray(value);
        if (cp.length == 0)
          throw new DatatypeException("invalid NCName value");
        // XXX XML 1.1 documents?
        if (cp[0] == ':' || !XMLParser.isNameStartCharacter(cp[0], false))
          throw new DatatypeException(0, "invalid NCName value");
        for (int i = 1; i < cp.length; i++)
          {
            if (cp[i] == ':')
              {
                if (ci != -1 || (i + 1 == cp.length))
                  throw new DatatypeException(i, "invalid NCName value");
                ci = i;
              }
            else if (!XMLParser.isNameCharacter(cp[i], false))
              throw new DatatypeException(i, "invalid NCName value");
          }
      }
    catch (IOException e)
      {
        DatatypeException e2 = new DatatypeException("invalid NCName value");
        e2.initCause(e);
        throw e2;
      }
    if (ci != -1)
      {
        String prefix = value.substring(0, ci);
        if (context.resolveNamespacePrefix(prefix) == null)
          throw new DatatypeException("invalid namespace prefix");
      }
  }

  public boolean isContextDependent()
  {
    return true;
  }

}
