/* MaxInclusiveFacet.java --
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Date;

/**
 * The <code>maxInclusive</code> facet.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class MaxInclusiveFacet
  extends Facet
{

  public final Object value;

  public final boolean fixed;

  public MaxInclusiveFacet(Object value, boolean fixed, Annotation annotation)
  {
    super(MAX_INCLUSIVE, annotation);
    this.value = value;
    this.fixed = fixed;
  }

  public int hashCode()
  {
    return value.hashCode();
  }

  public boolean equals(Object other)
  {
    return (other instanceof MaxInclusiveFacet &&
            ((MaxInclusiveFacet) other).value.equals(value));
  }

  boolean matches(Object test)
  {
    if (value instanceof Date)
      {
        Date dvalue = (Date) value;
        if (!(test instanceof Date))
          return false;
        Date dtest = (Date) test;
        return dtest.equals(dvalue) || dtest.before(dvalue);
      }
    else if (value instanceof BigInteger)
      {
        BigInteger ivalue = (BigInteger) value;
        if (!(test instanceof BigInteger))
          return false;
        return ((BigInteger) test).compareTo(ivalue) <= 0;
      }
    else if (value instanceof BigDecimal)
      {
        BigDecimal dvalue = (BigDecimal) value;
        if (!(test instanceof BigDecimal))
          return false;
        return ((BigDecimal) test).compareTo(dvalue) <= 0;
      }
    else if (value instanceof Comparable)
      {
        if (!(test.getClass().equals(value.getClass())))
          return false;
        return ((Comparable) test).compareTo(value) <= 0;
      }
    Number nvalue = (Number) value;
    if (!(test instanceof Number))
      return false;
    return ((Number) test).doubleValue() <= nvalue.doubleValue();
  }

}
