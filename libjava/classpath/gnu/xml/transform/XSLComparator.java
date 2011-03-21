/* XSLComparator.java --
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

package gnu.xml.transform;

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.text.Collator;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;

/**
 * Comparator for sorting lists of nodes according to a list of sort keys.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class XSLComparator
  implements Comparator<Node>
{

  final List<SortKey> sortKeys;

  XSLComparator(List<SortKey> sortKeys)
  {
    this.sortKeys = sortKeys;
  }

  public int compare(Node n1, Node n2)
  {
    for (SortKey sortKey : sortKeys)
      {
        String k1 = sortKey.key(n1);
        String k2 = sortKey.key(n2);
        if ("text".equals(sortKey.dataType))
          {
            Locale locale = (sortKey.lang == null) ? Locale.getDefault() :
              new Locale(sortKey.lang);
            Collator collator = Collator.getInstance(locale);
            int d = collator.compare(k1, k2);
            if (d != 0)
              {
                switch (sortKey.caseOrder)
                  {
                  case SortKey.UPPER_FIRST:
                    // TODO
                    break;
                  case SortKey.LOWER_FIRST:
                    // TODO
                    break;
                  }
                if (sortKey.descending)
                  {
                    d = -d;
                  }
                return d;
              }
          }
        else if ("number".equals(sortKey.dataType))
          {
            double kn1 = Expr._number(n1, k1);
            double kn2 = Expr._number(n2, k2);
            int d;
            if (Double.isNaN(kn1) || Double.isInfinite(kn2))
              {
                d = -1;
              }
            else if (Double.isNaN(kn2) || Double.isInfinite(kn1))
              {
                d = 1;
              }
            else
              {
                // conversion to int may give 0 for small numbers
                d = (kn1 > kn2) ? 1 : (kn1 < kn2) ? -1 : 0;
              }
            return (sortKey.descending) ? -d : d;
          }
      }
    return 0;
  }

}
