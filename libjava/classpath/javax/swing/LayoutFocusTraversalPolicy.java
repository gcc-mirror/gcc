/* LayoutFocusTraversalPolicy.java --
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


package javax.swing;

import java.awt.Component;
import java.io.Serializable;
import java.util.Comparator;

/**
 * @author Graydon Hoare
 * @author Michael Koch
 *
 * @since 1.4
 */
public class LayoutFocusTraversalPolicy
  extends SortingFocusTraversalPolicy
  implements Serializable
{
  private static class LayoutComparator
    implements Comparator
  {
    public LayoutComparator()
    {
      // Do nothing here.
    }

    public int compare(Object o1, Object o2)
    {
      Component comp1 = (Component) o1;
      Component comp2 = (Component) o2;

      int x1 = comp1.getX();
      int y1 = comp1.getY();
      int x2 = comp2.getX();
      int y2 = comp2.getY();

      if (x1 == x2 && y1 == y2)
        return 0;

      if ((y1 < y2) || ((y1 == y2) && (x1 < x2)))
        return -1;

      return 1;
    }
  }

  private static final long serialVersionUID = 4312146927238881442L;

  public LayoutFocusTraversalPolicy()
  {
    super(new LayoutComparator());
  }
}
