/* NameComponentComparator.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.NamingService;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CosNaming.NameComponent;

import java.util.Comparator;

/**
 * This class implements the name component comparator, needed to
 * sort and compare the name components in maps and sorted sets.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class NameComponentComparator
  implements Comparator
{
  /**
   * The singleton instance of the name comparator.
   */
  public static final NameComponentComparator singleton = new NameComponentComparator();

  /**
   * It is enough to have a singleton.
   */
  private NameComponentComparator()
  {
  }

  /**
   * Compare the two names components.
   *
   * @param nc_a the first name component.
   * @param nc_b the second name component.
   *
   * @return 0 if the name components are equal, non zero value
   * as result of comparison otherwise.
   *
   * @throws BAD_PARAM if one of the components is empty or
   * has {@link NameComponent#id} or {@link NameComponent#kind}
   * field intialised to null.
   */
  public final int compare(Object nc_a, Object nc_b)
  {
    NameComponent a = (NameComponent) nc_a;
    NameComponent b = (NameComponent) nc_b;

    int cn = a.id.compareTo(b.id);
    if (cn != 0)
      return cn;
    return a.kind.compareTo(b.kind);
  }

  /**
   * All instances of this class are equal.
   */
  public boolean equals(Object x)
  {
    return x instanceof NameComponentComparator;
  }
}
