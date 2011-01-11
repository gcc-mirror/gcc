/* Pair.java -- A heterogenous pair of objects.
   Copyright (C) 2006
   Free Software Foundation, Inc.

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


package gnu.classpath;

/**
 * A container for a pair of heterogenous objects.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class Pair<A,B>
{

  /**
   * The left-hand side of the pair.
   */
  private A left;

  /**
   * The right-hand side of the pair.
   */
  private B right;

  /**
   * Constructs a new pair using the given left and
   * right values.
   *
   * @param left the left-hand side of the pair.
   * @param right the right-hand side of the pair.
   */
  public Pair(A left, B right)
  {
    this.left = left;
    this.right = right;
  }

  /**
   * Returns the left-hand side of the pair.
   *
   * @return the left-hand value.
   */
  public A getLeft()
  {
    return left;
  }

  /**
   * Returns the right-hand side of the pair.
   *
   * @return the right-hand value.
   */
  public B getRight()
  {
    return right;
  }

  /**
   * Returns true if the specified object is also a
   * pair with equivalent left and right values.
   *
   * @param obj the object to compare.
   * @return true if the two are equal.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof Pair)
      {
        Pair<A,B> p = (Pair<A,B>) obj;
        A lp = p.getLeft();
        B rp = p.getRight();
        return (lp == null ? left == null : lp.equals(left)) &&
          (rp == null ? right == null : rp.equals(right));
      }
    return false;
  }

  /**
   * Returns a hashcode for the pair, created by the
   * summation of the hashcodes of the left and right
   * hand values.
   *
   * @return a hashcode for the pair.
   */
  public int hashCode()
  {
    return (left == null ? 0 : left.hashCode())
      + (right == null ? 0 : right.hashCode());
  }

}
