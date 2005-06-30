/* SetOfIntegerSyntax.java -- 
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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

package javax.print.attribute;

import java.io.Serializable;
import java.util.Vector;

/**
 * @author Michael Koch
 */
public abstract class SetOfIntegerSyntax
  implements Cloneable, Serializable
{
  private static final long serialVersionUID = 3666874174847632203L;

  private int[][] members;

  private static int[][] normalize(Vector vecMembers)
  {
    // XXX: Perhaps we should merge ranges that overlap.
    
    int current = 0;
    int[][] members = new int[vecMembers.size()][];

    while (vecMembers.size() > 0)
      {
        // Search the lowest range.
        int[] range = (int[]) vecMembers.elementAt(0);

        for (int index = 1; index < vecMembers.size(); index++)
          {
            int[] tmp = (int[]) vecMembers.elementAt(index);

            if (range[0] > tmp[0]
                || (range[0] == tmp[0]
                    && range[0] > tmp[0]))
              range = tmp;
          }

        members[current] = range;
        current++;
      }
    
    return members;
  }
  
  /**
   * Creates a <code>SetOfIntegerSyntax</code> object.
   *
   * @param member the member value
   *
   * @exception IllegalArgumentException if member is < 0
   */
  protected SetOfIntegerSyntax(int member)
  {
    if (member < 0)
      throw new IllegalArgumentException("member may not be less than 0");

    this.members = new int[][]{{member, member}};
  }

  /**
   * Creates a <code>SetOfIntegerSyntax</code> object.
   *
   * @param members the members to use in this set
   *
   * @exception IllegalArgumentException if any element is invalid
   * @exception NullPointerException if any element of members is null
   */
  protected SetOfIntegerSyntax(int[][] members)
  {
    Vector vecMembers = new Vector();
    
    if (members != null)
      {
        for (int index = 0; index < members.length; index++)
          {
            int lower;
            int upper;

            if (members[index].length == 1)
              {
                lower = members[index][0];
                upper = members[index][0];
              }
            else if (members[index].length == 2)
              {
                lower = members[index][0];
                upper = members[index][1];
              }
            else
              throw new IllegalArgumentException("invalid member element");

            if (lower <= upper && lower < 0)
              throw new IllegalArgumentException("invalid member element");

            if (lower <= upper)
              {
                int[] range = new int[2];
                range[0] = lower;
                range[1] = upper;
                vecMembers.add(range);
              }
          }
      }
    
    this.members = normalize(vecMembers);
  }

  /**
   * Creates a <code>SetOfIntegerSyntax</code> object.
   *
   * @param lowerBound the lower bound value
   * @param upperBound the upper bound value
   *
   * @exception IllegalArgumentException if lowerBound &lt;= upperbound
   * and lowerBound &lt; 0
   */
  protected SetOfIntegerSyntax(int lowerBound, int upperBound)
  {
    if (lowerBound <= upperBound
        && lowerBound < 0)
      throw new IllegalArgumentException();

    members = (lowerBound <= upperBound ? new int[][]{{lowerBound, upperBound}}
                                        : new int[0][]);
  }

  /**
   * Checks if this set contains value.
   *
   * @param value the value to test for
   *
   * @return true if this set contains value, false otherwise
   */
  public boolean contains(int value)
  {
    // This only works on a normalized member array.
    for (int index = 0; index < members.length; index++)
      {
        if (value < members[index][0])
          return false;
        else if (value < members[index][1])
          return true;
      }

    return false;
  }

  /**
   * Checks if this set contains value.
   *
   * @param value the value to test for
   *
   * @return true if this set contains value, false otherwise
   */
  public boolean contains(IntegerSyntax value)
  {
    return contains(value.getValue());
  }

  /**
   * Tests of obj is equal to this object.
   *
   * @param obj the object to test
   *
   * @return true if both objects are equal, false otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof SetOfIntegerSyntax))
      return false;

    throw new Error("not implemented");
  }

  /**
   * Returns an array describing the members included in this set.
   *
   * @return the array with the members
   */
  public int[][] getMembers()
  {
    throw new Error("not implemented");
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    throw new Error("not implemented");
  }

  /**
   * Returns the smallest value that is greater then x.
   *
   * @param x an integer value
   *
   * @return the next value
   */
  public int next(int x)
  {
    throw new Error("not implemented");
  }

  /**
   * Returns the string representation for this object.
   *
   * @return the string representation
   */
  public String toString()
  {
    throw new Error("not implemented");
  }
}
