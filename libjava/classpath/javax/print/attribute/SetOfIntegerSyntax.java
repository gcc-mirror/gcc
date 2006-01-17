/* SetOfIntegerSyntax.java -- 
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

/**
 * <code>SetOfIntegerSyntax</code> is the abstract base class of all attribute 
 * classes which provide a set of non-negative integers as value (e.g. the
 * page ranges to print) represented as single values or ranges of values.
 * <p>
 * A <code>SetOfIntegerSyntax</code> instance consists of an integer array of
 * ranges. Ranges may have the same lower and upper bound representing a single
 * integer value. Ranges with a lower bound greater than the upper bound are 
 * null ranges and discarded. Ranges may overlap in their values. In no case 
 * negative integers are allowed.
 * </p>
 * <p>
 * There are several constructors available:
 * <ul>
 * <li><code>SetOfIntegerSyntax(int member)</code><br>
 * Constructor for an instance with only one integer value.
 * </li><br>
 * <li><code>SetOfIntegerSyntax(int lowerBound, int upperBound)</code><br>
 * Constructor for an instance with one range of integer values.
 * </li><br>
 * <li><code>SetOfIntegerSyntax(int[][] members)</code><br>
 * Flexible constructor for an instance with several single integer values 
 * and/or several ranges of integer values. The allowed array form is an 
 * array of integer arrays of length one or two. Examples are: 
 * <code>int[0][]</code> for empty set of integers, <code>int[][] {{1}}</code>
 * , <code>int[][] {{1,5}}</code>, <code>int[][] {{1,5},{7,9}}</code>,
 * <code>int[][] {{3,7},{19}}</code>.
 * </li><br>
 * <li><code>SetOfIntegerSyntax(String s)</code><br>
 * Flexible constructor for an instance with several single integer values 
 * and/or several ranges of integer values. The allowed String instance have
 * to be a String with comma separated ranges of integer values or single 
 * values. Ranges are represented by two integer with a hypen (-) or colon (:)
 * between the lower and upper bound value. Whitespace characters are ignored.
 * Examples are: <code>""</code> for an empty set of integers, 
 * <code>"1"</code>, <code>"1-5"</code>, <code>"1-5,7-9"</code>, 
 * <code>"3-7,19"</code> and <code>"1:2,4"</code>.
 * </li>
 * </ul>
 * </p>
 * <p>
 * <b>Internal storage:</b><br>
 * The set of integers are stored internally in a normalized array form.
 * In the normalized array form the set of integer ranges are represented
 * in as few ranges as possible and overlapping ranges are merged. The ranges 
 * are always represented as an integer array of length two with ranges 
 * stored in {lower bound, upper bound} form. The ranges are stored in 
 * ascending order, without any null ranges.
 * </p>
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class SetOfIntegerSyntax
  implements Cloneable, Serializable
{
  private static final long serialVersionUID = 3666874174847632203L;

  private int[][] members;

  private static int[][] normalize(int[][] values, int size)
  {
    // Sort into increasing order.  First the first index is
    // compared, then the second.
    Arrays.sort(values, 0, size, new Comparator()
                {
                  public int compare(Object o1, Object o2)
                  {
                    int[] v1 = (int[]) o1;
                    int[] v2 = (int[]) o2;
                    if (v1[0] == v2[0])
                      return v1[1] - v2[1];
                    return v1[0] - v2[0];
                  }
                });

    // Now coalesce overlapping ranges.
    int outIndex = 0;
    for (int i = 0; i < size; ++i)
      {
        // Note that we compare with values[i][1]+1, since
        // we can coalesce {0,1} with {2,x}.
        int save = i;
        while (i + 1 < size && values[i + 1][0] <= values[i][1] + 1)
          {
            values[i][1] = Math.max(values[i][1], values[i + 1][1]);
            ++i;
          }
        values[outIndex++] = values[save];
      }
    
    int[][] result = new int[outIndex][];
    System.arraycopy(values, 0, result, 0, outIndex);
    
    return result;
  }
  
  /**
   * Creates a <code>SetOfIntegerSyntax</code> object.
   *
   * @param member the member value
   *
   * @exception IllegalArgumentException if member is &lt; 0
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
   * @param members the members to use in this set. If
   * <code>null</code> an empty set is created.
   *
   * @exception IllegalArgumentException if any element is invalid
   * @exception NullPointerException if any element of members is null
   */
  protected SetOfIntegerSyntax(int[][] members)
  {
    int[][] newMembers;
    int outIndex = 0;
    if (members == null)
      newMembers = new int[0][];
    else
      {
        newMembers = new int[members.length][];
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

            // We only want to reject non-null ranges where lower<0.
            if (lower <= upper && lower < 0)
              throw new IllegalArgumentException("invalid member element");

            if (lower <= upper)
              {
                int[] range = new int[2];
                range[0] = lower;
                range[1] = upper;
                newMembers[outIndex++] = range;
              }
          }
      }
    
    this.members = normalize(newMembers, outIndex);
  }
  
  private boolean skipWhitespace(StringCharacterIterator i)
  {
    while (Character.isWhitespace(i.current()))
      i.next();
    return i.current() == CharacterIterator.DONE;
  }
  
  private boolean skipNumber(StringCharacterIterator i)
  {
    boolean readAny = false;
    while (Character.isDigit(i.current()))
      {
        readAny = true;
        i.next();
      }
    return readAny;
  }

  /**
   * Creates a <code>SetOfIntegerSyntax</code> object.
   *
   * @param s the members to use in this set in string form. If
   * <code>null</code> an empty set is created.
   *
   * @exception IllegalArgumentException if any element is invalid
   */
  protected SetOfIntegerSyntax(String s)
  {
    if (s == null)
      this.members = normalize(new int[0][], 0);
    else
      {      
        ArrayList vals = new ArrayList();
        
        StringCharacterIterator it = new StringCharacterIterator(s);
        
        while (true)
          {
            // Skip whitespace.
            if (skipWhitespace(it))
              break;
            
            // Parse integer.
            int index = it.getIndex();
            if (! skipNumber(it))
              throw new IllegalArgumentException();
            int[] item = new int[2];
            item[0] = Integer.parseInt(s.substring(index, it.getIndex()));
            
            if (! skipWhitespace(it))
              {
                char c = it.current();
                if (c == ':' || c == '-')
                  {
                  it.next();
                  if (skipWhitespace(it))
                    throw new IllegalArgumentException();
                  index = it.getIndex();
                  if (! skipNumber(it))
                    throw new IllegalArgumentException();
                  item[1] = Integer.parseInt(s.substring(index, it.getIndex()));
                  }
                else
                  item[1] = item[0];
              }
            else
              item[1] = item[0];
            
            if (item[0] <= item[1]) 
              vals.add(item);
            
            if (skipWhitespace(it))
              break;
            if (it.current() != ',')
              throw new IllegalArgumentException();
            it.next();
          }
        
        members = normalize((int[][]) vals.toArray(new int[0][]), vals.size());
      }
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
    // We only want to reject non-null ranges where lower<0.
    if (lowerBound <= upperBound
        && lowerBound < 0)
      throw new IllegalArgumentException();

    members = (lowerBound <= upperBound ? new int[][]{{lowerBound, upperBound}}
                                        : new int[0][]);
  }

  /**
   * Checks if this set contains the given value.
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
        else if (value <= members[index][1])
          return true;
      }

    return false;
  }

  /**
   * Checks if this set contains the given value.
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
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return true if both objects are equal, false otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof SetOfIntegerSyntax))
      return false;
    SetOfIntegerSyntax other = (SetOfIntegerSyntax) obj;
    if (other.members.length != members.length)
      return false;
    for (int i = 0; i < members.length; ++i)
      {
        if (members[i][0] != other.members[i][0]
            || members[i][1] != other.members[i][1])
          return false;
      }
    return true;
  }

  /**
   * Returns an array describing the members included in this set.
   *
   * @return The members in normalized array form.
   */
  public int[][] getMembers()
  {
    return (int[][]) members.clone();
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return The hashcode.
   */
  public int hashCode()
  {
    int result = 0;
    for (int i = 0; i < members.length; ++i)
      result += members[i][0] + members[i][1];
    return result;
  }

  /**
   * Returns the smallest value that is greater than x which is in this set.
   *
   * @param x an integer value
   *
   * @return The next smallest integer value, or <code>-1</code> if there 
   * is no greater integer in the set.
   */
  public int next(int x)
  {
    for (int i = 0; i < members.length; ++i)
      {
        if (x >= members[i][1])
          continue;
        if (x < members[i][0])
          return members[i][0];
        // X is in this range.
        return x + 1;
      }
    return -1;
  }

  /**
   * Returns the string representation for this object.
   * The value is a zero length string for an empty set, or a comma seperated
   * list of ranges and single values in the form <code>"1-2,5-7,10"</code>.
   *
   * @return The string representation.
   */
  public String toString()
  {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < members.length; ++i)
      {
        if (i > 0)
          sb.append(',');
        sb.append(members[i][0]);
        if (members[i][0] != members[i][1])
          {
            sb.append('-');
            sb.append(members[i][1]);
          }
      }
    return sb.toString();
  }
}
