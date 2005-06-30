/* IntegerSyntax.java -- 
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

/**
 * @author Michael Koch
 */
public abstract class IntegerSyntax implements Cloneable, Serializable
{
  private int value;

  /**
   * Creates a <code>IntegerSyntax</code> with the given value.
   *
   * @param value the value to set
   */
  protected IntegerSyntax(int value)
  {
    this.value = value;
  }

  /**
   * Creates a <code>IntegerSyntax</code> with the given arguments.
   *
   * @param value the value to set
   * @param lowerBound the lower bound for the value
   * @param upperBound the upper bound for the value
   *
   * @exception IllegalArgumentException if value < lowerBound
   * or value > upperBound
   */
  protected IntegerSyntax(int value, int lowerBound, int upperBound)
  {
    if (value < lowerBound
        || value > upperBound)
      throw new IllegalArgumentException("value not in range");

    this.value = value;
  }

  /**
   * Returns the value of this object.
   *
   * @return the value
   */
  public int getValue()
  {
    return value;
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
    if(! (obj instanceof IntegerSyntax))
      return false;

    return value == ((IntegerSyntax) obj).getValue();
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return value;
  }

  /**
   * Returns the string representation for this object.
   *
   * @return the string representation
   */
  public String toString()
  {
    return "" + value;
  }
}
