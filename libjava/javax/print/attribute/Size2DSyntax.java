/* Size2DSyntax.java -- 
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
public abstract class Size2DSyntax implements Cloneable, Serializable
{
  /**
   * Constant for units of dots per mircometer to describe an inch.
   */
  public static final int INCH = 25400;

  /**
   * Constant for units of dots per mircometer to describe a centimeter.
   */
  public static final int MM = 1000;

  private int x;
  private int y;

  /**
   * Creates a <code>Size2DSyntax</code> object with the given arguments.
   *
   * @param x the size in x direction
   * @param y the size in y direction
   * @param units the units to use for the sizes
   *
   * @exception IllegalArgumentException if preconditions fail
   */
  protected Size2DSyntax(float x, float y, int units)
  {
    if (x < 0.0f || y < 0.0f)
      throw new IllegalArgumentException("x and/or y may not be less than 0");

    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    this.x = (int) (x * units + 0.5f);
    this.y = (int) (y * units + 0.5f);
  }

  /**
   * Creates a <code>Size2DSyntax</code> object with the given arguments.
   *
   * @param x the size in x direction
   * @param y the size in y direction
   * @param units the units to use for the sizes
   *
   * @exception IllegalArgumentException if preconditions fail
   */
  protected Size2DSyntax(int x, int y, int units)
  {
    if (x < 0 || y < 0)
      throw new IllegalArgumentException("x and/or y may not be less then 0");

    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    this.x = x * units;
    this.y = y * units;
  }

  /**
   * Tests of obj is equal to this object.
   *
   * @param obj the object to test
   *
   * @returns true if both objects are equal, false otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof Size2DSyntax))
      return false;

    Size2DSyntax tmp = (Size2DSyntax) obj;

    return (x == tmp.getXMicrometers()
            && y == tmp.getYMicrometers());
  }

  /**
   * Return the size described in this object as a two field array.
   * Index 0 contains the size in x direction, index 1 the size in
   * y direction.
   *
   * @param units the units to use
   *
   * @return the array that describes the size
   *
   * @exception IllegalArgumentException if units < 1
   */
  public float[] getSize(int units)
  {
    float[] size = new float[2];
    size[0] = getX(units);
    size[1] = getY(units);
    return size;
  }

  /**
   * Return the size in x direction.
   *
   * @param units the units to use
   *
   * @return the size value
   *
   * @exception IllegalArgumentException if units < 1
   */
  public float getX(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return ((float) x) / ((float) units);
  }

  /**
   * Returns the size in x direction in mircometers.
   *
   * @return the size value
   */
  protected int getXMicrometers()
  {
    return x;
  }

  /**
   * Return the size in y direction.
   *
   * @param units the units to use
   *
   * @return the size value
   *
   * @exception IllegalArgumentException if units < 1
   */
  public float getY(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return ((float) y) / ((float) units);
  }
  
  /**
   * Returns the size in y direction in mircometers.
   *
   * @return the size value
   */
  protected int getYMicrometers()
  {
    return y;
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return x + y;
  }

  /**
   * Returns the string representation for this object.
   *
   * @return the string representation
   */
  public String toString()
  {
    return toString(1, "um");
  }

  /**
   * Returns the string representation for this object.
   *
   * @param units the units to use
   * @param unitsName the name of the units
   *
   * @return the string representation
   */
  public String toString(int units, String unitsName)
  {
    return "" + getX(units) + "x" + getY(units) + " " + unitsName;
  }
}
