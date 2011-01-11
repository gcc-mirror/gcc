/* Size2DSyntax.java --
   Copyright (C) 2003, 2005 Free Software Foundation, Inc.

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
 * <code>Size2DSyntax</code> is the abstract base class of all attribute
 * classes which provide a two dimensional size as value (e.g. the size of
 * a media like Letter or A4).
 * <p>
 * A <code>Size2DSyntax</code> instance consists of two integer values
 * describing the size in the x and y dimension. The units of
 * the given values is determined by two defined constants:
 * <ul>
 * <li>INCH - defines an inch</li>
 * <li>MM - defines a millimeter</li>
 * </ul>
 * </p>
 * <p>
 * A size 2D attribute is constructed by two values for the size of the x and
 * y dimension and the actual units of the given values as defined by the
 * constants.
 * </p>
 * <p>
 * There are different methods provided to return the size values for the
 * dimensions in either of the two predefined units or with a given client
 * supplied units conversion factor.
 * </p>
 * <p>
 * <b>Internal storage:</b><br>
 * The size of the x,y dimensions are stored internally in micrometers. The
 * values of the provided constants for inch (value 25400) and millimeters
 * (value 1000) are used as conversion factors to the internal storage units.
 * To get the internal micrometers values a multiplication of a given
 * size value with its units constant value is done. Retrieving the size value
 * for specific units is done by dividing the internal stored value by the
 * units constant value. Clients are therefore able to provide their own
 * size units by supplying other conversion factors.
 * Subclasses of <code>Size2DSyntax</code> have access to the internal
 * size values through the protected methods
 * {@link #getXMicrometers()} and {@link #getYMicrometers()}.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class Size2DSyntax implements Cloneable, Serializable
{
  /**
   * Constant for the units of inches.
   * The actual value is the conversion factor to micrometers.
   */
  public static final int INCH = 25400;

  /**
   * Constant for the units of millimeters.
   * The actual value is the conversion factor to micrometers.
   */
  public static final int MM = 1000;

  /** x size in micrometers. */
  private int x;
  /** y size in micrometers. */
  private int y;

  /**
   * Creates a <code>Size2DSyntax</code> object with the given arguments.
   *
   * @param x the size in x direction
   * @param y the size in y direction
   * @param units the units to use for the sizes
   *
   * @exception IllegalArgumentException if x or y &lt; 0 or units &lt; 1
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
   * @exception IllegalArgumentException if x or y &lt; 0 or units &lt; 1
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
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal, <code>false</code> otherwise.
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
   * Returns the size described in this object as a two field array.
   * Index 0 contains the size in x direction, index 1 the size in
   * y direction.
   *
   * @param units the units to use
   *
   * @return The array with the size dimensions.
   *
   * @exception IllegalArgumentException if units &lt; 1
   */
  public float[] getSize(int units)
  {
    float[] size = new float[2];
    size[0] = getX(units);
    size[1] = getY(units);
    return size;
  }

  /**
   * Returns the size in x direction.
   *
   * @param units the units to use
   *
   * @return The size in x direction.
   *
   * @exception IllegalArgumentException if units &lt; 1
   */
  public float getX(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return ((float) x) / ((float) units);
  }

  /**
   * Returns the size in x direction in mircometers.
   * To be used by sublcasses that need access to the internal storage value.
   *
   * @return The size in x direction in micrometers.
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
   * @return The size in y direction.
   *
   * @exception IllegalArgumentException if units &lt; 1
   */
  public float getY(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    return ((float) y) / ((float) units);
  }

  /**
   * Returns the size in y direction in mircometers.
   * To be used by sublcasses that need access to the internal storage value.
   *
   * @return The size in y direction in micrometers.
   */
  protected int getYMicrometers()
  {
    return y;
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return The hashcode.
   */
  public int hashCode()
  {
    return x + y;
  }

  /**
   * Returns the string representation for this object.
   * <p>
   * The returned string is in the form "XxY um" with X standing
   * for size in x and Y for the size in y direction. The used
   * micrometers units is indicated by the appended "um" notation.
   * </p>
   *
   * @return The string representation in micrometers.
   */
  public String toString()
  {
    return getXMicrometers() + "x" + getYMicrometers() + " um";
  }

  /**
   * Returns the string representation for this object.
   * <p>
   * The returned string is in the form "XxY U" with X standing
   * for size in x and Y for the size in y direction. U denotes
   * the units name if one is supplied. The values are given as
   * floating point values.
   * </p>
   *
   * @param units the units to use
   * @param unitsName the name of the units. If <code>null</code>
   * it is ommitted from the string representation.
   *
   * @return The string representation.
   */
  public String toString(int units, String unitsName)
  {
    if (unitsName == null)
      return getX(units) + "x" + getY(units);

    return getX(units) + "x" + getY(units) + " " + unitsName;
  }
}
