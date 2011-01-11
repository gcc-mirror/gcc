/* MediaPrintableArea.java --
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import javax.print.attribute.Attribute;
import javax.print.attribute.DocAttribute;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;

/**
 * The <code>MediaPrintableArea</code> attribute specifies the area
 * of a media sheet which is available for printing.
 * <p>
 * Due to hardware limitation its not possible with most printers to use the
 * whole area of a media sheet for printing. This attribute defines the area
 * for printing through the values of the upper left corner position (x,y)
 * on the sheet and the available width and height of the area. The units of
 * the values are determined by two defined constants:
 * <ul>
 * <li>INCH - defines an inch</li>
 * <li>MM - defines a millimeter</li>
 * </ul>
 * </p>
 * <p>
 * <b>Internal storage:</b><br>
 * The values of x, y, width and height are stored internally in micrometers.
 * The values of the provided constants for inch (value 25400) and millimeters
 * (value 1000) are used as conversion factors to the internal storage units.
 * To get the internal micrometers values a multiplication of a given
 * size value with its units constant value is done. Retrieving the size value
 * for specific units is done by dividing the internal stored value by the
 * units constant value.
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> MediaPrintableArea is not an IPP 1.1 attribute.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class MediaPrintableArea
  implements DocAttribute, PrintJobAttribute, PrintRequestAttribute
{
  private static final long serialVersionUID = -1597171464050795793L;

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

  /** x in micrometers. */
  private int x;
  /** y in micrometers. */
  private int y;
  /** width in micrometers. */
  private int w;
  /** height in micrometers. */
  private int h;

  /**
   * Creates a new <code>MediaPrintableArea</code> object with the given
   * float values for the given units.
   *
   * @param x start of the printable area on the sheet in x direction.
   * @param y start of the printable area on the sheet in y direction.
   * @param w the width of the printable area.
   * @param h the height of the printable area.
   * @param units the units of the given values.
   *
   * @throws IllegalArgumentException if x i&lt; 0 or y i&lt; 0 or w i&lt;= 0
   * or h i&lt;= 0 or units i&lt; 1
   */
  public MediaPrintableArea(float x, float y, float w, float h, int units)
  {
    if (x < 0.0f || y < 0.0f || w <= 0.0f || h <= 0.0f)
      throw new IllegalArgumentException();

    this.x = (int) (x * units + 0.5f);
    this.y = (int) (y * units + 0.5f);
    this.w = (int) (w * units + 0.5f);
    this.h = (int) (h * units + 0.5f);
  }

  /**
   * Creates a new <code>MediaPrintableArea</code> object with the given
   * int values for the given units.
   *
   * @param x start of the printable area on the sheet in x direction.
   * @param y start of the printable area on the sheet in y direction.
   * @param w the width of the printable area.
   * @param h the height of the printable area.
   * @param units the units of the given values.
   *
   * @throws IllegalArgumentException if x i&lt; 0 or y i&lt; 0 or w i&lt;= 0
   * or h i&lt;= 0 or units i&lt; 1
   */
  public MediaPrintableArea(int x, int y, int w, int h, int units)
  {
    if (x < 0 || y < 0 || w <= 0 || h <= 0)
      throw new IllegalArgumentException();

    this.x = x * units;
    this.y = y * units;
    this.w = w * units;
    this.h = h * units;
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>MediaPrintableArea</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return MediaPrintableArea.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "media-printable-area".
   */
  public String getName()
  {
    return "media-printable-area";
  }

  /**
   * Returns the height of the printable area for the given units.
   *
   * @param units the units conversion factor.
   * @return The height.
   *
   * @throws IllegalArgumentException if <code>units</code> is &lt; 1
   */
  public float getHeight(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less than 1");

    return h / ((float)units);
  }

  /**
   * Returns the width of the printable area for the given units.
   *
   * @param units the units conversion factor.
   * @return The width.
   *
   * @throws IllegalArgumentException if <code>units</code> is &lt; 1
   */
  public float getWidth(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less than 1");

    return w / ((float)units);
  }

  /**
   * Returns the position in x direction of the printable area
   * for the given units.
   *
   * @param units the units conversion factor.
   * @return The position in x direction.
   *
   * @throws IllegalArgumentException if <code>units</code> is &lt; 1
   */
  public float getX(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less than 1");

    return x / ((float)units);
  }

  /**
   * Returns the position in y direction of the printable area
   * for the given units.
   *
   * @param units the units conversion factor.
   * @return The position in y direction.
   *
   * @throws IllegalArgumentException if <code>units</code> is &lt; 1
   */
  public float getY(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less than 1");

    return y / ((float)units);
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
    if (! (obj instanceof MediaPrintableArea))
      return false;

    MediaPrintableArea tmp = (MediaPrintableArea) obj;

    return (x == tmp.getX(1) && y == tmp.getY(1)
            && w == tmp.getWidth(1) && h == tmp.getHeight(1));
  }

  /**
   * Returns the string representation for this object in units of millimeters..
   * <p>
   * The returned string is in the form "(x,y)->(width,height)mm".
   * </p>
   * @return The string representation in millimeters.
   */
  public String toString()
  {
    return toString(MM, "mm");
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return The hashcode.
   */
  public int hashCode()
  {
    return x ^ y + w ^ h;
  }

  /**
   * Returns the string representation for this object in units of millimeters..
   * <p>
   * The returned string is in the form "(x,y)->(width,height)unitsName".
   * </p>
   * @param units the units to use for conversion.
   * @param unitsName the name of the used units, appended to the resulting
   * string if not <code>null</code>.
   * @return The string representation in millimeters.
   *
   * @throws IllegalArgumentException if <code>units</code> is &lt; 1
   */
  public String toString(int units, String unitsName)
  {
    if (units < 1)
      throw new IllegalArgumentException("units may not be less than 1");

    String tmp = "(" + getX(units) + "," + getY(units) + ")->("
                 + getWidth(units) + "," + getHeight(units) + ")";

    return unitsName == null ? tmp : tmp + unitsName;
  }

  /**
   * Returns the printable area as an float[] with 4 values
   * (order x, y, width, height) in the given units.
   *
   * @param units the units to use.
   * @return The printable area as float array.
   */
  public float[] getPrintableArea(int units)
  {
    return new float[] { getX(units), getY(units),
                         getWidth(units), getHeight(units) };
  }
}
