/* FontSize.java -- Converts CSS font size values into real values
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.swing.text.html.css;

/**
 * Converts CSS font-size values into real (point) values.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class FontSize
{

  /**
   * The CSS value.
   */
  private String value;

  /**
   * The actual font size.
   */
  private int size;

  /**
   * The index of one of the standard sizes that this font size maps to.
   * This is -1 if this fontsize doesn't map to one of the standard sizes.
   *
   * @see #SCALE
   */
  private int sizeIndex;

  /**
   * True when this font size is relative.
   */
  private boolean isRelative;

  /**
   * The default size for 'medium' absolute size. The other absolute sizes
   * are calculated from this.
   */
  public static final int DEFAULT_FONT_SIZE = 12;

  /**
   * The scaling factors relative to the medium size. Medium is at index 2.
   */
  private static final double[] SCALE = {0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8 };

  /**
   * Creates a new FontSize for the specified value.
   *
   * @param val the value to convert
   */
  public FontSize(String val)
  {
    value = val;
    sizeIndex = -1;
    isRelative = false;
    size = mapValue();
  }

  /**
   * Returns the font size value.
   *
   * @return the font size value
   */
  public int getValue(int p)
  {
    if (isRelative)
      mapRelative(p);
    return size;
  }

  public int getValue()
  {
    assert ! isRelative;
    return size;
  }

  /**
   * Returns the converted real value in point.
   *
   * @return the converted real value in point
   */
  private int mapValue()
  {
    int intVal;
    if (value.contains("pt"))
      intVal = mapPoints();
    else if (value.contains("px"))
      intVal = mapPixels();
    else if (value.contains("em") || value.contains("%")
        || value.contains("larger") || value.contains("smaller"))
      {
        intVal = -1;
        isRelative = true;
      }
    else
      intVal = mapAbsolute();
    return intVal;
  }

  /**
   * Maps point values ('XXXpt').
   *
   * @return the real font size
   */
  private int mapPoints()
  {
    int end = value.indexOf("pt");
    String number = value.substring(0, end);
    int intVal = (int) Double.parseDouble(number);
    return intVal;
  }

  /**
   * Maps pixel values ('XXXpx').
   *
   * @return the real font size
   */
  private int mapPixels()
  {
    int end = value.indexOf("px");
    if (end == -1)
      end = value.length();
    String number = value.substring(0, end);
    try
      {
        int intVal = (int) Double.parseDouble(number);
        return intVal;
      }
    catch (NumberFormatException ex)
      {
        return DEFAULT_FONT_SIZE;
      }
  }

  private int mapPercent(int par)
  {
    int end = value.indexOf("%");
    if (end == -1)
      end = value.length();
    String number = value.substring(0, end);
    try
      {
        int intVal = (int) Double.parseDouble(number);
        return intVal * par / 100;
      }
    catch (NumberFormatException ex)
      {
        System.err.println("couldn't map value: '" + value + "'");
        return DEFAULT_FONT_SIZE;
      }
  }

  private int mapEM(int par)
  {
    int end = value.indexOf("em");
    if (end == -1)
      end = value.length();
    String number = value.substring(0, end);
    try
      {
        float factor = Float.parseFloat(number);
        // FIXME: Should be relative to the parent element's size.
        return (int) (factor * par);
      }
    catch (NumberFormatException ex)
      {
        return DEFAULT_FONT_SIZE;
      }
  }

  private int mapSmaller(int par)
  {
    return (int) (par * 0.9);
  }

  private int mapLarger(int par)
  {
    return (int) (par * 0.9);
  }

  /**
   * Maps absolute font-size values.
   *
   * @return the real value
   */
  private int mapAbsolute()
  {
    int index;
    if (value.equals("xx-small") || value.equals("x-small"))
      index = 0;
    else if (value.equals("small"))
      index = 1;
    else if (value.equals("medium"))
      index = 2;
    else if (value.equals("large"))
      index = 3;
    else if (value.equals("x-large"))
      index = 4;
    else if (value.equals("xx-large"))
      index = 5;
    else
      index = 2;
    double scale = SCALE[index];
    // FIXME: Scale the real medium size of the document, rather than the
    // constant here.
    int intVal = (int) (scale * DEFAULT_FONT_SIZE);
    sizeIndex = index;
    return intVal;
  }

  /**
   * Returns the string representation.
   */
  public String toString()
  {
    return value;
  }

  private int mapRelative(int par)
  {
    if (value.indexOf('%') != -1)
      size = mapPercent(par);
    else if (value.indexOf("em") != -1)
      size = mapEM(par);
    else if (value.indexOf("larger") != -1)
      size = mapLarger(par);
    else if (value.indexOf("smaller") != -1)
      size = mapSmaller(par);
    return size;
  }

  public boolean isRelative()
  {
    return isRelative;
  }
}
