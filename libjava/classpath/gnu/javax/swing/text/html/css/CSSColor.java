/* CSSColor.java -- Converts CSS color values
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

import java.awt.Color;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

/**
 * Converts CSS color values into AWT Color values.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class CSSColor
{

  private static final HashMap COLOR_MAP;
  static
  {
    COLOR_MAP = new HashMap();
    COLOR_MAP.put("maroon", "#800000");
    COLOR_MAP.put("red", "#ff0000");
    COLOR_MAP.put("orange", "#ffa500");
    COLOR_MAP.put("yellow", "#ffff00");
    COLOR_MAP.put("olive", "#808000");
    COLOR_MAP.put("purple", "#800080");
    COLOR_MAP.put("fuchsia", "#ff00ff");
    COLOR_MAP.put("white", "#ffffff");
    COLOR_MAP.put("lime", "#00ff00");
    COLOR_MAP.put("green", "#008000");
    COLOR_MAP.put("navy", "#000080");
    COLOR_MAP.put("blue", "#0000ff");
    COLOR_MAP.put("aqua", "#00ffff");
    COLOR_MAP.put("teal", "#008080");
    COLOR_MAP.put("black", "#000000");
    COLOR_MAP.put("silver", "#c0c0c0");
    COLOR_MAP.put("gray", "#808080");
  }

  /**
   * The CSS value.
   */
  private String value;

  /**
   * The converted color.
   */
  private Color color;

  /**
   * Creates a new instance.
   *
   * @param val the CSS value
   */
  public CSSColor(String val)
  {
    value = val;
    color = convertValue(value);
  }

  /**
   * Converts a CSS color value to an AWT color.
   *
   * @param value the CSS color value
   *
   * @return the converted color value
   */
  public static Color convertValue(String value)
  {
    Color color;
    String val1 = value.toLowerCase();
    if (val1.charAt(0) != '#')
      val1 = (String) COLOR_MAP.get(val1);
    if (val1 != null)
      {
        String hexVal = val1.substring(1).trim();
        try
          {
            int rgb = Integer.parseInt(hexVal, 16);
            color = new Color(rgb);
          }
        catch (NumberFormatException ex)
          {
            color = Color.BLACK;
          }
      }
    else
      color = null;
    return color;
  }

  /**
   * Returns the converted color.
   *
   * @return the converted color
   */
  public Color getValue()
  {
    return color;
  }

  public String toString()
  {
    return value;
  }

  /**
   * Returns <code>true</code> if the specified value is a valid color value,
   * <code>false</code> otherwise.
   *
   * @param val the value to check
   *
   * @return <code>true</code> if the specified value is a valid color value,
   *         <code>false</code> otherwise
   */
  public static boolean isValidColor(String val)
  {
    boolean ret = false;
    if (val.charAt(0) == '#')
      ret = true;
    else
      {
        Set colors = COLOR_MAP.keySet();
        for (Iterator i = colors.iterator(); i.hasNext() && ret == false;)
          {
            String color = (String) i.next();
            if (color.equalsIgnoreCase(val))
              ret = true;
          }
      }
    return ret;
  }
}
