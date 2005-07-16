/* ColorUIResource.java
   Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf;

import java.awt.Color;


/**
 * A Color that is marked as <code>UIResource</code>, which indicates that
 * the color has been installed by a pluggable LookAndFeel. Such colors
 * are replaced when the LookAndFeel changes.
 *
 * @see java.awt.Color
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class ColorUIResource
  extends Color
  implements UIResource
{
  /**
   * Constructs a <code>ColorUIResource</code> using the specified
   * red, green, and blue values, which must be given as integers in
   * the range of 0-255. The alpha channel value will default to 255,
   * meaning that the color is fully opaque.
   *
   * @param r the red intensity, which must be in the range [0 .. 255].
   * @param g the green intensity, which must be in the range [0 .. 255].
   * @param b the blue intensity, which must be in the range [0 .. 255].
   *
   * @throws IllegalArgumentException if any of the values is outside the 
   *         specified range.
   */
  public ColorUIResource(int r, int g, int b)
  {
    super(r, g, b);
  }


  /**
   * Constructs a <code>ColorUIResource</code> using the specified
   * RGB value. The blue value is in bits 0-7, green in bits 8-15, and
   * red in bits 16-23. The other bits are ignored. The alpha value is set
   * to 255, meaning that the color is fully opaque.
   *
   * @param rgb the rgb value, as discussed above.
   */
  public ColorUIResource(int rgb)
  {
    super(rgb);
  }


  /**
   * Constructs a <code>ColorUIResource</code> using the specified
   * red, green, and blue intensities, which must be given as floats in
   * the range of 0-1. The alpha channel value will default to 1.0f,
   * meaning that the color is fully opaque.
   *
   * @param r the red intensity, which must be in the range [0.0 .. 1.0].
   * @param g the green intensity, which must be in the range [0.0 .. 1.0].
   * @param b the blue intensity, which must be in the range [0.0 .. 1.0].
   *
   * @throws IllegalArgumentException if any of the values is outside the 
   *         specified range.
   */
  public ColorUIResource(float r, float g, float b)
  {
    super(r, g, b);
  }


  /**
   * Constructs a <code>ColorUIResource</code>, using the intensities
   * of another color.
   *
   * @param c the color whose intensities will be considered when
   *        constructing this <code>ColorUIResource</code> (<code>null</code>
   *        not permitted).
   *
   * @throws NullPointerException if <code>c</code> is <code>null</code>.
   */
  public ColorUIResource(Color c)
  {
    super(c.getRGB());
  }
}
