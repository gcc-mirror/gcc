/* ColorSpace.java -- transforms between color spaces
   Copyright (C) 2000, 2002 Free Software Foundation

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


package java.awt.color;

import java.io.Serializable;

/**
 * NEEDS DOCUMENTATION
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 * @since 1.2
 */
public abstract class ColorSpace implements Serializable
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = -409452704308689724L;

  public static final int TYPE_XYZ = 0;
  public static final int TYPE_Lab = 1;
  public static final int TYPE_Luv = 2;
  public static final int TYPE_YCbCr = 3;
  public static final int TYPE_Yxy = 4;
  public static final int TYPE_RGB = 5;
  public static final int TYPE_GRAY = 6;
  public static final int TYPE_HSV = 7;
  public static final int TYPE_HLS = 8;
  public static final int TYPE_CMYK = 9;
  // mysterious gap in the enumeration sequenece
  public static final int TYPE_CMY = 11;
  public static final int TYPE_2CLR = 12;
  public static final int TYPE_3CLR = 13;
  public static final int TYPE_4CLR = 14;
  public static final int TYPE_5CLR = 15;
  public static final int TYPE_6CLR = 16;
  public static final int TYPE_7CLR = 17;
  public static final int TYPE_8CLR = 18;
  public static final int TYPE_9CLR = 19;
  public static final int TYPE_ACLR = 20;
  public static final int TYPE_BCLR = 21;
  public static final int TYPE_CCLR = 22;
  public static final int TYPE_DCLR = 23;
  public static final int TYPE_ECLR = 24;
  public static final int TYPE_FCLR = 25;

  public static final int CS_sRGB = 1000;
  public static final int CS_LINEAR_RGB = 1004;
  public static final int CS_CIEXYZ = 1001;
  public static final int CS_PYCC = 1002;
  public static final int CS_GRAY = 1003;

  private static final int CS_BASE = CS_sRGB;
  private static final int CS_END = CS_LINEAR_RGB + 1;
  private static final int CS_COUNT = CS_END - CS_BASE;

  // Instances are lazily instantiated
  private static final ColorSpace[] INSTANCES = new ColorSpace[CS_COUNT];

  /**
   * @serial
   */
  // Visible in subclass.
  final int type;

  /**
   * @serial
   */
  // Visible in subclass.
  final int numComponents;

  protected ColorSpace(int type, int numcomponents)
  {
    this.type = type;
    numComponents = numcomponents;
  }

  public static ColorSpace getInstance(int colorspace)
  {
    if ((colorspace >= CS_BASE) && (colorspace < CS_END))
      {
        int instanceIndex = colorspace - CS_BASE;
        if (INSTANCES[instanceIndex] == null)
          {
            ICC_Profile profile = new ICC_Profile(colorspace);
            INSTANCES[instanceIndex] = new ICC_ColorSpace(profile);
          }
        return INSTANCES[instanceIndex];
      }
    throw new IllegalArgumentException("unknown/unsupported colorspace");
  }

  public boolean isCS_sRGB()
  {
    return false;
  }

  /**
   * Transforms a color value assumed to be in this ColorSpace into a value in
   * the default CS_sRGB color space.
   *
   * @exception ArrayIndexOutOfBoundsException If array length is not at least
   * the number of components in this ColorSpace.
   */
  public abstract float[] toRGB(float[] colorvalue);

  public abstract float[] fromRGB(float[] rgbvalue);

  public abstract float[] toCIEXYZ(float[] colorvalue);

  public abstract float[] fromCIEXYZ(float[] colorvalue);

  public int getType()
  {
    return type;
  }

  public int getNumComponents()
  {
    return numComponents;
  }

  public String getName(int idx)
  {
    return "type " + type;
  }

  /**
   * @since 1.4
   */
  public float getMinValue(int idx)
  {
    if (idx < 0 || idx >= numComponents)
      throw new IllegalArgumentException();
    return 0;
  }

  /**
   * @since 1.4
   */
  public float getMaxValue(int idx)
  {
    if (idx < 0 || idx >= numComponents)
      throw new IllegalArgumentException();
    return 1;
  }
} // class ColorSpace
