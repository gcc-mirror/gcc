/* ICC_ColorSpace.java -- the canonical color space implementation
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

/**
 * NEEDS DOCUMENTATION
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 * @since 1.2
 */
public class ICC_ColorSpace extends ColorSpace
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 3455889114070431483L;

  /**
   * @serial
   */
  private ICC_Profile thisProfile;

  /**
   * @serial
   */
  private float[] minVal;

  /**
   * @serial
   */
  private float[] maxVal;

  /**
   * @serial
   */
  private float[] diffMinMax;

  /**
   * @serial
   */
  private float[] invDiffMinMax;

  /**
   * @serial
   */
  private boolean needScaleInit;

  /**
   * Constructs a new ICC_ColorSpace from an ICC_Profile object.
   *
   * @exception IllegalArgumentException If profile is inappropriate for
   * representing a ColorSpace.
   */
  public ICC_ColorSpace(ICC_Profile profile)
  {
    super(CS_sRGB, profile.getNumComponents());
    thisProfile = profile;
  }

  public ICC_Profile getProfile()
  {
    return thisProfile;
  }

  /**
   * Transforms a color value assumed to be in this ColorSpace into a value in
   * the default CS_sRGB color space.
   *
   * @exception ArrayIndexOutOfBoundsException If array length is not at least
   * the number of components in this ColorSpace.
   */
  public float[] toRGB(float[] colorvalue)
  {
    if (colorvalue.length < numComponents)
      throw new IllegalArgumentException ();
      
    // FIXME: Always assumes sRGB:
    return colorvalue;
  }

  /**
   * Transforms a color value assumed to be in the default CS_sRGB color space
   * into this ColorSpace.
   *
   * @exception ArrayIndexOutOfBoundsException If array length is not at
   * least 3.
   */
  public float[] fromRGB(float[] rgbvalue)
  {
    if (rgbvalue.length < 3)
      throw new IllegalArgumentException ();
    
    // FIXME: Always assumes sRGB:
    return rgbvalue;
  }

  /**
   * Transforms a color value assumed to be in this ColorSpace into the
   * CS_CIEXYZ conversion color space.
   *
   * @exception ArrayIndexOutOfBoundsException If array length is not at
   * least the number of components in this ColorSpace.
   */
  public float[] toCIEXYZ(float[] colorvalue)
  {
    // FIXME: Not implemented
    throw new UnsupportedOperationException();
  }

  /**
   * Transforms a color value assumed to be in the CS_CIEXYZ conversion color
   * space into this ColorSpace.
   *
   * @exception ArrayIndexOutOfBoundsException If array length is not at
   * least 3.
   */
  public float[] fromCIEXYZ(float[] colorvalue)
  {
    // FIXME: Not implemented
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the minimum normalized color component value for the specified
   * component.
   *
   * @exception IllegalArgumentException If component is less than 0 or greater
   * than numComponents - 1.
   *
   * @since 1.4
   */
  public float getMinValue(int idx)
  {
    if (type == TYPE_Lab && (idx == 1 || idx == 2))
      return -128;
    if (idx < 0 || idx >= numComponents)
      throw new IllegalArgumentException();
    return 0;
  }

  /**
   * Returns the maximum normalized color component value for the specified
   * component.
   *
   * @exception IllegalArgumentException If component is less than 0 or greater
   * than numComponents - 1.
   *
   * @since 1.4
   */
  public float getMaxValue(int idx)
  {
    if (type == TYPE_XYZ && idx >= 0 && idx <= 2)
      return 1 + 32767 / 32768f;
    else if (type == TYPE_Lab)
      {
        if (idx == 0)
          return 100;
        if (idx == 1 || idx == 2)
          return 127;
      }
    if (idx < 0 || idx >= numComponents)
      throw new IllegalArgumentException();
    return 1;
  }
} // class ICC_ColorSpace
