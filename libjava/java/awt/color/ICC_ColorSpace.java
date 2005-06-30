/* ICC_ColorSpace.java -- the canonical color space implementation
   Copyright (C) 2000, 2002, 2004 Free Software Foundation

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


package java.awt.color;

import gnu.java.awt.color.CieXyzConverter;
import gnu.java.awt.color.ClutProfileConverter;
import gnu.java.awt.color.ColorSpaceConverter;
import gnu.java.awt.color.GrayProfileConverter;
import gnu.java.awt.color.GrayScaleConverter;
import gnu.java.awt.color.LinearRGBConverter;
import gnu.java.awt.color.PyccConverter;
import gnu.java.awt.color.RgbProfileConverter;
import gnu.java.awt.color.SrgbConverter;

import java.io.IOException;
import java.io.ObjectInputStream;

/**
 * ICC_ColorSpace - an implementation of ColorSpace
 *
 * While an ICC_Profile class abstracts the data in an ICC profile file
 * an ICC_ColorSpace performs the color space conversions defined by
 * an ICC_Profile instance.
 *
 * Typically, an ICC_Profile will either be created using getInstance,
 * either from the built-in colorspaces, or from an ICC profile file.
 * Then a ICC_Colorspace will be used to perform transforms from the
 * device colorspace to and from the profile color space.
 *
 * The PCS used by ColorSpace is CIE XYZ relative a D50 white point.
 * (Profiles using a CIE Lab PCS will have their input and output converted
 * to D50 CIE XYZ accordingly.
 *
 * Note that a valid profile may not contain transforms in both directions,
 * in which case the output may be undefined.
 * All built-in colorspaces have bidirectional transforms, but developers
 * using an ICC profile file may want to check the profile class using
 * the ICC_Profile.getProfileClass() method. Input class profiles are
 * guaranteed to have transforms to the PCS, output class profiles are
 * guaranteed to have transforms from the PCS to device space.
 *
 * @author Sven de Marothy
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
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
   * Tells us if the PCS is CIE LAB (must be CIEXYZ otherwise)
   */
  private transient int type;
  private transient int nComponents;
  private transient ColorSpaceConverter converter;

  /**
   * Constructs a new ICC_ColorSpace from an ICC_Profile object.
   *
   * @exception IllegalArgumentException If profile is inappropriate for
   * representing a ColorSpace.
   */
  public ICC_ColorSpace(ICC_Profile profile)
  {
    super(profile.getColorSpaceType(), profile.getNumComponents());

    converter = getConverter(profile);
    thisProfile = profile;
    nComponents = profile.getNumComponents();
    type = profile.getColorSpaceType();
    makeArrays();
  }

  /**
   * Return the profile
   */
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
    return converter.toRGB(colorvalue);
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
    return converter.fromRGB(rgbvalue);
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
    return converter.toCIEXYZ(colorvalue);
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
    return converter.fromCIEXYZ(colorvalue);
  }

  public boolean isCS_sRGB()
  {
    return converter instanceof SrgbConverter;
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
    // FIXME: Not 100% certain of this. 
    if (type == ColorSpace.TYPE_Lab && (idx == 1 || idx == 2))
      return -128f;

    if (idx < 0 || idx >= nComponents)
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
    if (type == ColorSpace.TYPE_XYZ && idx >= 0 && idx <= 2)
      return 1 + 32767 / 32768f;
    else if (type == ColorSpace.TYPE_Lab)
      {
	if (idx == 0)
	  return 100;
	if (idx == 1 || idx == 2)
	  return 127;
      }
    if (idx < 0 || idx >= nComponents)
      throw new IllegalArgumentException();
    return 1;
  }

  /**
   * Returns a colorspace converter suitable for a given profile
   */
  private ColorSpaceConverter getConverter(ICC_Profile profile)
  {
    ColorSpaceConverter converter;
    switch (profile.isPredefined())
      {
      case CS_sRGB:
	converter = new SrgbConverter();
	break;
      case CS_CIEXYZ:
	converter = new CieXyzConverter();
	break;
      case CS_GRAY:
	converter = new GrayScaleConverter();
	break;
      case CS_LINEAR_RGB:
	converter = new LinearRGBConverter();
	break;
      case CS_PYCC:
	converter = new PyccConverter();
	break;
      default:
	if (profile instanceof ICC_ProfileRGB)
	  converter = new RgbProfileConverter((ICC_ProfileRGB) profile);
	else if (profile instanceof ICC_ProfileGray)
	  converter = new GrayProfileConverter((ICC_ProfileGray) profile);
	else
	  converter = new ClutProfileConverter(profile);
	break;
      }
    return converter;
  }

  /**
   * Serialization compatibility requires these variable to be set,
   * although we don't use them. Perhaps we should?
   */
  private void makeArrays()
  {
    minVal = new float[nComponents];
    maxVal = new float[nComponents];

    invDiffMinMax = diffMinMax = null;
    for (int i = 0; i < nComponents; i++)
      {
	minVal[i] = getMinValue(i);
	maxVal[i] = getMaxValue(i);
      }
    needScaleInit = true;
  }

  /**
   * Deserializes the object
   */
  private void readObject(ObjectInputStream s)
                   throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    // set up objects
    converter = getConverter(thisProfile);
    nComponents = thisProfile.getNumComponents();
    type = thisProfile.getColorSpaceType();
  }
} // class ICC_ColorSpace
