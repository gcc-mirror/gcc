/* ICC_ProfileGray.java -- the ICC profile for a Gray colorspace
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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
 * ICC_ProfileGray - a special case of ICC_Profiles.
 *
 * The ICC_Profile.getInstance() method will return an instance of the 
 * ICC_ProfileGray subclass when all the following conditions are met:
 * The device color space of the profile is TYPE_GRAY.
 * The profile contains a gray TRCTag.
 * The profile contains a mediaWhitePointTag.
 *
 * As per the ICC specification, the color space conversion can then
 * be done through the following method:
 * linearGray = grayTRC[deviceGray]
 *
 * Note that if the profile contains a CLUT for the color space conversion,
 * it should be used instead, and the TRC information ignored. 
 *
 * @author Sven de Marothy
 * @since 1.2
 */
public class ICC_ProfileGray extends ICC_Profile
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = -1124721290732002649L;
  private transient float[] whitePoint;

  /**
   * Package-private constructor used by ICC_ColorSpace for creating an
   * ICC_ProfileGray from a predefined ColorSpace (CS_GRAY)
   */
  ICC_ProfileGray(int cspace)
  {
    super(cspace);
    whitePoint = getXYZData(icSigMediaWhitePointTag);
  }

  /**
   * Package-private constructor used by ICC_ColorSpace for creating an
   * ICC_ProfileGray from profile data.
   */
  ICC_ProfileGray(byte[] data)
  {
    super(data);
    whitePoint = getXYZData(icSigMediaWhitePointTag);
  }


  /**
   * Returns the media white point of the profile.
   */
  public float[] getMediaWhitePoint()
  {
    float[] wp = new float[3];
    wp[0] = whitePoint[0];
    wp[1] = whitePoint[1];
    wp[2] = whitePoint[2];
    return wp;
  }

  /**
   * Returns the TRC gamma value.
   * @throws ProfileDataException if the TRC is described by a lookup
   * table and not a gamma value.
   */
  public float getGamma()
  {
    short[] data = getCurve(icSigGrayTRCTag);
    if (data == null)
      throw new IllegalArgumentException("Couldn't read Gray TRC data.");
    if (data.length != 1)
      throw new ProfileDataException("TRC is a table, not a gamma value.");

    // convert the unsigned 7.8 fixed-point gamma to a float.
    double gamma = (double) (data[0] & (0xFFFF)) / 256.0;
    return (float) gamma;
  }

  /**
   * Returns the TRC lookup table.
   * @throws ProfileDataException if the TRC is described by a gamma value
   * and not a lookup table.
   */
  public short[] getTRC()
  {
    short[] data = getCurve(icSigGrayTRCTag);
    if (data == null)
      throw new IllegalArgumentException("Couldn't read Gray TRC data.");
    if (data.length <= 1)
      throw new ProfileDataException("Gamma value, not a TRC table.");
    return data;
  }
} // class ICC_ProfileGray
