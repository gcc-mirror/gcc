/* ICC_ProfileRGB.java -- the ICC profile for a RGB colorspace
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

/**
 * ICC_ProfileRGB - a special case of ICC_Profiles.
 *
 * The ICC_Profile.getInstance() method will return an instance of the 
 * ICC_ProfileRGB subclass when all the following conditions are met:
 * The device color space of the profile is TYPE_RGB.
 * The profile contains red, green and blue ColorantTags.
 * The profile contains red, green and blue TRCTags.
 * The profile contains a mediaWhitePointTag included.
 *
 * As per the ICC specification, the color space conversion can then
 * be done through the following method:
 * linearR = redTRC[deviceR]
 * linearG = greenTRC[deviceG]
 * linearB = blueTRC[deviceB]
 * TRC curves are either a single gamma value, or a 1-dimensional lookup table.
 * 
 * Followed by the matrix transform:
 * PCS = M*linear
 *
 * Where PCS is the vector of profile color space (must be XYZ) coordinates,
 * linear is the vector of linear RGB coordinates, and the matrix M is 
 * constructed from the ColorantTags, where the columns are red, green and
 * blue respectively, and the rows are X, Y and Z.
 *
 * Note that if the profile contains a CLUT for the color space conversion,
 * it should be used instead, and the TRC information ignored. 
 *
 * @author Sven de Marothy
 * @since 1.2
 */
public class ICC_ProfileRGB extends ICC_Profile
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 8505067385152579334L;

  public static final int REDCOMPONENT = 0;

  public static final int GREENCOMPONENT = 1;

  public static final int BLUECOMPONENT = 2;

  private transient float[][] matrix;

  private transient float[] gamma;

  private transient float[] whitePoint;


  /**
   * Package-private constructor used by ICC_ColorSpace for creating an
   * ICC_ProfileRGB from a predefined ColorSpace (CS_LINEAR_RGB and CS_sRGB)
   */
  ICC_ProfileRGB(int cspace)
  {
    super(cspace);
    matrix = createMatrix();
    whitePoint = getXYZData(icSigMediaWhitePointTag);
  }

  /**
   * Package-private constructor used by ICC_ColorSpace for creating an
   * ICC_ProfileRGB from profile data.
   */
  ICC_ProfileRGB(byte[] data)
  {
    super(data);
    matrix = createMatrix();
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
   * Returns the colorant matrix of the conversion.
   */
  public float[][] getMatrix()
  {
    float[][] mat = new float[3][3];
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
	mat[i][j] = matrix[i][j];
    return mat;
  }

  /**
   * Returns the gamma value of a component
   * @throws ProfileDataException if the TRC is described by a lookup
   * table and not a gamma value.
   */
  public float getGamma(int component)
  {
    short[] data;
    switch (component)
      {
      case REDCOMPONENT:
	data = getCurve(icSigRedTRCTag);
	break;
      case GREENCOMPONENT:
	data = getCurve(icSigGreenTRCTag);
	break;
      case BLUECOMPONENT:
	data = getCurve(icSigBlueTRCTag);
	break;
      default:
	throw new IllegalArgumentException("Not a valid component");
      }
    if (data == null)
      throw new IllegalArgumentException("Error reading TRC");

    if (data.length != 1)
      throw new ProfileDataException("Not a single-gamma TRC");

    // convert the unsigned 7.8 fixed-point gamma to a float.
    float gamma = (float) (((int) data[0] & 0xFF00) >> 8);
    double fraction = ((int) data[0] & 0x00FF) / 256.0;
    gamma += (float) fraction;
    return gamma;
  }

  /**
   * Returns the TRC lookup table for a component
   * @throws ProfileDataException if the TRC is described by a gamma
   * value and not a lookup table.
   */
  public short[] getTRC(int component)
  {
    short[] data;
    switch (component)
      {
      case REDCOMPONENT:
	data = getCurve(icSigRedTRCTag);
	break;
      case GREENCOMPONENT:
	data = getCurve(icSigGreenTRCTag);
	break;
      case BLUECOMPONENT:
	data = getCurve(icSigBlueTRCTag);
	break;
      default:
	throw new IllegalArgumentException("Not a valid component");
      }
    if (data == null)
      throw new IllegalArgumentException("Error reading TRC");

    if (data.length <= 1)
      throw new ProfileDataException("Gamma value, not a TRC table.");

    return data;
  }

  /**
   * Creates the colorspace conversion matrix from the RGB tristimulus
   * values.
   */
  private float[][] createMatrix() throws IllegalArgumentException
  {
    float[][] mat = new float[3][3];
    float[] r;
    float[] g;
    float[] b;
    r = getXYZData(icSigRedColorantTag);
    g = getXYZData(icSigGreenColorantTag);
    b = getXYZData(icSigBlueColorantTag);
    if (r == null || g == null || b == null)
      throw new IllegalArgumentException("Error reading colorant tags!");
    for (int i = 0; i < 3; i++)
      {
	mat[i][0] = r[i];
	mat[i][1] = g[i];
	mat[i][2] = b[i];
      }
    return mat;
  }
} // class ICC_ProfileRGB
