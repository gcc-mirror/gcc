/* RgbProfileConverter.java -- RGB Profile conversion class
   Copyright (C) 2004 Free Software Foundation

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


package gnu.java.awt.color;

import java.awt.color.ICC_Profile;
import java.awt.color.ICC_ProfileRGB;
import java.awt.color.ProfileDataException;

/**
 * RgbProfileConverter - converts RGB profiles (ICC_ProfileRGB)
 *
 * This type of profile contains a matrix and three
 * tone reproduction curves (TRCs).
 *
 * Device RGB --&gt; CIE XYZ is done through first multiplying with
 * a matrix, then each component is looked-up against it's TRC.
 *
 * The opposite transform is done using the inverse of the matrix,
 * and TRC:s.
 *
 * @author Sven de Marothy
 */
public class RgbProfileConverter implements ColorSpaceConverter
{
  private float[][] matrix;
  private float[][] inv_matrix;
  private ToneReproductionCurve rTRC;
  private ToneReproductionCurve gTRC;
  private ToneReproductionCurve bTRC;
  private ColorLookUpTable toPCS;
  private ColorLookUpTable fromPCS;

  /**
   * CIE 1931 D50 white point (in Lab coordinates)
   */
  private static float[] D50 = { 0.96422f, 1.00f, 0.82521f };

  /**
   * Constructs an RgbProfileConverter from a given ICC_ProfileRGB
   */
  public RgbProfileConverter(ICC_ProfileRGB profile)
  {
    toPCS = fromPCS = null;
    matrix = profile.getMatrix();

    // get TRCs
    try
      {
        rTRC = new ToneReproductionCurve(profile.getGamma(ICC_ProfileRGB.REDCOMPONENT));
      }
    catch (ProfileDataException e)
      {
        rTRC = new ToneReproductionCurve(profile.getTRC(ICC_ProfileRGB.REDCOMPONENT));
      }
    try
      {
        gTRC = new ToneReproductionCurve(profile.getGamma(ICC_ProfileRGB.GREENCOMPONENT));
      }
    catch (ProfileDataException e)
      {
        gTRC = new ToneReproductionCurve(profile.getTRC(ICC_ProfileRGB.GREENCOMPONENT));
      }
    try
      {
        bTRC = new ToneReproductionCurve(profile.getGamma(ICC_ProfileRGB.BLUECOMPONENT));
      }
    catch (ProfileDataException e)
      {
        bTRC = new ToneReproductionCurve(profile.getTRC(ICC_ProfileRGB.BLUECOMPONENT));
      }

    // If a CLUT is available, it should be used, and the TRCs ignored.
    // Note: A valid profile may only have CLUTs in one direction, and
    // TRC:s without useful info, making reverse-transforms impossible.
    // In this case the TRC will be used for the reverse-transform with
    // unpredictable results. This is in line with the Java specification,
    try
      {
        toPCS = new ColorLookUpTable(profile, ICC_Profile.icSigAToB0Tag);
      }
    catch (Exception e)
      {
        toPCS = null;
      }

    try
      {
        fromPCS = new ColorLookUpTable(profile, ICC_Profile.icSigBToA0Tag);
      }
    catch (Exception e)
      {
        fromPCS = null;
      }

    // Calculate the inverse matrix if no reverse CLUT is available
    if(fromPCS == null)
        inv_matrix = invertMatrix(matrix);
    else
      {
        // otherwise just set it to an identity matrix
        inv_matrix = new float[3][3];
        inv_matrix[0][0] = inv_matrix[1][1] = inv_matrix[2][2] = 1.0f;
      }
  }

  public float[] toCIEXYZ(float[] in)
  {
    // CLUT takes precedence
    if (toPCS != null)
      return toPCS.lookup(in);

    float[] temp = new float[3];
    float[] out = new float[3];

    // device space --> linear gamma
    temp[0] = rTRC.lookup(in[0]);
    temp[1] = gTRC.lookup(in[1]);
    temp[2] = bTRC.lookup(in[2]);

    // matrix multiplication
    out[0] = matrix[0][0] * temp[0] + matrix[0][1] * temp[1]
             + matrix[0][2] * temp[2];
    out[1] = matrix[1][0] * temp[0] + matrix[1][1] * temp[1]
             + matrix[1][2] * temp[2];
    out[2] = matrix[2][0] * temp[0] + matrix[2][1] * temp[1]
             + matrix[2][2] * temp[2];

    return out;
  }

  public float[] toRGB(float[] in)
  {
    return SrgbConverter.XYZtoRGB(toCIEXYZ(in));
  }

  public float[] fromCIEXYZ(float[] in)
  {
    if (fromPCS != null)
      return fromPCS.lookup(in);

    float[] temp = new float[3];
    float[] out = new float[3];

    // matrix multiplication
    temp[0] = inv_matrix[0][0] * in[0] + inv_matrix[0][1] * in[1]
              + inv_matrix[0][2] * in[2];
    temp[1] = inv_matrix[1][0] * in[0] + inv_matrix[1][1] * in[1]
              + inv_matrix[1][2] * in[2];
    temp[2] = inv_matrix[2][0] * in[0] + inv_matrix[2][1] * in[1]
              + inv_matrix[2][2] * in[2];

    // device space --> linear gamma
    out[0] = rTRC.reverseLookup(temp[0]);
    out[1] = gTRC.reverseLookup(temp[1]);
    out[2] = bTRC.reverseLookup(temp[2]);

    // FIXME: Sun appears to clip the return values to [0,1]
    // I don't believe that is a Good Thing,
    // (some colorspaces may allow values outside that range.)
    // So we return the actual values here.
    return out;
  }

  public float[] fromRGB(float[] in)
  {
    return fromCIEXYZ(SrgbConverter.RGBtoXYZ(in));
  }

  /**
   * Inverts a 3x3 matrix, returns the inverse,
   * throws an IllegalArgumentException if the matrix is not
   * invertible (this shouldn't happen for a valid profile)
   */
  private float[][] invertMatrix(float[][] matrix)
  {
    float[][] out = new float[3][3];
    double determinant = matrix[0][0] * (matrix[1][1] * matrix[2][2]
                         - matrix[2][1] * matrix[1][2])
                         - matrix[0][1] * (matrix[1][0] * matrix[2][2]
                         - matrix[2][0] * matrix[1][2])
                         + matrix[0][2] * (matrix[1][0] * matrix[2][1]
                         - matrix[2][0] * matrix[1][1]);

    if (determinant == 0.0)
      throw new IllegalArgumentException("Can't invert conversion matrix.");
    float invdet = (float) (1.0 / determinant);

    out[0][0] = invdet * (matrix[1][1] * matrix[2][2]
                - matrix[1][2] * matrix[2][1]);
    out[0][1] = invdet * (matrix[0][2] * matrix[2][1]
                - matrix[0][1] * matrix[2][2]);
    out[0][2] = invdet * (matrix[0][1] * matrix[1][2]
                - matrix[0][2] * matrix[1][1]);
    out[1][0] = invdet * (matrix[1][2] * matrix[2][0]
                - matrix[1][0] * matrix[2][2]);
    out[1][1] = invdet * (matrix[0][0] * matrix[2][2]
                - matrix[0][2] * matrix[2][0]);
    out[1][2] = invdet * (matrix[0][2] * matrix[1][0]
                - matrix[0][0] * matrix[1][2]);
    out[2][0] = invdet * (matrix[1][0] * matrix[2][1]
                - matrix[1][1] * matrix[2][0]);
    out[2][1] = invdet * (matrix[0][1] * matrix[2][0]
                - matrix[0][0] * matrix[2][1]);
    out[2][2] = invdet * (matrix[0][0] * matrix[1][1]
                - matrix[0][1] * matrix[1][0]);
    return out;
  }
}
