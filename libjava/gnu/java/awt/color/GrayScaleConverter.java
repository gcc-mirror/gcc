/* GrayScaleConverter.java -- Linear grayscale conversion class
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

package gnu.java.awt.color;


/**
 * Linear Grayscale converter
 *
 * @author Sven de Marothy
 */
public class GrayScaleConverter implements ColorSpaceConverter
{
  // intensity factors (ITU Rec. BT.709)
  double[] coeff = { 0.2125f, 0.7154f, 0.0721f };

  /**
   * CIE 1931 D50 white point (in Lab coordinates)
   */
  private static float[] D50 = { 0.96422f, 1.00f, 0.82521f };

  public float[] toCIEXYZ(float[] in)
  {
    float g = in[0];
    if (g < 0)
      g = 1 + g;
    float[] out = { g * D50[0], g * D50[1], g * D50[2] }; // White spot
    return out;
  }

  public float[] toRGB(float[] in)
  {
    float[] out = new float[3];
    if (in[0] <= 0.00304f)
      out[0] = in[0] * 12.92f;
    else
      out[0] = 1.055f * ((float) Math.exp((1 / 2.4) * Math.log(in[0])))
               - 0.055f;
    out[1] = out[2] = out[0];
    return out;
  }

  public float[] fromCIEXYZ(float[] in)
  {
    float[] temp = new float[3];
    temp[0] = 3.1338f * in[0] - 1.6171f * in[1] - 0.4907f * in[2];
    temp[1] = -0.9785f * in[0] + 1.9160f * in[1] + 0.0334f * in[2];
    temp[2] = 0.0720f * in[0] - 0.2290f * in[1] + 1.4056f * in[2];
    float[] out = new float[1];
    for (int i = 0; i < 3; i++)
      out[0] = (float) (temp[i] * coeff[i]);
    return out;
  }

  public float[] fromRGB(float[] in)
  {
    float[] out = new float[1];

    // Convert non-linear RGB coordinates to linear ones,
    //  numbers from the w3 spec.
    out[0] = 0;
    for (int i = 0; i < 3; i++)
      {
	float n = in[i];
	if (n < 0)
	  n = 0f;
	if (n > 1)
	  n = 1f;
	if (n <= 0.03928f)
	  out[0] += (float) (coeff[i] * n / 12.92);
	else
	  out[0] += (float) (coeff[i] * Math.exp(2.4 * Math.log((n + 0.055) / 1.055)));
      }
    return out;
  }
}
