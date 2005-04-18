/* SrgbConverter.java -- sRGB conversion class
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
 * SrgbConverter - conversion routines for the sRGB colorspace
 * sRGB is a standard for RGB colorspaces, adopted by the w3c.
 *
 * The specification is available at:
 * http://www.w3.org/Graphics/Color/sRGB.html
 *
 * @author Sven de Marothy
 */
/**
 *
 * Note the matrix numbers used here are NOT identical to those in the
 * w3 spec, as those numbers are CIE XYZ relative a D65 white point.
 * The CIE XYZ we use is relative a D50 white point, so therefore a
 * linear Bradford transform matrix for D65->D50 mapping has been applied.
 * (The ICC documents describe this transform)
 *
 *   Linearized Bradford transform:
 *    0.8951    0.2664   -0.1614
 *   -0.7502    1.7135    0.0367
 *    0.0389   -0.0685    1.0296
 *
 *   Inverse:
 *   0.9870   -0.1471    0.1600
 *   0.4323    0.5184    0.0493
 *  -0.00853   0.0400    0.9685
 */
public class SrgbConverter implements ColorSpaceConverter
{
  public float[] fromCIEXYZ(float[] in)
  {
    return XYZtoRGB(in);
  }

  public float[] toCIEXYZ(float[] in)
  {
    return RGBtoXYZ(in);
  }

  public float[] toRGB(float[] in)
  {
    float[] out = new float[3];
    System.arraycopy(in, 0, out, 0, 3);
    return out;
  }

  public float[] fromRGB(float[] in)
  {
    float[] out = new float[3];
    System.arraycopy(in, 0, out, 0, 3);
    return out;
  }

  /**
   * CIE XYZ (D50 relative) --> sRGB
   *
   * Static as it's used by other ColorSpaceConverters to
   * convert to sRGB if the color space is defined in XYZ.
   */
  public static float[] XYZtoRGB(float[] in)
  {
    float[] temp = new float[3];
    temp[0] = 3.1338f * in[0] - 1.6171f * in[1] - 0.4907f * in[2];
    temp[1] = -0.9785f * in[0] + 1.9160f * in[1] + 0.0334f * in[2];
    temp[2] = 0.0720f * in[0] - 0.2290f * in[1] + 1.4056f * in[2];

    float[] out = new float[3];
    for (int i = 0; i < 3; i++)
      {
	if (temp[i] < 0)
	  temp[i] = 0.0f;
	if (temp[i] > 1)
	  temp[i] = 1.0f;
	if (temp[i] <= 0.00304f)
	  out[i] = temp[i] * 12.92f;
	else
	  out[i] = 1.055f * ((float) Math.exp((1 / 2.4) * Math.log(temp[i])))
	           - 0.055f;
      }
    return out;
  }

  /**
   * sRGB --> CIE XYZ (D50 relative)
   *
   * Static as it's used by other ColorSpaceConverters to
   * convert to XYZ if the color space is defined in RGB.
   */
  public static float[] RGBtoXYZ(float[] in)
  {
    float[] temp = new float[3];
    float[] out = new float[3];
    for (int i = 0; i < 3; i++)
      if (in[i] <= 0.03928f)
	temp[i] = in[i] / 12.92f;
      else
	temp[i] = (float) Math.exp(2.4 * Math.log((in[i] + 0.055) / 1.055));

    /*
     * Note: The numbers which were used to calculate this only had four
     * digits of accuracy. So don't be fooled by the number of digits here.
     * If someone has more accurate source, feel free to update this.
     */
    out[0] = (float) (0.436063750222 * temp[0] + 0.385149601465 * temp[1]
             + 0.143086418888 * temp[2]);
    out[1] = (float) (0.222450894035 * temp[0] + 0.71692584775 * temp[1]
             + 0.060624511256 * temp[2]);
    out[2] = (float) (0.0138985186 * temp[0] + 0.097079690112 * temp[1]
             + 0.713996045725 * temp[2]);
    return out;
  }
}
