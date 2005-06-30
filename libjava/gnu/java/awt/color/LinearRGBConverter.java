/* LinearRGBConverter.java -- conversion to a linear RGB color space
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


/**
 * LinearRGBConverter - conversion routines for a linear sRGB colorspace
 * sRGB is a standard for RGB colorspaces, adopted by the w3c.
 *
 * The specification is available at:
 * http://www.w3.org/Graphics/Color/sRGB.html
 *
 * @author Sven de Marothy
 */
public class LinearRGBConverter implements ColorSpaceConverter
{
  /**
   * linear RGB --> sRGB
   * Use the inverse gamma curve
   */
  public float[] toRGB(float[] in)
  {
    float[] out = new float[3];
    for (int i = 0; i < 3; i++)
      {
	float n = in[i];
	if (n < 0)
	  n = 0f;
	if (n > 1)
	  n = 1f;
	if (n <= 0.00304f)
	  out[i] = in[0] * 12.92f;
	else
	  out[i] = 1.055f * ((float) Math.exp((1 / 2.4) * Math.log(n)))
	           - 0.055f;
      }
    return out;
  }

  /**
   * sRGB --> linear RGB
   * Use the gamma curve (gamma=2.4 in sRGB)
   */
  public float[] fromRGB(float[] in)
  {
    float[] out = new float[3];

    // Convert non-linear RGB coordinates to linear ones,
    //  numbers from the w3 spec.
    for (int i = 0; i < 3; i++)
      {
	float n = in[i];
	if (n < 0)
	  n = 0f;
	if (n > 1)
	  n = 1f;
	if (n <= 0.03928f)
	  out[i] = (float) (n / 12.92);
	else
	  out[i] = (float) (Math.exp(2.4 * Math.log((n + 0.055) / 1.055)));
      }
    return out;
  }

  /**
   * Linear RGB --> CIE XYZ (D50 relative)
   * This is a simple matrix transform, the matrix (relative D65)
   * is given in the sRGB spec. This has been combined with a
   * linear Bradford transform for the D65-->D50 mapping, resulting
   * in a single matrix which does the whole thing.
   *
   */
  public float[] fromCIEXYZ(float[] in)
  {
    /*
     * Note: The numbers which were used to calculate this only had four
     * digits of accuracy. So don't be fooled by the number of digits here.
     * If someone has more accurate source, feel free to update this.
     */
    float[] out = new float[3];
    out[0] = (float) (3.13383065124221 * in[0] - 1.61711949411313 * in[1]
             - 0.49071914111101 * in[2]);
    out[1] = (float) (-0.97847026691142 * in[0] + 1.91597856031996 * in[1]
             + 0.03340430640699 * in[2]);
    out[2] = (float) (0.07203679486279 * in[0] - 0.22903073553113 * in[1]
             + 1.40557835776234 * in[2]);
    if (out[0] < 0)
      out[0] = 0f;
    if (out[1] < 0)
      out[1] = 0f;
    if (out[2] < 0)
      out[2] = 0f;
    if (out[0] > 1.0f)
      out[0] = 1.0f;
    if (out[1] > 1.0f)
      out[1] = 1.0f;
    if (out[2] > 1.0f)
      out[2] = 1.0f;
    return out;
  }

  /**
   * Linear RGB --> CIE XYZ (D50 relative)
   * Uses the inverse of the above matrix.
   */
  public float[] toCIEXYZ(float[] in)
  {
    float[] out = new float[3];
    out[0] = (float) (0.43606375022190 * in[0] + 0.38514960146481 * in[1]
             + 0.14308641888799 * in[2]);
    out[1] = (float) (0.22245089403542 * in[0] + 0.71692584775182 * in[1]
             + 0.06062451125578 * in[2]);
    out[2] = (float) (0.01389851860679 * in[0] + 0.09707969011198 * in[1]
             + 0.71399604572506 * in[2]);
    return out;
  }
}
