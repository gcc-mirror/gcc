/* ToneReproductionCurve.java -- Representation of an ICC 'curv' type TRC
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
   exception statement from your version.
*/

package gnu.java.awt.color;


/**
 * ToneReproductionCurve - TRCs are used to describe RGB
 * and Grayscale profiles. The TRC is essentially the gamma
 * function of the color space.
 *
 * For example, Apple RGB has a gamma of 1.8, most monitors are ~2.2,
 * sRGB is 2.4 with a small linear part near 0.
 * Linear spaces are of course 1.0.
 * (The exact function is implemented in SrgbConverter)
 *
 * The ICC specification allows the TRC to be described as a single
 * Gamma value, where the function is thus out = in**gamma.
 * Alternatively, the gamma function may be represented by a lookup table
 * of values, in which case linear interpolation is used.
 *
 * @author Sven de Marothy
 */
public class ToneReproductionCurve
{
  private float[] trc;
  private float gamma;
  private float[] reverseTrc;

  /**
   * Constructs a TRC from a gamma values
   */
  public ToneReproductionCurve(float gamma)
  {
    trc = null;
    reverseTrc = null;
    this.gamma = gamma;
  }

  /**
   * Constructs a TRC from a set of float values
   */
  public ToneReproductionCurve(float[] trcValues)
  {
    trc = new float[trcValues.length];
    System.arraycopy(trcValues, 0, trc, 0, trcValues.length);
    setupReverseTrc();
  }

  /**
   * Constructs a TRC from a set of short values normalized to
   * the 0-65535 range (as in the ICC profile file).
   * (Note the values are treated as unsigned)
   */
  public ToneReproductionCurve(short[] trcValues)
  {
    trc = new float[trcValues.length];
    for (int i = 0; i < trcValues.length; i++)
      trc[i] = (float) ((int) trcValues[i] & (0xFFFF)) / 65535.0f;
    setupReverseTrc();
  }

  /**
   * Performs a TRC lookup
   */
  public float lookup(float in)
  {
    float out;

    if (trc == null)
      {
	if (in == 0f)
	  return 0.0f;
	return (float) Math.exp(gamma * Math.log(in));
      }
    else
      {
	double alpha = in * (trc.length - 1);
	int index = (int) Math.floor(alpha);
	alpha = alpha - (double) index;
	if (index >= trc.length - 1)
	  return trc[trc.length - 1];
	if (index <= 0)
	  return trc[0];
	out = (float) (trc[index] * (1.0 - alpha) + trc[index + 1] * alpha);
      }
    return out;
  }

  /**
   * Performs an reverse lookup
   */
  public float reverseLookup(float in)
  {
    float out;

    if (trc == null)
      {
	if (in == 0f)
	  return 0.0f;
	return (float) Math.exp((1.0 / gamma) * Math.log(in));
      }
    else
      {
	double alpha = in * (reverseTrc.length - 1);
	int index = (int) Math.floor(alpha);
	alpha = alpha - (double) index;
	if (index >= reverseTrc.length - 1)
	  return reverseTrc[reverseTrc.length - 1];
	if (index <= 0)
	  return reverseTrc[0];
	out = (float) (reverseTrc[index] * (1.0 - alpha)
	      + reverseTrc[index + 1] * alpha);
      }
    return out;
  }

  /**
   * Calculates a reverse-lookup table.
   * We use a whopping 10,000 entries.. This is should be more than any
   * real-life TRC table (typically around 256-1024) so we won't be losing
   * any precision.
   *
   * This will of course generate completely invalid results if the curve
   * is not monotonic and invertable. But what's the alternative?
   */
  public void setupReverseTrc()
  {
    reverseTrc = new float[10000];
    int j = 0;
    for (int i = 0; i < 10000; i++)
      {
	float n = ((float) i) / 10000f;
	while (trc[j + 1] < n && j < trc.length - 2)
	  j++;

	if (j == trc.length - 2)
	  reverseTrc[i] = trc[trc.length - 1];
	else
	  reverseTrc[i] = (j + (n - trc[j]) / (trc[j + 1] - trc[j])) / ((float) trc.length);
      }
  }
}
