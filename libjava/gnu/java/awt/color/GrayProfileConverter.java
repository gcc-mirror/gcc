/* GrayProfileConverter.java -- Gray profile conversion class
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

import java.awt.color.ICC_Profile;
import java.awt.color.ICC_ProfileGray;
import java.awt.color.ProfileDataException;

/**
 * GrayProfileConverter - converts Grayscale profiles (ICC_ProfileGray)
 *
 * This type of profile contains a single tone reproduction curve (TRC).
 * Conversion consists of simple TRC lookup.
 *
 * This implementation is very lazy and does everything applying the TRC and
 * utilizing the built-in linear grayscale color space.
 *
 * @author Sven de Marothy
 */
public class GrayProfileConverter implements ColorSpaceConverter
{
  private GrayScaleConverter gc;
  private ToneReproductionCurve trc;
  private ColorLookUpTable toPCS;
  private ColorLookUpTable fromPCS;

  /**
   * Constructs the converter described by an ICC_ProfileGray object
   */
  public GrayProfileConverter(ICC_ProfileGray profile)
  {
    try
      {
	trc = new ToneReproductionCurve(profile.getGamma());
      }
    catch (ProfileDataException e)
      {
	trc = new ToneReproductionCurve(profile.getTRC());
      }

    // linear grayscale converter
    gc = new GrayScaleConverter();

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
  }

  public float[] toCIEXYZ(float[] in)
  {
    if (toPCS != null)
      return toPCS.lookup(in);
    float[] gray = new float[1];
    gray[0] = trc.lookup(in[0]);
    return gc.toCIEXYZ(gray);
  }

  public float[] toRGB(float[] in)
  {
    float[] gray = new float[1];
    gray[0] = trc.lookup(in[0]);
    return gc.toRGB(gray);
  }

  public float[] fromRGB(float[] in)
  {
    // get linear grayscale value
    float[] gray = gc.fromRGB(in);
    gray[0] = trc.reverseLookup(gray[0]);
    return gray;
  }

  public float[] fromCIEXYZ(float[] in)
  {
    if (fromPCS != null)
      return fromPCS.lookup(in);

    float[] gray = gc.fromCIEXYZ(in);
    gray[0] = trc.reverseLookup(gray[0]);
    return gray;
  }
}
