/* ClutProfileConverter.java -- Conversion routines for CLUT-Based profiles
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


/**
 * ClutProfileConverter - conversions through a CLUT-based profile
 *
 * @author Sven de Marothy
 */
public class ClutProfileConverter implements ColorSpaceConverter
{
  private ColorLookUpTable toPCS;
  private ColorLookUpTable fromPCS;
  private int nChannels;

  public ClutProfileConverter(ICC_Profile profile)
  {
    nChannels = profile.getNumComponents();

    // Sun does not specifiy which rendering intent should be used,
    // neither does the ICC v2 spec really.
    // Try intent 0
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

    if (toPCS != null || fromPCS != null)
      return;

    // If no intent 0 clut is available, look for a intent 1 clut.
    try
      {
        toPCS = new ColorLookUpTable(profile, ICC_Profile.icSigAToB1Tag);
      }
    catch (Exception e)
      {
        toPCS = null;
      }

    try
      {
        fromPCS = new ColorLookUpTable(profile, ICC_Profile.icSigBToA1Tag);
      }
    catch (Exception e)
      {
        fromPCS = null;
      }

    if (toPCS != null || fromPCS != null)
      return;

    // Last shot.. intent 2 CLUT.
    try
      {
        toPCS = new ColorLookUpTable(profile, ICC_Profile.icSigAToB2Tag);
      }
    catch (Exception e)
      {
        toPCS = null;
      }

    try
      {
        fromPCS = new ColorLookUpTable(profile, ICC_Profile.icSigBToA2Tag);
      }
    catch (Exception e)
      {
        fromPCS = null;
      }

    if (toPCS == null && fromPCS == null)
      throw new IllegalArgumentException("No CLUTs in profile!");
  }

  public float[] toCIEXYZ(float[] in)
  {
    if (toPCS != null)
      return toPCS.lookup(in);
    else
      return new float[3];
  }

  public float[] toRGB(float[] in)
  {
    return SrgbConverter.XYZtoRGB(toCIEXYZ(in));
  }

  public float[] fromCIEXYZ(float[] in)
  {
    if (fromPCS != null)
      return fromPCS.lookup(in);
    else
      return new float[nChannels];
  }

  public float[] fromRGB(float[] in)
  {
    return fromCIEXYZ(SrgbConverter.RGBtoXYZ(in));
  }
}
