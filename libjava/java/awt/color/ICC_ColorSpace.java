/* Copyright (C) 2000, 2002  Free Software Foundation

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.awt.color;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class ICC_ColorSpace extends ColorSpace
{
  private ICC_Profile profile;

  public ICC_ColorSpace(ICC_Profile profile)
  {
    super(CS_sRGB, profile.getNumComponents());
    
    this.profile = profile;
  }

  public ICC_Profile getProfile()
  {
    return profile;
  }

  public float[] toRGB(float[] colorvalue)
  {
    // FIXME: Always assumes sRGB:
    return colorvalue;
  }

  public float[] fromRGB(float[] rgbvalue)
  {
    // FIXME: Always assumes sRGB:
    return rgbvalue;
  }

  public float[] toCIEXYZ(float[] colorvalue)
  {
    // FIXME: Not implemented
    throw new UnsupportedOperationException();
  }

  public float[] fromCIEXYZ(float[] colorvalue)
  {
    // FIXME: Not implemented
    throw new UnsupportedOperationException();
  }
}
