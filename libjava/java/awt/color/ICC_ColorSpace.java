/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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
