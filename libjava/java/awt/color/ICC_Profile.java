/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.color;

// Currently just a stub.

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class ICC_Profile
{
  long profileID; // why long?
  
  ICC_Profile(long profileID)
  {
    this.profileID = profileID;
  }

  public int getNumComponents()
  {
    switch ((int) profileID)
      {
      case ColorSpace.CS_sRGB:
      case ColorSpace.CS_LINEAR_RGB:
      case ColorSpace.CS_CIEXYZ:
	return 3;
      case ColorSpace.CS_GRAY:
	return 1;
      case ColorSpace.CS_PYCC:    // have no clue about this one
      default:
	throw new UnsupportedOperationException("profile not implemented");
      }
  }
}
