/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Status: Incomplete. Needs a Locale lookup table. */

package java.awt;

import java.util.Locale;
import java.util.ResourceBundle;

public final class ComponentOrientation implements java.io.Serializable
{
  // Here is a wild guess.
  private static int HORIZONTAL_ID    = 1 << 0,
                     LEFT_TO_RIGHT_ID = 1 << 1;

  public static final ComponentOrientation LEFT_TO_RIGHT
    = new ComponentOrientation(HORIZONTAL_ID & LEFT_TO_RIGHT_ID);
  public static final ComponentOrientation RIGHT_TO_LEFT
    = new ComponentOrientation(HORIZONTAL_ID);
  public static final ComponentOrientation UNKNOWN
    = new ComponentOrientation(0);

  // FIXME: This field is from the serialization spec, but what are the 
  // correct values?
  int orientation;

  ComponentOrientation(int orientation)
  {
    this.orientation = orientation;
  }

  public boolean isHorizontal()
  {
    return ((orientation & HORIZONTAL_ID) != 0);
  }

  public boolean isLeftToRight()
  {
    return ((orientation & LEFT_TO_RIGHT_ID) != 0);
  }

  public static ComponentOrientation getOrientation(Locale locale)
  {
    // FIXME: Use a table to look this up.
    return LEFT_TO_RIGHT;
  }

  public static ComponentOrientation getOrientation(ResourceBundle bdl)
  {
    ComponentOrientation r;

    try
    {
      Object obj = bdl.getObject("Orientation");
      r = (ComponentOrientation) obj;
      if (r != null)
	return r;  
    }
    catch (Exception x)
    {
      // Fall through
    }

    try
    {
      Locale l = bdl.getLocale();
      r = getOrientation(l);
      if (r != null)
	return r;
    }
    catch (Exception x)
    {
      // Fall through  
    }

    return (getOrientation (Locale.getDefault ()));
  }
}
