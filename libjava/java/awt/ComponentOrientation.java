/* Copyright (C) 2000, 2001, 2002  Free Software Foundation

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

/**
 * @author Bryce McKinlay  <bryce@albatross.co.nz>
 */

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
