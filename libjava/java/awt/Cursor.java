/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

/* A somewhat incomplete placeholder. */

public class Cursor implements java.io.Serializable
{
  public static final int DEFAULT_CURSOR   = 0,
			  CROSSHAIR_CURSOR = 1,
			  TEXT_CURSOR      = 2,
			  WAIT_CURSOR      = 3,
			  SW_RESIZE_CURSOR = 4,
			  SE_RESIZE_CURSOR = 5,
			  NW_RESIZE_CURSOR = 6,
			  NE_RESIZE_CURSOR = 7,
			  N_RESIZE_CURSOR  = 8,
			  S_RESIZE_CURSOR  = 9,
			  W_RESIZE_CURSOR  = 10,
			  E_RESIZE_CURSOR  = 11,
			  HAND_CURSOR      = 12,
			  MOVE_CURSOR      = 13,
			  CUSTOM_CURSOR    = 0xFFFFFFFF;

  private static final int PREDEFINED_COUNT = 14;

  protected static Cursor[] predefined = new Cursor[PREDEFINED_COUNT];
  protected String name;
  int type;

  public Cursor(int type)
  {
    if (type < 0 || type >= PREDEFINED_COUNT)
      throw new IllegalArgumentException ("invalid cursor " + type);
    this.type = type;
    // FIXME: lookup and set name?
  }

  /** This constructor is used internally only. 
    * Application code should call Toolkit.createCustomCursor().
    */
  protected Cursor(String name)
  {
    this.name = name;
    this.type = CUSTOM_CURSOR;
  }

  public static Cursor getPredefinedCursor(int type)
  {
    if (type < 0 || type >= PREDEFINED_COUNT)
      throw new IllegalArgumentException ("invalid cursor " + type);
    if (predefined[type] == null)
      predefined[type] = new Cursor(type);
    return predefined[type];
  }

  public static Cursor getSystemCustomCursor(String name)
                                      throws AWTException
  {
    // FIXME
    return null;
  }

  public static Cursor getDefaultCursor()
  {
    return getPredefinedCursor(0);
  }

  public int getType()
  {
    return type;
  }

  public String getName()
  {
    return name;
  }

  public String toString()
  {
    return (this.getClass() + "[" + getName() + "]");
  }
}
