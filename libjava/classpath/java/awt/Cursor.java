/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

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


package java.awt;

/**
  * This class represents various predefined cursor types.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Cursor implements java.io.Serializable
{
  static final long serialVersionUID = 8028237497568985504L;

  /**
  * Constant for the system default cursor type
  */
  public static final int DEFAULT_CURSOR = 0;

  /**
  * Constant for a cross-hair cursor.
  */
  public static final int CROSSHAIR_CURSOR = 1;

  /**
  * Constant for a cursor over a text field.
  */
  public static final int TEXT_CURSOR = 2;

  /**
  * Constant for a cursor to display while waiting for an action to complete.
  */
  public static final int WAIT_CURSOR = 3;

  /**
  * Cursor used over SW corner of window decorations.
  */
  public static final int SW_RESIZE_CURSOR = 4;

  /**
  * Cursor used over SE corner of window decorations.
  */
  public static final int SE_RESIZE_CURSOR = 5;

  /**
  * Cursor used over NW corner of window decorations.
  */
  public static final int NW_RESIZE_CURSOR = 6;

  /**
  * Cursor used over NE corner of window decorations.
  */
  public static final int NE_RESIZE_CURSOR = 7;

  /**
  * Cursor used over N edge of window decorations.
  */
  public static final int N_RESIZE_CURSOR = 8;

  /**
  * Cursor used over S edge of window decorations.
  */
  public static final int S_RESIZE_CURSOR = 9;

  /**
  * Cursor used over W edge of window decorations.
  */
  public static final int W_RESIZE_CURSOR = 10;

  /**
  * Cursor used over E edge of window decorations.
  */
  public static final int E_RESIZE_CURSOR = 11;

  /**
  * Constant for a hand cursor.
  */
  public static final int HAND_CURSOR = 12;

  /**
  * Constant for a cursor used during window move operations.
  */
  public static final int MOVE_CURSOR = 13;

  private static String[] NAMES = { "Default Cursor", "Crosshair Cursor",
                                  "Text Cursor", "Wait Cursor",
                                  "Southwest Resize Cursor",
                                  "Southeast Resize Cursor",
                                  "Northwest Resize Cursor",
                                  "Northeast Resize Cursor",
                                  "North Resize Cursor", "South Resize Cursor",
                                  "West Resize Cursor", "East Resize Cursor",
                                  "Hand Cursor", "Move Cursor" };
  
  public static final int CUSTOM_CURSOR    = 0xFFFFFFFF;

  private static final int PREDEFINED_COUNT = 14;

  protected static Cursor[] predefined = new Cursor[PREDEFINED_COUNT];
  protected String name;

  /**
   * @serial The numeric id of this cursor.
   */
  int type;

  /**
   * Initializes a new instance of <code>Cursor</code> with the specified
   * type.
   *
   * @param type The cursor type.
   *
   * @exception IllegalArgumentException If the specified cursor type is invalid
   */
  public Cursor(int type)
  {
    if (type < 0 || type >= PREDEFINED_COUNT)
      throw new IllegalArgumentException ("invalid cursor " + type);

    this.type = type;

    name = NAMES[type];
      
    // FIXME: lookup?
  }

  /** This constructor is used internally only. 
   * Application code should call Toolkit.createCustomCursor().
   */
  protected Cursor(String name)
  {
    this.name = name;
    this.type = CUSTOM_CURSOR;
  }

  /**
   * Returns an instance of <code>Cursor</code> for one of the specified
   * predetermined types.
   *
   * @param type The type contant from this class.
   *
   * @return The requested predefined cursor.
   *
   * @exception IllegalArgumentException If the constant is not one of the
   * predefined cursor type constants from this class.
   */
  public static Cursor getPredefinedCursor(int type)
  {
    if (type < 0 || type >= PREDEFINED_COUNT)
      throw new IllegalArgumentException ("invalid cursor " + type);
    if (predefined[type] == null)
      predefined[type] = new Cursor(type);
    return predefined[type];
  }

  /**
   * Retrieves the system specific custom Cursor named Cursor names are,
   * for example: "Invalid.16x16".
   *
   * @exception AWTException
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true.
   */
  public static Cursor getSystemCustomCursor(String name)
                                      throws AWTException
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException ();

    // FIXME
    return null;
  }

  /**
   * Returns an instance of the system default cursor type.
   *
   * @return The system default cursor.
   */
  public static Cursor getDefaultCursor()
  {
    return getPredefinedCursor(DEFAULT_CURSOR);
  }

  /**
   * Returns the numeric type identifier for this cursor.
   *
   * @return The cursor id.
   */
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
    return (this.getClass()
	    + "[type=" + getType()
	    + ",name=" + getName() + "]");
  }
}
