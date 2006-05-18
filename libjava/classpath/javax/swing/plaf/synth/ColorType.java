/* ColorType.java -- En enumeration of color types
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.plaf.synth;

/**
 * A typesafe enumeration of color types.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public class ColorType
{

  /**
   * A constant used to identify the foreground color of a component.
   */
  public static final ColorType FOREGROUND = new ColorType("Foreground");

  /**
   * A constant used to identify the background color of a component.
   */
  public static final ColorType BACKGROUND = new ColorType("Background");

  /**
   * A constant used to identify the foreground color of text of a component.
   */
  public static final ColorType TEXT_FOREGROUND
                                           = new ColorType("TextForeground");

  /**
   * A constant used to identify the background color of text of a component.
   */
  public static final ColorType TEXT_BACKGROUND
                                           = new ColorType("TextBackground");

  /**
   * A constant used to identify the focus color of a component.
   */
  public static final ColorType FOCUS = new ColorType("Focus");

  /**
   * The maximum number of color types.
   */
  public static final int MAX_COUNT;
  static
  {
    // This is not a constant in the JDK.
    MAX_COUNT = 5;
  }

  /**
   * A counter used to assign an ID to the created color types.
   */
  private static int count = 0;

  /**
   * The ID of the color type.
   */
  private int id;

  /**
   * The description of the color type.
   */
  private String description;

  /**
   * Creates a new <code>Color</code> color type with the specified
   * description.
   *
   * @param desc the textual description of the color type
   */
  protected ColorType(String desc)
  {
    description = desc;
    id = count;
    count++;
  }

  /**
   * Returns the unique ID of the color type.
   *
   * @return the unique ID of the color type
   */
  public final int getID()
  {
    return id;
  }

  /**
   * Returns the textual description of the color type.
   *
   * @return the textual description of the color type
   */
  public String toString()
  {
    return description;
  }
}
