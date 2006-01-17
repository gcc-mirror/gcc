/* MediaTray.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package javax.print.attribute.standard;

import javax.print.attribute.EnumSyntax;

/**
 * <code>MediaTray</code> is a subclass of the <code>Media</code> printing 
 * attribute and provides selection of media to be used by the means of the
 * input tray of the printer. The class pre-defines commonly available types
 * of input trays in printers. This media type enumeration may be used in
 * alternative to MediaSizeName/MediaName.
 * <p>
 * <b>IPP Compatibility:</b> MediaTray is not an IPP 1.1 attribute on its own.
 * It provides parts of the <code>media</code> attribute type values.
 * </p>
 * 
 * @author Sven de Marothy
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class MediaTray extends Media
{
  
  private static final long serialVersionUID = -982503611095214703L;
  
  /**
   * Top tray
   */
  public static final MediaTray TOP = new MediaTray(0);
  
  /**
   * Middle tray
   */
  public static final MediaTray MIDDLE = new MediaTray(1);
  
  /**
   * Bottom tray
   */
  public static final MediaTray BOTTOM = new MediaTray(2);

  /**
   * Envelope tray
   */
  public static final MediaTray ENVELOPE = new MediaTray(3);

  /**
   * Manual-feed tray
   */ 
  public static final MediaTray MANUAL = new MediaTray(4);
  
  /**
   * Large capacity tray
   */
  public static final MediaTray LARGE_CAPACITY = new MediaTray(5);

  /**
   * Main tray
   */
  public static final MediaTray MAIN = new MediaTray(6);
  
  /**
   * Side tray
   */
  public static final MediaTray SIDE = new MediaTray(7);  
  
  private static final String[] stringTable = { "top", "middle", "bottom",
                                                "envelope", "manual", 
                                                "large-capacity", "main", 
                                                "side" };
  
  private static final MediaTray[] enumValueTable = { TOP, MIDDLE, BOTTOM, 
                                                      ENVELOPE, MANUAL,
                                                      LARGE_CAPACITY, MAIN,
                                                      SIDE };  
  
  /**
   * Creates a <code>MediaTray</code> object.
   *
   * @param i the enum value.
   */
  protected MediaTray(int i)
  {
    super( i );
  }
 
  /**
   * Returns a table with the enumeration values represented as strings
   * for this object.
   *
   * @return The enumeration values as strings.
   */
  protected String[] getStringTable()
  {
    return stringTable;
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * @return The enumeration values.
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return enumValueTable;
  }
}

