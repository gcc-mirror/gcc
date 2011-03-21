/* MediaName.java --
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
 * <code>MediaName</code> is a subclass of the <code>Media</code> printing
 * attribute and provides selection of media to be used by the means of
 * defined names. The class pre-defines commonly available media names.
 * This media type enumeration may be used in alternative to
 * MediaSizeName/MediaTray.
 * <p>
 * <b>IPP Compatibility:</b> MediaName is not an IPP 1.1 attribute on its own.
 * It provides parts of the <code>media</code> attribute type values.
 * </p>
 *
 * @author Sven de Marothy
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class MediaName extends Media
{
  private static final long serialVersionUID = 4653117714524155448L;

  /**
   * The North American letter white medium.
   */
  public static final MediaName NA_LETTER_WHITE = new MediaName(0);

  /**
   * The North American letter transparent medium.
   */
  public static final MediaName NA_LETTER_TRANSPARENT = new MediaName(1);

  /**
   * The ISO A4 white medium.
   */
  public static final MediaName ISO_A4_WHITE = new MediaName(2);

  /**
   * The ISO A4 transparent medium.
   */
  public static final MediaName ISO_A4_TRANSPARENT = new MediaName(3);

  private static final String[] stringTable = { "na-letter-white",
                                                "na-letter-transparent",
                                                "iso-a4-white",
                                                "iso-a4-transparent" };

  private static final MediaName[] enumValueTable = { NA_LETTER_WHITE,
                                                      NA_LETTER_TRANSPARENT,
                                                      ISO_A4_WHITE,
                                                      ISO_A4_TRANSPARENT };

  /**
   * Creates a <code>MediaName</code> object.
   *
   * @param i the enum value.
   */
  protected MediaName(int i)
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
