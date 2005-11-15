/* DefaultMetalTheme.java -- A modern theme for the Metal L&F
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

package javax.swing.plaf.metal;

import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;

/**
 * A modern theme for the Metal Look &amp; Feel.
 * @since 1.5
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class OceanTheme extends DefaultMetalTheme
{
  /**
   * The OceanTheme value for black.
   */
  static final ColorUIResource BLACK = new ColorUIResource(51, 51, 51);

  /**
   * The OceanTheme value for primary1.
   */
  static final ColorUIResource PRIMARY1 = new ColorUIResource(99, 130, 191);

  /**
   * The OceanTheme value for primary1.
   */
  static final ColorUIResource PRIMARY2 = new ColorUIResource(163, 184, 204);

  /**
   * The OceanTheme value for primary1.
   */
  static final ColorUIResource PRIMARY3 = new ColorUIResource(184, 207, 229);

  /**
   * The OceanTheme value for secondary1.
   */
  static final ColorUIResource SECONDARY1 = new ColorUIResource(122, 138, 153);

  /**
   * The OceanTheme value for secondary2.
   */
  static final ColorUIResource SECONDARY2 = new ColorUIResource(184, 207, 229);

  /**
   * The OceanTheme value for secondary3.
   */
  static final ColorUIResource SECONDARY3 = new ColorUIResource(238, 238, 238);

  /**
   * The OceanTheme value for inactive control text.
   */
  static final ColorUIResource INACTIVE_CONTROL_TEXT =
    new ColorUIResource(153, 153, 153);

  /**
   * Returns the name of this theme, &quot;Ocean&quot;
   */
  public String getName()
  {
    return "Ocean";
  }

  /**
   * Returns the color for control text, which is the
   * value of the theme's black value.
   */
  public ColorUIResource getControlTextColor()
  {
    return getBlack();
  }

  /**
   * Returns the desktop color, which is the theme's white color.
   */
  public ColorUIResource getDesktopColor()
  {
    return getWhite();
  }

  /**
   * Returns the color for inactive control text, which is the
   * RGB value (153, 153, 153).
   */
  public ColorUIResource getInactiveControlTextColor()
  {
    return INACTIVE_CONTROL_TEXT;
  }

  /**
   * Returns the OceanTheme's color for disabled menu foreground,
   * 
   */
  public ColorUIResource getMenuDisabledForeground()
  {
    return INACTIVE_CONTROL_TEXT;
  }

  
  /**
   * Returns the OceanTheme's color for black, the RGB value
   * (51, 51, 51).
   *
   * @return Returns the OceanTheme's value for black 
   */
  protected ColorUIResource getBlack()
  {
    return BLACK;
  }

  /**
   * Return the OceanTheme's value for primary 1, the RGB value
   * (99, 130, 191).
   */
  protected ColorUIResource getPrimary1()
  {
    return PRIMARY1;
  }
  
  /**
   * Return the OceanTheme's value for primary 2, the RGB value
   * (163, 184, 204).
   */
  protected ColorUIResource getPrimary2()
  {
    return PRIMARY2;
  }
  
  /**
   * Return the OceanTheme's value for primary 1, the RGB value
   * (184, 207, 229).
   */
  protected ColorUIResource getPrimary3()
  {
    return PRIMARY3;
  }

  /**
   * Return the OceanTheme's value for secondary 1, the RGB value
   * (122, 138, 153).
   */
  protected ColorUIResource getSecondary1()
  {
    return SECONDARY1;
  }

  /**
   * Return the OceanTheme's value for secondary 2, the RGB value
   * (184, 207, 229).
   */
  protected ColorUIResource getSecondary2()
  {
    return SECONDARY2;
  }
  /**
   * Return the OceanTheme's value for secondary 3, the RGB value
   * (238, 238, 238).
   */
  protected ColorUIResource getSecondary3()
  {
    return SECONDARY3;
  }

  /**
   * Adds customized entries to the UIDefaults table.
   *
   * @param defaults the UI defaults table
   */
  public void addCustomEntriesToTable(UIDefaults defaults)
  {
    defaults.put("Button.rollover", Boolean.TRUE);
  }
}
