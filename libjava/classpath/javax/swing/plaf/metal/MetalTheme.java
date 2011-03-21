/* MetalTheme.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

import java.awt.Color;

import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;

/**
 * The base class for themes used by the {@link MetalLookAndFeel}.  A default
 * theme ({@link DefaultMetalTheme}) is provided, or you can create and use
 * your own.
 *
 * @see MetalLookAndFeel#setCurrentTheme(MetalTheme)
 */
public abstract class MetalTheme
{
  private ColorUIResource BLACK = new ColorUIResource(Color.BLACK);
  private ColorUIResource WHITE = new ColorUIResource(Color.WHITE);

  /**
   * Default constructor.
   */
  public MetalTheme()
  {
    // Do nothing here.
  }

  /**
   * Returns the name of the theme.
   *
   * @return The name of the theme.
   */
  public abstract String getName();

  /**
   * Adds custom entries to the UI defaults table.  This method is empty.
   *
   * @param table  the table.
   */
  public void addCustomEntriesToTable(UIDefaults table)
  {
    // Do nothing here.
    // This method needs to be overridden to actually do something.
    // It is called from MetalLookAndFeel.getDefaults().
  }

  /**
   * Returns the accelerator foreground color.  The default implementation
   * returns the color from {@link #getPrimary1()}.
   *
   * @return The accelerator foreground color.
   */
  public ColorUIResource getAcceleratorForeground()
  {
    return getPrimary1();
  }

  /**
   * Returns the accelerator selected foreground color.  The default
   * implementation returns the color from {@link #getBlack()}.
   *
   * @return The accelerator selected foreground color.
   */
  public ColorUIResource getAcceleratorSelectedForeground()
  {
    return getBlack();
  }

  /**
   * Returns the control color.  The default implementation returns the color
   * from {@link #getSecondary3()}.
   *
   * @return The control color.
   */
  public ColorUIResource getControl()
  {
    return getSecondary3();
  }

  /**
   * Returns the color used for dark shadows on controls.  The default
   * implementation returns the color from  {@link #getSecondary1()}.
   *
   * @return The color used for dark shadows on controls.
   */
  public ColorUIResource getControlDarkShadow()
  {
    return getSecondary1();
  }

  /**
   * Returns the color used for disabled controls.  The default implementation
   * returns the color from {@link #getSecondary1()}.
   *
   * @return The color used for disabled controls.
   */
  public ColorUIResource getControlDisabled()
  {
    return getSecondary2();
  }

  /**
   * Returns the color used to draw highlights for controls.  The default
   * implementation returns the color from {@link #getWhite()}.
   *
   * @return The color used to draw highlights for controls.
   */
  public ColorUIResource getControlHighlight()
  {
    return getWhite();
  }

  /**
   * Returns the color used to display control info.  The default
   * implementation returns the color from {@link #getBlack()}.
   *
   * @return The color used to display control info.
   */
  public ColorUIResource getControlInfo()
  {
    return getBlack();
  }

  /**
   * Returns the color used to draw shadows for controls.  The default
   * implementation returns the color from {@link #getSecondary2()}.
   *
   * @return The color used to draw shadows for controls.
   */
  public ColorUIResource getControlShadow()
  {
    return getSecondary2();
  }

  /**
   * Returns the color used for text on controls.  The default implementation
   * returns the color from {@link #getControlInfo()}.
   *
   * @return The color used for text on controls.
   */
  public ColorUIResource getControlTextColor()
  {
    return getControlInfo();
  }

  /**
   * Returns the color used for the desktop background.  The default
   * implementation returns the color from {@link #getPrimary2()}.
   *
   * @return The color used for the desktop background.
   */
  public ColorUIResource getDesktopColor()
  {
    return getPrimary2();
  }

  /**
   * Returns the color used to draw focus highlights.  The default
   * implementation returns the color from {@link #getPrimary2()}.
   *
   * @return The color used to draw focus highlights.
   */
  public ColorUIResource getFocusColor()
  {
    return getPrimary2();
  }

  /**
   * Returns the color used to draw highlighted text.  The default
   * implementation returns the color from {@link #getHighlightedTextColor()}.
   *
   * @return The color used to draw highlighted text.
   */
  public ColorUIResource getHighlightedTextColor()
  {
    return getControlTextColor();
  }

  /**
   * Returns the color used to draw text on inactive controls.  The default
   * implementation returns the color from {@link #getControlDisabled()}.
   *
   * @return The color used to draw text on inactive controls.
   */
  public ColorUIResource getInactiveControlTextColor()
  {
    return getControlDisabled();
  }

  /**
   * Returns the color used to draw inactive system text.  The default
   * implementation returns the color from {@link #getSecondary2()}.
   *
   * @return The color used to draw inactive system text.
   */
  public ColorUIResource getInactiveSystemTextColor()
  {
    return getSecondary2();
  }

  /**
   * Returns the background color for menu items.  The default implementation
   * returns the color from {@link #getSecondary3()}.
   *
   * @return The background color for menu items.
   *
   * @see #getMenuSelectedBackground()
   */
  public ColorUIResource getMenuBackground()
  {
    return getSecondary3();
  }

  /**
   * Returns the foreground color for disabled menu items.  The default
   * implementation returns the color from {@link #getSecondary2()}.
   *
   * @return The foreground color for disabled menu items.
   *
   * @see #getMenuForeground()
   */
  public ColorUIResource getMenuDisabledForeground()
  {
    return getSecondary2();
  }

  /**
   * Returns the foreground color for menu items.  The default implementation
   * returns the color from {@link #getBlack()}.
   *
   * @return The foreground color for menu items.
   *
   * @see #getMenuDisabledForeground()
   * @see #getMenuSelectedForeground()
   */
  public ColorUIResource getMenuForeground()
  {
    return getBlack();
  }

  /**
   * Returns the background color for selected menu items.  The default
   * implementation returns the color from {@link #getPrimary2()}.
   *
   * @return The background color for selected menu items.
   *
   * @see #getMenuBackground()
   */
  public ColorUIResource getMenuSelectedBackground()
  {
    return getPrimary2();
  }

  /**
   * Returns the foreground color for selected menu items.  The default
   * implementation returns the value from {@link #getBlack()}.
   *
   * @return The foreground color for selected menu items.
   *
   * @see #getMenuForeground()
   */
  public ColorUIResource getMenuSelectedForeground()
  {
    return getBlack();
  }

  /**
   * Returns the primary color for controls.  The default implementation
   * returns the color from {@link #getPrimary3()}.
   *
   * @return The primary color for controls.
   */
  public ColorUIResource getPrimaryControl()
  {
    return getPrimary3();
  }

  /**
   * Returns the primary color for the dark shadow on controls.  The default
   * implementation returns the color from {@link #getPrimary1()}.
   *
   * @return The primary color for the dark shadow on controls.
   */
  public ColorUIResource getPrimaryControlDarkShadow()
  {
    return getPrimary1();
  }

  /**
   * Returns the primary color for the highlight on controls.  The default
   * implementation returns the color from {@link #getWhite()}.
   *
   * @return The primary color for the highlight on controls.
   */
  public ColorUIResource getPrimaryControlHighlight()
  {
    return getWhite();
  }

  /**
   * Returns the primary color for the information on controls.  The default
   * implementation returns the color from {@link #getBlack()}.
   *
   * @return The primary color for the information on controls.
   */
  public ColorUIResource getPrimaryControlInfo()
  {
    return getBlack();
  }

  /**
   * Returns the primary color for the shadow on controls.  The default
   * implementation returns the color from {@link #getPrimary2()}.
   *
   * @return The primary color for the shadow on controls.
   */
  public ColorUIResource getPrimaryControlShadow()
  {
    return getPrimary2();
  }

  /**
   * Returns the background color for separators.  The default implementation
   * returns the color from {@link #getWhite()}.
   *
   * @return The background color for separators.
   */
  public ColorUIResource getSeparatorBackground()
  {
    return getWhite();
  }

  /**
   * Returns the foreground color for separators.  The default implementation
   * returns the value from {@link #getPrimary1()}.
   *
   * @return The foreground color for separators.
   */
  public ColorUIResource getSeparatorForeground()
  {
    return getPrimary1();
  }

  /**
   * Returns the color used for system text.  The default implementation
   * returns the color from {@link #getBlack()}.
   *
   * @return The color used for system text.
   */
  public ColorUIResource getSystemTextColor()
  {
    return getBlack();
  }

  /**
   * Returns the color used to highlight text.  The default implementation
   * returns the color from {@link #getPrimary3()}.
   *
   * @return The color used to highlight text.
   */
  public ColorUIResource getTextHighlightColor()
  {
    return getPrimary3();
  }

  /**
   * Returns the color used to display user text.  The default implementation
   * returns the color from {@link #getBlack()}.
   *
   * @return The color used to display user text.
   */
  public ColorUIResource getUserTextColor()
  {
    return getBlack();
  }

  /**
   * Returns the window background color.  The default implementation returns
   * the color from {@link #getWhite()}.
   *
   * @return The window background color.
   */
  public ColorUIResource getWindowBackground()
  {
    return getWhite();
  }

  /**
   * Returns the window title background color.  The default implementation
   * returns the color from {@link #getPrimary3()}.
   *
   * @return The window title background color.
   */
  public ColorUIResource getWindowTitleBackground()
  {
    return getPrimary3();
  }

  /**
   * Returns the window title foreground color.  The default implementation
   * returns the color from {@link #getBlack()}.
   *
   * @return The window title foreground color.
   */
  public ColorUIResource getWindowTitleForeground()
  {
    return getBlack();
  }

  /**
   * Returns the background color for an inactive window title.  The default
   * implementation returns the color from {@link #getSecondary3()}.
   *
   * @return The background color for an inactive window title.
   */
  public ColorUIResource getWindowTitleInactiveBackground()
  {
    return getSecondary3();
  }

  /**
   * Returns the foreground color for an inactive window title.  The default
   * implementation returns the color from {@link #getBlack()}.
   *
   * @return The foreground color for an inactive window title.
   */
  public ColorUIResource getWindowTitleInactiveForeground()
  {
    return getBlack();
  }

  /**
   * Returns the color used for black.
   *
   * @return The color used for black.
   */
  protected ColorUIResource getBlack()
  {
    return BLACK;
  }

  /**
   * Returns the color used for white.
   *
   * @return The color used for white.
   */
  protected ColorUIResource getWhite()
  {
    return WHITE;
  }

  /**
   * Returns the first primary color for this theme.
   *
   * @return The first primary color.
   */
  protected abstract ColorUIResource getPrimary1();

  /**
   * Returns the second primary color for this theme.
   *
   * @return The second primary color.
   */
  protected abstract ColorUIResource getPrimary2();

  /**
   * Returns the third primary color for this theme.
   *
   * @return The third primary color.
   */
  protected abstract ColorUIResource getPrimary3();

  /**
   * Returns the first secondary color for this theme.
   *
   * @return The first secondary color.
   */
  protected abstract ColorUIResource getSecondary1();

  /**
   * Returns the second secondary color for this theme.
   *
   * @return The second secondary color.
   */
  protected abstract ColorUIResource getSecondary2();

  /**
   * Returns the third secondary color for this theme.
   *
   * @return The third secondary color.
   */
  protected abstract ColorUIResource getSecondary3();

  /**
   * Returns the font used for text on controls.
   *
   * @return The font used for text on controls.
   */
  public abstract FontUIResource getControlTextFont();

  /**
   * Returns the font used for text in menus.
   *
   * @return The font used for text in menus.
   */
  public abstract FontUIResource getMenuTextFont();

  /**
   * Returns the font used for sub text.
   *
   * @return The font used for sub text.
   */
  public abstract FontUIResource getSubTextFont();

  /**
   * Returns the font used for system text.
   *
   * @return The font used for system text.
   */
  public abstract FontUIResource getSystemTextFont();

  /**
   * Returns the font used for user text.
   *
   * @return The font used for user text.
   */
  public abstract FontUIResource getUserTextFont();

  /**
   * Returns the font used for window titles.
   *
   * @return The font used for window titles.
   */
  public abstract FontUIResource getWindowTitleFont();

}
