/* MetalLookAndFeel.java
   Copyright (C) 2002 Free Software Foundation, Inc.

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
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.basic.BasicLookAndFeel;

public class MetalLookAndFeel extends BasicLookAndFeel
{	   
  private static final long serialVersionUID = 6680646159193457980L;
  private static MetalTheme theme;
  private UIDefaults LAF_defaults;

  public MetalLookAndFeel()
  {
    createDefaultTheme();
  }

  protected void createDefaultTheme()
  {
    setCurrentTheme(new DefaultMetalTheme());
  }

  public boolean isNativeLookAndFeel()
  {
    return true;
  }

  public boolean isSupportedLookAndFeel()
  {
    return true;
  }

  public String getDescription()
  {
    return "Metal look and feel";
  }

  public String getID()
  {
    return "MetalLookAndFeel";
  }

  public String getName()
  {
    return "MetalLookAndFeel";
  }

  public UIDefaults getDefaults()
  {
    if (LAF_defaults == null)
      LAF_defaults = super.getDefaults();

    // Returns the default values for this look and feel. 
    return LAF_defaults;
  }

  public static ColorUIResource getAcceleratorForeground()
  {
    return theme.getAcceleratorForeground();
  }

  public static ColorUIResource getAcceleratorSelectedForeground()
  {
    return theme.getAcceleratorSelectedForeground();
  }

  public static ColorUIResource getBlack()
  {
    return theme.getBlack();
  }

  public static ColorUIResource getControl()
  {
    return theme.getControl();
  }

  public static ColorUIResource getControlDarkShadow()
  {
    return theme.getControlDarkShadow();
  }

  public static ColorUIResource getControlDisabled()
  {
    return theme.getControlDisabled();
  }

  public static ColorUIResource getControlHighlight()
  {
    return theme.getControlHighlight();
  }

  public static ColorUIResource getControlInfo()
  {
    return theme.getControlInfo();
  }

  public static ColorUIResource getControlShadow()
  {
    return theme.getControlShadow();
  }

  public static ColorUIResource getControlTextColor()
  {
    return theme.getControlTextColor();
  }

  public static FontUIResource getControlTextFont()
  {
    return theme.getControlTextFont();
  }

  public static ColorUIResource getDesktopColor()
  {
    return theme.getDesktopColor();
  }

  public static ColorUIResource getFocusColor()
  {
    return theme.getFocusColor();
  }

  public static ColorUIResource getHighlightedTextColor()
  {
    return theme.getHighlightedTextColor();
  }

  public static ColorUIResource getInactiveControlTextColor()
  {
    return theme.getInactiveControlTextColor();
  }

  public static ColorUIResource getInactiveSystemTextColor()
  {
    return theme.getInactiveSystemTextColor();
  }

  public static ColorUIResource getMenuBackground()
  {
    return theme.getMenuBackground();
  }

  public static ColorUIResource getMenuDisabledForeground()
  {
    return theme.getMenuDisabledForeground();
  }

  public static ColorUIResource getMenuForeground()
  {
    return theme.getMenuForeground();
  }

  public static ColorUIResource getMenuSelectedBackground()
  {
    return theme.getMenuSelectedBackground();
  }

  public static ColorUIResource getMenuSelectedForeground()
  {
    return theme.getMenuSelectedForeground();
  }

  public static FontUIResource getMenuTextFont()
  {
    return theme.getMenuTextFont();
  }

  public static ColorUIResource getPrimaryControl()
  {
    return theme.getPrimaryControl();
  }

  public static ColorUIResource getPrimaryControlDarkShadow()
  {
    return theme.getPrimaryControlDarkShadow();
  }

  public static ColorUIResource getPrimaryControlHighlight()
  {
    return theme.getPrimaryControlHighlight();
  }

  public static ColorUIResource getPrimaryControlInfo()
  {
    return theme.getPrimaryControlInfo();
  }

  public static ColorUIResource getPrimaryControlShadow()
  {
    return theme.getPrimaryControlShadow();
  }

  public static ColorUIResource getSeparatorBackground()
  {
    return theme.getSeparatorBackground();
  }

  public static ColorUIResource getSeparatorForeground()
  {
    return theme.getSeparatorForeground();
  }

  public static FontUIResource getSubTextFont()
  {
    return theme.getSubTextFont();
  }

  public static ColorUIResource getSystemTextColor()
  {
    return theme.getSystemTextColor();
  }

  public static FontUIResource getSystemTextFont()
  {
    return theme.getSystemTextFont();
  }

  public static ColorUIResource getTextHighlightColor()
  {
    return theme.getTextHighlightColor();
  }

  public static ColorUIResource getUserTextColor()
  {
    return theme.getUserTextColor();
  }

  public static FontUIResource getUserTextFont()
  {
    return theme.getUserTextFont();
  }

  public static ColorUIResource getWhite()
  {
    return theme.getWhite();
  }

  public static ColorUIResource getWindowBackground()
  {
    return theme.getWindowBackground();
  }

  public static ColorUIResource getWindowTitleBackground()
  {
    return theme.getWindowTitleBackground();
  }

  public static FontUIResource getWindowTitleFont()
  {
    return theme.getWindowTitleFont();
  }

  public static ColorUIResource getWindowTitleForeground()
  {
    return theme.getWindowTitleForeground();
  }

  public static ColorUIResource getWindowTitleInactiveBackground()
  {
    return theme.getWindowTitleInactiveBackground();
  }

  public static ColorUIResource getWindowTitleInactiveForeground()
  {
    return theme.getWindowTitleInactiveForeground();
  }

  public static void setCurrentTheme(MetalTheme theme)
  {
    MetalLookAndFeel.theme = theme;
  }
}
