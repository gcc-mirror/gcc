/* MetalTheme.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import java.awt.Color;

import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;

public abstract class MetalTheme
{
  private ColorUIResource BLACK = new ColorUIResource(Color.BLACK);
  private ColorUIResource WHITE = new ColorUIResource(Color.WHITE);

  public MetalTheme()
  {
    // Do nothing here.
  }

  public abstract String getName();

  public void addCustomEntriesToTable(UIDefaults table)
  {
    // Do nothing here.
    // This method needs to be overloaded to actuall do something.
  }

  public ColorUIResource getAcceleratorForeground()
  {
    return getPrimary1();
  }

  public ColorUIResource getAcceleratorSelectedForeground()
  {
    return getBlack();
  }
  
  public ColorUIResource getControl()
  {
    return getSecondary3();
  }

  public ColorUIResource getControlDarkShadow()
  {
    return getSecondary1();
  }

  public ColorUIResource getControlDisabled()
  {
    return getSecondary2();
  }

  public ColorUIResource getControlHighlight()
  {
    return getWhite();
  }

  public ColorUIResource getControlInfo()
  {
    return getBlack();
  }

  public ColorUIResource getControlShadow()
  {
    return getSecondary2();
  }

  public ColorUIResource getControlTextColor()
  {
    return getControlInfo();
  }

  public ColorUIResource getDesktopColor()
  {
    return getPrimary2();
  }

  public ColorUIResource getFocusColor()
  {
    return getPrimary2();
  }

  public ColorUIResource getHighlightedTextColor()
  {
    return getControlTextColor();
  }

  public ColorUIResource getInactiveControlTextColor()
  {
    return getControlDisabled();
  }

  public ColorUIResource getInactiveSystemTextColor()
  {
    return getSecondary2();
  }

  public ColorUIResource getMenuBackground()
  {
    return getSecondary3();
  }

  public ColorUIResource getMenuDisabledForeground()
  {
    return getSecondary3();
  }

  public ColorUIResource getMenuForeground()
  {
    return getBlack();
  }

  public ColorUIResource getMenuSelectedBackground()
  {
    return getPrimary2();
  }

  public ColorUIResource getMenuSelectedForeground()
  {
    return getBlack();
  }

  public ColorUIResource getPrimaryControl()
  {
    return getPrimary3();
  }

  public ColorUIResource getPrimaryControlDarkShadow()
  {
    return getPrimary1();
  }

  public ColorUIResource getPrimaryControlHighlight()
  {
    return getWhite();
  }

  public ColorUIResource getPrimaryControlInfo()
  {
    return getBlack();
  }

  public ColorUIResource getPrimaryControlShadow()
  {
    return getPrimary2();
  }

  public ColorUIResource getSeparatorBackground()
  {
    return getWhite();
  }

  public ColorUIResource getSeparatorForeground()
  {
    return getPrimary1();
  }

  public ColorUIResource getSystemTextColor()
  {
    return getBlack();
  }

  public ColorUIResource getTextHighlightColor()
  {
    return getPrimary3();
  }

  public ColorUIResource getUserTextColor()
  {
    return getBlack();
  }
  
  public ColorUIResource getWindowBackground()
  {
    return getWhite();
  }

  public ColorUIResource getWindowTitleBackground()
  {
    return getPrimary3();
  }

  public ColorUIResource getWindowTitleForeground()
  {
    return getBlack();
  }

  public ColorUIResource getWindowTitleInactiveBackground()
  {
    return getSecondary3();
  }

  public ColorUIResource getWindowTitleInactiveForeground()
  {
    return getBlack();
  }

  protected ColorUIResource getBlack()
  {
    return BLACK;
  }

  protected ColorUIResource getWhite()
  {
    return WHITE;
  }

  protected abstract ColorUIResource getPrimary1();
  protected abstract ColorUIResource getPrimary2();
  protected abstract ColorUIResource getPrimary3();
  protected abstract ColorUIResource getSecondary1();
  protected abstract ColorUIResource getSecondary2();
  protected abstract ColorUIResource getSecondary3();

  public abstract FontUIResource getControlTextFont();
  public abstract FontUIResource getMenuTextFont();
  public abstract FontUIResource getSubTextFont();
  public abstract FontUIResource getSystemTextFont();
  public abstract FontUIResource getUserTextFont();
  public abstract FontUIResource getWindowTitleFont();
}
