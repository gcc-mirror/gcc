/* DefaultMetalTheme.java --
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

import java.awt.Font;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;

public class DefaultMetalTheme extends MetalTheme
{
  private static final ColorUIResource PRIMARY1 =
    new ColorUIResource(102, 102, 153);
  private static final ColorUIResource PRIMARY2 =
    new ColorUIResource(153, 153, 204);
  private static final ColorUIResource PRIMARY3 = 
    new ColorUIResource(204, 204, 255);
  private static final ColorUIResource SECONDARY1 = 
    new ColorUIResource(102, 102, 102);
  private static final ColorUIResource SECONDARY2 = 
    new ColorUIResource(153, 153, 153);
  private static final ColorUIResource SECONDARY3 = 
    new ColorUIResource(204, 204, 204);
  
  private static final FontUIResource CONTROL_TEXT_FONT =
    new FontUIResource("Dialog", Font.BOLD, 12);
  private static final FontUIResource MENU_TEXT_FONT =
    new FontUIResource("Dialog", Font.BOLD, 12);
  private static final FontUIResource SUB_TEXT_FONT =
    new FontUIResource("Dialog", Font.PLAIN, 10);
  private static final FontUIResource SYSTEM_TEXT_FONT =
    new FontUIResource("Dialog", Font.PLAIN, 12);
  private static final FontUIResource USER_TEXT_FONT =
    new FontUIResource("Dialog", Font.PLAIN, 12);
  private static final FontUIResource WINDOW_TITLE_FONT =
    new FontUIResource("Dialog", Font.BOLD, 12);
  
  public DefaultMetalTheme()
  {
    // Do nothing here.
  }

  public String getName()
  {
    return "Steel";
  }

  protected ColorUIResource getPrimary1()
  {
    return PRIMARY1;
  }

  protected ColorUIResource getPrimary2()
  {
    return PRIMARY2;
  }

  protected ColorUIResource getPrimary3()
  {
    return PRIMARY3;
  }

  protected ColorUIResource getSecondary1()
  {
    return SECONDARY1;
  }

  protected ColorUIResource getSecondary2()
  {
    return SECONDARY2;
  }

  protected ColorUIResource getSecondary3()
  {
    return SECONDARY3;
  }

  public FontUIResource getControlTextFont()
  {
    return CONTROL_TEXT_FONT;
  }

  public FontUIResource getMenuTextFont()
  {
    return MENU_TEXT_FONT;
  }
  
  public FontUIResource getSubTextFont()
  {
    return SUB_TEXT_FONT;
  }
  
  public FontUIResource getSystemTextFont()
  {
    return SYSTEM_TEXT_FONT;
  }
  
  public FontUIResource getUserTextFont()
  {
    return USER_TEXT_FONT;
  }
  
  public FontUIResource getWindowTitleFont()
  {
    return WINDOW_TITLE_FONT;
  }
}
