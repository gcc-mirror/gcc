/* GNULookAndFeel.java -- An example of using the javax.swing UI.
   Copyright (C) 2005  Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
*/

package gnu.classpath.examples.swing;

import java.awt.Color;

import javax.swing.ImageIcon;
import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.basic.BasicLookAndFeel;

public class GNULookAndFeel extends BasicLookAndFeel
{

  static Color blueGray = new Color(0xdc, 0xda, 0xd5);

  public boolean isNativeLookAndFeel()    { return true; }
  public boolean isSupportedLookAndFeel() { return true; }
  public String getDescription()          { return "GNU Look and Feel"; }
  public String getID()                   { return "GNULookAndFeel"; }
  public String getName()                 { return "GNU"; }

  static UIDefaults LAF_defaults;

  private final static String iconspath = "/gnu/javax/swing/plaf/gtk/icons/";

  public UIDefaults getDefaults()
  {
    if (LAF_defaults == null)
      {
        LAF_defaults = super.getDefaults();
        Object[] myDefaults = new Object[] {
          "Button.background", new ColorUIResource(blueGray),
          "CheckBox.background", new ColorUIResource(blueGray),
          "CheckBoxMenuItem.background", new ColorUIResource(blueGray),
          "ToolBar.background", new ColorUIResource(blueGray),
          "Panel.background", new ColorUIResource(blueGray),
          "Slider.background", new ColorUIResource(blueGray),
          "OptionPane.background", new ColorUIResource(blueGray),
          "ProgressBar.background", new ColorUIResource(blueGray),
          "TabbedPane.background", new ColorUIResource(blueGray),
          "Label.background", new ColorUIResource(blueGray),
          "Menu.background", new ColorUIResource(blueGray),
          "MenuBar.background", new ColorUIResource(blueGray),
          "MenuItem.background", new ColorUIResource(blueGray),
          "ScrollBar.background", new ColorUIResource(blueGray),

	  "Tree.closedIcon",
	  new IconUIResource(new ImageIcon
			     (getClass().getResource
			      (iconspath + "TreeClosed.png"))),
	  "Tree.leafIcon",
	  new IconUIResource(new ImageIcon
			     (getClass().getResource
			      (iconspath + "TreeLeaf.png"))),
	  "Tree.openIcon",
	  new IconUIResource(new ImageIcon
			     (getClass().getResource
			      (iconspath + "TreeOpen.png"))),
        };
        LAF_defaults.putDefaults(myDefaults);
      }
    return LAF_defaults;
  }
}
