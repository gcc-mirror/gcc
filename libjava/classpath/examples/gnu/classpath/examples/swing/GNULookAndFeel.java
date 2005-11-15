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
import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JRadioButton;
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
          "CheckBox.icon", new CheckBoxIcon(),
          "RadioButton.icon", new RadioButtonIcon(),
	  
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
  
  /**
   * The icon used for CheckBoxes in the BasicLookAndFeel. This is an empty
   * icon with a size of 13x13 pixels.
   */
  static class CheckBoxIcon
    implements Icon
  {
    /**
     * Returns the height of the icon. The BasicLookAndFeel CheckBox icon
     * has a height of 13 pixels.
     *
     * @return the height of the icon
     */
    public int getIconHeight()
    {
      return 13;
    }

    /**
     * Returns the width of the icon. The BasicLookAndFeel CheckBox icon
     * has a width of 13 pixels.
     *
     * @return the height of the icon
     */
    public int getIconWidth()
    {
      return 13;
    }

    /**
     * Paints the icon. The BasicLookAndFeel CheckBox icon is empty and does
     * not need to be painted.
     *
     * @param c the component to be painted
     * @param g the Graphics context to be painted with
     * @param x the x position of the icon
     * @param y the y position of the icon
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      Color save = g.getColor();
      g.setColor(c.getForeground());
      g.drawRect(x, y, getIconWidth(), getIconHeight());    
      
      JCheckBox item = (JCheckBox) c;
      if (item.isSelected()) 
        {
          g.drawLine(3 + x, 5 + y, 3 + x, 9 + y);
          g.drawLine(4 + x, 5 + y, 4 + x, 9 + y);
          g.drawLine(5 + x, 7 + y, 9 + x, 3 + y);
          g.drawLine(5 + x, 8 + y, 9 + x, 4 + y);
        }
      
      g.setColor(save);
    }
  }
  
  /**
   * The icon used for RadioButtons in the GNULookAndFeel. This is an empty
   * icon with a size of 13x13 pixels.
   */
  static class RadioButtonIcon
    implements Icon
  {
    /**
     * Returns the height of the icon. The GNULookAndFeel RadioButton icon
     * has a height of 13 pixels.
     *
     * @return the height of the icon
     */
    public int getIconHeight()
    {
      return 13;
    }

    /**
     * Returns the width of the icon. The GNULookAndFeel RadioButton icon
     * has a width of 13 pixels.
     *
     * @return the height of the icon
     */
    public int getIconWidth()
    {
      return 13;
    }

    /**
     * Paints the icon. The GNULookAndFeel RadioButton icon is empty and does
     * not need to be painted.
     *
     * @param c the component to be painted
     * @param g the Graphics context to be painted with
     * @param x the x position of the icon
     * @param y the y position of the icon
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      Color savedColor = g.getColor();
      JRadioButton b = (JRadioButton) c;
      
      // draw outer circle
      if (b.isEnabled())
        g.setColor(Color.GRAY);
      else
        g.setColor(Color.GRAY);
      g.drawLine(x + 2, y + 1, x + 3, y + 1);
      g.drawLine(x + 4, y, x + 7, y);
      g.drawLine(x + 8, y + 1, x + 9, y + 1);
      g.drawLine(x + 10, y + 2, x + 10, y + 3);
      g.drawLine(x + 11, y + 4, x + 11, y + 7);
      g.drawLine(x + 10, y + 8, x + 10, y + 9);
      g.drawLine(x + 8, y + 10, x + 9, y + 10);
      g.drawLine(x + 4, y + 11, x + 7, y + 11);
      g.drawLine(x + 2, y + 10, x + 3, y + 10);
      g.drawLine(x + 1, y + 9, x + 1, y + 8);
      g.drawLine(x, y + 7, x, y + 4);
      g.drawLine(x + 1, y + 2, x + 1, y + 3);

      if (b.getModel().isArmed())
        {
          g.setColor(Color.GRAY);
          g.drawLine(x + 4, y + 1, x + 7, y + 1);
          g.drawLine(x + 4, y + 10, x + 7, y + 10);
          g.drawLine(x + 1, y + 4, x + 1, y + 7);
          g.drawLine(x + 10, y + 4, x + 10, y + 7);
          g.fillRect(x + 2, y + 2, 8, 8);
        }
      else 
        {
          // only draw inner highlight if not filled
          if (b.isEnabled())
            {
              g.setColor(Color.WHITE);
          
              g.drawLine(x + 2, y + 8, x + 2, y + 9);
              g.drawLine(x + 1, y + 4, x + 1, y + 7);
              g.drawLine(x + 2, y + 2, x + 2, y + 3);
              g.drawLine(x + 3, y + 2, x + 3, y + 2);
              g.drawLine(x + 4, y + 1, x + 7, y + 1);
              g.drawLine(x + 8, y + 2, x + 9, y + 2);
            }
        }

      // draw outer highlight
      if (b.isEnabled())
        {
          g.setColor(Color.WHITE);
          
          // outer
          g.drawLine(x + 10, y + 1, x + 10, y + 1);
          g.drawLine(x + 11, y + 2, x + 11, y + 3);
          g.drawLine(x + 12, y + 4, x + 12, y + 7);
          g.drawLine(x + 11, y + 8, x + 11, y + 9);
          g.drawLine(x + 10, y + 10, x + 10, y + 10);
          g.drawLine(x + 8, y + 11, x + 9, y + 11);
          g.drawLine(x + 4, y + 12, x + 7, y + 12);
          g.drawLine(x + 2, y + 11, x + 3, y + 11);
        }
      
      if (b.isSelected())
        {
          if (b.isEnabled())
            g.setColor(Color.BLACK);
          else
            g.setColor(Color.GRAY);
          g.drawLine(x + 4, y + 3, x + 7, y + 3);
          g.fillRect(x + 3, y + 4, 6, 4);
          g.drawLine(x + 4, y + 8, x + 7, y + 8);
        }
      g.setColor(savedColor);
    }  
  }
}
