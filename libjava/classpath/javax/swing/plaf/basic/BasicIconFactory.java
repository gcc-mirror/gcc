/* BasicIconFactory.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.io.Serializable;

import javax.swing.Icon;
import javax.swing.JCheckBoxMenuItem;

/**
 * Creates icons for the {@link BasicLookAndFeel}.
 */
public class BasicIconFactory implements Serializable
{
  static final long serialVersionUID = 5605588811185324383L;

  private static class DummyIcon 
    implements Icon
  {    
    public int getIconHeight() 
    { 
      return 10; 
    }
    public int getIconWidth() 
    { 
      return 10; 
    }
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      Color save = g.getColor();
      g.setColor(c.getForeground());
      g.drawRect(x, y, 10, 10);
      g.setColor(save);
    }
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
      // The icon is empty and needs no painting.
    }
  }

  /**
   * The icon used for {@link JCheckBoxMenuItem}s in the 
   * {@link BasicLookAndFeel}. This icon has a size of 9x9 pixels.
   */
  static class CheckBoxMenuItemIcon
    implements Icon
  {
    /**
     * Returns the height of the icon in pixels.
     *
     * @return the height of the icon
     */
    public int getIconHeight()
    {
      return 9;
    }

    /**
     * Returns the width of the icon in pixels.
     *
     * @return the height of the icon
     */
    public int getIconWidth()
    {
      return 9;
    }

    /**
     * Paints the icon.
     *
     * @param c the component to be painted
     * @param g the Graphics context to be painted with
     * @param x the x position of the icon
     * @param y the y position of the icon
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      JCheckBoxMenuItem item = (JCheckBoxMenuItem) c;
      if (item.isSelected()) 
        {
          // paint the check...
          g.setColor(Color.black);
          g.drawLine(x + 1, y + 3, x + 1, y + 4);
          g.drawLine(x + 2, y + 4, x + 2, y + 5);
          for (int i = 0; i < 5; i++)
            g.drawLine(x + 3 + i, y + 5 - i, x + 3 + i, y + 6 - i);    
        }
    }
  }

  /**
   * The icon used for RadioButtons in the BasicLookAndFeel. This is an empty
   * icon with a size of 13x13 pixels.
   */
  static class RadioButtonIcon
    implements Icon
  {
    /**
     * Returns the height of the icon. The BasicLookAndFeel RadioButton icon
     * has a height of 13 pixels.
     *
     * @return the height of the icon
     */
    public int getIconHeight()
    {
      return 13;
    }

    /**
     * Returns the width of the icon. The BasicLookAndFeel RadioButton icon
     * has a width of 13 pixels.
     *
     * @return the height of the icon
     */
    public int getIconWidth()
    {
      return 13;
    }

    /**
     * Paints the icon. The BasicLookAndFeel RadioButton icon is empty and does
     * not need to be painted.
     *
     * @param c the component to be painted
     * @param g the Graphics context to be painted with
     * @param x the x position of the icon
     * @param y the y position of the icon
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      // The icon is empty and needs no painting.
    }
  }
  /** The cached CheckBoxIcon instance. */
  private static CheckBoxIcon checkBoxIcon;
  
  /** The cached RadioButtonIcon instance. */
  private static RadioButtonIcon radioButtonIcon;

  public static Icon getMenuItemCheckIcon()
  {
    return new Icon()
    {
      public int getIconHeight()
      {
        return 13;
      }

      public int getIconWidth()
      {
        return 13;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        Color saved = g.getColor();
        g.setColor(Color.BLACK);
        g.drawLine(3 + x, 5 + y, 3 + x, 9 + y);
        g.drawLine(4 + x, 5 + y, 4 + x, 9 + y);
        g.drawLine(5 + x, 7 + y, 9 + x, 3 + y);
        g.drawLine(5 + x, 8 + y, 9 + x, 4 + y);
        g.setColor(saved);
      }
    };
  }
  public static Icon getMenuItemArrowIcon()
  {
    return new DummyIcon();
  }
  
  /**
   * Returns a new instance of a 4 x 8 icon showing a small black triangle that
   * points to the right.  This is displayed in menu items that have a 
   * sub menu.
   * 
   * @return The icon.
   */
  public static Icon getMenuArrowIcon()
  {
    return new Icon()
      {
	public int getIconHeight()
	{
	  return 8;
	}
	public int getIconWidth()
	{
	  return 4;
	}
	public void paintIcon(Component c, Graphics g, int x, int y)
	{
	  Color saved = g.getColor();
	  g.setColor(Color.BLACK);
          for (int i = 0; i < 4; i++)
            g.drawLine(x + i, y + i, x + i, y + 7 - i);
	  g.setColor(saved);
	}
      };
  }

  /**
   * Returns an icon for CheckBoxes in the BasicLookAndFeel. CheckBox icons
   * in the Basic L&amp;F are empty and have a size of 13x13 pixels.
   * This method returns a shared single instance of this icon.
   *
   * @return an icon for CheckBoxes in the BasicLookAndFeel
   */
  public static Icon getCheckBoxIcon()
  {
    if (checkBoxIcon == null)
      checkBoxIcon = new CheckBoxIcon();
    return checkBoxIcon;
  }

  /**
   * Returns an icon for RadioButtons in the BasicLookAndFeel. RadioButton
   * icons in the Basic L&amp;F are empty and have a size of 13x13 pixels.
   * This method returns a shared single instance of this icon.
   *
   * @return an icon for RadioButtons in the BasicLookAndFeel
   */
  public static Icon getRadioButtonIcon()
  {
    if (radioButtonIcon == null)
      radioButtonIcon = new RadioButtonIcon();
    return radioButtonIcon;
  }

  /**
   * Creates and returns an icon used when rendering {@link JCheckBoxMenuItem}
   * components.
   * 
   * @return An icon.
   */
  public static Icon getCheckBoxMenuItemIcon()
  {
    return new CheckBoxMenuItemIcon();
  }
  
  public static Icon getRadioButtonMenuItemIcon()
  {
    return getRadioButtonIcon();
  }
  
  public static Icon createEmptyFrameIcon()
  {
    return new DummyIcon();
  }
} // class BasicIconFactory
