/* MetalIconFactory.java --
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.io.Serializable;

import javax.swing.Icon;
import javax.swing.JSlider;

/**
 * Creates icons for the {@link MetalLookAndFeel}.
 */
public class MetalIconFactory implements Serializable 
{

  /** A constant representing "dark". */
  public static final boolean DARK = false;
    
  /** A constant representing "light". */
  public static final boolean LIGHT = true;
    
  /**
   * An icon representing a file (drawn as a piece of paper with the top-right
   * corner turned down).
   */
  public static class FileIcon16 implements Icon, Serializable 
  {
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return 16;
    }

    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 16;
    }
    
    /**
     * Paints the icon at the location (x, y).
     * 
     * @param c  the component.
     * @param g  the graphics context.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      // TODO: pick up appropriate UI colors
      g.setColor(Color.black);
      g.drawLine(x, y, x + 9, y);            
      g.drawLine(x, y + 1, x, y + 15);            
      g.drawLine(x, y + 15, x + 12, y + 15);            
      g.drawLine(x + 12, y + 15, x + 12, y + 6);            
      g.drawLine(x + 12, y + 6, x + 9, y);           

      g.drawLine(x + 7, y + 2, x + 11, y + 6);
      g.drawLine(x + 8, y + 1, x + 9, y + 1);

      g.setColor(new Color(204, 204, 255));
      g.drawLine(x + 1, y + 1, x + 7, y + 1);            
      g.drawLine(x + 1, y + 1, x + 1, y + 14);            
      g.drawLine(x + 1, y + 14, x + 11, y + 14);            
      g.drawLine(x + 11, y + 14, x + 11, y + 7);            
      g.drawLine(x + 8, y + 2, x + 10, y + 4);
    }
    
    /**
     * Returns the additional height (???).
     * 
     * @return The additional height.
     */
    public int getAdditionalHeight() 
    {
      return 0;
    }
        
    /**
     * Returns the shift (???).
     * 
     * @return The shift.
     */
    public int getShift() 
    {
      return 0;
    }
        
  }
    
  /**
   * An icon representing a folder.
   */
  public static class FolderIcon16 implements Icon, Serializable 
  {
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() {
      return 16;
    }
    
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 16;
    }

    /**
     * Paints the icon at the location (x, y).
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      // TODO: pick up appropriate UI colors
      g.setColor(Color.black);
      g.drawLine(x, y + 3, x, y + 12);
      g.drawLine(x, y + 12, x + 15, y + 12);
      g.drawLine(x + 15, y + 12, x + 15, y + 2);
      g.drawLine(x + 14, y + 3, x + 9, y + 3);
      g.drawLine(x + 8, y + 2, x + 1, y + 2);
      g.setColor(new Color(204, 204, 255));
      g.fillRect(x + 2, y + 4, 7, 8);
      g.fillRect(x + 9, y + 5, 6, 7);
      g.setColor(new Color(102, 102, 153));
      g.drawLine(x + 9, y + 2, x + 14, y + 2);
      g.setColor(new Color(50, 50, 120));
      g.drawLine(x + 9, y + 1, x + 15, y + 1);
      g.drawLine(x + 10, y, x + 15, y);
    }
    
    /**
     * Returns the additional height (???).
     * 
     * @return The additional height.
     */
    public int getAdditionalHeight() 
    {
      return 0;
    }
    
    /**
     * Returns the shift (???).
     * 
     * @return The shift.
     */
    public int getShift() 
    {
      return 0;
    }
        
  }
   
    /**
   * The icon used to display the thumb control on a horizontally oriented
   * {@link JSlider} component.
   */
  private static class HorizontalSliderThumbIcon 
      implements Icon, Serializable 
  {

    /**
     * Creates a new instance.
     */
    public HorizontalSliderThumbIcon() 
    {
    }
    
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return 15;
    }
    
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 16;
    }
    
    /**
     * Paints the icon, taking into account whether or not the component has 
     * the focus.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      boolean focus = false;
      if (c != null) 
        focus = c.hasFocus();    
      // TODO: pick up the colors from the look and feel
      
      // draw the outline
      g.setColor(Color.black);
      g.drawLine(x + 1, y, x + 13, y);
      g.drawLine(x + 14, y + 1, x + 14, y + 7);
      g.drawLine(x + 14, y + 8, x + 7, y + 15);
      g.drawLine(x + 6, y + 14, x, y + 8);
      g.drawLine(x, y + 7, x, y + 1);
      
      // fill the icon
      g.setColor(focus ? new Color(153, 153, 204) : new Color(204, 204, 204));  // medium
      g.fillRect(x + 2, y + 2, 12, 7);
      g.drawLine(x + 2, y + 9, x + 12, y + 9);
      g.drawLine(x + 3, y + 10, x + 11, y + 10);
      g.drawLine(x + 4, y + 11, x + 10, y + 11);
      g.drawLine(x + 5, y + 12, x + 9, y + 12);
      g.drawLine(x + 6, y + 13, x + 8, y + 13);
      g.drawLine(x + 7, y + 14, x + 7, y + 14);
      
      // draw highlights
      g.setColor(focus ? new Color(204, 204, 255) : new Color(255, 255, 255));  // light
      g.drawLine(x + 1, y + 1, x + 13, y + 1);
      g.drawLine(x + 1, y + 2, x + 1, y + 8);
      g.drawLine(x + 2, y + 2, x + 2, y + 2);
      g.drawLine(x + 6, y + 2, x + 6, y + 2);
      g.drawLine(x + 10, y + 2, x + 10, y + 2);

      g.drawLine(x + 4, y + 4, x + 4, y + 4);
      g.drawLine(x + 8, y + 4, x + 8, y + 4);

      g.drawLine(x + 2, y + 6, x + 2, y + 6);
      g.drawLine(x + 6, y + 6, x + 6, y + 6);
      g.drawLine(x + 10, y + 6, x + 10, y + 6);

      // draw dots
      g.setColor(focus ? new Color(102, 102, 153) : Color.black);                 // dark
      g.drawLine(x + 3, y + 3, x + 3, y + 3);
      g.drawLine(x + 7, y + 3, x + 7, y + 3);
      g.drawLine(x + 11, y + 3, x + 11, y + 3);

      g.drawLine(x + 5, y + 5, x + 5, y + 5);
      g.drawLine(x + 9, y + 5, x + 9, y + 5);

      g.drawLine(x + 3, y + 7, x + 3, y + 7);
      g.drawLine(x + 7, y + 7, x + 7, y + 7);
      g.drawLine(x + 11, y + 7, x + 11, y + 7);

    }        
  }
  
  /**
   * The icon used to display the thumb control on a horizontally oriented
   * {@link JSlider} component.
   */
  private static class VerticalSliderThumbIcon implements Icon, Serializable 
  {
    /**
     * Creates a new instance.
     */
    public VerticalSliderThumbIcon() 
    {
    }
    
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return 16;
    }
    
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 15;
    }
    
    /**
     * Paints the icon taking into account whether the slider control has the
     * focus or not.
     * 
     * @param c  the slider (must be a non-<code>null</code> instance of
     *           {@link JSlider}.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      boolean focus = false;
      if (c != null) 
        focus = c.hasFocus();    
      // TODO: pick up the colors from the look and feel
      
      // draw the outline
      g.setColor(Color.black);
      g.drawLine(x + 1, y, x + 7, y);
      g.drawLine(x + 8, y, x + 15, y + 7);
      g.drawLine(x + 14, y + 8, x + 8, y + 14);
      g.drawLine(x + 8, y + 14, x + 1, y + 14);
      g.drawLine(x, y + 13, x, y + 1);
      
      // fill the icon
      g.setColor(focus ? new Color(153, 153, 204) : new Color(204, 204, 204));  // medium
      g.fillRect(x + 2, y + 2, 7, 12);
      g.drawLine(x + 9, y + 2, x + 9, y + 12);
      g.drawLine(x + 10, y + 3, x + 10, y + 11);
      g.drawLine(x + 11, y + 4, x + 11, y + 10);
      g.drawLine(x + 12, y + 5, x + 12, y + 9);
      g.drawLine(x + 13, y + 6, x + 13, y + 8);
      g.drawLine(x + 14, y + 7, x + 14, y + 7);
      
      // draw highlights
      g.setColor(focus ? new Color(204, 204, 255) : new Color(255, 255, 255));  // light
      g.drawLine(x + 1, y + 1, x + 8, y + 1);
      g.drawLine(x + 1, y + 2, x + 1, y + 13);
      g.drawLine(x + 2, y + 2, x + 2, y + 2);
      g.drawLine(x + 2, y + 6, x + 2, y + 6);
      g.drawLine(x + 2, y + 10, x + 2, y + 10);

      g.drawLine(x + 4, y + 4, x + 4, y + 4);
      g.drawLine(x + 4, y + 8, x + 4, y + 8);

      g.drawLine(x + 6, y + 2, x + 6, y + 2);
      g.drawLine(x + 6, y + 6, x + 6, y + 6);
      g.drawLine(x + 6, y + 10, x + 6, y + 10);

      // draw dots
      g.setColor(focus ? new Color(102, 102, 153) : Color.black);                 // dark
      g.drawLine(x + 3, y + 3, x + 3, y + 3);
      g.drawLine(x + 3, y + 7, x + 3, y + 7);
      g.drawLine(x + 3, y + 11, x + 3, y + 11);

      g.drawLine(x + 5, y + 5, x + 5, y + 5);
      g.drawLine(x + 5, y + 9, x + 5, y + 9);

      g.drawLine(x + 7, y + 3, x + 7, y + 3);
      g.drawLine(x + 7, y + 7, x + 7, y + 7);
      g.drawLine(x + 7, y + 11, x + 7, y + 11);
    }        
  }
  
  /**
   * A tree control icon.  This icon can be in one of two states: expanded and
   * collapsed.
   */
  public static class TreeControlIcon implements Icon, Serializable 
  {
    
    /** ???. */
    protected boolean isLight;
    
    /** A flag that controls whether or not the icon is collapsed. */
    private boolean collapsed;
    
    /**
     * Creates a new icon.
     * 
     * @param isCollapsed  a flag that controls whether the icon is in the
     *                     collapsed state or the expanded state.
     */
    public TreeControlIcon(boolean isCollapsed) 
    {
      collapsed = isCollapsed;
    }
    
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return 18;
    }
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 18;
    }
    
    /**
     * Paints the icon at the location (x, y).
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      x = x + 5;
      y = y + 5;
      if (collapsed) 
      {
        // TODO: pick up appropriate UI colors
        g.setColor(Color.black);
        g.drawLine(x + 2, y, x + 5, y);
        g.drawLine(x + 6, y + 1, x + 7, y + 2);
        g.fillRect(x + 7, y + 3, 5, 2);
        g.drawLine(x + 7, y + 5, x + 6, y + 6);
        g.drawLine(x + 1, y + 1, x + 1, y + 1);
        g.drawLine(x, y + 2, x, y + 5);
        g.drawLine(x + 1, y + 6, x + 1, y + 6);
        g.drawLine(x + 2, y + 7, x + 5, y + 7);
        g.fillRect(x + 3, y + 3, 2, 2);

        g.setColor(new Color(204, 204, 255));
        g.drawLine(x + 3, y + 2, x + 4, y + 2);
        g.drawLine(x + 2, y + 3, x + 2, y + 4);
        g.drawLine(x + 3, y + 5, x + 3, y + 5);
        g.drawLine(x + 5, y + 3, x + 5, y + 3);
        
        g.setColor(new Color(153, 153, 204));
        g.drawLine(x + 2, y + 2, x + 2, y + 2);
        g.drawLine(x + 2, y + 5, x + 2, y + 5);
        g.drawLine(x + 2, y + 6, x + 5, y + 6);
        g.drawLine(x + 5, y + 2, x + 5, y + 2);
        g.drawLine(x + 6, y + 2, x + 6, y + 5);
        
        g.setColor(new Color(102, 102, 153));
        g.drawLine(x + 2, y + 1, x + 5, y + 1);
        g.drawLine(x + 1, y + 2, x + 1, y + 5);
      }
      else
      {
        // TODO: pick up appropriate UI colors
        g.setColor(Color.black);
        g.drawLine(x + 2, y, x + 5, y);
        g.drawLine(x + 6, y + 1, x + 7, y + 2);
        g.drawLine(x + 7, y + 2, x + 7, y + 5);
        g.fillRect(x + 3, y + 7, 2, 5);
        g.drawLine(x + 7, y + 5, x + 6, y + 6);
        g.drawLine(x + 1, y + 1, x + 1, y + 1);
        g.drawLine(x, y + 2, x, y + 5);
        g.drawLine(x + 1, y + 6, x + 1, y + 6);
        g.drawLine(x + 2, y + 7, x + 5, y + 7);
        g.fillRect(x + 3, y + 3, 2, 2);

        g.setColor(new Color(204, 204, 255));
        g.drawLine(x + 3, y + 2, x + 4, y + 2);
        g.drawLine(x + 2, y + 3, x + 2, y + 4);
        g.drawLine(x + 3, y + 5, x + 3, y + 5);
        g.drawLine(x + 5, y + 3, x + 5, y + 3);
        
        g.setColor(new Color(153, 153, 204));
        g.drawLine(x + 2, y + 2, x + 2, y + 2);
        g.drawLine(x + 2, y + 5, x + 2, y + 5);
        g.drawLine(x + 2, y + 6, x + 5, y + 6);
        g.drawLine(x + 5, y + 2, x + 5, y + 2);
        g.drawLine(x + 6, y + 2, x + 6, y + 5);
        
        g.setColor(new Color(102, 102, 153));
        g.drawLine(x + 2, y + 1, x + 5, y + 1);
        g.drawLine(x + 1, y + 2, x + 1, y + 5);
      }
    } 
    
    /**
     * Simply calls {@link #paintIcon(Component, Graphics, int, int)}.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintMe(Component c, Graphics g, int x, int y) 
    {
      paintIcon(c, g, x, y);  
    }
  }
    
  /**
   * A tree folder icon.
   */
  public static class TreeFolderIcon extends FolderIcon16 
  {
    /**
     * Creates a new instance.
     */
    public TreeFolderIcon() 
    {     
    }
    
    /**
     * Returns the additional height (???).
     * 
     * @return The additional height.
     */
    public int getAdditionalHeight() 
    {
      return 2;
    }
    
    /**
     * Returns the shift (???).
     * 
     * @return The shift.
     */
    public int getShift() 
    {
      return -1;
    }
  }
    
  /**
   * A tree leaf icon.
   */
  public static class TreeLeafIcon extends FileIcon16 
  {
    /**
     * Creates a new instance.
     */
    public TreeLeafIcon() 
    {
    }
    
    /**
     * Returns the additional height (???).
     * 
     * @return The additional height.
     */
    public int getAdditionalHeight() 
    {
      return 4;
    }
    
    /**
     * Returns the shift (???).
     * 
     * @return The shift.
     */
    public int getShift() 
    {
      return 2;
    }
  }
    
  /**
   * Creates a new instance.  All the methods are static, so creating an 
   * instance isn't necessary.
   */
  public MetalIconFactory() 
  {   
  }
  
  /**
   * Returns the icon used to display the thumb for a horizontally oriented
   * {@link JSlider}.
   * 
   * @return The icon.
   */
  public static Icon getHorizontalSliderThumbIcon() 
  {
    return new HorizontalSliderThumbIcon();
  }
    
  /**
   * Returns the icon used to display the thumb for a vertically oriented
   * {@link JSlider}.
   * 
   * @return The icon.
   */
  public static Icon getVerticalSliderThumbIcon() 
  {
    return new VerticalSliderThumbIcon();
  }
    
  /**
   * Creates and returns a new tree folder icon.
   * 
   * @return A new tree folder icon.
   */  
  public static Icon getTreeFolderIcon() 
  {
    return new TreeFolderIcon();
  }
    
  /**
   * Creates and returns a new tree leaf icon.
   * 
   * @return A new tree leaf icon.
   */
  public static Icon getTreeLeafIcon() 
  {
    return new TreeLeafIcon();
  }
  
  /**
   * Creates and returns a tree control icon.
   * 
   * @param isCollapsed  a flag that controls whether the icon is in the 
   *                     collapsed or expanded state.
   * 
   * @return A tree control icon.
   */
  public static Icon getTreeControlIcon(boolean isCollapsed) 
  {
    return new TreeControlIcon(isCollapsed);
  }

}
