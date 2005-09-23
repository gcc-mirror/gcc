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
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JInternalFrame;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSlider;
import javax.swing.plaf.UIResource;


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
   * An icon displayed for {@link JCheckBoxMenuItem} components.
   */
  private static class CheckBoxMenuItemIcon implements Icon, Serializable 
  {
    /**
     * Creates a new icon instance.
     */
    public CheckBoxMenuItemIcon() 
    {
    }
      
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon (10 pixels).
     */
    public int getIconWidth() 
    {
      return 10;
    }
    
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon (10 pixels).
     */
    public int getIconHeight() 
    {
      return 10;
    }
    
    /**
     * Paints the icon.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      JCheckBoxMenuItem item = (JCheckBoxMenuItem) c;
        
      if (item.isArmed())
        g.setColor(MetalLookAndFeel.getBlack());
      else
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
      g.drawLine(x, y, x + 8, y);
      g.drawLine(x, y + 1, x, y + 8);
      g.drawLine(x + 2, y + 8, x + 8, y + 8);
      g.drawLine(x + 8, y + 2, x + 8, y + 7);
      
      g.setColor(MetalLookAndFeel.getWhite());
      g.drawLine(x + 1, y + 1, x + 7, y + 1);
      g.drawLine(x + 1, y + 2, x + 1, y + 7);
      g.drawLine(x + 1, y + 9, x + 9, y + 9);
      g.drawLine(x + 9, y + 1, x + 9, y + 8);

      // if the item is selected, we should draw a tick
      if (item.isSelected())
      {
        g.setColor(MetalLookAndFeel.getBlack());
        g.fillRect(x + 2, y + 2, 2, 5);
        for (int i = 0; i < 6; i++)
          g.drawLine(x + 8 - i, y + i, x + 9 - i, y + i);
      }

    }        
  }

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
   * An {@link Icon} implementation for {@link JCheckBox}es in the
   * Metal Look &amp; Feel.
   *
   * @author Roman Kennke (roman@kennke.org)
   */
  static class RadioButtonIcon
    implements Icon, UIResource, Serializable
  {
    /**
     * Draws the check in the RadioButton.
     *
     * @param c the component to draw on
     * @param g the Graphics context to draw with
     */
    protected void drawCheck(Component c, Graphics g)
    {
      g.setColor(MetalLookAndFeel.getBlack());
      g.fillRect(4, 3, 4, 6);
      g.drawLine(3, 4, 3, 7);
      g.drawLine(8, 4, 8, 7);
    }

    /**
     * Returns the width of the icon in pixels.
     *
     * @return the width of the icon in pixels
     */
    public int getIconWidth()
    {
      return 13;
    }

    /**
     * Returns the height of the icon in pixels.
     *
     * @return the height of the icon in pixels
     */
    public int getIconHeight()
    {
      return 13;
    }

    /**
     * Paints the icon. This first paints the border of the RadioButton and
     * if the CheckBox is selected it calls {@link #drawCheck} to draw
     * the check.
     *
     * @param c the Component to draw on (gets casted to JCheckBox)
     * @param g the Graphics context to draw with
     * @param x the X position
     * @param y the Y position
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      Color dark = MetalLookAndFeel.getControlDarkShadow();
      Color light = MetalLookAndFeel.getWhite();
      g.translate(x, y);

      // The light 'circle'
      g.setColor(light);
      g.drawLine(4, 1, 10, 1);
      g.drawLine(2, 2, 3, 2);
      g.drawLine(8, 2, 11, 2);
      g.drawLine(2, 3, 2, 3);
      g.drawLine(11, 2, 11, 9);
      g.drawLine(1, 4, 1, 7);
      g.drawLine(12, 4, 12, 7);
      g.drawLine(2, 8, 2, 11);
      g.drawLine(11, 8, 11, 9);
      g.drawLine(10, 10, 10, 10);
      g.drawLine(2, 11, 9, 11);
      g.drawLine(4, 12, 7, 12);

      // The dark 'circle'
      g.setColor(dark);
      g.drawLine(4, 0, 7, 0);
      g.drawLine(2, 1, 3, 1);
      g.drawLine(8, 1, 9, 1);
      g.drawLine(1, 2, 1, 3);
      g.drawLine(10, 2, 10, 3);
      g.drawLine(0, 4, 0, 7);
      g.drawLine(11, 4, 11, 7);
      g.drawLine(1, 8, 1, 9);
      g.drawLine(10, 8, 10, 9);
      g.drawLine(2, 10, 3, 10);
      g.drawLine(8, 10, 9, 10);
      g.drawLine(4, 11, 7, 11);

      JRadioButton rb = (JRadioButton) c;
      if (rb.isSelected())
        drawCheck(c, g);

      g.translate(-x, -y);
    }
  }

  /**
   * An icon displayed for {@link JRadioButtonMenuItem} components.
   */
  private static class RadioButtonMenuItemIcon 
      implements Icon, Serializable 
  {
    /**
     * Creates a new icon instance.
     */
    public RadioButtonMenuItemIcon() 
    {  
    }

    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return 10;
    }

    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight()   
    {
      return 10;
    }

    /**
     * Paints the icon.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color savedColor = g.getColor();
      JRadioButtonMenuItem item = (JRadioButtonMenuItem) c;
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 2, y, x + 6, y);
      g.drawLine(x + 7, y + 1, x + 7, y + 1);
      g.drawLine(x + 8, y + 2, x + 8, y + 6);
      g.drawLine(x + 7, y + 7, x + 7, y + 7);
      g.drawLine(x + 2, y + 8, x + 6, y + 8);
      g.drawLine(x + 1, y + 7, x + 1, y + 7);
      g.drawLine(x, y + 2, x, y + 6);
      g.drawLine(x + 1, y + 1, x + 1, y + 1);
      
      if (item.isSelected())
        {
          g.drawLine(x + 3, y + 2, x + 5, y + 2);
          g.fillRect(x + 2, y + 3, 5, 3);
          g.drawLine(x + 3, y + 6, x + 5, y + 6);
        }

      // highlight
      g.setColor(MetalLookAndFeel.getControlHighlight());
      g.drawLine(x + 3, y + 1, x + 6, y + 1);
      g.drawLine(x + 8, y + 1, x + 8, y + 1);
      g.drawLine(x + 9, y + 2, x + 9, y + 7);
      g.drawLine(x + 8, y + 8, x + 8, y + 8);
      g.drawLine(x + 2, y + 9, x + 7, y + 9);
      g.drawLine(x + 1, y + 8, x + 1, y + 8);
      g.drawLine(x + 1, y + 3, x + 1, y + 6);
      g.drawLine(x + 2, y + 2, x + 2, y + 2);
      g.setColor(savedColor);
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
   * An icon used for the 'close' button in the title frame of a 
   * {@link JInternalFrame}.
   */
  private static class InternalFrameCloseIcon implements Icon, Serializable 
  {
    /** The icon size in pixels. */
    private int size;
    
    /**
     * Creates a new icon.
     * 
     * @param size  the icon size (width and height) in pixels.
     */
    public InternalFrameCloseIcon(int size) 
    {
      this.size = size;
    }
    
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return size;
    }
    
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return size;
    }
    
    /**
     * Paints the icon.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      // draw the gray areas first
      g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      g.drawLine(x + 1, y + 1, x + 13, y + 1);
      g.drawLine(x + 1, y + 2, x + 1, y + 12);
      g.drawLine(x + 1, y + 13, x + 13, y + 13);
      g.drawLine(x + 13, y + 2, x + 13, y + 12);
      
      g.fillRect(x + 4, y + 4, 2, 2);
      g.fillRect(x + 4, y + 9, 2, 2);
      g.fillRect(x + 9, y + 4, 2, 2);
      g.fillRect(x + 9, y + 9, 2, 2);
      g.fillRect(x + 5, y + 5, 5, 5);
      
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x, y, x + 13, y);
      g.drawLine(x, y + 1, x, y + 13);
      g.drawLine(x + 3, y + 4, x + 4, y + 3);
      g.drawLine(x + 3, y + 9, x + 5, y + 7);
      g.drawLine(x + 7, y + 5, x + 9, y + 3);
      
      g.drawLine(x + 12, y + 3, x + 12, y + 11);
      g.drawLine(x + 3, y + 12, x + 12, y + 12);
      
      g.setColor(MetalLookAndFeel.getWhite());
      g.drawLine(x + 1, y + 14, x + 14, y + 14);
      g.drawLine(x + 14, y + 1, x + 14, y + 14);
      
      g.drawLine(x + 5, y + 10, x + 5, y + 10);
      g.drawLine(x + 6, y + 9, x + 7, y + 9);
      g.drawLine(x + 10, y + 5, x + 10, y + 5);
      g.drawLine(x + 9, y + 6, x + 9, y + 7);
      g.drawLine(x + 10, y + 10, x + 11, y + 10);
      g.drawLine(x + 10, y + 11, x + 10, y + 11);
    }        
  }

  /**
   * The icon displayed at the top-left corner of a {@link JInternalFrame}.
   */
  private static class InternalFrameDefaultMenuIcon 
      implements Icon, Serializable 
  {
       
    /**
     * Creates a new instance.
     */
    public InternalFrameDefaultMenuIcon() 
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
      return 16;
    }
    
    /**
     * Paints the icon at the specified location.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      g.setColor(new Color(102, 102, 153));
      g.fillRect(x + 1, y, 14, 2);
      g.fillRect(x, y + 1, 2, 14);
      g.fillRect(x + 1, y + 14, 14, 2);
      g.fillRect(x + 14, y + 1, 2, 14);
      g.drawLine(x + 2, y + 5, x + 14, y + 5);
      
      g.setColor(new Color(204, 204, 255));
      g.fillRect(x + 2, y + 2, 12, 3);
      
      g.setColor(new Color(102, 102, 153));
      g.drawLine(x + 3, y + 3, x + 3, y + 3);
      g.drawLine(x + 6, y + 3, x + 6, y + 3);
      g.drawLine(x + 9, y + 3, x + 9, y + 3);
      g.drawLine(x + 12, y + 3, x + 12, y + 3);

      g.setColor(Color.white);
      g.fillRect(x + 2, y + 6, 12, 8);
      g.drawLine(x + 2, y + 2, x + 2, y + 2);
      g.drawLine(x + 5, y + 2, x + 5, y + 2);
      g.drawLine(x + 8, y + 2, x + 8, y + 2);
      g.drawLine(x + 11, y + 2, x + 11, y + 2);
    }        
  }

  /**
   * An icon used in the title frame of a {@link JInternalFrame}.  When you 
   * maximise an internal frame, this icon will replace the 'maximise' icon to
   * provide a 'restore' option.
   */
  private static class InternalFrameAltMaximizeIcon 
      implements Icon, Serializable 
  {
    /** The icon size in pixels. */
    private int size;
    
    /**
     * Creates a new icon.
     * 
     * @param size  the icon size in pixels.
     */
    public InternalFrameAltMaximizeIcon(int size) 
    {
      this.size = size;
    }
    
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return size;
    }
    
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return size;
    }
    
    /**
     * Paints the icon at the specified location.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color color = MetalLookAndFeel.getControlDarkShadow();
      if (c instanceof JInternalFrame)
        {
          JInternalFrame f = (JInternalFrame) c;
          if (f.isSelected())
            color = MetalLookAndFeel.getPrimaryControlShadow();
        }
      g.setColor(color);
      g.drawLine(x + 12, y + 1, x + 13, y + 1);
      g.drawLine(x + 11, y + 2, x + 12, y + 2);
      g.drawLine(x + 10, y + 3, x + 11, y + 3);
      g.drawLine(x + 8, y + 2, x + 8, y + 3);
      g.fillRect(x + 8, y + 4, 3, 3);
      g.drawLine(x + 11, y + 6, x + 12, y + 6);
      
      g.drawLine(x + 1, y + 5, x + 5, y + 5);
      g.drawLine(x + 1, y + 6, x + 1, y + 12);
      g.drawLine(x + 9, y + 9, x + 9, y + 12);
      g.drawLine(x + 1, y + 13, x + 9, y + 13);
      
      g.drawLine(x + 2, y + 12, x + 2, y + 12);
      
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 12, y, x + 9, y + 3);
      g.drawLine(x + 7, y + 1, x + 8, y + 1);
      g.drawLine(x + 7, y + 2, x + 7, y + 6);
      g.drawLine(x + 11, y + 5, x + 12, y + 5);
      g.drawLine(x, y + 4, x + 5, y + 4);
      g.drawLine(x, y + 5, x, y + 13);
      g.drawLine(x + 3, y + 12, x + 8, y + 12);
      g.drawLine(x + 8, y + 8, x + 8, y + 11);
      g.drawLine(x + 9, y + 8, x + 9, y + 8);
      
      g.setColor(MetalLookAndFeel.getWhite());
      g.drawLine(x + 9, y + 2, x + 9, y + 2);
      g.drawLine(x + 11, y + 4, x + 13, y + 2);
      g.drawLine(x + 13, y + 6, x + 13, y + 6);
      g.drawLine(x + 8, y + 7, x + 13, y + 7);
      g.drawLine(x + 6, y + 5, x + 6, y + 5);
      g.drawLine(x + 2, y + 6, x + 6, y + 6);
      g.drawLine(x + 2, y + 6, x + 2, y + 11);
      g.drawLine(x + 10, y + 8, x + 10, y + 13);
      g.drawLine(x + 1, y + 14, x + 10, y + 14);
    }        
  }
  
  /**
   * An icon used for the 'maximize' button in the title frame of a 
   * {@link JInternalFrame}.
   */
  private static class InternalFrameMaximizeIcon 
      implements Icon, Serializable 
  {
    
    /**
     * Creates a new instance.
     */
    public InternalFrameMaximizeIcon() 
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
      return 16;
    }
    
    /**
     * Paints the icon at the specified location.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color color = MetalLookAndFeel.getControlDarkShadow();
      if (c instanceof JInternalFrame)
        {
          JInternalFrame f = (JInternalFrame) c;
          if (f.isSelected())
            color = MetalLookAndFeel.getPrimaryControlShadow();
        }
      g.setColor(color);
      g.drawLine(x + 9, y + 1, x + 10, y + 1);
      g.fillRect(x + 11, y + 1, 3, 3);
      g.fillRect(x + 12, y + 4, 2, 2);
      g.drawLine(x + 10, y + 3, x + 10, y + 3);
      g.drawLine(x + 9, y + 4, x + 10, y + 4);
      g.drawLine(x + 1, y + 5, x + 9, y + 5);
      g.drawLine(x + 1, y + 6, x + 1, y + 12);
      g.drawLine(x + 9, y + 6, x + 9, y + 12);
      g.drawLine(x + 1, y + 13, x + 9, y + 13);
      
      // fill
      g.drawLine(x + 7, y + 6, x + 8, y + 6);
      g.drawLine(x + 6, y + 7, x + 8, y + 7);
      g.drawLine(x + 5, y + 8, x + 6, y + 8);
      g.drawLine(x + 4, y + 9, x + 5, y + 9);
      g.drawLine(x + 3, y + 10, x + 4, y + 10);
      g.drawLine(x + 2, y + 11, x + 3, y + 11);
      g.drawLine(x + 2, y + 12, x + 4, y + 12);
      g.drawLine(x + 8, y + 8, x + 8, y + 8);
      
      // draw black
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 8, y, x + 13, y);
      g.drawLine(x + 8, y + 1, x + 8, y + 1);
      g.drawLine(x + 10, y + 2, x + 9, y + 3);
      g.drawLine(x, y + 4, x + 8, y + 4);
      g.drawLine(x, y + 5, x, y + 13);
      
      g.drawLine(x + 2, y + 10, x + 6, y + 6);
      g.drawLine(x + 8, y + 9, x + 8, y + 11);
      g.drawLine(x + 5, y + 12, x + 8, y + 12);
      
      // draw white
      g.setColor(MetalLookAndFeel.getWhite());
      g.drawLine(x + 2, y + 6, x + 5, y + 6);
      g.drawLine(x + 2, y + 7, x + 2, y + 9);
      g.drawLine(x + 4, y + 11, x + 7, y + 8);
      
      g.drawLine(x + 1, y + 14, x + 10, y + 14);
      g.drawLine(x + 10, y + 5, x + 10, y + 13);
      
      g.drawLine(x + 9, y + 2, x + 9, y + 2);
      g.drawLine(x + 11, y + 4, x + 11, y + 5);
      g.drawLine(x + 13, y + 6, x + 14, y + 6);
      g.drawLine(x + 14, y + 1, x + 14, y + 5);
    }        
  }

  /**
   * An icon used in the title frame of a {@link JInternalFrame}.
   */
  private static class InternalFrameMinimizeIcon 
      implements Icon, Serializable 
  {
  
    /**
     * Creates a new instance.
     */
    public InternalFrameMinimizeIcon() 
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
      return 16;
    }
    
    /**
     * Paints the icon at the specified location.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x coordinate.
     * @param y  the y coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color color = MetalLookAndFeel.getControlDarkShadow();
      if (c instanceof JInternalFrame)
        {
          JInternalFrame f = (JInternalFrame) c;
          if (f.isSelected())
            color = MetalLookAndFeel.getPrimaryControlShadow();
        }
      g.setColor(color);
      g.drawLine(x + 12, y + 1, x + 13, y + 1);
      g.drawLine(x + 11, y + 2, x + 12, y + 2);
      g.drawLine(x + 10, y + 3, x + 11, y + 3);
      g.drawLine(x + 8, y + 2, x + 8, y + 3);
      g.fillRect(x + 8, y + 4, 3, 3);
      g.drawLine(x + 11, y + 6, x + 12, y + 6);
      
      g.drawLine(x + 1, y + 8, x + 6, y + 8);
      g.drawLine(x + 1, y + 9, x + 1, y + 12);
      g.drawLine(x + 6, y + 9, x + 6, y + 12);
      g.drawLine(x + 1, y + 13, x + 6, y + 13);
      
      g.drawLine(x + 5, y + 9, x + 5, y + 9);
      g.drawLine(x + 2, y + 12, x + 2, y + 12);
      
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 12, y, x + 9, y + 3);
      g.drawLine(x + 7, y + 1, x + 8, y + 1);
      g.drawLine(x + 7, y + 2, x + 7, y + 6);
      g.drawLine(x, y + 7, x + 6, y + 7);
      g.drawLine(x, y + 8, x, y + 13);
      g.drawLine(x + 3, y + 12, x + 5, y + 12);
      g.drawLine(x + 5, y + 10, x + 5, y + 11);
      g.drawLine(x + 11, y + 5, x + 12, y + 5);
      
      g.setColor(MetalLookAndFeel.getWhite());
      g.drawLine(x + 9, y + 2, x + 9, y + 2);
      g.drawLine(x + 11, y + 4, x + 13, y + 2);
      g.drawLine(x + 13, y + 6, x + 13, y + 6);
      g.drawLine(x + 8, y + 7, x + 13, y + 7);
      g.drawLine(x + 2, y + 9, x + 4, y + 9);
      g.drawLine(x + 2, y + 10, x + 2, y + 11);
      g.drawLine(x + 7, y + 9, x + 7, y + 13);
      g.drawLine(x + 1, y + 14, x + 7, y + 14);
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
    
  /** The cached RadioButtonIcon instance. */
  private static RadioButtonIcon radioButtonIcon;

  /**
   * Creates a new instance.  All the methods are static, so creating an 
   * instance isn't necessary.
   */
  public MetalIconFactory() 
  {   
  }

  /**
   * Returns an icon for use when rendering the {@link JCheckBox} component.
   * 
   * @return A check box icon.
   * 
   * @since 1.3
   */
  public static Icon getCheckBoxIcon() 
  {
    return new MetalCheckBoxIcon();
  }
  
  /**
   * Returns an icon for use when rendering the {@link JCheckBoxMenuItem} 
   * component.
   * 
   * @return An icon.
   */
  public static Icon getCheckBoxMenuItemIcon() 
  {
    return new CheckBoxMenuItemIcon();
  }

  /**
   * Returns an icon for RadioButtons in the Metal L&amp;F.
   *
   * @return an icon for RadioButtons in the Metal L&amp;F
   */
  public static Icon getRadioButtonIcon()
  {
    if (radioButtonIcon == null)
      radioButtonIcon = new RadioButtonIcon();
    return radioButtonIcon;
  }

  /**
   * Creates a new instance of the icon used in a {@link JRadioButtonMenuItem}.
   * 
   * @return A new icon instance.
   */
  public static Icon getRadioButtonMenuItemIcon() 
  {
    return new RadioButtonMenuItemIcon();
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
   * Creates a new icon used to represent the 'close' button in the title
   * pane of a {@link JInternalFrame}.
   * 
   * @param size  the icon size.
   * 
   * @return A close icon.
   */
  public static Icon getInternalFrameCloseIcon(int size) 
  {
    return new InternalFrameCloseIcon(size);
  }

  /**
   * Creates a new icon for the menu in a {@link JInternalFrame}.  This is the
   * icon displayed at the top left of the frame.
   * 
   * @return A menu icon.
   */
  public static Icon getInternalFrameDefaultMenuIcon() 
  {
    return new InternalFrameDefaultMenuIcon();
  }
  
  /**
   * Creates a new icon for the 'maximize' button in a {@link JInternalFrame}.
   * 
   * @param size  the icon size in pixels.
   * 
   * @return The icon.
   * 
   * @see #getInternalFrameAltMaximizeIcon(int)
   */
  public static Icon getInternalFrameMaximizeIcon(int size) 
  {
    return new InternalFrameMaximizeIcon();
  }
    
  /**
   * Returns the icon used for the minimize button in the frame title for a
   * {@link JInternalFrame}.
   * 
   * @param size  the icon size in pixels (ignored by this implementation).
   * 
   * @return The icon.
   */
  public static Icon getInternalFrameMinimizeIcon(int size) 
  {
    return new InternalFrameMinimizeIcon();
  }

  /**
   * Creates a new icon for the 'restore' button in a {@link JInternalFrame}
   * that has been maximised.
   * 
   * @param size  the icon size in pixels.
   * 
   * @return The icon.
   * 
   * @see #getInternalFrameMaximizeIcon(int)
   */
  public static Icon getInternalFrameAltMaximizeIcon(int size) 
  {
    return new InternalFrameAltMaximizeIcon(size);
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
