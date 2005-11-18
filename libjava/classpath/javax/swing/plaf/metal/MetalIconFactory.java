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

import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
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
      // Nothing to do here.
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
   * An icon used for the "detail view" button on a {@link JFileChooser} under
   * the {@link MetalLookAndFeel}.
   * 
   * @see MetalIconFactory#getFileChooserDetailViewIcon()
   */
  private static class FileChooserDetailViewIcon implements Icon, Serializable
  {

    /**
     * Creates a new icon.
     */
    public FileChooserDetailViewIcon() 
    {
      // Nothing to do here.
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
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color savedColor = g.getColor();
      g.setColor(MetalLookAndFeel.getBlack());

      // file 1 outline
      g.drawLine(x + 2, y + 2, x + 5, y + 2);
      g.drawLine(x + 6, y + 3, x + 6, y + 7);
      g.drawLine(x + 2, y + 7, x + 6, y + 7);
      g.drawLine(x + 2, y + 2, x + 2, y + 7);
      
      // file 2 outline
      g.drawLine(x + 2, y + 10, x + 5, y + 10);
      g.drawLine(x + 6, y + 11, x + 6, y + 15);
      g.drawLine(x + 2, y + 15, x + 6, y + 15);
      g.drawLine(x + 2, y + 10, x + 2, y + 15);

      // detail lines
      g.drawLine(x + 8, y + 5, x + 15, y + 5);
      g.drawLine(x + 8, y + 13, x + 15, y + 13);
      
      // fill files
      g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 3, y + 3, 3, 4);
      g.fillRect(x + 3, y + 11, 3, 4);
      
      // highlight files
      g.setColor(MetalLookAndFeel.getPrimaryControlHighlight());
      g.drawLine(x + 4, y + 4, x + 4, y + 5);
      g.drawLine(x + 4, y + 12, x + 4, y + 13);
      
      g.setColor(savedColor);
    }        
  }

  /**
   * An icon used for the "home folder" button on a {@link JFileChooser} under
   * the {@link MetalLookAndFeel}.
   * 
   * @see MetalIconFactory#getFileChooserHomeFolderIcon()
   */
  private static class FileChooserHomeFolderIcon implements Icon, Serializable
  {

    /**
     * Creates a new icon.
     */
    public FileChooserHomeFolderIcon() 
    {
      // Nothing to do here.
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
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {   
      Color savedColor = g.getColor();
      g.setColor(MetalLookAndFeel.getBlack());
      
      // roof
      g.drawLine(x + 1, y + 8, x + 8, y + 1);
      g.drawLine(x + 8, y + 1, x + 15, y + 8);
      
      // base of house
      g.drawLine(x + 3, y + 6, x + 3, y + 15);
      g.drawLine(x + 3, y + 15, x + 13, y + 15);
      g.drawLine(x + 13, y + 6, x + 13, y + 15);
      
      // door frame
      g.drawLine(x + 6, y + 9, x + 6, y + 15);
      g.drawLine(x + 6, y + 9, x + 10, y + 9);
      g.drawLine(x + 10, y + 9, x + 10, y + 15);
      
      // chimney
      g.drawLine(x + 11, y + 2, x + 11, y + 4);
      g.drawLine(x + 12, y + 2, x + 12, y + 5);
      
      g.setColor(MetalLookAndFeel.getControlDarkShadow());
      
      // roof paint
      int xx = x + 8;
      for (int i = 0; i < 4; i++)
        g.drawLine(xx - i, y + 2 + i, xx + i, y + 2 + i);
      g.fillRect(x + 4, y + 6, 9, 2);
      
      // door knob
      g.drawLine(x + 9, y + 12, x + 9, y + 12);
      
      // house paint
      g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.drawLine(x + 4, y + 8, x + 12, y + 8);
      g.fillRect(x + 4, y + 9, 2, 6);
      g.fillRect(x + 11, y + 9, 2, 6);
      
      g.setColor(savedColor);
    }        
  }
    
  /**
   * An icon used for the "list view" button on a {@link JFileChooser} under
   * the {@link MetalLookAndFeel}.
   * 
   * @see MetalIconFactory#getFileChooserListViewIcon()
   */
  private static class FileChooserListViewIcon implements Icon, Serializable 
  {
    /**
     * Creates a new icon.
     */
    public FileChooserListViewIcon() 
    {
      // Nothing to do here.
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
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color savedColor = g.getColor();
      g.setColor(MetalLookAndFeel.getBlack());

      // file 1 outline
      g.drawLine(x + 2, y + 2, x + 5, y + 2);
      g.drawLine(x + 6, y + 3, x + 6, y + 7);
      g.drawLine(x + 2, y + 7, x + 6, y + 7);
      g.drawLine(x + 2, y + 2, x + 2, y + 7);
      
      // file 2 outline
      g.drawLine(x + 2, y + 10, x + 5, y + 10);
      g.drawLine(x + 6, y + 11, x + 6, y + 15);
      g.drawLine(x + 2, y + 15, x + 6, y + 15);
      g.drawLine(x + 2, y + 10, x + 2, y + 15);
      
      // file 3 outline
      g.drawLine(x + 10, y + 2, x + 13, y + 2);
      g.drawLine(x + 14, y + 3, x + 14, y + 7);
      g.drawLine(x + 10, y + 7, x + 14, y + 7);
      g.drawLine(x + 10, y + 2, x + 10, y + 7);
      
      // file 4 outline
      g.drawLine(x + 10, y + 10, x + 13, y + 10);
      g.drawLine(x + 14, y + 11, x + 14, y + 15);
      g.drawLine(x + 10, y + 15, x + 14, y + 15);
      g.drawLine(x + 10, y + 10, x + 10, y + 15);
      
      g.drawLine(x + 8, y + 5, x + 8, y + 5);
      g.drawLine(x + 8, y + 13, x + 8, y + 13);
      g.drawLine(x + 16, y + 5, x + 16, y + 5);
      g.drawLine(x + 16, y + 13, x + 16, y + 13);
      
      // fill files
      g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 3, y + 3, 3, 4);
      g.fillRect(x + 3, y + 11, 3, 4);
      g.fillRect(x + 11, y + 3, 3, 4);
      g.fillRect(x + 11, y + 11, 3, 4);
      
      // highlight files
      g.setColor(MetalLookAndFeel.getPrimaryControlHighlight());
      g.drawLine(x + 4, y + 4, x + 4, y + 5);
      g.drawLine(x + 4, y + 12, x + 4, y + 13);
      g.drawLine(x + 12, y + 4, x + 12, y + 5);
      g.drawLine(x + 12, y + 12, x + 12, y + 13);

      g.setColor(savedColor);
    }        
  }
    
  /**
   * An icon used for the "new folder" button on a {@link JFileChooser} under
   * the {@link MetalLookAndFeel}.
   * 
   * @see MetalIconFactory#getFileChooserNewFolderIcon()
   */
  private static class FileChooserNewFolderIcon  implements Icon, Serializable
  {
    /** 
     * Creates a new icon.
     */
    public FileChooserNewFolderIcon() 
    {
      // Nothing to do here.
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
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {      
      Color savedColor = g.getColor();
      g.setColor(MetalLookAndFeel.getBlack());
      
      g.drawLine(x + 2, y + 5, x + 9, y + 5);
      g.drawLine(x + 10, y + 6, x + 15, y + 6);
      g.drawLine(x + 15, y + 5, x + 15, y + 14);
      g.drawLine(x + 2, y + 14, x + 15, y + 14);
      g.drawLine(x + 1, y + 6, x + 1, y + 14);
      
      g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      g.drawLine(x + 11, y + 3, x + 15, y + 3);
      g.drawLine(x + 10, y + 4, x + 15, y + 4);
      
      g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 3, y + 7, 7, 7);
      g.fillRect(x + 10, y + 8, 5, 6);
      g.drawLine(x + 10, y + 5, x + 14, y + 5);
      
      g.setColor(MetalLookAndFeel.getPrimaryControlHighlight());
      g.drawLine(x + 10, y + 7, x + 14, y + 7);
      g.drawLine(x + 2, y + 6, x + 9, y + 6);
      g.drawLine(x + 2, y + 6, x + 2, y + 13);
      g.setColor(savedColor);
    }        
  }

  /**
   * An icon used for the "up folder" button on a {@link JFileChooser} under
   * the {@link MetalLookAndFeel}.
   * 
   * @see MetalIconFactory#getFileChooserNewFolderIcon()
   */
  private static class FileChooserUpFolderIcon extends FileChooserNewFolderIcon
    implements Icon, Serializable 
  {
    /**
     * Creates a new icon.
     */
    public FileChooserUpFolderIcon() 
    {
      // Nothing to do here.
    }
    
    /**
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color savedColor = g.getColor();

      // draw the folder
      super.paintIcon(c, g, x, y);
      
      // now draw the up arrow
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 8, y + 9, x + 8, y + 16);
      int xx = x + 8;
      for (int i = 0; i < 4; i++)
        g.drawLine(xx - i, y + 9 + i, xx + i, y + 9 + i);
      g.setColor(savedColor);
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
     * Returns the height of the icon, in pixels.  The height returned is 
     * <code>16</code> plus the value returned by 
     * {@link #getAdditionalHeight()}.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 16 + getAdditionalHeight();
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
     * Returns the additional height for the icon.  The 
     * {@link #getIconHeight()} method adds this value to the icon height it
     * returns.  Subclasses can override this method to adjust the icon height.
     * 
     * @return The additional height (<code>0</code> unless overridden).
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
     * Returns the height of the icon, in pixels.  The height returned is 
     * <code>16</code> plus the value returned by 
     * {@link #getAdditionalHeight()}.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 16 + getAdditionalHeight();
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
     * Returns the additional height for the icon.  The 
     * {@link #getIconHeight()} method adds this value to the icon height it
     * returns.  Subclasses can override this method to adjust the icon height.
     * 
     * @return The additional height (<code>0</code> unless overridden).
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
   * An icon used by the {@link MetalInternalFrameUI} class when the frame
   * is displayed as a palette.
   * 
   * @since 1.3
   */
  public static class PaletteCloseIcon
    implements Icon, Serializable, UIResource
  {
    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return The width of the icon.
     */
    public int getIconWidth() 
    {
      return 7;
    }
    
    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return The height of the icon.
     */
    public int getIconHeight() 
    {
      return 7;
    }
    
    /**
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color savedColor = g.getColor();
      AbstractButton button = (AbstractButton) c;
      if (button.getModel().isPressed())
        g.setColor(MetalLookAndFeel.getBlack());
      else
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
      g.fillRect(x + 2, y + 2, 3, 3);
      g.drawLine(x + 1, y, x + 1, y + 2);
      g.drawLine(x, y + 1, x + 2, y + 1);
      g.drawLine(x + 5, y, x + 5, y + 2);
      g.drawLine(x + 4, y + 1, x + 6, y + 1);
      g.drawLine(x + 1, y + 4, x + 1, y + 6);
      g.drawLine(x, y + 5, x + 2, y + 5);
      g.drawLine(x + 5, y + 4, x + 5, y + 6);
      g.drawLine(x + 4, y + 5, x + 6, y + 5);
      g.setColor(MetalLookAndFeel.getControlHighlight());
      g.drawLine(x + 2, y + 6, x + 3, y + 5);
      g.drawLine(x + 5, y + 3, x + 6, y + 2);
      g.drawLine(x + 6, y + 6, x + 6, y + 6);
      g.setColor(savedColor);
    }        
  }
  
  /**
   * An {@link Icon} implementation for {@link JCheckBox}es in the
   * Metal Look &amp; Feel.
   *
   * @author Roman Kennke (roman@kennke.org)
   */
  static class RadioButtonIcon implements Icon, UIResource, Serializable
  {

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
     * Paints the icon, taking into account whether or not the component is
     * enabled, selected and/or armed.
     *
     * @param c the Component to draw on (must be an instance of 
     *          {@link JRadioButton})
     * @param g the Graphics context to draw with
     * @param x the X position
     * @param y the Y position
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      if (UIManager.get("RadioButton.gradient") != null)
        MetalUtils.paintGradient(g, x, y, getIconWidth(), getIconHeight(),
                              SwingConstants.VERTICAL, "RadioButton.gradient");

      Color savedColor = g.getColor();
      JRadioButton b = (JRadioButton) c;
      
      // draw outer circle
      if (b.isEnabled())
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
      else
        g.setColor(MetalLookAndFeel.getControlDisabled());
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
          g.setColor(MetalLookAndFeel.getControlShadow());
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
              g.setColor(MetalLookAndFeel.getWhite());
          
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
          g.setColor(MetalLookAndFeel.getWhite());
          
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
            g.setColor(MetalLookAndFeel.getBlack());
          else
            g.setColor(MetalLookAndFeel.getControlDisabled());
          g.drawLine(x + 4, y + 3, x + 7, y + 3);
          g.fillRect(x + 3, y + 4, 6, 4);
          g.drawLine(x + 4, y + 8, x + 7, y + 8);
        }
      g.setColor(savedColor);
    }        
  }

  /**
   * An icon displayed for {@link JRadioButtonMenuItem} components.
   */
  private static class RadioButtonMenuItemIcon implements Icon, Serializable 
  {
    /**
     * Creates a new icon instance.
     */
    public RadioButtonMenuItemIcon() 
    {
      // Nothing to do here.
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
  private static class HorizontalSliderThumbIcon  implements Icon, Serializable
  {

    /**
     * Creates a new instance.
     */
    public HorizontalSliderThumbIcon() 
    {
      // Nothing to do here.
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
      boolean enabled = false;
      boolean focus = false;
      if (c != null)
        {
          enabled = c.isEnabled();
          focus = c.hasFocus();    
        }
      
      // draw the outline
      if (enabled) 
        g.setColor(MetalLookAndFeel.getBlack());
      else
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
      g.drawLine(x + 1, y, x + 13, y);
      g.drawLine(x + 14, y + 1, x + 14, y + 7);
      g.drawLine(x + 14, y + 8, x + 7, y + 15);
      g.drawLine(x + 6, y + 14, x, y + 8);
      g.drawLine(x, y + 7, x, y + 1);
      
      // fill the icon
      if (focus)
        g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      else
        g.setColor(MetalLookAndFeel.getControl());
      g.fillRect(x + 1, y + 2, 13, 7);
      g.drawLine(x + 2, y + 9, x + 12, y + 9);
      g.drawLine(x + 3, y + 10, x + 11, y + 10);
      g.drawLine(x + 4, y + 11, x + 10, y + 11);
      g.drawLine(x + 5, y + 12, x + 9, y + 12);
      g.drawLine(x + 6, y + 13, x + 8, y + 13);
      g.drawLine(x + 7, y + 14, x + 7, y + 14);
      
      // if the slider is enabled, draw dots and highlights
      if (c.isEnabled())
        {
          if (focus)
            g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
          else
            g.setColor(MetalLookAndFeel.getBlack());
          g.drawLine(x + 3, y + 3, x + 3, y + 3);
          g.drawLine(x + 7, y + 3, x + 7, y + 3);
          g.drawLine(x + 11, y + 3, x + 11, y + 3);

          g.drawLine(x + 5, y + 5, x + 5, y + 5);
          g.drawLine(x + 9, y + 5, x + 9, y + 5);

          g.drawLine(x + 3, y + 7, x + 3, y + 7);
          g.drawLine(x + 7, y + 7, x + 7, y + 7);
          g.drawLine(x + 11, y + 7, x + 11, y + 7);

          // draw highlights
          if (focus)
            g.setColor(MetalLookAndFeel.getPrimaryControl());
          else
            g.setColor(MetalLookAndFeel.getWhite());
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
        }

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
     * @param c  the component (an {@link JInternalFrame} is expected).
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color savedColor = g.getColor();
      AbstractButton b = (AbstractButton) c;
      
      // fill the interior
      if (b.getModel().isPressed())
        // FIXME: also need to take into account whether the internal frame is
        // selected
        g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      else
        g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 2, y + 2, 10, 10);
      
      // draw the outline box and the cross
      if (b.getModel().isPressed())
        g.setColor(MetalLookAndFeel.getBlack());
      else
        {
          // FIXME: also need to take into account whether the internal frame is
          // selected
          boolean selected = true;
          if (selected)
            g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
          else
            g.setColor(MetalLookAndFeel.getControlDarkShadow());
        }
      g.drawLine(x + 1, y + 1, x + 13, y + 1);
      g.drawLine(x + 1, y + 2, x + 1, y + 12);
      g.drawLine(x + 1, y + 13, x + 13, y + 13);
      g.drawLine(x + 13, y + 2, x + 13, y + 12);
      g.drawLine(x + 2, y + 12, x + 2, y + 12);
      g.drawLine(x + 12, y + 2, x + 12, y + 2);
      
      g.fillRect(x + 4, y + 4, 2, 2);
      g.fillRect(x + 5, y + 5, 4, 4);
      g.drawLine(x + 9, y + 4, x + 10, y + 4);
      g.drawLine(x + 9, y + 4, x + 9, y + 5);
      g.drawLine(x + 4, y + 9, x + 4, y + 10);
      g.drawLine(x + 4, y + 9, x + 5, y + 9);
      g.drawLine(x + 9, y + 8, x + 9, y + 10);
      g.drawLine(x + 8, y + 9, x + 10, y + 9);
      
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
      
      if (!b.getModel().isPressed())
        {
          g.drawLine(x + 5, y + 10, x + 5, y + 10);
          g.drawLine(x + 6, y + 9, x + 7, y + 9);
          g.drawLine(x + 10, y + 5, x + 10, y + 5);
          g.drawLine(x + 9, y + 6, x + 9, y + 7);
          g.drawLine(x + 10, y + 10, x + 11, y + 10);
          g.drawLine(x + 10, y + 11, x + 10, y + 11);
        }
      g.setColor(savedColor);
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
      // Nothing to do here.
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
      Color savedColor = g.getColor();

      AbstractButton b = (AbstractButton) c;

      // fill the small box interior
      if (b.getModel().isPressed())
        g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      else
        g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 2, y + 6, 7, 7);
      
      
      if (b.getModel().isPressed())
        g.setColor(MetalLookAndFeel.getBlack());
      else
        g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
        
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
      g.drawLine(x + 10, y + 8, x + 10, y + 13);
      g.drawLine(x + 1, y + 14, x + 10, y + 14);
      
      if (!b.getModel().isPressed())
        {
          g.drawLine(x + 2, y + 6, x + 6, y + 6);
          g.drawLine(x + 2, y + 6, x + 2, y + 11);
        }
      
      g.setColor(savedColor);
    }        
  }
  
  /**
   * An icon used for the 'maximize' button in the title frame of a 
   * {@link JInternalFrame}.
   */
  private static class InternalFrameMaximizeIcon implements Icon, Serializable
  {
    
    /**
     * Creates a new instance.
     */
    public InternalFrameMaximizeIcon() 
    {
      // Nothing to do here.
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
      Color savedColor = g.getColor();
      
      AbstractButton b = (AbstractButton) c;
      
      // fill the interior
      if (b.getModel().isPressed())
        g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      else
        g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 2, y + 6, 7, 7);

      if (b.getModel().isPressed())
        g.setColor(MetalLookAndFeel.getBlack());
      else
        g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
          
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
      if (!b.getModel().isPressed())
        {
          g.drawLine(x + 2, y + 6, x + 5, y + 6);
          g.drawLine(x + 2, y + 7, x + 2, y + 9);
          g.drawLine(x + 4, y + 11, x + 7, y + 8);
        }
      
      g.drawLine(x + 1, y + 14, x + 10, y + 14);
      g.drawLine(x + 10, y + 5, x + 10, y + 13);
      
      g.drawLine(x + 9, y + 2, x + 9, y + 2);
      g.drawLine(x + 11, y + 4, x + 11, y + 5);
      g.drawLine(x + 13, y + 6, x + 14, y + 6);
      g.drawLine(x + 14, y + 1, x + 14, y + 5);
      g.setColor(savedColor);
    }        
  }

  /**
   * An icon used in the title frame of a {@link JInternalFrame}.
   */
  private static class InternalFrameMinimizeIcon implements Icon, Serializable
  {
  
    /**
     * Creates a new instance.
     */
    public InternalFrameMinimizeIcon() 
    {
      // Nothing to do here.
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
      Color savedColor = g.getColor();
      
      AbstractButton b = (AbstractButton) c;
      
      if (b.getModel().isPressed())
        g.setColor(MetalLookAndFeel.getBlack());
      else
        // FIXME: here the color depends on whether or not the internal frame 
        // is selected 
        g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      
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
      g.drawLine(x + 7, y + 9, x + 7, y + 13);
      g.drawLine(x + 1, y + 14, x + 7, y + 14);

      if (b.getModel().isPressed())
        {
          g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
          g.fillRect(x + 2, y + 9, 3, 3);
        }
      else
        {
          g.drawLine(x + 2, y + 9, x + 4, y + 9);
          g.drawLine(x + 2, y + 10, x + 2, y + 11);
        }

      g.setColor(savedColor);
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
      // Nothing to do here.
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
      boolean enabled = false;
      boolean focus = false;
      if (c != null)
        {
          enabled = c.isEnabled();
          focus = c.hasFocus();    
        }
      
      // draw the outline
      if (enabled) 
        g.setColor(MetalLookAndFeel.getBlack());
      else
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
      g.drawLine(x + 1, y, x + 7, y);
      g.drawLine(x + 8, y, x + 15, y + 7);
      g.drawLine(x + 14, y + 8, x + 8, y + 14);
      g.drawLine(x + 8, y + 14, x + 1, y + 14);
      g.drawLine(x, y + 13, x, y + 1);
      
      // fill the icon
      if (focus)
        g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      else
        g.setColor(MetalLookAndFeel.getControl());
      g.fillRect(x + 2, y + 1, 7, 13);
      g.drawLine(x + 9, y + 2, x + 9, y + 12);
      g.drawLine(x + 10, y + 3, x + 10, y + 11);
      g.drawLine(x + 11, y + 4, x + 11, y + 10);
      g.drawLine(x + 12, y + 5, x + 12, y + 9);
      g.drawLine(x + 13, y + 6, x + 13, y + 8);
      g.drawLine(x + 14, y + 7, x + 14, y + 7);
      
      // if the slider is enabled, draw dots and highlights
      if (enabled)
        {
          if (focus)
            g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
          else
            g.setColor(MetalLookAndFeel.getBlack());
          g.drawLine(x + 3, y + 3, x + 3, y + 3);
          g.drawLine(x + 3, y + 7, x + 3, y + 7);
          g.drawLine(x + 3, y + 11, x + 3, y + 11);

          g.drawLine(x + 5, y + 5, x + 5, y + 5);
          g.drawLine(x + 5, y + 9, x + 5, y + 9);

          g.drawLine(x + 7, y + 3, x + 7, y + 3);
          g.drawLine(x + 7, y + 7, x + 7, y + 7);
          g.drawLine(x + 7, y + 11, x + 7, y + 11);

          // draw highlights
          if (focus)
            g.setColor(MetalLookAndFeel.getPrimaryControl());
          else
            g.setColor(MetalLookAndFeel.getWhite());
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
        
        }
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
      // Nothing to do here.
    }
    
    /**
     * Returns the additional height for this icon, in this case <code>2</code>
     * pixels.
     * 
     * @return <code>2</code>.
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
      // Nothing to do here.
    }
    
    /**
     * Returns the additional height for this icon, in this case <code>4</code>
     * pixels.
     * 
     * @return <code>4</code>.
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
   * An icon representing a hard disk.
   * 
   * @see MetalIconFactory#getTreeHardDriveIcon()
   */
  private static class TreeHardDriveIcon implements Icon, Serializable
  {

    /**
     * Creates a new icon instance.
     */
    public TreeHardDriveIcon() 
    {
      // Nothing to do here.
    }

    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return <code>16</code>.
     */
    public int getIconWidth() 
    { 
      return 16;
    }

    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return <code>16</code>.
     */
    public int getIconHeight()   
    {
      return 16;
    }

    /**
     * Paints the icon at the specified location, using colors from the 
     * current theme.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color saved = g.getColor();
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 1, y + 4, x + 1, y + 5);
      g.drawLine(x + 14, y + 4, x + 14, y + 5);
      g.drawLine(x + 1, y + 7, x + 1, y + 8);
      g.drawLine(x + 14, y + 7, x + 14, y + 8);
      g.drawLine(x + 1, y + 10, x + 1, y + 11);
      g.drawLine(x + 14, y + 10, x + 14, y + 11);
      
      g.drawLine(x + 2, y + 3, x + 3, y + 3);
      g.drawLine(x + 12, y + 3, x + 13, y + 3);
      g.drawLine(x + 2, y + 6, x + 3, y + 6);
      g.drawLine(x + 12, y + 6, x + 13, y + 6);
      g.drawLine(x + 2, y + 9, x + 3, y + 9);
      g.drawLine(x + 12, y + 9, x + 13, y + 9);
      g.drawLine(x + 2, y + 12, x + 3, y + 12);
      g.drawLine(x + 12, y + 12, x + 13, y + 12);
      
      g.drawLine(x + 4, y + 2, x + 11, y + 2);
      g.drawLine(x + 4, y + 7, x + 11, y + 7);
      g.drawLine(x + 4, y + 10, x + 11, y + 10);
      g.drawLine(x + 4, y + 13, x + 11, y + 13);
      
      g.setColor(MetalLookAndFeel.getWhite());
      g.fillRect(x + 4, y + 3, 2, 2);
      g.drawLine(x + 6, y + 4, x + 6, y + 4);
      g.drawLine(x + 7, y + 3, x + 9, y + 3);
      g.drawLine(x + 8, y + 4, x + 8, y + 4);
      g.drawLine(x + 11, y + 3, x + 11, y + 3);
      g.fillRect(x + 2, y + 4, 2, 2); 
      g.fillRect(x + 2, y + 7, 2, 2); 
      g.fillRect(x + 2, y + 10, 2, 2); 
      g.drawLine(x + 4, y + 6, x + 4, y + 6);
      g.drawLine(x + 4, y + 9, x + 4, y + 9);
      g.drawLine(x + 4, y + 12, x + 4, y + 12);
      
      g.setColor(MetalLookAndFeel.getControlShadow());
      g.drawLine(x + 13, y + 4, x + 13, y + 4);
      g.drawLine(x + 12, y + 5, x + 13, y + 5);
      g.drawLine(x + 13, y + 7, x + 13, y + 7);
      g.drawLine(x + 12, y + 8, x + 13, y + 8);
      g.drawLine(x + 13, y + 10, x + 13, y + 10);
      g.drawLine(x + 12, y + 11, x + 13, y + 11);
      
      g.drawLine(x + 10, y + 5, x + 10, y + 5);
      g.drawLine(x + 7, y + 6, x + 7, y + 6);
      g.drawLine(x + 9, y + 6, x + 9, y + 6);
      g.drawLine(x + 11, y + 6, x + 11, y + 6);

      g.drawLine(x + 10, y + 8, x + 10, y + 8);
      g.drawLine(x + 7, y + 9, x + 7, y + 9);
      g.drawLine(x + 9, y + 9, x + 9, y + 9);
      g.drawLine(x + 11, y + 9, x + 11, y + 9);

      g.drawLine(x + 10, y + 11, x + 10, y + 11);
      g.drawLine(x + 7, y + 12, x + 7, y + 12);
      g.drawLine(x + 9, y + 12, x + 9, y + 12);
      g.drawLine(x + 11, y + 12, x + 11, y + 12);

      g.setColor(saved);
    }        
  }  
  
  /**
   * An icon representing a floppy disk.
   * 
   * @see MetalIconFactory#getTreeFloppyDriveIcon()
   */
  private static class TreeFloppyDriveIcon implements Icon, Serializable
  {

    /**
     * Creates a new icon instance.
     */
    public TreeFloppyDriveIcon() 
    {
      // Nothing to do here.
    }

    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return <code>16</code>.
     */
    public int getIconWidth() 
    { 
      return 16;
    }

    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return <code>16</code>.
     */
    public int getIconHeight()   
    {
      return 16;
    }

    /**
     * Paints the icon at the specified location, using colors from the 
     * current theme.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color saved = g.getColor();
      
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 1, y + 1, x + 13, y + 1);
      g.drawLine(x + 1, y + 1, x + 1, y + 14);
      g.drawLine(x + 1, y + 14, x + 14, y + 14);
      g.drawLine(x + 14, y + 2, x + 14, y + 14);
      
      g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 2, y + 2, 12, 12);
      
      g.setColor(MetalLookAndFeel.getControlShadow());
      g.fillRect(x + 5, y + 2, 6, 5);
      g.drawLine(x + 4, y + 8, x + 11, y + 8);
      g.drawLine(x + 3, y + 9, x + 3, y + 13);
      g.drawLine(x + 12, y + 9, x + 12, y + 13);
      
      g.setColor(MetalLookAndFeel.getWhite());
      g.fillRect(x + 8, y + 3, 2, 3);
      g.fillRect(x + 4, y + 9, 8, 5);
      
      g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      g.drawLine(x + 5, y + 10, x + 9, y + 10);
      g.drawLine(x + 5, y + 12, x + 8, y + 12);

      g.setColor(saved);
    }        
  }  

  /**
   * An icon representing a computer.
   * 
   * @see MetalIconFactory#getTreeComputerIcon()
   */
  private static class TreeComputerIcon implements Icon, Serializable
  {

    /**
     * Creates a new icon instance.
     */
    public TreeComputerIcon() 
    {
      // Nothing to do here.
    }

    /**
     * Returns the width of the icon, in pixels.
     * 
     * @return <code>16</code>.
     */
    public int getIconWidth() 
    { 
      return 16;
    }

    /**
     * Returns the height of the icon, in pixels.
     * 
     * @return <code>16</code>.
     */
    public int getIconHeight()   
    {
      return 16;
    }

    /**
     * Paints the icon at the specified location, using colors from the 
     * current theme.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate for the top-left of the icon.
     * @param y  the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y) 
    {
      Color saved = g.getColor();
      
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 3, y + 1, x + 12, y + 1);
      g.drawLine(x + 2, y + 2, x + 2, y + 8);
      g.drawLine(x + 13, y + 2, x + 13, y + 8);
      g.drawLine(x + 3, y + 9, x + 3, y + 9);
      g.drawLine(x + 12, y + 9, x + 12, y + 9);
      g.drawRect(x + 1, y + 10, 13, 4);
      g.drawLine(x + 5, y + 3, x + 10, y + 3);
      g.drawLine(x + 5, y + 8, x + 10, y + 8);
      g.drawLine(x + 4, y + 4, x + 4, y + 7);
      g.drawLine(x + 11, y + 4, x + 11, y + 7);

      g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.fillRect(x + 5, y + 4, 6, 4);
      
      g.setColor(MetalLookAndFeel.getControlShadow());
      g.drawLine(x + 6, y + 12, x + 8, y + 12);
      g.drawLine(x + 10, y + 12, x + 12, y + 12);
      g.setColor(saved);
    }        
  }  
    
  /** The icon returned by {@link #getCheckBoxIcon()}. */
  private static Icon checkBoxIcon;
  
  /** The icon returned by {@link #getCheckBoxMenuItemIcon()}. */
  private static Icon checkBoxMenuItemIcon;
  
  /** The icon returned by {@link #getFileChooserDetailViewIcon()}. */
  private static Icon fileChooserDetailViewIcon;

  /** The icon returned by {@link #getFileChooserHomeFolderIcon()}. */
  private static Icon fileChooserHomeFolderIcon;

  /** The icon returned by {@link #getFileChooserListViewIcon()}. */
  private static Icon fileChooserListViewIcon;

  /** The icon returned by {@link #getFileChooserNewFolderIcon()}. */
  private static Icon fileChooserNewFolderIcon;

  /** The icon returned by {@link #getFileChooserUpFolderIcon()}. */
  private static Icon fileChooserUpFolderIcon;

  /** The cached RadioButtonIcon instance. */
  private static RadioButtonIcon radioButtonIcon;

  /** The icon returned by {@link #getRadioButtonMenuItemIcon()}. */
  private static Icon radioButtonMenuItemIcon;

  /** The icon returned by {@link #getInternalFrameDefaultMenuIcon()}. */
  private static Icon internalFrameDefaultMenuIcon;

  /** The icon returned by {@link #getTreeComputerIcon()}. */
  private static Icon treeComputerIcon;
  
  /** The icon instance returned by {@link #getTreeFloppyDriveIcon()}. */
  private static Icon treeFloppyDriveIcon;
  
  /** The icon instance returned by {@link #getTreeHardDriveIcon()}. */
  private static Icon treeHardDriveIcon;
  
  /**
   * Creates a new instance.  All the methods are static, so creating an 
   * instance isn't necessary.
   */
  public MetalIconFactory() 
  {
    // Nothing to do here.
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
    if (checkBoxIcon == null)
      checkBoxIcon = new MetalCheckBoxIcon();
    return checkBoxIcon;
  }
  
  /**
   * Returns an icon for use when rendering the {@link JCheckBoxMenuItem} 
   * component.
   * 
   * @return An icon.
   */
  public static Icon getCheckBoxMenuItemIcon() 
  {
    if (checkBoxMenuItemIcon == null)
      checkBoxMenuItemIcon = new CheckBoxMenuItemIcon();
    return checkBoxMenuItemIcon;
  }

  /**
   * Returns an icon for use by the {@link JFileChooser} component.
   * 
   * @return An icon.
   */
  public static Icon getFileChooserDetailViewIcon() 
  {
    if (fileChooserDetailViewIcon == null)
      fileChooserDetailViewIcon = new FileChooserDetailViewIcon();
    return fileChooserDetailViewIcon;
  }
    
  /**
   * Returns an icon for use by the {@link JFileChooser} component.
   * 
   * @return An icon.
   */
  public static Icon getFileChooserHomeFolderIcon() 
  {
    if (fileChooserHomeFolderIcon == null)
      fileChooserHomeFolderIcon = new FileChooserHomeFolderIcon();
    return fileChooserHomeFolderIcon;        
  }
    
  /**
   * Returns an icon for use by the {@link JFileChooser} component.
   * 
   * @return An icon.
   */
  public static Icon getFileChooserListViewIcon() 
  {
    if (fileChooserListViewIcon == null)
      fileChooserListViewIcon = new FileChooserListViewIcon();
    return fileChooserListViewIcon;
  }
    
  /**
   * Returns an icon for use by the {@link JFileChooser} component.
   * 
   * @return An icon.
   */
  public static Icon getFileChooserNewFolderIcon() 
  {
    if (fileChooserNewFolderIcon == null)
      fileChooserNewFolderIcon = new FileChooserNewFolderIcon();
    return fileChooserNewFolderIcon;
  }
    
  /**
   * Returns an icon for use by the {@link JFileChooser} component.
   * 
   * @return An icon.
   */
  public static Icon getFileChooserUpFolderIcon() 
  {
    if (fileChooserUpFolderIcon == null)
      fileChooserUpFolderIcon = new FileChooserUpFolderIcon();
    return fileChooserUpFolderIcon;
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
    if (radioButtonMenuItemIcon == null)
      radioButtonMenuItemIcon = new RadioButtonMenuItemIcon();
    return radioButtonMenuItemIcon;
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
    if (internalFrameDefaultMenuIcon == null)
      internalFrameDefaultMenuIcon = new InternalFrameDefaultMenuIcon();
    return internalFrameDefaultMenuIcon;
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

  /**
   * Returns a <code>16x16</code> icon representing a computer.
   * 
   * @return The icon.
   */
  public static Icon getTreeComputerIcon() 
  {
    if (treeComputerIcon == null)
      treeComputerIcon = new TreeComputerIcon();
    return treeComputerIcon;        
  }
    
  /**
   * Returns a <code>16x16</code> icon representing a floppy disk.
   * 
   * @return The icon.
   */
  public static Icon getTreeFloppyDriveIcon() 
  {
    if (treeFloppyDriveIcon == null)
      treeFloppyDriveIcon = new TreeFloppyDriveIcon();
    return treeFloppyDriveIcon;
  }
    
  /**
   * Returns a <code>16x16</code> icon representing a hard disk.
   * 
   * @return The icon.
   */
  public static Icon getTreeHardDriveIcon() 
  {
    if (treeHardDriveIcon == null)
      treeHardDriveIcon = new TreeHardDriveIcon();
    return treeHardDriveIcon;
  }

}
