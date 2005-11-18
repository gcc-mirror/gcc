/* MetalBorders.java
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
import java.awt.Insets;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.JButton;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicBorders;
import javax.swing.text.JTextComponent;


/**
 * A factory class that creates borders for the different Swing components.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class MetalBorders
{

  /** The shared instance for getButtonBorder(). */
  private static Border buttonBorder;

  /** The shared instance for getToggleButtonBorder(). */
  private static Border toggleButtonBorder;

  /** The shared instance for getDesktopIconBorder(). */
  private static Border desktopIconBorder;

  /** The shared instance for getRolloverButtonBorder(). */
  private static Border toolbarButtonBorder;

  /** The shared instance for getTextFieldBorder(). */
  private static Border textFieldBorder;

  /** The shared instance for getTextBorder(). */
  private static Border textBorder;

  /** The shared instance for getRolloverBorder(). */
  private static Border rolloverBorder;

  /**
   * A MarginBorder that gets shared by multiple components.
   * Created on demand by the private helper function {@link
   * #getMarginBorder()}.
   */
  private static BasicBorders.MarginBorder marginBorder;

  /**
   * A border used for {@link JButton} components.
   */
  public static class ButtonBorder extends AbstractBorder implements UIResource
  {
    /** The borders insets. */
    protected static Insets borderInsets = new Insets(3, 3, 3, 3);

    /**
     * Creates a new instance of <code>ButtonBorder</code>.
     */
    public ButtonBorder()
    {
      // Nothing to do here.
    }

    /**
     * Paints the button border.
     *
     * @param c the component for which we paint the border
     * @param g the Graphics context to use
     * @param x the X coordinate of the upper left corner of c
     * @param y the Y coordinate of the upper left corner of c
     * @param w the width of c
     * @param h the height of c
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w,
                            int h)
    {
      // With the OceanTheme the button border is painted entirely different.
      // However, I couldn't figure out how this is determined besides checking
      // for instanceof OceanTheme. The button painting is definitely not
      // influenced by a UI default property and it is definitely performed
      // by the same Border class.
      if (MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme)
        paintOceanButtonBorder(c, g, x, y, w, h);
      else
        {
          ButtonModel bmodel = null;
      
          if (c instanceof AbstractButton)
            bmodel = ((AbstractButton) c).getModel();

          Color darkShadow = MetalLookAndFeel.getControlDarkShadow();
          Color shadow = MetalLookAndFeel.getControlShadow();
          Color light = MetalLookAndFeel.getControlHighlight();
          Color middle = MetalLookAndFeel.getControl();

          if (c.isEnabled())
            {
              // draw dark border
              g.setColor(darkShadow);
              g.drawRect(x, y, w - 2, h - 2);

              if (!bmodel.isPressed())
                {
                  // draw light border
                  g.setColor(light);
                  g.drawRect(x + 1, y + 1, w - 2, h - 2);

                  // draw crossing pixels of both borders
                  g.setColor(middle);
                  g.drawLine(x + 1, y + h - 2, x + 1, y + h - 2);
                  g.drawLine(x + w - 2, y + 1, x + w - 2, y + 1);
                }
              else
                {
                  // draw light border
                  g.setColor(light);
                  g.drawLine(x + w - 1, y + 1, x + w - 1, y + h - 1);
                  g.drawLine(x + 1, y + h - 1, x + w - 1, y + h - 1);

                  // draw shadow border
                  g.setColor(middle);
                  g.drawLine(x + 1, y + 1, x + w - 2, y + 1);
                  g.drawLine(x + 1, y + 1, x + 1, y + h - 2);
 
                  // draw crossing pixels of both borders
                  g.setColor(shadow);
                  g.drawRect(x + 1, y + h - 2, 0, 0);
                  g.drawRect(x + w - 2, y + 1, 0, 0);
                }
            }
          else 
            {
              // draw disabled border
              g.setColor(MetalLookAndFeel.getInactiveControlTextColor());
              g.drawRect(x, y, w - 2, h - 2);          
            }
        }
    }

    /**
     * Paints the button border for the OceanTheme.
     *
     * @param c the button
     * @param g the graphics context
     * @param x the X coordinate of the upper left corner of the painting rect
     * @param y the Y coordinate of the upper left corner of the painting rect
     * @param w the width of the painting rect
     * @param h the height of the painting rect
     */
    private void paintOceanButtonBorder(Component c, Graphics g, int x,
                                        int y, int w, int h)
    {
      ButtonModel bmodel = null;
      
      if (c instanceof AbstractButton)
        bmodel = ((AbstractButton) c).getModel();

      Color darkShadow = MetalLookAndFeel.getControlDarkShadow();
      Color shadow = MetalLookAndFeel.getControlShadow();
      Color light = MetalLookAndFeel.getControlHighlight();
      Color middle = MetalLookAndFeel.getControl();

      if (c.isEnabled())
        {
          if (bmodel.isPressed())
            {
              // draw fat border
              g.drawLine(x + 1, y + 1, x + w - 2, y + 1);
              g.drawLine(x + 1, y + 1, x + 1, y + h - 2);
            }
          else if (bmodel.isRollover())
            {
              g.setColor(shadow);
              g.drawRect(x, y, w - 1, h - 1);
              g.drawRect(x + 2, y + 2, w - 5, h - 5);
              g.setColor(darkShadow);
              g.drawRect(x + 1, y + 1, w - 3, h - 3);
            }
          else
            {
              g.setColor(darkShadow);
              g.drawRect(x, y, w - 1, h - 1);
            }
        }
      else 
        {
          // draw disabled border
          g.setColor(MetalLookAndFeel.getInactiveControlTextColor());
          g.drawRect(x, y, w - 2, h - 2);          
        }
    }

    /**
     * Returns the insets of the <code>ButtonBorder</code>.
     *
     * @param c the component for which the border is used
     *
     * @return The insets of the ButtonBorder
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }

    /**
     * Returns the insets of the <code>ButtonBorder</code> in the specified 
     * <code>newInsets</code> object.
     *
     * @param c the component for which the border is used
     * @param newInsets the insets object where to put the values (if 
     *        <code>null</code>, a new instance is created).
     *
     * @return The insets.
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(0, 0, 0, 0);

      newInsets.bottom = borderInsets.bottom;
      newInsets.left = borderInsets.left;
      newInsets.right = borderInsets.right;
      newInsets.top = borderInsets.top;
      return newInsets;
    }
  }

  /**
   * A border used when painting {@link JInternalFrame} instances.
   */
  static class DesktopIconBorder extends AbstractBorder
    implements UIResource
  {
    /**
     * Creates a new border instance.
     */
    public DesktopIconBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(3, 3, 2, 3);
      else
        {
          newInsets.top = 3;
          newInsets.left = 3;
          newInsets.bottom = 2;
          newInsets.right = 3;
        }
      return newInsets;  
    }
    
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
        int h)
    {
      g.setColor(MetalLookAndFeel.getControlDarkShadow());      
      g.drawRect(x, y, w - 1, h - 1); 
    }
    
  }

  /**
   * A simple 3D border.
   */
  public static class Flush3DBorder extends AbstractBorder
    implements UIResource
  {
    /**
     * Creates a new border instance.
     */
    public Flush3DBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(2, 2, 2, 2);
      else
        {
          newInsets.top = 2;
          newInsets.left = 2;
          newInsets.bottom = 2;
          newInsets.right = 2;
        }
      return newInsets;  
    }
    
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
        int h)
    {              
      Color savedColor = g.getColor();
      g.setColor(MetalLookAndFeel.getControlDarkShadow());
      g.drawRect(x, y, w - 2, h - 2);
      g.setColor(MetalLookAndFeel.getControlHighlight());
      g.drawRect(x + 1, y + 1, w - 2, h - 2);
      g.setColor(MetalLookAndFeel.getControl());
      g.drawLine(x + 1, y + h - 2, x + 1, y + h - 2);
      g.drawLine(x + w - 2, y + 1, x + w - 2, y + 1);
      g.setColor(savedColor);
    }
    
  }
    
  /**
   * A border used for a {@link JInternalFrame} when it is being used as a 
   * palette.
   * 
   * @since 1.3
   */
  public static class PaletteBorder extends AbstractBorder
    implements UIResource
  {
    /**
     * Creates a new <code>PaletteBorder</code>.
     */
    public PaletteBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }

    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * @param newInsets  the insets object that, if non-<code>null</code>, will 
     *                   be populated with the result from this method.
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {        
      if (newInsets == null)
        newInsets = new Insets(1, 1, 1, 1);
      else
        {
          newInsets.top = 1;
          newInsets.left = 1;
          newInsets.bottom = 1;
          newInsets.right = 1;
        }
      return newInsets;  
    }
    
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
            int h)
    {
      Color savedColor = g.getColor();
      
      // draw the outline
      g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      g.drawRect(x, y, w - 1, h - 1);
      
      // put a dot in each corner
      g.setColor(MetalLookAndFeel.getControl());
      g.fillRect(x, y, 1, 1);
      g.fillRect(x + w - 1, y, 1, 1);
      g.fillRect(x + w - 1, y + h - 1, 1, 1);
      g.fillRect(x, y + h - 1, 1, 1);      
      g.setColor(savedColor);
    }

  }
    
  /**
   * A border used for the {@link JTextField} component.
   */
  public static class TextFieldBorder extends Flush3DBorder
    implements UIResource
  {
    /**
     * Creates a new border instance.
     */
    public TextFieldBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
        int h)
    {
      boolean enabledTextBorder;
      if (c instanceof JTextComponent)
	{
	  JTextComponent tc = (JTextComponent) c;
	  enabledTextBorder = tc.isEnabled() && tc.isEditable();
	}
      else
	enabledTextBorder = false;

      if (enabledTextBorder)
        super.paintBorder(c, g, x, y, w, h);
      else
        {
          Color savedColor = g.getColor();
          g.setColor(MetalLookAndFeel.getControlShadow());
          g.drawRect(x, y, w - 1, h - 1);
          g.setColor(savedColor);
        }
    }
    
  }

  /**
   * A border used for the {@link JInternalFrame} component.
   */
  public static class InternalFrameBorder extends AbstractBorder
    implements UIResource
  {
    /**
     * Creates a new border instance.
     */
    public InternalFrameBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(5, 5, 5, 5);
      else
        {
          newInsets.top = 5;
          newInsets.left = 5;
          newInsets.bottom = 5;
          newInsets.right = 5;
        }
      return newInsets;  
    }
    
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
        int h)
    {
        
      JInternalFrame f = (JInternalFrame) c;
      if (f.isSelected())
        g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      else
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
      
      // fill the border background
      g.fillRect(x, y, w, 5);
      g.fillRect(x, y, 5, h);
      g.fillRect(x + w - 5, y, 5, h);
      g.fillRect(x, y + h - 5, w, 5);
      
      // draw a dot in each corner
      g.setColor(MetalLookAndFeel.getControl());
      g.fillRect(x, y, 1, 1);
      g.fillRect(x + w - 1, y, 1, 1);
      g.fillRect(x + w - 1, y + h - 1, 1, 1);
      g.fillRect(x, y + h - 1, 1, 1);
      
      // draw the lines
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 14, y + 2, x + w - 15, y + 2);
      g.drawLine(x + 14, y + h - 3, x + w - 15, y + h - 3);
      g.drawLine(x + 2, y + 14, x + 2, y + h - 15);
      g.drawLine(x + w - 3, y + 14, x + w - 3, y + h - 15);
      
      // draw the line highlights
      if (f.isSelected())
        g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      else 
        g.setColor(MetalLookAndFeel.getControlShadow());
      g.drawLine(x + 15, y + 3, x + w - 14, y + 3);
      g.drawLine(x + 15, y + h - 2, x + w - 14, y + h - 2);
      g.drawLine(x + 3, y + 15, x + 3, y + h - 14);
      g.drawLine(x + w - 2, y + 15, x + w - 2, y + h - 14);
    }
    
  }

  /**
   * A border used for {@link JInternalFrame} components that are
   * presented as dialogs (by the {@link JOptionPane} class).
   */
  public static class OptionDialogBorder extends AbstractBorder
    implements UIResource
  {
      
    /**
     * Creates a new border instance.
     */
    public OptionDialogBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(3, 3, 3, 3);
      else
        {
          newInsets.top = 3;
          newInsets.left = 3;
          newInsets.bottom = 3;
          newInsets.right = 3;
        }
      return newInsets;  
    }
        
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
        int h)
    {
        
      JInternalFrame f = (JInternalFrame) c;
      g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      if (f.getContentPane() instanceof JOptionPane)
        {
          JOptionPane pane = (JOptionPane) f.getContentPane();
          int type = pane.getMessageType();
          if (type == JOptionPane.QUESTION_MESSAGE)
            {
              Color bc = UIManager.getColor(
                  "OptionPane.questionDialog.border.background");
              if (bc != null)
                g.setColor(bc);
            }
          if (type == JOptionPane.WARNING_MESSAGE)
            {
              Color bc = UIManager.getColor(
                  "OptionPane.warningDialog.border.background");
              if (bc != null)
                g.setColor(bc);              
            }
          else if (type == JOptionPane.ERROR_MESSAGE)
            {
              Color bc = UIManager.getColor(
                  "OptionPane.errorDialog.border.background");
              if (bc != null)
                g.setColor(bc);              
            }
        }
      
      // fill the border background
      g.fillRect(x, y, w, 3);
      g.fillRect(x, y, 3, h);
      g.fillRect(x + w - 3, y, 3, h);
      g.fillRect(x, y + h - 3, w, 3);
      
      // draw a dot in each corner
      g.setColor(MetalLookAndFeel.getControl());
      g.fillRect(x, y, 1, 1);
      g.fillRect(x + w - 1, y, 1, 1);
      g.fillRect(x + w - 1, y + h - 1, 1, 1);
      g.fillRect(x, y + h - 1, 1, 1);
      
    }
    
  }

  /**
   * A border used for {@link JMenu} and {@link JMenuItem} components.
   */
  public static class MenuItemBorder extends AbstractBorder
    implements UIResource
  {
    /** The border insets. */
    protected static Insets borderInsets = new Insets(1, 1, 1, 1);
    
    /**
     * Creates a new border instance.
     */
    public MenuItemBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Paints the border for the component.  A border is painted only if the
     * component is a selected {@link JMenu} or an armed {@link JMenuItem}.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate of the border area.
     * @param y  the y-coordinate of the border area.
     * @param w  the width of the border area.
     * @param h  the height of the border area.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w,
        int h)
    {
      Color dark = MetalLookAndFeel.getPrimaryControlDarkShadow();
      Color light = MetalLookAndFeel.getPrimaryControlHighlight();
      if (c instanceof JMenu) {
        JMenu menu = (JMenu) c;
        if (menu.isSelected())
        {
          g.setColor(dark);
          g.drawLine(x, y, x, y + h);
          g.drawLine(x, y, x + w, y);
          g.drawLine(x + w - 2, y + 1, x + w - 2, y + h);
          g.setColor(light);
          g.drawLine(x + w - 1, y + 1, x + w - 1, y + h);
        }
      }
      else if (c instanceof JMenuItem)
      {
        JMenuItem item = (JMenuItem) c;
        if (item.isArmed()) 
          {
            g.setColor(dark);
            g.drawLine(x, y, x + w, y);
            g.setColor(light);
            g.drawLine(x, y + h - 1, x + w, y + h - 1);
          }
        else
          {
            // Normally we draw a light line on the left.
            g.setColor(light);
            g.drawLine(x, y, x, y + h);
          }
      }
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return borderInsets;
    }
    
    /**
     * Populates <code>insets</code> with the border insets, then returns it.
     * 
     * @param c  the component (ignored).
     * @param insets  the object to populate with the border insets.
     * 
     * @return The border insets.
     * 
     * @throws NullPointerException if <code>insets</code> is <code>null</code>.
     */
    public Insets getBorderInsets(Component c, Insets insets)
    {
      insets.left = borderInsets.left;
      insets.top = borderInsets.top;
      insets.bottom = borderInsets.bottom;
      insets.right = borderInsets.right;
      return insets;
    }
  }

  /**
   * A border used for {@link JMenuBar} components.
   */
  public static class MenuBarBorder
      extends AbstractBorder
      implements UIResource
  {
    /** The border insets. */
    protected static Insets borderInsets = new Insets(1, 0, 1, 0);
    
    // TODO: find where this color really comes from
    private static Color borderColor = new Color(153, 153, 153);
    
    /**
     * Creates a new border instance.
     */
    public MenuBarBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Paints the border for the component.  A border is painted only if the
     * component is a selected {@link JMenu} or an armed {@link JMenuItem}.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate of the border area.
     * @param y  the y-coordinate of the border area.
     * @param w  the width of the border area.
     * @param h  the height of the border area.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w,
        int h)
    {
      g.setColor(borderColor);
      g.drawLine(x, y + h - 1, x + w, y + h - 1);
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return borderInsets;
    }
    
    /**
     * Populates <code>insets</code> with the border insets, then returns it.
     * 
     * @param c  the component (ignored).
     * @param insets  the object to populate with the border insets.
     * 
     * @return The border insets.
     * 
     * @throws NullPointerException if <code>insets</code> is <code>null</code>.
     */
    public Insets getBorderInsets(Component c, Insets insets)
    {
      insets.left = borderInsets.left;
      insets.top = borderInsets.top;
      insets.bottom = borderInsets.bottom;
      insets.right = borderInsets.right;
      return insets;
    }
  }

  /**
   * A border for {@link JScrollPane} components.
   */
  public static class ScrollPaneBorder
    extends AbstractBorder
    implements UIResource
  {
    /** The border insets. */
    private static Insets insets = new Insets(1, 1, 2, 2);
    
    /**
     * Constructs a new ScrollPaneBorder.
     */
    public ScrollPaneBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the insets of the border for the Component <code>c</code>.
     *
     * @param c the Component for which we return the border insets
     */
    public Insets getBorderInsets(Component c)
    {
      return insets;
    }

    /**
     * Paints the border.
     *
     * @param c the Component for which the border is painted
     * @param g the Graphics context
     * @param x the X coordinate of the upper left corner of the border
     * @param y the Y coordinate of the upper left corner of the border
     * @param w the width of the border
     * @param h the height of the border
     */
    public void paintBorder(Component c, Graphics g, int x, int y,
                            int w, int h)
    {
      Color darkShadow = MetalLookAndFeel.getControlDarkShadow();
      Color shadow = MetalLookAndFeel.getControlShadow();
      Color light = MetalLookAndFeel.getWhite();
      Color middle = MetalLookAndFeel.getControl();

      // paint top border line
      g.setColor(darkShadow);
      g.drawLine(x, y, x + w - 2, y);

      // paint left border line
      g.drawLine(x, y, x, y + h - 2);
 
      // paint right inner border line
      g.drawLine(x + w - 2, y, x + w - 2, y + h + 1);

      // paint bottom inner border line
      g.drawLine(x + 2, y + h - 2, x + w - 2, y + h - 2);

      // draw right outer border line
      g.setColor(light);
      g.drawLine(x + w - 1, y, x + w - 1, y + h - 1);

      // draw bottom outer border line
      g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);

      // paint the lighter points
      g.setColor(middle);
      g.drawLine(x + w - 1, y, x + w - 1, y);
      g.drawLine(x + w - 2, y + 2, x + w - 2, y + 2);
      g.drawLine(x, y + h - 1, x, y + h - 1);
      g.drawLine(x + 1, y + h - 2, x + 1, y + h - 2);

    }
    
  }
  
  /**
   * A button border that is only visible when the mouse pointer is within 
   * the button's bounds.
   */
  public static class RolloverButtonBorder
    extends MetalBorders.ButtonBorder
  {
    /**
     * Creates a new border instance.
     */
    public RolloverButtonBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Paints the border.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
            int h)
    {
      boolean mouseIsOver = false;
      if (c instanceof AbstractButton)
        {
          ButtonModel bmodel = ((AbstractButton) c).getModel();
          mouseIsOver = bmodel.isRollover();
        }
      if (mouseIsOver)
        super.paintBorder(c, g, x, y, w, h);
    }
  }
  
  /**
   * This border is used in Toolbar buttons as inner border.
   */
  static class RolloverMarginBorder extends AbstractBorder
  {
    /** The borders insets. */
    protected static Insets borderInsets = new Insets(3, 3, 3, 3);

    /**
     * Creates a new instance of RolloverBorder.
     */
    public RolloverMarginBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the insets of the RolloverBorder.
     *
     * @param c the component for which the border is used
     *
     * @return the insets of the RolloverBorder
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }

    /**
     * Returns the insets of the RolloverMarginBorder in the specified
     * Insets object.
     *
     * @param c the component for which the border is used
     * @param newInsets the insets object where to put the values
     *
     * @return the insets of the RolloverMarginBorder
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(0, 0, 0, 0);

      AbstractButton b = (AbstractButton) c;
      Insets margin = b.getMargin();
      newInsets.bottom = borderInsets.bottom;
      newInsets.left = borderInsets.left;
      newInsets.right = borderInsets.right;
      newInsets.top = borderInsets.top;
      return newInsets;
    }
  }

  /**
   * A border implementation for popup menus.
   */
  public static class PopupMenuBorder
    extends AbstractBorder
    implements UIResource
  {

    /** The border's insets. */
    protected static Insets borderInsets = new Insets(3, 1, 2, 1);

    /**
     * Constructs a new PopupMenuBorder.
     */
    public PopupMenuBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the insets of the border, creating a new Insets instance
     * with each call.
     *
     * @param c the component for which we return the border insets
     *          (not used here)
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }
    
    /**
     * Returns the insets of the border, using the supplied Insets instance.
     *
     * @param c the component for which we return the border insets
     *          (not used here)
     * @param i the Insets instance to fill with the Insets values
     */
    public Insets getBorderInsets(Component c, Insets i)
    {
      Insets insets;
      if (i == null)
        insets = new Insets(borderInsets.top, borderInsets.left,
                            borderInsets.bottom, borderInsets.right);
      else
        {
          insets = i;
          insets.top = borderInsets.top;
          insets.left = borderInsets.left;
          insets.bottom = borderInsets.bottom;
          insets.right = borderInsets.right;
        }
      
      return insets;
    }

    /**
     * Paints the border for component <code>c</code> using the
     * Graphics context <code>g</code> with the dimension
     * <code>x, y, w, h</code>.
     *
     * @param c the component for which we paint the border
     * @param g the Graphics context to use
     * @param x the X coordinate of the upper left corner of c
     * @param y the Y coordinate of the upper left corner of c
     * @param w the width of c
     * @param h the height of c
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w,
                            int h)
    {
      Color darkShadow = MetalLookAndFeel.getPrimaryControlDarkShadow();
      Color light = MetalLookAndFeel.getPrimaryControlHighlight();

      // draw dark outer border
      g.setColor(darkShadow);
      g.drawRect(x, y, w - 1, h - 1);
      
      // draw highlighted inner border (only top and left)
      g.setColor(light);
      g.drawLine(x + 1, y + 1, x + w - 2, y + 1);
    }
    
  }

  /**
   * A border used for the {@link JToggleButton} component.
   * 
   * @since 1.3
   */
  public static class ToggleButtonBorder
    extends ButtonBorder 
  {
    /**
     * Creates a new border instance.
     */
    public ToggleButtonBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Paints the toggle button border.
     *
     * @param c the component for which we paint the border
     * @param g the Graphics context to use
     * @param x the X coordinate of the upper left corner of c
     * @param y the Y coordinate of the upper left corner of c
     * @param w the width of c
     * @param h the height of c
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w,
                            int h)
    {
      ButtonModel bmodel = null;
      
      if (c instanceof AbstractButton)
        bmodel = ((AbstractButton) c).getModel();

      Color darkShadow = MetalLookAndFeel.getControlDarkShadow();
      Color shadow = MetalLookAndFeel.getControlShadow();
      Color light = MetalLookAndFeel.getWhite();
      Color middle = MetalLookAndFeel.getControl();

      if (c.isEnabled())
        {
          // draw dark border
          g.setColor(darkShadow);
          g.drawRect(x, y, w - 2, h - 2);

          if (!bmodel.isArmed())
            {
              // draw light border
              g.setColor(light);
              g.drawLine(x + 1, y + h - 1, x + w - 1, y + h - 1);
              g.drawLine(x + w - 1, y + 1, x + w - 1, y + h - 1);
              if (bmodel.isSelected())
                g.setColor(middle);
              g.drawLine(x + 1, y + 1, x + w - 3, y + 1);
              g.drawLine(x + 1, y + 1, x + 1, y + h - 3);

              // draw crossing pixels of both borders
              g.setColor(shadow);
              g.drawLine(x + 1, y + h - 2, x + 1, y + h - 2);
              g.drawLine(x + w - 2, y + 1, x + w - 2, y + 1);
            }
          else
            {
              // draw light border
              g.setColor(light);
              g.drawLine(x + w - 1, y + 1, x + w - 1, y + h - 1);
              g.drawLine(x + 1, y + h - 1, x + w - 1, y + h - 1);

              // draw shadow border
              g.setColor(shadow);
              g.drawLine(x + 1, y + 1, x + w - 2, y + 1);
              g.drawLine(x + 1, y + 1, x + 1, y + h - 2);
 
              // draw crossing pixels of both borders
              g.setColor(shadow);
              g.drawLine(x + 1, y + h - 2, x + 1, y + h - 2);
              g.drawLine(x + w - 2, y + 1, x + w - 2, y + 1);
              
            }
          // draw corners
          g.setColor(middle);
          g.drawLine(x, y + h - 1, x, y + h - 1);
          g.drawLine(x + w - 1, y, x + w - 1, y);
        }
      else 
        {
          // draw disabled border
          g.setColor(MetalLookAndFeel.getControlDisabled());
          g.drawRect(x, y, w - 2, h - 2);          
        }
    }
  }

  /**
   * A border used for the {@link JToolBar} component.
   */
  public static class ToolBarBorder extends AbstractBorder
    implements UIResource, SwingConstants
  {
    /**
     * Creates a new border instance.
     */
    public ToolBarBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * 
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }
    
    /**
     * Returns the border insets.
     * 
     * @param c  the component (ignored).
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      JToolBar tb = (JToolBar) c;
      if (tb.getOrientation() == JToolBar.HORIZONTAL)
        {   
          if (newInsets == null)
            newInsets = new Insets(2, 16, 2, 2);
          else
            {
              newInsets.top = 2;
              newInsets.left = 16;
              newInsets.bottom = 2;
              newInsets.right = 2;
            }
          return newInsets;  
        }
      else // assume JToolBar.VERTICAL
        { 
          if (newInsets == null)
            newInsets = new Insets(16, 2, 2, 2);
          else
            {
              newInsets.top = 16;
              newInsets.left = 2;
              newInsets.bottom = 2;
              newInsets.right = 2;
            }
          return newInsets;  
        }

    }
    
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component.
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
        int h)
    {
        
      JToolBar tb = (JToolBar) c;
      if (tb.getOrientation() == JToolBar.HORIZONTAL)
        {
           MetalUtils.fillMetalPattern(tb, g, x + 2, y + 2, x + 11, y + h - 5, 
                  MetalLookAndFeel.getControlHighlight(), 
                  MetalLookAndFeel.getControlDarkShadow());
        }
      else
        { 
          MetalUtils.fillMetalPattern(tb, g, x + 2, y + 2, x + w - 5, y + 11, 
                  MetalLookAndFeel.getControlHighlight(), 
                  MetalLookAndFeel.getControlDarkShadow());
        }
    }
    
  }
  
  /**
   * A border for table header cells.
   *
   * @since 1.3
   */
  public static class TableHeaderBorder extends AbstractBorder
  {
    /**
     * The insets of this border.
     */
    // TODO: According to tests that I have done, this is really the border
    // that should be returned by getBorderInsets(). However, the name
    // is very distracting. Is there any deeper meaning in it?
    protected Insets editorBorderInsets;

    /**
     * Creates a new instance of <code>TableHeaderBorder</code>.
     */
    public TableHeaderBorder()
    {
      editorBorderInsets = new Insets(1, 1, 1, 1);
    }

    /**
     * Return the insets of this border.
     *
     * @return the insets of this border
     */
    public Insets getBorderInsets(Component c)
    {
      return editorBorderInsets;
    }

    /**
     * Paints the border.
     *
     * @param c the component for which to paint the border
     * @param g the graphics context to use
     * @param x the x cooridinate of the border rectangle
     * @param y the y cooridinate of the border rectangle
     * @param w the width of the border rectangle
     * @param h the height of the border rectangle
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, int h)
    {
      Color dark = MetalLookAndFeel.getControlDarkShadow();
      Color light = MetalLookAndFeel.getWhite();
      Color old = g.getColor();
      g.setColor(light);
      g.drawLine(x, y, x + w - 2, y);
      g.drawLine(x, y, x, y + h - 2);
      g.setColor(dark);
      g.drawLine(x + w - 1, y, x + w - 1, y + h - 1);
      g.drawLine(x + 1, y + h - 1, x + w - 1, y + h - 1);
      g.setColor(old);
    }
  }

  /**
   * Returns a border for Swing buttons in the Metal Look &amp; Feel.
   *
   * @return a border for Swing buttons in the Metal Look &amp; Feel
   */
  public static Border getButtonBorder()
  {
    if (buttonBorder == null)
      {
        Border outer = new ButtonBorder();
        Border inner = getMarginBorder();
        buttonBorder = new BorderUIResource.CompoundBorderUIResource
            (outer, inner);
      }
    return buttonBorder;
  }
  
  /**
   * Returns a border for use with {@link JToggleButton} components.
   *
   * @return A border.
   * 
   * @since 1.3
   */
  public static Border getToggleButtonBorder()
  {
    if (toggleButtonBorder == null)
      {
        Border outer = new ToggleButtonBorder();
        Border inner = getMarginBorder();
        toggleButtonBorder = new BorderUIResource.CompoundBorderUIResource
            (outer, inner);
      }
    return toggleButtonBorder;
  }

  /**
   * Returns a border instance that is used with a {@link JInternalFrame} when
   * it is in the iconified state.
   * 
   * @return A border.
   * 
   * @since 1.3
   */
  public static Border getDesktopIconBorder()
  {
    if (desktopIconBorder == null)
      desktopIconBorder = new DesktopIconBorder();
    return desktopIconBorder;      
  }

  /**
   * Returns a border for use by the {@link JTextField} component.
   * 
   * @return A border.
   * 
   * @since 1.3
   */
  public static Border getTextFieldBorder()
  {
    if (textFieldBorder == null)
      {
        Border inner = getMarginBorder();
        Border outer = new TextFieldBorder();
        textFieldBorder =
          new BorderUIResource.CompoundBorderUIResource(outer, inner);
      }
    return textFieldBorder;
  }

  /**
   * Returns the border that is used for text components (except text fields,
   * which use {@link #getTextFieldBorder}.
   *
   * @return the border that is used for text components
   *
   * @since 1.3
   */
  public static Border getTextBorder()
  {
    if (textBorder == null)
      {
        Border inner = getMarginBorder();
        Border outer = new Flush3DBorder();
        textBorder =
          new BorderUIResource.CompoundBorderUIResource(outer, inner);
      }
    return textBorder;
  }

  /**
   * Returns a border for Toolbar buttons in the Metal Look &amp; Feel.
   *
   * @return a border for Toolbar buttons in the Metal Look &amp; Feel
   */
  static Border getToolbarButtonBorder()
  {
    if (toolbarButtonBorder == null)
      {
        Border outer = new ButtonBorder();
        Border inner = new RolloverMarginBorder();
        toolbarButtonBorder = new BorderUIResource.CompoundBorderUIResource
          (outer, inner);
      }
    return toolbarButtonBorder;
  }

  /**
   * Returns a shared instance of {@link BasicBorders.MarginBorder}.
   *
   * @return a shared instance of {@link BasicBorders.MarginBorder}
   */
  static Border getMarginBorder()
  {
    if (marginBorder == null)
      marginBorder = new BasicBorders.MarginBorder();
    return marginBorder;
  }

  /**
   * Returns a shared instance of a compound border for rollover buttons.
   * 
   * @return A shared border instance.
   */
  static Border getRolloverBorder()
  {
    if (rolloverBorder == null)
      {
        Border outer = new MetalBorders.RolloverButtonBorder();
        Border inner = MetalBorders.getMarginBorder();
        rolloverBorder = new BorderUIResource.CompoundBorderUIResource(outer, 
            inner);
      }
    return rolloverBorder;
  }

}
