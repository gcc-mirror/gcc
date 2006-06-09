/* MetalInternalFrameTitlePane.java
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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Icon;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicInternalFrameTitlePane;


/**
 * The title pane for a {@link JInternalFrame} (see 
 * {@link MetalInternalFrameUI#createNorthPane(JInternalFrame)}).  This can 
 * be displayed in two styles: one for regular internal frames, and the other 
 * for "palette" style internal frames.
 */
public class MetalInternalFrameTitlePane extends BasicInternalFrameTitlePane 
{
 
  /**
   * A property change handler that listens for changes to the 
   * <code>JInternalFrame.isPalette</code> property and updates the title
   * pane as appropriate.
   */
  class MetalInternalFrameTitlePanePropertyChangeHandler
    extends PropertyChangeHandler
  {
    /**
     * Creates a new handler.
     */
    public MetalInternalFrameTitlePanePropertyChangeHandler()
    {
      super();
    }
    
    /**
     * Handles <code>JInternalFrame.isPalette</code> property changes, with all
     * other property changes being passed to the superclass.
     * 
     * @param e  the event.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      String propName = e.getPropertyName();
      if (e.getPropertyName().equals(JInternalFrame.FRAME_ICON_PROPERTY))
        {
          title.setIcon(frame.getFrameIcon());
        }
      else if (propName.equals("JInternalFrame.isPalette"))
        {
          if (e.getNewValue().equals(Boolean.TRUE))
            setPalette(true);
          else
            setPalette(false);
        }
      else
        super.propertyChange(e);
    }
  }

  /**
   * A layout manager for the title pane.
   * 
   * @see #createLayout()
   */
  private class MetalTitlePaneLayout implements LayoutManager
  {
    /**
     * Creates a new <code>TitlePaneLayout</code> object.
     */
    public MetalTitlePaneLayout()
    {
      // Do nothing.
    }

    /**
     * Adds a Component to the Container.
     *
     * @param name The name to reference the added Component by.
     * @param c The Component to add.
     */
    public void addLayoutComponent(String name, Component c)
    {
      // Do nothing.
    }

    /**
     * This method is called to lay out the children of the Title Pane.
     *
     * @param c The Container to lay out.
     */
    public void layoutContainer(Container c)
    {

      Dimension size = c.getSize();
      Insets insets = c.getInsets();
      int width = size.width - insets.left - insets.right;
      int height = size.height - insets.top - insets.bottom;


      int loc = width - insets.right - 1;
      int top = insets.top + 2;
      int buttonHeight = height - 4;
      if (closeButton.isVisible())
        {
          int buttonWidth = closeIcon.getIconWidth();
          loc -= buttonWidth + 2;
          closeButton.setBounds(loc, top, buttonWidth, buttonHeight);
          loc -= 6;
        }

      if (maxButton.isVisible())
        {
          int buttonWidth = maxIcon.getIconWidth();
          loc -= buttonWidth + 4;
          maxButton.setBounds(loc, top, buttonWidth, buttonHeight);
        }

      if (iconButton.isVisible())
        {
          int buttonWidth = minIcon.getIconWidth();
          loc -= buttonWidth + 4;
          iconButton.setBounds(loc, top, buttonWidth, buttonHeight);
          loc -= 2;
        }

      Dimension titlePreferredSize = title.getPreferredSize();
      title.setBounds(insets.left + 5, insets.top, 
              Math.min(titlePreferredSize.width, loc - insets.left - 10), 
              height);

    }

    /**
     * This method returns the minimum size of the given Container given the
     * children that it has.
     *
     * @param c The Container to get a minimum size for.
     *
     * @return The minimum size of the Container.
     */
    public Dimension minimumLayoutSize(Container c)
    {
      return preferredLayoutSize(c);
    }

    /**
     * Returns the preferred size of the given Container taking
     * into account the children that it has.
     *
     * @param c The Container to lay out.
     *
     * @return The preferred size of the Container.
     */
    public Dimension preferredLayoutSize(Container c)
    {
      if (isPalette)
        return new Dimension(paletteTitleHeight, paletteTitleHeight);
      else
        return new Dimension(22, 22);
    }

    /**
     * Removes a Component from the Container.
     *
     * @param c The Component to remove.
     */
    public void removeLayoutComponent(Component c)
    {
      // Nothing to do here.
    }
  }

  /** A flag indicating whether the title pane uses the palette style. */
  protected boolean isPalette;
  
  /** 
   * The icon used for the close button - this is fetched from the look and
   * feel defaults using the key <code>InternalFrame.paletteCloseIcon</code>. 
   */
  protected Icon paletteCloseIcon;
  
  /**
   * The height of the title pane when <code>isPalette</code> is 
   * <code>true</code>.  This value is fetched from the look and feel defaults 
   * using the key <code>InternalFrame.paletteTitleHeight</code>.
   */
  protected int paletteTitleHeight;
   
  /** The label used to display the title for the internal frame. */
  JLabel title;
  
  /**
   * Creates a new title pane for the specified frame.
   * 
   * @param f  the internal frame.
   */
  public MetalInternalFrameTitlePane(JInternalFrame f)
  {
    super(f);
    isPalette = false;
  }
  
  /**
   * Fetches the colors used in the title pane.
   */
  protected void installDefaults()
  {
    super.installDefaults();
    selectedTextColor = MetalLookAndFeel.getControlTextColor();
    selectedTitleColor = MetalLookAndFeel.getWindowTitleBackground();
    notSelectedTextColor = MetalLookAndFeel.getInactiveControlTextColor();
    notSelectedTitleColor = MetalLookAndFeel.getWindowTitleInactiveBackground();
    
    paletteTitleHeight = UIManager.getInt("InternalFrame.paletteTitleHeight");
    paletteCloseIcon = UIManager.getIcon("InternalFrame.paletteCloseIcon");
    minIcon = MetalIconFactory.getInternalFrameAltMaximizeIcon(16);

    title = new JLabel(frame.getTitle(), 
            MetalIconFactory.getInternalFrameDefaultMenuIcon(), 
            SwingConstants.LEFT);
  }
  
  /**
   * Clears the colors used for the title pane.
   */
  protected void uninstallDefaults()
  {  
    super.uninstallDefaults();
    selectedTextColor = null;
    selectedTitleColor = null;
    notSelectedTextColor = null;
    notSelectedTitleColor = null;
    paletteCloseIcon = null;
    minIcon = null;
    title = null;
  }
  
  /**
   * Calls the super class to create the buttons, then calls
   * <code>setBorderPainted(false)</code> and 
   * <code>setContentAreaFilled(false)</code> for each button.
   */
  protected void createButtons()
  { 
    super.createButtons();
    closeButton.setBorderPainted(false);
    closeButton.setContentAreaFilled(false);
    iconButton.setBorderPainted(false);
    iconButton.setContentAreaFilled(false);
    maxButton.setBorderPainted(false);
    maxButton.setContentAreaFilled(false);
  }
  
  /**
   * Overridden to do nothing.
   */
  protected void addSystemMenuItems(JMenu systemMenu)
  {
    // do nothing
  }
  
  /**
   * Overridden to do nothing.
   */
  protected void showSystemMenu()
  {
      // do nothing    
  }
  
  /**
   * Adds the sub components of the title pane.
   */
  protected void addSubComponents()
  {
    // FIXME:  this method is probably overridden to only add the required 
    // buttons
    add(title);
    add(closeButton);
    add(iconButton);
    add(maxButton);
  }

  /**
   * Creates a new instance of <code>MetalTitlePaneLayout</code> (not part of
   * the public API).
   * 
   * @return A new instance of <code>MetalTitlePaneLayout</code>.
   */
  protected LayoutManager createLayout()
  {
    return new MetalTitlePaneLayout();
  }
  
  /**
   * Draws the title pane in the palette style.
   * 
   * @param g  the graphics device.
   * 
   * @see #paintComponent(Graphics)
   */
  public void paintPalette(Graphics g)
  {
    Color savedColor = g.getColor();
    Rectangle b = SwingUtilities.getLocalBounds(this);

    if (UIManager.get("InternalFrame.activeTitleGradient") != null
        && frame.isSelected())
      {
        MetalUtils.paintGradient(g, b.x, b.y, b.width, b.height,
                                 SwingConstants.VERTICAL,
                                 "InternalFrame.activeTitleGradient");
      }
    MetalUtils.fillMetalPattern(this, g, b.x + 4, b.y + 2, b.width 
            - paletteCloseIcon.getIconWidth() - 13, b.height - 5,
            MetalLookAndFeel.getPrimaryControlHighlight(), 
            MetalLookAndFeel.getBlack());
    
    // draw a line separating the title pane from the frame content
    Dimension d = getSize();
    g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
    g.drawLine(0, d.height - 1, d.width - 1, d.height - 1);
    
    g.setColor(savedColor);
  }

  /**
   * Paints a representation of the current state of the internal frame.
   * 
   * @param g  the graphics device.
   */
  public void paintComponent(Graphics g)
  {
    Color savedColor = g.getColor();
    if (isPalette)
      paintPalette(g);
    else
      {
        paintTitleBackground(g);
        paintChildren(g);
        Dimension d = getSize();
        if (frame.isSelected())
          g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
        else
          g.setColor(MetalLookAndFeel.getControlDarkShadow());
        
        // put a dot in each of the top corners
        g.drawLine(0, 0, 0, 0);
        g.drawLine(d.width - 1, 0, d.width - 1, 0);
        
        g.drawLine(0, d.height - 1, d.width - 1, d.height - 1);
        
        // draw the metal pattern
        if (UIManager.get("InternalFrame.activeTitleGradient") != null
            && frame.isSelected())
          {
            MetalUtils.paintGradient(g, 0, 0, getWidth(), getHeight(),
                                     SwingConstants.VERTICAL,
                                     "InternalFrame.activeTitleGradient");
          }

        Rectangle b = title.getBounds();
        int startX = b.x + b.width + 5;
        int endX = startX;
        if (iconButton.isVisible())
          endX = Math.max(iconButton.getX(), endX);
        else if (maxButton.isVisible()) 
          endX = Math.max(maxButton.getX(), endX);
        else if (closeButton.isVisible())
          endX = Math.max(closeButton.getX(), endX);
        endX -= 7;
        if (endX > startX)
          MetalUtils.fillMetalPattern(this, g, startX, 3, endX - startX, 
              getHeight() - 6, Color.white, Color.gray);
      }
    g.setColor(savedColor);
  }
  
  /**
   * Sets the flag that controls whether the title pane is drawn in the 
   * palette style or the regular style.
   *  
   * @param b  the new value of the flag.
   */
  public void setPalette(boolean b)
  {
    isPalette = b;
    title.setVisible(!isPalette);
    iconButton.setVisible(!isPalette && frame.isIconifiable());
    maxButton.setVisible(!isPalette && frame.isMaximizable());
    if (isPalette)
      closeButton.setIcon(paletteCloseIcon);
    else
      closeButton.setIcon(closeIcon);
  }
  
  /**
   * Creates and returns a property change handler for the title pane.
   * 
   * @return The property change handler.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new MetalInternalFrameTitlePanePropertyChangeHandler();   
  }
}

