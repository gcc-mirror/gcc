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

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.LayoutManager;

import javax.swing.Icon;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.plaf.basic.BasicInternalFrameTitlePane;

/**
 * The title pane for a {@link JInternalFrame}.
 */
public class MetalInternalFrameTitlePane extends BasicInternalFrameTitlePane 
{
 
  protected boolean isPalette;
  
  protected Icon paletteCloseIcon = MetalIconFactory.getInternalFrameCloseIcon(16);
  
  protected int paletteTitleHeight = 12;
 
  /**
   * Creates a new title pane.
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
   * Creates a layout manager for the components in the title pane.
   * 
   * @return A layout manager.
   */   
  protected LayoutManager createLayout()
  {
    return new TitlePaneLayout()
    {
      public Dimension preferredLayoutSize(Container c)
      {
        return new Dimension(24, 24);
      }
    };
  }
  
  public void paintPalette(Graphics g)
  {
    // FIXME:  needs implementing
    // most likely this is equivalent to paintComponent(g) when the isPalette
    // flag is true
  }
  
  public void paintComponent(Graphics g)
  {
    // probably need to check the isPalette flag here, if true pass over to
    // paintPalette(Graphics)
    super.paintComponent(g);
    Dimension d = getSize();
    if (frame.isSelected())
      g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
    else
      g.setColor(MetalLookAndFeel.getControlDarkShadow());
    g.drawLine(0, d.height - 1, d.width - 1, d.height - 1);
  }
  
  public void setPalette(boolean b)
  {
    isPalette = b;  
  }
}

