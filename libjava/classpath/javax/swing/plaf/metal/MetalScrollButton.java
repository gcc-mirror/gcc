/* MetalScrollButton.java
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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicArrowButton;

/**
 * A button used by the {@link MetalScrollBarUI}.  The button appearance
 * varies according to the button direction, whether or not it is part of a 
 * "free standing" scroll bar, and the current state of the button. 
 */
public class MetalScrollButton extends BasicArrowButton 
{
  
  /** 
   * The maximum size for buttons.
   * @see #getMaximumSize()
   */
  private static Dimension maximumSize;     
  
  /** The width of the button. */
  private int buttonWidth;
  
  /** 
   * A flag that indicates whether the button is part of a free standing 
   * scroll bar.  This affects how the border is drawn.
   */
  private boolean freeStanding;
  
  /**
   * Creates a new button.
   * 
   * @param direction  the direction (this should be one of {@link #NORTH}, 
   *                   {@link #SOUTH}, {@link #EAST} and {@link #WEST}, but 
   *                   this is not enforced).
   * @param width  the button width.
   * @param freeStanding  a flag indicating whether the scroll button is free
   *                      standing or not.
   */
  public MetalScrollButton(int direction, int width, boolean freeStanding)
  {
    super(direction);
    buttonWidth = width;
    this.freeStanding = freeStanding;
    setFocusable(false);
  }
  
  /**
   * Returns the button width.
   * 
   * @return The button width.
   */
  public int getButtonWidth()
  {
    return buttonWidth;   
  }

  /**
   * Sets the free standing flag.  This controls how the button border is
   * drawn.
   * 
   * @param freeStanding  the new value of the flag.
   */
  public void setFreeStanding(boolean freeStanding)
  {
    this.freeStanding = freeStanding;
  }
  
  /**
   * Paints the button.
   * 
   * @param g  the graphics device.
   */
  public void paint(Graphics g)
  {
    Rectangle bounds = SwingUtilities.getLocalBounds(this);

    // fill the background
    if (getModel().isPressed())
      g.setColor(MetalLookAndFeel.getControlShadow());
    else
      g.setColor(MetalLookAndFeel.getControl());
    g.fillRect(0, 0, bounds.width, bounds.height);
    
    paintArrow(g, bounds.width, bounds.height);
    
    // paint a border manually - I tried using a real (custom) Border
    // but couldn't get it to stay set for the button, something was 
    // overwriting it...
    if (freeStanding) 
      {
        if (direction == WEST)
          paintWestBorderFreeStanding(g, bounds.width, bounds.height);        
        else if (direction == EAST)
          paintEastBorderFreeStanding(g, bounds.width, bounds.height);
        else if (direction == SOUTH)
          paintSouthBorderFreeStanding(g, bounds.width, bounds.height);
        else // asume NORTH
          paintNorthBorderFreeStanding(g, bounds.width, bounds.height);
      }
    else
      {
        if (direction == WEST)
          paintWestBorder(g, bounds.width, bounds.height);        
        else if (direction == EAST)
          paintEastBorder(g, bounds.width, bounds.height);
        else if (direction == SOUTH)
          paintSouthBorder(g, bounds.width, bounds.height);
        else // asume NORTH
          paintNorthBorder(g, bounds.width, bounds.height);
      }
  }
  
  private void paintArrow(Graphics g, int w, int h)
  {
    if (isEnabled())
      g.setColor(MetalLookAndFeel.getBlack());
    else
      g.setColor(MetalLookAndFeel.getControlDisabled());
    
    if (direction == SOUTH)
      {
        int x = w / 2;
        int y = h / 2 + 2;
        for (int i = 1; i < 5; i++)
          g.drawLine(x - i, y - i, x + i - 1, y - i);
      }
    else if (direction == EAST)
      {
        int x = w / 2 + 2;
        int y = h / 2;
        for (int i = 1; i < 5; i++)
          g.drawLine(x - i, y - i, x - i, y + i - 1);
      }
    else if (direction == WEST)
      {
        int x = w / 2 - 3;
        int y = h / 2;
        for (int i = 1; i < 5; i++)
          g.drawLine(x + i, y - i, x + i, y + i - 1);        
      }
    else // assume NORTH
      {
        int x = w / 2;
        int y = h / 2 - 3;
        for (int i = 1; i < 5; i++)
          g.drawLine(x - i, y + i, x + i - 1, y + i);
      }
  }
  /**
   * Paints the border for a button with a {@link #NORTH} direction that
   * belongs to a free standing scroll bar.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintNorthBorderFreeStanding(Graphics g, int w, int h) 
  {
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, w - 2, 0);
        g.drawLine(0, 0, 0, h - 1);
        g.drawLine(2, h - 1, w - 2, h - 1);
        g.drawLine(w - 2, 2, w - 2, h - 1);
        
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(1, 1, 1, h - 2);
        g.drawLine(1, 1, w - 3, 1);
        g.drawLine(w - 1, 1, w - 1, h - 1);
      
        g.setColor(MetalLookAndFeel.getControl());
        g.drawLine(1, h - 1, 1, h - 1);
        g.drawLine(w - 2, 1, w - 2, 1);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, 0, w - 1, 0);
        g.drawLine(w - 1, 0, w - 1, h - 1);
        g.drawLine(0, 0, 0, h - 1);
      }
  }
  
  /**
   * Paints the border for a button with a {@link #SOUTH} direction that
   * belongs to a free standing scroll bar.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintSouthBorderFreeStanding(Graphics g, int w, int h)
  {
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, w - 2, 0);
        g.drawLine(0, 0, 0, h - 1);
        g.drawLine(2, h - 1, w - 2, h - 1);
        g.drawLine(w - 2, 2, w - 2, h - 1);
        
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(1, 1, 1, h - 1);
        g.drawLine(1, 1, w - 1, 1);
        g.drawLine(w - 1, 1, w - 1, h - 1);
      
        g.setColor(MetalLookAndFeel.getControl());
        g.drawLine(1, h - 1, 1, h - 1);
        g.drawLine(w - 1, 1, w - 1, 1);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, h - 1, w - 1, h - 1);
        g.drawLine(w - 1, 0, w - 1, h - 1);
        g.drawLine(0, 0, 0, h - 1);
      }
  }
  
  /**
   * Paints the border for a button with an {@link #EAST} direction that
   * belongs to a free standing scroll bar.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintEastBorderFreeStanding(Graphics g, int w, int h)
  {
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, w - 2, 0);
        g.drawLine(w - 2, 0, w - 2, h - 2);
        g.drawLine(0, h - 2, w - 2, h - 2);
        
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(0, 1, w - 1, 1);
        g.drawLine(w - 1, 1, w - 1, h - 1);
        g.drawLine(0, h - 1, w - 1, h - 1);
      
        g.setColor(MetalLookAndFeel.getControl());
        g.drawLine(w - 2, 1, w - 2, 1);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, 0, w - 1, 0);
        g.drawLine(w - 1, 0, w - 1, h - 1);
        g.drawLine(0, h - 1, w - 1, h - 1);
      }
  }
  
  /**
   * Paints the border for a button with a {@link #WEST} direction that
   * belongs to a free standing scroll bar.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintWestBorderFreeStanding(Graphics g, int w, int h)
  {
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, w - 1, 0);
        g.drawLine(0, 0, 0, h - 2);
        g.drawLine(0, h - 2, w - 1, h - 2);
        
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(1, 1, w - 1, 1);
        g.drawLine(1, 1, 1, h - 1);
        g.drawLine(1, h - 1, w - 1, h - 1);
      
        g.setColor(MetalLookAndFeel.getControl());
        g.drawLine(1, h - 2, 1, h - 2);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, 0, w - 1, 0);
        g.drawLine(0, 0, 0, h - 1);
        g.drawLine(0, h - 1, w - 1, h - 1);
      }
  }
  
  /**
   * Paints the border for a button with a {@link #NORTH} direction that
   * belongs to a scroll bar that is not free standing.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintNorthBorder(Graphics g, int w, int h) 
  {
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, 0, h - 1);
         
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(1, 0, 1, h - 1);
        g.drawLine(1, 0, w - 1, 0);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, 0, 0, h - 1);
      }
  }
  
  /**
   * Paints the border for a button with a {@link #SOUTH} direction that
   * belongs to a scroll bar that is not free standing.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintSouthBorder(Graphics g, int w, int h)
  {
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, 0, h - 1);
        g.drawLine(0, h - 1, w - 1, h - 1);
         
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(1, 0, 1, h - 1);
        g.drawLine(1, 0, w - 1, 0);
        
        g.setColor(MetalLookAndFeel.getControl());
        g.drawLine(1, h - 1, 1, h - 1);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, 0, 0, h - 1);
      }
  }

  /**
   * Paints the border for a button with an {@link #EAST} direction that
   * belongs to a scroll bar that is not free standing.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintEastBorder(Graphics g, int w, int h)
  {
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, w - 1, 0);
        g.drawLine(w - 1, 2, w - 1, h - 1);
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(0, 1, w - 2, 1);
        g.drawLine(0, 1, 0, h - 1);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, 0, w - 1, 0);
      }
  }
  
  /**
   * Paints the border for a button with a {@link #WEST} direction that
   * belongs to a scroll bar that is not free standing.
   * 
   * @param g  the graphics device.
   * @param w  the button width.
   * @param h  the button height.
   */
  private void paintWestBorder(Graphics g, int w, int h)
  {
    Rectangle bounds = SwingUtilities.getLocalBounds(this);
    if (isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(0, 0, bounds.width - 1, 0);
        g.setColor(MetalLookAndFeel.getControlHighlight());
        g.drawLine(0, 1, bounds.width - 1, 1);
        g.drawLine(0, 1, 0, bounds.height - 1);
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        g.drawLine(0, 0, bounds.width - 1, 0);
      }
  }
    
  /**
   * Returns the preferred size for the button, which varies depending on 
   * the direction of the button and whether or not it is free standing.
   * 
   * @return The preferred size.
   */
  public Dimension getPreferredSize()
  {
    int adj = 1;
    if (!freeStanding)
      adj = 2;
    
    if (direction == EAST)
      return new Dimension(buttonWidth - adj, buttonWidth);    
    else if (direction == WEST)
      return new Dimension(buttonWidth - 2, buttonWidth);
    else if (direction == SOUTH)
      return new Dimension(buttonWidth, buttonWidth - adj);
    else // assume NORTH
      return new Dimension(buttonWidth, buttonWidth - 2);
  }
  
  /**
   * Returns the minimum size for the button.
   * 
   * @return The minimum size for the button.
   */
  public Dimension getMinimumSize()
  {
    return getPreferredSize();
  }
 
  /**
   * Returns the maximum size for the button.
   * 
   * @return <code>Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE)</code>.
   */
  public Dimension getMaximumSize()
  {
    if (maximumSize == null)
      maximumSize = new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
    return maximumSize; 
  }
  
}
