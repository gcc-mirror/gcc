/* MetalScrollBarUI.java
   Copyright (C) 2005, 2006, Free Software Foundation, Inc.

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
import java.awt.Insets;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollBarUI;

/**
 * A UI delegate for the {@link JScrollBar} component.
 */
public class MetalScrollBarUI extends BasicScrollBarUI
{
  
  /**
   * A property change handler for the UI delegate that monitors for
   * changes to the "JScrollBar.isFreeStanding" property, and updates
   * the buttons and track rendering as appropriate.
   */
  class MetalScrollBarPropertyChangeHandler 
    extends BasicScrollBarUI.PropertyChangeHandler
  {
    /**
     * Creates a new handler.
     * 
     * @see #createPropertyChangeListener()
     */
    public MetalScrollBarPropertyChangeHandler()
    {
      // Nothing to do here.
    }
    
    /**
     * Handles a property change event.  If the event name is
     * <code>JSlider.isFreeStanding</code>, this method updates the 
     * delegate, otherwise the event is passed up to the super class.
     * 
     * @param e  the property change event.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals(FREE_STANDING_PROP))
        {
          Boolean prop = (Boolean) e.getNewValue();
          isFreeStanding = prop == null ? true : prop.booleanValue();
          if (increaseButton != null)
            increaseButton.setFreeStanding(isFreeStanding);
          if (decreaseButton != null)
            decreaseButton.setFreeStanding(isFreeStanding);
        }
      else
        super.propertyChange(e);
    }
  }
  
  /** The name for the 'free standing' property. */
  public static final String FREE_STANDING_PROP = "JScrollBar.isFreeStanding";

  /** The minimum thumb size for a scroll bar that is not free standing. */
  private static final Dimension MIN_THUMB_SIZE = new Dimension(15, 15);

  /** The minimum thumb size for a scroll bar that is free standing. */
  private static final Dimension MIN_THUMB_SIZE_FREE_STANDING 
    = new Dimension(17, 17);
  
  /** The button that increases the value in the scroll bar. */
  protected MetalScrollButton increaseButton;
  
  /** The button that decreases the value in the scroll bar. */
  protected MetalScrollButton decreaseButton;
  
  /** 
   * The scroll bar width. 
   */
  protected int scrollBarWidth;
  
  /** 
   * A flag that indicates whether the scroll bar is "free standing", which 
   * means it has complete borders and can be used anywhere in the UI.  A 
   * scroll bar which is not free standing has borders missing from one
   * side, and relies on being part of another container with its own borders
   * to look right visually. */
  protected boolean isFreeStanding = true;
  
  /** 
   * The color for the scroll bar shadow (this is read from the UIDefaults in 
   * the installDefaults() method).
   */
  Color scrollBarShadowColor;
  
  /**
   * Constructs a new instance of <code>MetalScrollBarUI</code>, with no
   * specific initialisation.
   */
  public MetalScrollBarUI()
  {
    super();
  }

  /**
   * Returns a new instance of <code>MetalScrollBarUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return An instance of MetalScrollBarUI
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalScrollBarUI();
  }

  /**
   * Installs the defaults.
   */
  protected void installDefaults()
  {    
    // need to initialise isFreeStanding before calling the super class, 
    // so that the value is set when createIncreaseButton() and 
    // createDecreaseButton() are called (unless there is somewhere earlier
    // that we can do this).
    Boolean prop = (Boolean) scrollbar.getClientProperty(FREE_STANDING_PROP);
    isFreeStanding = prop == null ? true : prop.booleanValue();
    scrollBarShadowColor = UIManager.getColor("ScrollBar.shadow");
    scrollBarWidth = UIManager.getInt("ScrollBar.width");
    super.installDefaults();
  }
    
  /**
   * Creates a property change listener for the delegate to use.  This
   * overrides the method to provide a custom listener for the 
   * {@link MetalLookAndFeel} that can handle the 
   * <code>JScrollBar.isFreeStanding</code> property.
   * 
   * @return A property change listener.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new MetalScrollBarPropertyChangeHandler();
  }
  
  /**
   * Creates a new button to use as the control at the lower end of the
   * {@link JScrollBar}.  This method assigns the new button (an instance of
   * {@link MetalScrollButton} to the {@link #decreaseButton} field, and also 
   * returns the button.  The button width is determined by the 
   * <code>ScrollBar.width</code> setting in the UI defaults.
   * 
   * @param orientation  the orientation of the button ({@link #NORTH},
   *                     {@link #SOUTH}, {@link #EAST} or {@link #WEST}).
   * 
   * @return The button.
   */
  protected JButton createDecreaseButton(int orientation)
  {
    decreaseButton = new MetalScrollButton(orientation, scrollBarWidth, 
            isFreeStanding);
    return decreaseButton;
  }

  /**
   * Creates a new button to use as the control at the upper end of the
   * {@link JScrollBar}.  This method assigns the new button (an instance of
   * {@link MetalScrollButton} to the {@link #increaseButton} field, and also 
   * returns the button.  The button width is determined by the 
   * <code>ScrollBar.width</code> setting in the UI defaults.
   * 
   * @param orientation  the orientation of the button ({@link #NORTH},
   *                     {@link #SOUTH}, {@link #EAST} or {@link #WEST}).
   * 
   * @return The button.
   */
  protected JButton createIncreaseButton(int orientation)
  {
    increaseButton = new MetalScrollButton(orientation, scrollBarWidth, 
            isFreeStanding);
    return increaseButton;
  }
  
  /**
   * Paints the track for the scrollbar.
   * 
   * @param g  the graphics device.
   * @param c  the component.
   * @param trackBounds  the track bounds.
   */
  protected void paintTrack(Graphics g, JComponent c, Rectangle trackBounds)
  {
    g.setColor(MetalLookAndFeel.getControl());
    g.fillRect(trackBounds.x, trackBounds.y, trackBounds.width, 
            trackBounds.height);
    if (scrollbar.getOrientation() == HORIZONTAL) 
      paintTrackHorizontal(g, c, trackBounds.x, trackBounds.y, 
          trackBounds.width, trackBounds.height);
    else 
      paintTrackVertical(g, c, trackBounds.x, trackBounds.y, 
          trackBounds.width, trackBounds.height);
    
  }
  
  /**
   * Paints the track for a horizontal scrollbar.
   * 
   * @param g  the graphics device.
   * @param c  the component.
   * @param x  the x-coordinate for the track bounds.
   * @param y  the y-coordinate for the track bounds.
   * @param w  the width for the track bounds.
   * @param h  the height for the track bounds.
   */
  private void paintTrackHorizontal(Graphics g, JComponent c, 
      int x, int y, int w, int h)
  {
    if (c.isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(x, y, x, y + h - 1);
        g.drawLine(x, y, x + w - 1, y);
        g.drawLine(x + w - 1, y, x + w - 1, y + h - 1);
        
        g.setColor(scrollBarShadowColor);
        g.drawLine(x + 1, y + 1, x + 1, y + h - 1);
        g.drawLine(x + 1, y + 1, x + w - 2, y + 1);
        
        if (isFreeStanding) 
          {
            g.setColor(MetalLookAndFeel.getControlDarkShadow());
            g.drawLine(x, y + h - 2, x + w - 1, y + h - 2);
            g.setColor(scrollBarShadowColor);
            g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);
          }
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        if (isFreeStanding)
          g.drawRect(x, y, w - 1, h - 1);
        else
          {
            g.drawLine(x, y, x + w - 1, y);
            g.drawLine(x, y, x, y + h - 1);
            g.drawLine(x + w - 1, y, x + w - 1, y + h - 1);
          }
      }
  }
    
  /**
   * Paints the track for a vertical scrollbar.
   * 
   * @param g  the graphics device.
   * @param c  the component.
   * @param x  the x-coordinate for the track bounds.
   * @param y  the y-coordinate for the track bounds.
   * @param w  the width for the track bounds.
   * @param h  the height for the track bounds.
   */
  private void paintTrackVertical(Graphics g, JComponent c, 
      int x, int y, int w, int h)
  {
    if (c.isEnabled())
      {
        g.setColor(MetalLookAndFeel.getControlDarkShadow());
        g.drawLine(x, y, x, y + h - 1);
        g.drawLine(x, y, x + w - 1, y);
        g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);
        
        g.setColor(scrollBarShadowColor);
        g.drawLine(x + 1, y + 1, x + w - 1, y + 1);
        g.drawLine(x + 1, y + 1, x + 1, y + h - 2);
        
        if (isFreeStanding) 
          {
            g.setColor(MetalLookAndFeel.getControlDarkShadow());
            g.drawLine(x + w - 2, y, x + w - 2, y + h - 1);
            g.setColor(MetalLookAndFeel.getControlHighlight());
            g.drawLine(x + w - 1, y, x + w - 1, y + h - 1);
          }
      }
    else
      {
        g.setColor(MetalLookAndFeel.getControlDisabled());
        if (isFreeStanding)
          g.drawRect(x, y, w - 1, h - 1);
        else
          {
            g.drawLine(x, y, x + w - 1, y);
            g.drawLine(x, y, x, y + h - 1);
            g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);
          }
      }
  }

  /**
   * Paints the slider button of the ScrollBar.
   *
   * @param g the Graphics context to use
   * @param c the JComponent on which we paint
   * @param thumbBounds the rectangle that is the slider button
   */
  protected void paintThumb(Graphics g, JComponent c, Rectangle thumbBounds)
  {
    // a disabled scrollbar has no thumb in the metal look and feel
    if (!c.isEnabled())
      return;
    if (scrollbar.getOrientation() == HORIZONTAL)
      paintThumbHorizontal(g, c, thumbBounds);
    else 
      paintThumbVertical(g, c, thumbBounds);

    // Draw the pattern when the theme is not Ocean.
    if (! (MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme))
      {
        MetalUtils.fillMetalPattern(c, g, thumbBounds.x + 3, thumbBounds.y + 3,
                                    thumbBounds.width - 6,
                                    thumbBounds.height - 6,
                                    thumbHighlightColor,
                                    thumbLightShadowColor);
      }
  }

  /**
   * Paints the thumb for a horizontal scroll bar.
   * 
   * @param g  the graphics device.
   * @param c  the scroll bar component.
   * @param thumbBounds  the thumb bounds.
   */
  private void paintThumbHorizontal(Graphics g, JComponent c, 
          Rectangle thumbBounds) 
  {
    int x = thumbBounds.x;
    int y = thumbBounds.y;
    int w = thumbBounds.width;
    int h = thumbBounds.height;
    
    // First we fill the background.
    MetalTheme theme = MetalLookAndFeel.getCurrentTheme();
    if (theme instanceof OceanTheme
        && UIManager.get("ScrollBar.gradient") != null)
      {
        MetalUtils.paintGradient(g, x + 2, y + 2, w - 4, h - 2,
                                 SwingConstants.VERTICAL,
                                 "ScrollBar.gradient");
      }
    else
      {
        g.setColor(thumbColor);
        if (isFreeStanding)
          g.fillRect(x, y, w, h - 1);
        else
          g.fillRect(x, y, w, h);
      }

    // then draw the dark box
    g.setColor(thumbLightShadowColor);
    if (isFreeStanding)
      g.drawRect(x, y, w - 1, h - 2);
    else
      {
        g.drawLine(x, y, x + w - 1, y);
        g.drawLine(x, y, x, y + h - 1);
        g.drawLine(x + w - 1, y, x + w - 1, y + h - 1);
      }
    
    // then the highlight
    g.setColor(thumbHighlightColor);
    if (isFreeStanding)
      {
        g.drawLine(x + 1, y + 1, x + w - 3, y + 1);
        g.drawLine(x + 1, y + 1, x + 1, y + h - 3);
      }
    else
      {
        g.drawLine(x + 1, y + 1, x + w - 3, y + 1);
        g.drawLine(x + 1, y + 1, x + 1, y + h - 1);
      }
    
    // draw the shadow line
    g.setColor(UIManager.getColor("ScrollBar.shadow"));
    g.drawLine(x + w, y + 1, x + w, y + h - 1);

    // For the OceanTheme, draw the 3 lines in the middle.
    if (theme instanceof OceanTheme)
      {
        g.setColor(thumbLightShadowColor);
        int middle = x + w / 2;
        g.drawLine(middle - 2, y + 4, middle - 2, y + h - 5);
        g.drawLine(middle, y + 4, middle, y + h - 5);
        g.drawLine(middle + 2, y + 4, middle + 2, y + h - 5);
        g.setColor(UIManager.getColor("ScrollBar.highlight"));
        g.drawLine(middle - 1, y + 5, middle - 1, y + h - 4);
        g.drawLine(middle + 1, y + 5, middle + 1, y + h - 4);
        g.drawLine(middle + 3, y + 5, middle + 3, y + h - 4);
      }
  }
  
  /**
   * Paints the thumb for a vertical scroll bar.
   * 
   * @param g  the graphics device.
   * @param c  the scroll bar component.
   * @param thumbBounds  the thumb bounds.
   */
  private void paintThumbVertical(Graphics g, JComponent c, 
          Rectangle thumbBounds)
  {
    int x = thumbBounds.x;
    int y = thumbBounds.y;
    int w = thumbBounds.width;
    int h = thumbBounds.height;
    
    // First we fill the background.
    MetalTheme theme = MetalLookAndFeel.getCurrentTheme();
    if (theme instanceof OceanTheme
        && UIManager.get("ScrollBar.gradient") != null)
      {
        MetalUtils.paintGradient(g, x + 2, y + 2, w - 2, h - 4,
                                 SwingConstants.HORIZONTAL,
                                 "ScrollBar.gradient");
      }
    else
      {
        g.setColor(thumbColor);
        if (isFreeStanding)
          g.fillRect(x, y, w - 1, h);
        else
          g.fillRect(x, y, w, h);
      }

    // then draw the dark box
    g.setColor(thumbLightShadowColor);
    if (isFreeStanding)
      g.drawRect(x, y, w - 2, h - 1);
    else
      {
        g.drawLine(x, y, x + w - 1, y);
        g.drawLine(x, y, x, y + h - 1);
        g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);
      }
    
    // then the highlight
    g.setColor(thumbHighlightColor);
    if (isFreeStanding)
      {
        g.drawLine(x + 1, y + 1, x + w - 3, y + 1);
        g.drawLine(x + 1, y + 1, x + 1, y + h - 3);
      }
    else
      {
        g.drawLine(x + 1, y + 1, x + w - 1, y + 1);
        g.drawLine(x + 1, y + 1, x + 1, y + h - 3);
      }
    
    // draw the shadow line
    g.setColor(UIManager.getColor("ScrollBar.shadow"));
    g.drawLine(x + 1, y + h, x + w - 2, y + h);

    // For the OceanTheme, draw the 3 lines in the middle.
    if (theme instanceof OceanTheme)
      {
        g.setColor(thumbLightShadowColor);
        int middle = y + h / 2;
        g.drawLine(x + 4, middle - 2, x + w - 5, middle - 2);
        g.drawLine(x + 4, middle, x + w - 5, middle);
        g.drawLine(x + 4, middle + 2, x + w - 5, middle + 2);
        g.setColor(UIManager.getColor("ScrollBar.highlight"));
        g.drawLine(x + 5, middle - 1, x + w - 4, middle - 1);
        g.drawLine(x + 5, middle + 1, x + w - 4, middle + 1);
        g.drawLine(x + 5, middle + 3, x + w - 4, middle + 3);
      }
  }
  
  /**
   * Returns the minimum thumb size.  For a free standing scroll bar the 
   * minimum size is <code>17 x 17</code> pixels, whereas for a non free 
   * standing scroll bar the minimum size is <code>15 x 15</code> pixels.
   *
   * @return The minimum thumb size.
   */
  protected Dimension getMinimumThumbSize()
  {
    Dimension retVal;
    if (scrollbar != null)
      {
        if (isFreeStanding)
          retVal = MIN_THUMB_SIZE_FREE_STANDING;
        else
          retVal = MIN_THUMB_SIZE;
      }
    else
      retVal = new Dimension(0, 0);
    return retVal;
  }

  /**
   * Returns the <code>preferredSize</code> for the specified scroll bar.
   * For a vertical scrollbar the height is the sum of the preferred heights
   * of the buttons plus <code>30</code>. The width is fetched from the
   * <code>UIManager</code> property <code>ScrollBar.width</code>.
   *
   * For horizontal scrollbars the width is the sum of the preferred widths
   * of the buttons plus <code>30</code>. The height is fetched from the
   * <code>UIManager</code> property <code>ScrollBar.height</code>.
   *
   * @param c the scrollbar for which to calculate the preferred size
   *
   * @return the <code>preferredSize</code> for the specified scroll bar
   */
  public Dimension getPreferredSize(JComponent c)
  {
    int height;
    int width;
    height = width = 0;

    if (scrollbar.getOrientation() == SwingConstants.HORIZONTAL)
      {
        width += incrButton.getPreferredSize().getWidth();
        width += decrButton.getPreferredSize().getWidth();
        width += 30;
        height = UIManager.getInt("ScrollBar.width");
      }
    else
      {
        height += incrButton.getPreferredSize().getHeight();
        height += decrButton.getPreferredSize().getHeight();
        height += 30;
        width = UIManager.getInt("ScrollBar.width");
      }

    Insets insets = scrollbar.getInsets();

    height += insets.top + insets.bottom;
    width += insets.left + insets.right;

    return new Dimension(width, height);
  } 
}

