/* MetalSliderUI.java
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
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 * A UI delegate for the {@link JSlider} component.
 */
public class MetalSliderUI extends BasicSliderUI
{
  /**
   * A property change handler that updates the rendered component in response
   * to specific property change events.  This custom handler is used to
   * intercept the "JSlider.isFilled" property, which is only recognised by
   * the {@link MetalLookAndFeel}.
   */
  protected class MetalPropertyListener
    extends BasicSliderUI.PropertyChangeHandler
  {
    /**
     * Creates a new listener.
     */
    protected MetalPropertyListener()
    {
      // Nothing to do here.
    }
    
    /**
     * Handles property change events.  Events with the name "JSlider.isFilled"
     * are handled here, and other events are passed to the superclass.
     * 
     * @param e  the property change event.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals(SLIDER_FILL))
      {
        Boolean b = (Boolean) e.getNewValue();
        if (b == null)
          filledSlider = false;
        else
          filledSlider = b.booleanValue();   
      }
      else
        super.propertyChange(e);
    }
  }
  
  /** The thumb color (unused, because an icon is used to draw the thumb). */
  protected static Color thumbColor;
  
  /** 
   * The highlight color used for drawing the track rect when the slider is
   * enabled.
   */
  protected static Color highlightColor;
  
  /**
   * The shadow color used for drawing the track rect when the slider is
   * enabled.
   */
  protected static Color darkShadowColor;
  
  /** The track width. */
  protected static int trackWidth = UIManager.getInt("Slider.trackWidth");
  
  /** The length of the major tick marks. */
  protected static int tickLength = UIManager.getInt("Slider.majorTickLength");
  
  /** The icon used for the thumb control of horizontally oriented sliders. */
  protected static Icon horizThumbIcon = UIManager.getIcon(
          "Slider.horizontalThumbIcon");
  
  /** The icon used for the thumb control of vertically oriented sliders. */
  protected static Icon vertThumbIcon = UIManager.getIcon(
          "Slider.verticalThumbIcon");

  /** The gap between the track and the tick marks. */
  protected final int TICK_BUFFER = 4;

  /** A key to look up the filledSlider setting in the {@link UIManager}. */
  protected final String SLIDER_FILL = "JSlider.isFilled";
  
  /** 
   * A flag that controls whether or not the track is filled up to the value
   * of the slider.
   */
  protected boolean filledSlider;

  /**
   * Constructs a new instance.
   */
  public MetalSliderUI()
  {
    super(null);
    filledSlider = UIManager.getBoolean(SLIDER_FILL);
    darkShadowColor = MetalLookAndFeel.getControlDarkShadow();
    highlightColor = MetalLookAndFeel.getControlHighlight();
  }

  /**
   * Returns a new instance of <code>MetalSliderUI</code>.
   *
   * @param component the component (ignored).
   *
   * @return A new instance of <code>MetalSliderUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalSliderUI();
  }
  
  /**
   * Installs the default for this UI delegate in the supplied component.
   * 
   * @param c  the component.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    Boolean b = (Boolean) c.getClientProperty(SLIDER_FILL);
    if (b != null) 
      filledSlider = b.booleanValue();
  }

  /**
   * Creates a property change listener for the slider.  
   * 
   * @param slider  the slider.
   * 
   * @return A new instance of {@link MetalPropertyListener}.
   */
  protected PropertyChangeListener createPropertyChangeListener(JSlider slider)
  {
    return new MetalPropertyListener();    
  }
  
  /**
   * Paints the thumb icon for the slider.
   * 
   * @param g  the graphics device.
   */
  public void paintThumb(Graphics g) 
  {
    Color save = g.getColor();
    g.setColor(thumbColor);
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      horizThumbIcon.paintIcon(slider, g, thumbRect.x, thumbRect.y);
    else
      vertThumbIcon.paintIcon(slider, g, thumbRect.x, thumbRect.y);
    g.setColor(save);
  }
  
  /**
   * Paints the track along which the thumb control moves.
   * 
   * @param g  the graphics device.
   */
  public void paintTrack(Graphics g)
  {
    Color shadowColor = MetalLookAndFeel.getControlShadow();
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        int trackX = trackRect.x;
        int trackY = trackRect.y + (trackRect.height - getTrackWidth()) / 2;
        int trackW = trackRect.width;
        int trackH = getTrackWidth();
        
        // draw border
        if (slider.isEnabled())
          BasicGraphicsUtils.drawEtchedRect(g, trackX, trackY, trackW, trackH, 
              darkShadowColor, shadowColor, darkShadowColor, highlightColor);
        else
          {
            g.setColor(MetalLookAndFeel.getControlShadow());
            g.drawRect(trackX, trackY, trackW - 2, trackH - 2);
          }

        // fill track (if required)
        if (MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme)
          {
            if (slider.isEnabled())
              {
                int xPos = xPositionForValue(slider.getValue());
                int x = slider.getInverted() ? xPos : trackRect.x;
                int w = slider.getInverted() ? trackX + trackW - xPos 
                                             : xPos - trackRect.x;
                g.setColor(MetalLookAndFeel.getWhite());
                g.drawLine(x + 1, trackY + 1, x + w - 3, trackY + 1);
                g.setColor(UIManager.getColor("Slider.altTrackColor"));
                g.drawLine(x + 1, trackY + 2, x + w - 3, trackY + 2);
                g.setColor(MetalLookAndFeel.getControlShadow());
                g.drawLine(x + 1, trackY + 3, x + w - 3, trackY + 3);
                g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
                g.drawLine(x + 1, trackY + 4, x + w - 3, trackY + 4);
              }
          }
        else if (filledSlider) 
          {
            int xPos = xPositionForValue(slider.getValue());
            int x = slider.getInverted() ? xPos : trackRect.x;
            int w = slider.getInverted() ? trackX + trackW - xPos 
                                         : xPos - trackRect.x;
            g.setColor(MetalLookAndFeel.getControlShadow());
            g.fillRect(x + 1, trackY + 1, w - 3, getTrackWidth() - 3);
            if (slider.isEnabled())
              {
                g.setColor(MetalLookAndFeel.getControl());
                g.drawLine(x + 1, trackY + 1, x + w - 3, trackY + 1);
                g.drawLine(x + 1, trackY + 1, x + 1, 
                           trackY + getTrackWidth() - 3);
              }
          }
      }
    else
      {
        int trackX = trackRect.x  + (trackRect.width - getTrackWidth()) / 2;
        int trackY = trackRect.y;
        int trackW = getTrackWidth();
        int trackH = trackRect.height;
        if (slider.isEnabled())
          BasicGraphicsUtils.drawEtchedRect(g, trackX, trackY, trackW, trackH, 
              darkShadowColor, shadowColor, darkShadowColor, highlightColor);
        else
          {
            g.setColor(MetalLookAndFeel.getControlShadow());
            g.drawRect(trackX, trackY, trackW - 2, trackH - 2);
          }

        // Fill track if necessary.
        if (MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme)
          {
            if (slider.isEnabled())
              {
                int yPos = yPositionForValue(slider.getValue());
                int y = slider.getInverted() ? trackY : yPos;
                int h = slider.getInverted() ? yPos - trackY 
                        : trackY + trackH - yPos;
                
                g.setColor(MetalLookAndFeel.getWhite());
                g.drawLine(trackX + 1, y + 1, trackX + 1, y + h - 3);
                g.setColor(UIManager.getColor("Slider.altTrackColor"));
                g.drawLine(trackX + 2, y + 1, trackX + 2, y + h - 3);
                g.setColor(MetalLookAndFeel.getControlShadow());
                g.drawLine(trackX + 3, y + 1, trackX + 3, y + h - 3);
                g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
                g.drawLine(trackX + 4, y + 1, trackX + 4, y + h - 3);
              }
          }
        else if (filledSlider) 
          {
          int yPos = yPositionForValue(slider.getValue());
          int y = slider.getInverted() ? trackY : yPos;
          int h = slider.getInverted() ? yPos - trackY 
                  : trackY + trackH - yPos;
          g.setColor(MetalLookAndFeel.getControlShadow());
          g.fillRect(trackX + 1, y + 1, getTrackWidth() - 3, h - 3);
          if (slider.isEnabled())
            {
              g.setColor(MetalLookAndFeel.getControl());
              g.drawLine(trackX + 1, y + 1, trackX + trackW - 3, y + 1);
              g.drawLine(trackX + 1, y + 1, trackX + 1, y + h - 3);
            }
          }
      }
  }
  
  /**
   * Draws the focus rectangle for the slider.  The Metal look and feel 
   * indicates that the {@link JSlider} has the focus by changing the color of 
   * the thumb control - this is handled elsewhere and so this method is empty 
   * (it overrides the method in the {@link BasicSliderUI} class to prevent
   * a default focus highlight from being drawn).
   * 
   * @param g  the graphics device.
   */
  public void paintFocus(Graphics g)
  {
    thumbColor = getFocusColor();
    paintThumb(g);
  }
  
  /**
   * Returns the size of the thumb icon.
   * 
   * @return The size of the thumb icon.
   */
  protected Dimension getThumbSize()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      return new Dimension(horizThumbIcon.getIconWidth(), 
              horizThumbIcon.getIconHeight());
    else
      return new Dimension(vertThumbIcon.getIconWidth(), 
              vertThumbIcon.getIconHeight());
  }
  
  /**
   * Returns the length of the major tick marks.
   * 
   * @return The length of the major tick marks.
   */
  public int getTickLength()
  {
    int len = tickLength + TICK_BUFFER + 1;
    if (slider.getOrientation() == JSlider.VERTICAL)
      len += 2;
    return len;
  }
  
  /**
   * Returns the track width.
   * 
   * @return The track width.
   */
  protected int getTrackWidth()
  {
    return trackWidth;
  }
  
  /**
   * Returns the track length.
   * 
   * @return The track length.
   */
  protected int getTrackLength()
  {
    return slider.getOrientation() == JSlider.HORIZONTAL 
           ? tickRect.width : tickRect.height;
  }
  
  /**
   * Returns the thumb overhang.
   * 
   * @return The thumb overhang.
   */
  protected int getThumbOverhang()
  {
    // FIXME:  for what might this method be used?
    return 0;
  }
  
  protected void scrollDueToClickInTrack(int dir)
  {
    // FIXME:  for what might this method be overridden?
    super.scrollDueToClickInTrack(dir);
  }
  
  /**
   * Paints the minor ticks for a slider with a horizontal orientation.
   * 
   * @param g  the graphics device.
   * @param tickBounds  the tick bounds.
   * @param x  the x value for the tick.
   */
  protected void paintMinorTickForHorizSlider(Graphics g, Rectangle tickBounds,
                                              int x)
  {
    // Note the incoming 'g' has a translation in place to get us to the 
    // start of the tick rect already...
    if (slider.isEnabled())
      g.setColor(slider.getForeground());
    else
      g.setColor(MetalLookAndFeel.getControlShadow());
    g.drawLine(x, TICK_BUFFER, x, TICK_BUFFER + tickLength / 2);
  }
 
  /**
   * Paints the major ticks for a slider with a horizontal orientation.
   * 
   * @param g  the graphics device.
   * @param tickBounds  the tick bounds.
   * @param x  the x value for the tick.
   */
  protected void paintMajorTickForHorizSlider(Graphics g, Rectangle tickBounds,
                                              int x)
  {
    // Note the incoming 'g' has a translation in place to get us to the 
    // start of the tick rect already...
    if (slider.isEnabled())
      g.setColor(slider.getForeground());
    else
      g.setColor(MetalLookAndFeel.getControlShadow());
    g.drawLine(x, TICK_BUFFER, x, TICK_BUFFER + tickLength - 1);
  }
  
  /**
   * Paints the minor ticks for a slider with a vertical orientation.
   * 
   * @param g  the graphics device.
   * @param tickBounds  the tick bounds.
   * @param y  the y value for the tick.
   */
  protected void paintMinorTickForVertSlider(Graphics g, Rectangle tickBounds,
                                             int y)
  {
    // Note the incoming 'g' has a translation in place to get us to the 
    // start of the tick rect already...
    if (slider.isEnabled())
      g.setColor(slider.getForeground());
    else
      g.setColor(MetalLookAndFeel.getControlShadow());
    g.drawLine(TICK_BUFFER, y, TICK_BUFFER + tickLength / 2, y);
  }
  
  /**
   * Paints the major ticks for a slider with a vertical orientation.
   * 
   * @param g  the graphics device.
   * @param tickBounds  the tick bounds.
   * @param y  the y value for the tick.
   */
  protected void paintMajorTickForVertSlider(Graphics g, Rectangle tickBounds,
                                             int y)
  {
    // Note the incoming 'g' has a translation in place to get us to the 
    // start of the tick rect already...
    if (slider.isEnabled())
      g.setColor(slider.getForeground());
    else
      g.setColor(MetalLookAndFeel.getControlShadow());
    g.drawLine(TICK_BUFFER, y, TICK_BUFFER + tickLength, y);
  }
  
}
