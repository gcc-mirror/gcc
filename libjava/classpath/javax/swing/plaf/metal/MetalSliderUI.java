/* MetalSliderUI.java
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
import java.beans.PropertyChangeListener;
import java.util.HashMap;

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
public class MetalSliderUI
  extends BasicSliderUI
{
  // TODO: find a use for this
  protected static Color thumbColor;
  
  // TODO: find a use for this
  protected static Color highlightColor;
  
  // TODO: find a use for this
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

  /** 
   * A flag that controls whether or not the track is filled up to the value
   * of the slider.
   */
  protected boolean filledSlider;
    
  /** A key to look up the filledSlider setting in the {@link UIManager}. */
  protected final String SLIDER_FILL = "JSlider.isFilled";
  
  /** The UI instances for MetalSliderUIs */
  private static HashMap instances;

  /**
   * Constructs a new instance.
   */
  public MetalSliderUI()
  {
    super(null);
    filledSlider = UIManager.getBoolean(SLIDER_FILL);
  }

  /**
   * Returns an instance of MetalSliderUI.
   *
   * @param component the component for which we return an UI instance
   *
   * @return an instance of MetalSliderUI
   */
  public static ComponentUI createUI(JComponent component)
  {
    if (instances == null)
      instances = new HashMap();

    Object o = instances.get(component);
    MetalSliderUI instance;
    if (o == null)
      {
        instance = new MetalSliderUI();
        instances.put(component, instance);
      }
    else
      instance = (MetalSliderUI) o;

    return instance;
  }
  
  public void installUI(JComponent c)
  {
    super.installUI(c);
    Boolean b = (Boolean) c.getClientProperty(SLIDER_FILL);
    if (b != null) 
      filledSlider = b.booleanValue();
  }

  /**
   * Paints the thumb icon for the slider.
   * 
   * @param g  the graphics device.
   */
  public void paintThumb(Graphics g) 
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      horizThumbIcon.paintIcon(slider, g, thumbRect.x, thumbRect.y);
    else
      vertThumbIcon.paintIcon(slider, g, thumbRect.x, thumbRect.y);
  }
  
  /**
   * Creates a property change listener for the slider.
   * 
   * @param slider  the slider.
   */
  protected PropertyChangeListener createPropertyChangeListener(JSlider slider)
  {
    // TODO: try to figure out why it might be necessary to override this 
    // method as is done in Sun's implementation
    return super.createPropertyChangeListener(slider);    
  }
  
  /**
   * Paints the track along which the thumb control moves.
   * 
   * @param g  the graphics device.
   */
  public void paintTrack(Graphics g)
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
    {
      if (filledSlider) 
      {
        // TODO: fill the track
      }
      BasicGraphicsUtils.drawEtchedRect(g, trackRect.x, trackRect.y 
          + (trackRect.height - getTrackWidth()) / 2, trackRect.width - 1, 
          getTrackWidth(), Color.darkGray, Color.gray, Color.darkGray, 
          Color.white);
    }
    else
    {
      if (filledSlider) 
      {
        // TODO: fill the track
      }
      BasicGraphicsUtils.drawEtchedRect(g, trackRect.x  + (trackRect.width 
          - getTrackWidth()) / 2, trackRect.y, getTrackWidth(), 
          trackRect.height - 1, Color.darkGray, Color.gray, Color.darkGray, 
          Color.white);
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
    // do nothing as focus is shown by different color on thumb control
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
    return tickLength + TICK_BUFFER;
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
    return (slider.getOrientation() == JSlider.HORIZONTAL 
            ? tickRect.width : tickRect.height);
  }
  
  /**
   * Returns the thumb overhang.
   * 
   * @return The thumb overhang.
   */
  protected int getThumbOverhang()
  {
    // TODO: figure out what this is used for
    return 0;
  }
  
  protected void scrollDueToClickInTrack(int dir)
  {
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
    // TODO: get color from UIManager...
    g.setColor(new Color(153, 153, 204));
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
    // TODO: get color from UIManager...
    g.setColor(new Color(153, 153, 204));
    g.drawLine(x, TICK_BUFFER, x, TICK_BUFFER + tickLength);
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
    // TODO: get color from UIManager...
    g.setColor(new Color(153, 153, 204));
    g.drawLine(TICK_BUFFER - 1, y, TICK_BUFFER - 1 + tickLength / 2, y);
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
    // TODO: get color from UIManager...
    g.setColor(new Color(153, 153, 204));
    g.drawLine(TICK_BUFFER - 1, y, TICK_BUFFER - 1 + tickLength, y);
  }
  
}
