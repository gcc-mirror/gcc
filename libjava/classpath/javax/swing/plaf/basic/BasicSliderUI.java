/* BasicSliderUI.java --
   Copyright (C) 2004, 2005, 2006,  Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Dictionary;
import java.util.Enumeration;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.BoundedRangeModel;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MouseInputAdapter;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.SliderUI;

/**
 * <p>
 * BasicSliderUI.java This is the UI delegate in the Basic look and feel that
 * paints JSliders.
 * </p>
 * 
 * <p>
 * The UI delegate keeps track of 6 rectangles that place the various parts of
 * the JSlider inside the component.
 * </p>
 * 
 * <p>
 * The rectangles are organized as follows:
 * </p>
 * <pre>
 *     +-------------------------------------------------------+ <-- focusRect
 *     |                                                       |
 *     |  +==+-------------------+==+--------------------+==+<------ contentRect
 *     |  |  |                   |  |<---thumbRect       |  |  |
 *     |  |  |    TRACK          |  |                    |<--------- trackRect
 *     |  |  +-------------------+==+--------------------+  |  |
 *     |  |  |                                           |  |  |
 *     |  |  |          TICKS GO HERE                    |<-------- tickRect
 *     |  |  |                                           |  |  |
 *     |  +==+-------------------------------------------+==+  |
 *     |  |  |                                           |  |  |
 *     |  |  |                                           |  |<----- labelRect
 *     |  |  |                 LABELS GO HERE            |  |  |
 *     |  |  |                                           |  |  |
 *     |  |  |                                           |  |  |
 *     |  |  |                                           |  |  |
 *     |  |  |                                           |  |  |
 *     |  |                                              |  |  |
 * </pre>
 * 
 * <p>
 * The space between the contentRect and the focusRect are the FocusInsets.
 * </p>
 * 
 * <p>
 * The space between the focusRect and the component bounds is the insetCache
 * which are the component's insets.
 * </p>
 * 
 * <p>
 * The top of the thumb is the top of the contentRect. The trackRect has to be
 * as tall as the thumb.
 * </p>
 * 
 * <p>
 * The trackRect and tickRect do not start from the left edge of the
 * focusRect. They are trackBuffer away from each side of the focusRect. This
 * is so that the thumb has room to move.
 * </p>
 * 
 * <p>
 * The labelRect does start right against the contentRect's left and right
 * edges and it gets all remaining space.
 * </p>
 */
public class BasicSliderUI extends SliderUI
{
  /**
   * Helper class that listens to the {@link JSlider}'s model for changes.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class ChangeHandler implements ChangeListener
  {
    /**
     * Called when the slider's model has been altered. The UI delegate should
     * recalculate any rectangles that are dependent on the model for their
     * positions and repaint.
     *
     * @param e A static {@link ChangeEvent} passed from the model.
     */
    public void stateChanged(ChangeEvent e)
    {
      // Maximum, minimum, and extent values will be taken
      // care of automatically when the slider is repainted.
      // Only thing that needs recalculation is the thumb.
      calculateThumbLocation();
      slider.repaint();
    }
  }

  /**
   * Helper class that listens for resize events.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class ComponentHandler extends ComponentAdapter
  {
    /**
     * Called when the size of the component changes. The UI delegate should
     * recalculate any rectangles that are dependent on the model for their
     * positions and repaint.
     *
     * @param e A {@link ComponentEvent}.
     */
    public void componentResized(ComponentEvent e)
    {
      calculateGeometry();
      slider.repaint();
    }
  }

  /**
   * Helper class that listens for focus events.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class FocusHandler implements FocusListener
  {
    /**
     * Called when the {@link JSlider} has gained focus.  It should repaint
     * the slider with the focus drawn.
     *
     * @param e A {@link FocusEvent}.
     */
    public void focusGained(FocusEvent e)
    {
      slider.repaint();
    }

    /**
     * Called when the {@link JSlider} has lost focus. It  should repaint the
     * slider without the focus drawn.
     *
     * @param e A {@link FocusEvent}.
     */
    public void focusLost(FocusEvent e)
    {
      slider.repaint();
    }
  }

  /**
   * Helper class that listens for changes to the properties of the {@link
   * JSlider}.
   */
  public class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * Called when one of the properties change. The UI should recalculate any
     * rectangles if necessary and repaint.
     *
     * @param e A {@link PropertyChangeEvent}.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      // Check for orientation changes.
      String prop = e.getPropertyName();
      if (prop.equals("orientation")
          || prop.equals("inverted")
          || prop.equals("labelTable")
          || prop.equals("majorTickSpacing")
          || prop.equals("minorTickSpacing")
          || prop.equals("paintTicks")
          || prop.equals("paintTrack")
          || prop.equals("paintLabels"))
        {
          calculateGeometry();
          slider.repaint();
        }
      else if (e.getPropertyName().equals("model"))
        {
          BoundedRangeModel oldModel = (BoundedRangeModel) e.getOldValue();
          oldModel.removeChangeListener(changeListener);
          slider.getModel().addChangeListener(changeListener);
          calculateThumbLocation();
          slider.repaint();
        }
    }
  }

  /**
   * Helper class that listens to our swing timer. This class is responsible
   * for listening to the timer and moving the thumb in the proper direction
   * every interval.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class ScrollListener implements ActionListener
  {
    /** Indicates which direction the thumb should scroll. */
    private transient int direction;

    /** Indicates whether we should scroll in blocks or in units. */
    private transient boolean block;

    /**
     * Creates a new ScrollListener object.
     */
    public ScrollListener()
    {
      direction = POSITIVE_SCROLL;
      block = false;
    }

    /**
     * Creates a new ScrollListener object.
     *
     * @param dir The direction to scroll in.
     * @param block If movement will be in blocks.
     */
    public ScrollListener(int dir, boolean block)
    {
      direction = dir;
      this.block = block;
    }

    /**
     * Called every time the swing timer reaches its interval. If the thumb
     * needs to move, then this method will move the thumb one block or  unit
     * in the direction desired. Otherwise, the timer can be stopped.
     *
     * @param e An {@link ActionEvent}.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (! trackListener.shouldScroll(direction))
        {
          scrollTimer.stop();
          return;
        }

      if (block)
        scrollByBlock(direction);
      else
        scrollByUnit(direction);
    }

    /**
     * Sets the direction to scroll in.
     *
     * @param direction The direction to scroll in.
     */
    public void setDirection(int direction)
    {
      this.direction = direction;
    }

    /**
     * Sets whether movement will be in blocks.
     *
     * @param block If movement will be in blocks.
     */
    public void setScrollByBlock(boolean block)
    {
      this.block = block;
    }
  }

  /**
   * Helper class that listens for mouse events.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class TrackListener extends MouseInputAdapter
  {
    /** The current X position of the mouse. */
    protected int currentMouseX;

    /** The current Y position of the mouse. */
    protected int currentMouseY;

    /**
     * The offset between the current slider value and the cursor's position.
     */
    protected int offset;

    /**
     * Called when the mouse has been dragged. This should find the mouse's
     * current position and adjust the value of the {@link JSlider}
     * accordingly.
     *
     * @param e A {@link MouseEvent}
     */
    public void mouseDragged(MouseEvent e)
    {
      dragging = true;
      if (slider.isEnabled())
        {
          currentMouseX = e.getX();
          currentMouseY = e.getY();
          if (slider.getValueIsAdjusting())
            {
              int value;
              if (slider.getOrientation() == JSlider.HORIZONTAL)
                value = valueForXPosition(currentMouseX) - offset;
              else
                value = valueForYPosition(currentMouseY) - offset;

              slider.setValue(value);
            }
        }
    }

    /**
     * Called when the mouse has moved over a component but no buttons have
     * been pressed yet.
     *
     * @param e A {@link MouseEvent}
     */
    public void mouseMoved(MouseEvent e)
    {
      // Don't care that we're moved unless we're dragging.
    }

    /**
     * Called when the mouse is pressed. When the press occurs on the thumb
     * itself, the {@link JSlider} should have its value set to where the
     * mouse was pressed. If the press occurs on the track, then the thumb
     * should move one block towards the direction of the mouse.
     *
     * @param e A {@link MouseEvent}
     */
    public void mousePressed(MouseEvent e)
    {
      if (slider.isEnabled())
        {
          currentMouseX = e.getX();
          currentMouseY = e.getY();

          int value;
          if (slider.getOrientation() == JSlider.HORIZONTAL)
            value = valueForXPosition(currentMouseX);
          else
            value = valueForYPosition(currentMouseY);

          if (slider.getSnapToTicks())
            value = findClosestTick(value);

          // If the thumb is hit, then we don't need to set the timers to 
          // move it. 
          if (! thumbRect.contains(e.getPoint()))
            {
              // The mouse has hit some other part of the slider.
              // The value moves no matter where in the slider you hit.
              if (value > slider.getValue())
                scrollDueToClickInTrack(POSITIVE_SCROLL);
              else
                scrollDueToClickInTrack(NEGATIVE_SCROLL);
            }
          else
            {
              slider.setValueIsAdjusting(true);
              offset = value - slider.getValue();
            }
        }
    }

    /**
     * Called when the mouse is released.  This should stop the timer that
     * scrolls the thumb.
     *
     * @param e A {@link MouseEvent}
     */
    public void mouseReleased(MouseEvent e)
    {
      dragging = false;
      if (slider.isEnabled())
        {
          currentMouseX = e.getX();
          currentMouseY = e.getY();

          if (slider.getValueIsAdjusting())
            {
              slider.setValueIsAdjusting(false);
              if (slider.getSnapToTicks())
                slider.setValue(findClosestTick(slider.getValue()));
            }
          if (scrollTimer != null)
            scrollTimer.stop();
        }
      slider.repaint();
    }

    /**
     * Indicates whether the thumb should scroll in the given direction.
     *
     * @param direction The direction to check.
     *
     * @return True if the thumb should move in that direction.
     */
    public boolean shouldScroll(int direction)
    {
      int value;
      if (slider.getOrientation() == JSlider.HORIZONTAL)
        value = valueForXPosition(currentMouseX);
      else
        value = valueForYPosition(currentMouseY);

      if (direction == POSITIVE_SCROLL)
        return value > slider.getValue();
      else
        return value < slider.getValue();
    }
  }

  /**
   * This class is no longer used as of JDK1.3.
   */
  public class ActionScroller extends AbstractAction
  {
    /**
     * Not used.
     *
     * @param slider not used
     * @param dir not used
     * @param block not used
     */
    public ActionScroller(JSlider slider, int dir, boolean block)
    {
      // Not used.
    }

    /**
     * Not used.
     *
     * @param event not used
     */
    public void actionPerformed(ActionEvent event)
    {
      // Not used.
    }
  }

  /** Listener for changes from the model. */
  protected ChangeListener changeListener;

  /** Listener for changes to the {@link JSlider}. */
  protected PropertyChangeListener propertyChangeListener;

  /** Listener for the scrollTimer. */
  protected ScrollListener scrollListener;

  /** Listener for component resizing. */
  protected ComponentListener componentListener;

  /** Listener for focus handling. */
  protected FocusListener focusListener;

  /** Listener for mouse events. */
  protected TrackListener trackListener;

  /** The insets between the FocusRectangle and the ContentRectangle. */
  protected Insets focusInsets;

  /** The {@link JSlider}'s insets. */
  protected Insets insetCache;

  /** Rectangle describing content bounds. See diagram above. */
  protected Rectangle contentRect;

  /** Rectangle describing focus bounds. See diagram above. */
  protected Rectangle focusRect;

  /** Rectangle describing the thumb's bounds. See diagram above. */
  protected Rectangle thumbRect;

  /** Rectangle describing the tick bounds. See diagram above. */
  protected Rectangle tickRect;

  /** Rectangle describing the label bounds. See diagram above. */
  protected Rectangle labelRect;

  /** Rectangle describing the track bounds. See diagram above. */
  protected Rectangle trackRect;

  /** FIXME: use this somewhere. */
  public static final int MAX_SCROLL = 2;

  /** FIXME: use this somewhere. */
  public static final int MIN_SCROLL = -2;

  /** A constant describing scrolling towards the minimum. */
  public static final int NEGATIVE_SCROLL = -1;

  /** A constant describing scrolling towards the maximum. */
  public static final int POSITIVE_SCROLL = 1;

  /** The gap between the edges of the contentRect and trackRect. */
  protected int trackBuffer;

  /** Whether this slider is actually drawn left to right. */
  protected boolean leftToRightCache;

  /** A timer that periodically moves the thumb. */
  protected Timer scrollTimer;

  /** A reference to the {@link JSlider} that this UI was created for. */
  protected JSlider slider;

  /** The shadow color. */
  private transient Color shadowColor;

  /** The highlight color. */
  private transient Color highlightColor;

  /** The focus color. */
  private transient Color focusColor;

  /** True if the user is dragging the slider. */
  boolean dragging;

  /**
   * Creates a new Basic look and feel Slider UI.
   *
   * @param b The {@link JSlider} that this UI was created for.
   */
  public BasicSliderUI(JSlider b)
  {
    super();
  }

  /**
   * Returns true if the user is dragging the slider.
   * 
   * @return true if the slider is being dragged.
   * 
   * @since 1.5
   */
  protected boolean isDragging()
  {
    return dragging;
  }
  
  /**
   * Gets the shadow color to be used for this slider. The shadow color is the
   * color used for drawing the top and left edges of the track.
   *
   * @return The shadow color.
   */
  protected Color getShadowColor()
  {
    return shadowColor;
  }

  /**
   * Gets the highlight color to be used for this slider. The highlight color
   * is the color used for drawing the bottom and right edges of the track.
   *
   * @return The highlight color.
   */
  protected Color getHighlightColor()
  {
    return highlightColor;
  }

  /**
   * Gets the focus color to be used for this slider. The focus color is the
   * color used for drawing the focus rectangle when the component gains
   * focus.
   *
   * @return The focus color.
   */
  protected Color getFocusColor()
  {
    return focusColor;
  }

  /**
   * Factory method to create a BasicSliderUI for the given {@link
   * JComponent}, which should be a {@link JSlider}.
   *
   * @param b The {@link JComponent} a UI is being created for.
   *
   * @return A BasicSliderUI for the {@link JComponent}.
   */
  public static ComponentUI createUI(JComponent b)
  {
    return new BasicSliderUI((JSlider) b);
  }

  /**
   * Installs and initializes all fields for this UI delegate. Any properties
   * of the UI that need to be initialized and/or set to defaults will be
   * done now. It will also install any listeners necessary.
   *
   * @param c The {@link JComponent} that is having this UI installed.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    if (c instanceof JSlider)
      {
        slider = (JSlider) c;

        focusRect = new Rectangle();
        contentRect = new Rectangle();
        thumbRect = new Rectangle();
        trackRect = new Rectangle();
        tickRect = new Rectangle();
        labelRect = new Rectangle();

        insetCache = slider.getInsets();
        leftToRightCache = ! slider.getInverted();

        scrollTimer = new Timer(200, null);
        scrollTimer.setRepeats(true);

        installDefaults(slider);
        installListeners(slider);
        installKeyboardActions(slider);

        calculateFocusRect();

        calculateContentRect();
        calculateThumbSize();
        calculateTrackBuffer();
        calculateTrackRect();
        calculateThumbLocation();

        calculateTickRect();
        calculateLabelRect();
      }
  }

  /**
   * Performs the opposite of installUI. Any properties or resources that need
   * to be cleaned up will be done now. It will also uninstall any listeners
   * it has. In addition, any properties of this UI will be nulled.
   *
   * @param c The {@link JComponent} that is having this UI uninstalled.
   */
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);

    uninstallKeyboardActions(slider);
    uninstallListeners(slider);

    scrollTimer = null;

    focusRect = null;
    contentRect = null;
    thumbRect = null;
    trackRect = null;
    tickRect = null;
    labelRect = null;

    focusInsets = null;
  }

  /**
   * Initializes any default properties that this UI has from the defaults for
   * the Basic look and feel.
   *
   * @param slider The {@link JSlider} that is having this UI installed.
   */
  protected void installDefaults(JSlider slider)
  {
    LookAndFeel.installColors(slider, "Slider.background",
                              "Slider.foreground");
    LookAndFeel.installBorder(slider, "Slider.border");
    shadowColor = UIManager.getColor("Slider.shadow");
    highlightColor = UIManager.getColor("Slider.highlight");
    focusColor = UIManager.getColor("Slider.focus");
    focusInsets = UIManager.getInsets("Slider.focusInsets");
    slider.setOpaque(true);
  }

  /**
   * Creates a new {@link TrackListener}.
   *
   * @param slider The {@link JSlider} that this {@link TrackListener} is
   *        created for.
   *
   * @return A new {@link TrackListener}.
   */
  protected TrackListener createTrackListener(JSlider slider)
  {
    return new TrackListener();
  }

  /**
   * Creates a new {@link ChangeListener}.
   *
   * @param slider The {@link JSlider} that this {@link ChangeListener} is
   *        created for.
   *
   * @return A new {@link ChangeListener}.
   */
  protected ChangeListener createChangeListener(JSlider slider)
  {
    return new ChangeHandler();
  }

  /**
   * Creates a new {@link ComponentListener}.
   *
   * @param slider The {@link JSlider} that this {@link ComponentListener} is
   *        created for.
   *
   * @return A new {@link ComponentListener}.
   */
  protected ComponentListener createComponentListener(JSlider slider)
  {
    return new ComponentHandler();
  }

  /**
   * Creates a new {@link FocusListener}.
   *
   * @param slider The {@link JSlider} that this {@link FocusListener} is
   *        created for.
   *
   * @return A new {@link FocusListener}.
   */
  protected FocusListener createFocusListener(JSlider slider)
  {
    return new FocusHandler();
  }

  /**
   * Creates a new {@link ScrollListener}.
   *
   * @param slider The {@link JSlider} that this {@link ScrollListener} is
   *        created for.
   *
   * @return A new {@link ScrollListener}.
   */
  protected ScrollListener createScrollListener(JSlider slider)
  {
    return new ScrollListener();
  }

  /**
   * Creates a new {@link PropertyChangeListener}.
   *
   * @param slider The {@link JSlider} that this {@link
   *        PropertyChangeListener} is created for.
   *
   * @return A new {@link PropertyChangeListener}.
   */
  protected PropertyChangeListener createPropertyChangeListener(JSlider slider)
  {
    return new PropertyChangeHandler();
  }

  /**
   * Creates and registers all the listeners for this UI delegate. This
   * includes creating the ScrollListener and registering it to the timer.
   *
   * @param slider The {@link JSlider} is having listeners installed.
   */
  protected void installListeners(JSlider slider)
  {
    propertyChangeListener = createPropertyChangeListener(slider);
    componentListener = createComponentListener(slider);
    trackListener = createTrackListener(slider);
    focusListener = createFocusListener(slider);
    changeListener = createChangeListener(slider);
    scrollListener = createScrollListener(slider);

    slider.addPropertyChangeListener(propertyChangeListener);
    slider.addComponentListener(componentListener);
    slider.addMouseListener(trackListener);
    slider.addMouseMotionListener(trackListener);
    slider.addFocusListener(focusListener);
    slider.getModel().addChangeListener(changeListener);

    scrollTimer.addActionListener(scrollListener);
  }

  /**
   * Unregisters all the listeners that this UI delegate was using. In
   * addition, it will also null any listeners that it was using.
   *
   * @param slider The {@link JSlider} that is having listeners removed.
   */
  protected void uninstallListeners(JSlider slider)
  {
    slider.removePropertyChangeListener(propertyChangeListener);
    slider.removeComponentListener(componentListener);
    slider.removeMouseListener(trackListener);
    slider.removeMouseMotionListener(trackListener);
    slider.removeFocusListener(focusListener);
    slider.getModel().removeChangeListener(changeListener);

    scrollTimer.removeActionListener(scrollListener);

    propertyChangeListener = null;
    componentListener = null;
    trackListener = null;
    focusListener = null;
    changeListener = null;
    scrollListener = null;
  }

  /**
   * Installs any keyboard actions. The list of keys that need to be bound are
   * listed in Basic look and feel's defaults.
   *
   * @param slider The {@link JSlider} that is having keyboard actions
   *        installed.
   */
  protected void installKeyboardActions(JSlider slider)
  {
    InputMap keyMap = getInputMap(JComponent.WHEN_FOCUSED);
    SwingUtilities.replaceUIInputMap(slider, JComponent.WHEN_FOCUSED, keyMap);
    ActionMap map = getActionMap();
    SwingUtilities.replaceUIActionMap(slider, map);
  }

  /**
   * Uninstalls any keyboard actions. The list of keys used  are listed in
   * Basic look and feel's defaults.
   *
   * @param slider The {@link JSlider} that is having keyboard actions
   *        uninstalled.
   */
  protected void uninstallKeyboardActions(JSlider slider)
  {
    SwingUtilities.replaceUIActionMap(slider, null);
    SwingUtilities.replaceUIInputMap(slider, JComponent.WHEN_FOCUSED, null);
  }

  /* XXX: This is all after experimentation with SUN's implementation.

     PreferredHorizontalSize seems to be 200x21.
     PreferredVerticalSize seems to be 21x200.

     MinimumHorizontalSize seems to be 36x21.
     MinimumVerticalSize seems to be 21x36.

     PreferredSize seems to be 200x63. Or Components.getBounds?

     MinimumSize seems to be 36x63.

     MaximumSize seems to be 32767x63.
   */

  /**
   * This method returns the preferred size when the slider is horizontally
   * oriented.
   *
   * @return The dimensions of the preferred horizontal size.
   */
  public Dimension getPreferredHorizontalSize()
  {
    Dimension dim = UIManager.getDimension("Slider.horizontalSize");
    if (dim == null) // Just to be sure we mirror the default.
      dim = new Dimension(200, 21);
    return dim;
  }

  /**
   * This method returns the preferred size when the slider is vertically
   * oriented.
   *
   * @return The dimensions of the preferred vertical size.
   */
  public Dimension getPreferredVerticalSize()
  {
    Dimension dim = UIManager.getDimension("Slider.verticalSize");
    if (dim == null) // Just to be sure we mirror the default.
      dim = new Dimension(21, 200);
    return dim;
  }

  /**
   * This method returns the minimum size when the slider is horizontally
   * oriented.
   *
   * @return The dimensions of the minimum horizontal size.
   */
  public Dimension getMinimumHorizontalSize()
  {
    Dimension dim = UIManager.getDimension("Slider.minimumHorizontalSize");
    if (dim == null) // Just to be sure we mirror the default.
      dim = new Dimension(36, 21);
    return dim;
  }

  /**
   * This method returns the minimum size of the slider when it  is vertically
   * oriented.
   *
   * @return The dimensions of the minimum vertical size.
   */
  public Dimension getMinimumVerticalSize()
  {
    Dimension dim = UIManager.getDimension("Slider.minimumVerticalSize");
    if (dim == null) // Just to be sure we mirror the default.
      dim = new Dimension(21, 36);
    return dim;
  }

  /**
   * This method returns the preferred size of the component. If it returns
   * null, then it is up to the Layout Manager to give the {@link JComponent}
   * a size.
   *
   * @param c The {@link JComponent} to find the preferred size for.
   *
   * @return The dimensions of the preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    recalculateIfInsetsChanged();
    Dimension dim;
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        // Create copy here to protect the UIManager value.
        dim = new Dimension(getPreferredHorizontalSize());
        dim.height = insetCache.top + insetCache.bottom;
        dim.height += focusInsets.top + focusInsets.bottom;
        dim.height += trackRect.height + tickRect.height + labelRect.height;
      }
    else
      {
        // Create copy here to protect the UIManager value.
        dim = new Dimension(getPreferredVerticalSize());
        dim.width = insetCache.left + insetCache.right;
        dim.width += focusInsets.left + focusInsets.right;
        dim.width += trackRect.width + tickRect.width + labelRect.width;
      }
    return dim;
  }

  /**
   * This method returns the minimum size for this {@link JSlider}  for this
   * look and feel. If it returns null, then it is up to the Layout Manager
   * to give the {@link JComponent} a size.
   *
   * @param c The {@link JComponent} to find the minimum size for.
   *
   * @return The dimensions of the minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    recalculateIfInsetsChanged();
    Dimension dim;
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        // Create copy here to protect the UIManager value.
        dim = new Dimension(getMinimumHorizontalSize());
        dim.height = insetCache.top + insetCache.bottom;
        dim.height += focusInsets.top + focusInsets.bottom;
        dim.height += trackRect.height + tickRect.height + labelRect.height;
      }
    else
      {
        // Create copy here to protect the UIManager value.
        dim = new Dimension(getMinimumVerticalSize());
        dim.width = insetCache.left + insetCache.right;
        dim.width += focusInsets.left + focusInsets.right;
        dim.width += trackRect.width + tickRect.width + labelRect.width;
      }
    return dim;
  }

  /**
   * This method returns the maximum size for this {@link JSlider} for this
   * look and feel.
   *
   * @param c The {@link JComponent} to find a maximum size for.
   *
   * @return The dimensions of the maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    Dimension dim = getPreferredSize(c);
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      dim.width = Short.MAX_VALUE;
    else
      dim.height = Short.MAX_VALUE;
    return dim;
  }

  /**
   * This method calculates all the sizes of the rectangles by delegating to
   * the helper methods calculateXXXRect.
   */
  protected void calculateGeometry()
  {
    calculateFocusRect();
    calculateContentRect();
    calculateThumbSize();
    calculateTrackBuffer();
    calculateTrackRect();
    calculateTickRect();
    calculateLabelRect();
    calculateThumbLocation();
  }

  /**
   * This method calculates the size and position of the focusRect. This
   * method does not need to be called if the orientation changes.
   */
  protected void calculateFocusRect()
  {
    focusRect.x = insetCache.left;
    focusRect.y = insetCache.top;
    focusRect.width = slider.getWidth() - insetCache.left - insetCache.right;
    focusRect.height = slider.getHeight() - insetCache.top - insetCache.bottom;
  }

  /**
   * Sets the width and height of the <code>thumbRect</code> field, using the
   * dimensions returned by {@link #getThumbSize()}.
   */
  protected void calculateThumbSize()
  {
    Dimension d = getThumbSize();
    thumbRect.width = d.width;
    thumbRect.height = d.height;
  }

  /**
   * Updates the <code>contentRect</code> field to an area inside the 
   * <code>focusRect</code>. This method does not need to be called if the 
   * orientation changes.
   */
  protected void calculateContentRect()
  {
    contentRect.x = focusRect.x + focusInsets.left;
    contentRect.y = focusRect.y + focusInsets.top;
    
    contentRect.width = focusRect.width - focusInsets.left - focusInsets.right;
    contentRect.height = focusRect.height - focusInsets.top
                         - focusInsets.bottom;
  }

  /**
   * Calculates the position of the thumbRect based on the current value of
   * the slider. It must take into  account the orientation of the slider.
   */
  protected void calculateThumbLocation()
  {
    int value = slider.getValue();

    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        thumbRect.x = xPositionForValue(value) - thumbRect.width / 2;
        thumbRect.y = trackRect.y + 1;
      }
    else
      {
        thumbRect.x = trackRect.x + 1;
        thumbRect.y = yPositionForValue(value) - thumbRect.height / 2;
      }
  }

  /**
   * Calculates the gap size between the edge of the <code>contentRect</code> 
   * and the edge of the <code>trackRect</code>, storing the result in the
   * <code>trackBuffer</code> field.  Sufficient space needs to be reserved 
   * for the slider thumb and/or the labels at each end of the slider track.
   */
  protected void calculateTrackBuffer()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        int w = Math.max(getWidthOfLowValueLabel(), getWidthOfHighValueLabel());
        trackBuffer = Math.max(thumbRect.width / 2, w / 2);
        
      }
    else
      {
        int h = Math.max(getHeightOfLowValueLabel(), 
                         getHeightOfHighValueLabel());
        trackBuffer = Math.max(thumbRect.height / 2, h / 2);
      }
  }

  /**
   * Returns the size of the slider's thumb.  The size is hard coded to
   * <code>11 x 20</code> for horizontal sliders, and <code>20 x 11</code> for 
   * vertical sliders. Note that a new instance of {@link Dimension} is 
   * returned for every call to this method (this seems wasteful, but 
   * {@link Dimension} instances are not immutable, so this is probably 
   * unavoidable).
   *
   * @return The size of the slider's thumb.
   */
  protected Dimension getThumbSize()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      return new Dimension(11, 20);
    else
      return new Dimension(20, 11);
  }

  /**
   * Calculates the size and position of the trackRect. It must take into
   * account the orientation of the slider.
   */
  protected void calculateTrackRect()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        int center = thumbRect.height;
        if (slider.getPaintTicks())
          center += getTickLength();
        if (slider.getPaintLabels())
          center += getHeightOfTallestLabel();
        trackRect.x = contentRect.x + trackBuffer;
        trackRect.y = contentRect.y + (contentRect.height - center - 1) / 2;
        trackRect.width = contentRect.width - 2 * trackBuffer;
        trackRect.height = thumbRect.height;
      }
    else
      {
        int center = thumbRect.width;
        if (slider.getPaintTicks())
          center += getTickLength();
        if (slider.getPaintLabels())
          center += getWidthOfWidestLabel();
        trackRect.x = contentRect.x + (contentRect.width - center - 1) / 2;
        trackRect.y = contentRect.y + trackBuffer;
        trackRect.width = thumbRect.width;
        trackRect.height = contentRect.height - 2 * trackBuffer;
      }
  }

  /**
   * This method returns the height of the tick area box if the slider  is
   * horizontal and the width of the tick area box is the slider is vertical.
   * It not necessarily how long the ticks will be. If a gap between the edge
   * of tick box and the actual tick is desired, then that will need to be
   * handled in the tick painting methods.
   *
   * @return The height (or width if the slider is vertical) of the tick
   *         rectangle.
   */
  protected int getTickLength()
  {
    return 8;
  }

  /**
   * This method calculates the size and position of the tickRect. It must
   * take into account the orientation of the slider.
   */
  protected void calculateTickRect()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        tickRect.x = trackRect.x;
        tickRect.y = trackRect.y + trackRect.height;
        tickRect.width = trackRect.width;
        tickRect.height = getTickLength();
        
        // this makes our Mauve tests pass...can't explain it!
        if (!slider.getPaintTicks())
          {
            tickRect.y--;
            tickRect.height = 0;
          }
      }
    else
      {
        tickRect.x = trackRect.x + trackRect.width;
        tickRect.y = trackRect.y;
        tickRect.width = getTickLength();
        tickRect.height = trackRect.height;

        // this makes our Mauve tests pass...can't explain it!
        if (!slider.getPaintTicks())
          {
            tickRect.x--;
            tickRect.width = 0;
          }
      }
  }

  /**
   * Calculates the <code>labelRect</code> field, taking into account the 
   * orientation of the slider.
   */
  protected void calculateLabelRect()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        if (slider.getPaintLabels())
          {
            labelRect.x = tickRect.x - trackBuffer;
            labelRect.y = tickRect.y + tickRect.height;
            labelRect.width = tickRect.width + trackBuffer * 2;
            labelRect.height = getHeightOfTallestLabel();
          }
        else
          {
            labelRect.x = tickRect.x;
            labelRect.y = tickRect.y + tickRect.height;
            labelRect.width = tickRect.width;
            labelRect.height = 0;
          }
      }
    else
      {
        if (slider.getPaintLabels())
          {
            labelRect.x = tickRect.x + tickRect.width;
            labelRect.y = tickRect.y - trackBuffer;
            labelRect.width = getWidthOfWidestLabel();
            labelRect.height = tickRect.height + trackBuffer * 2;
          }
        else
          {
            labelRect.x = tickRect.x + tickRect.width;
            labelRect.y = tickRect.y;
            labelRect.width = 0;
            labelRect.height = tickRect.height;
          }
      }
  }

  /**
   * This method returns the width of the widest label  in the slider's label
   * table.
   *
   * @return The width of the widest label or 0 if no label table exists.
   */
  protected int getWidthOfWidestLabel()
  {
    int widest = 0;
    Dictionary table = slider.getLabelTable();
    if (table != null)
      {
        for (Enumeration list = slider.getLabelTable().elements();
             list.hasMoreElements();)
          {
            Component label = (Component) list.nextElement();
            widest = Math.max(label.getPreferredSize().width, widest);
          }
      }
    return widest;
  }

  /**
   * This method returns the height of the tallest label in the slider's label
   * table.
   *
   * @return The height of the tallest label or 0 if no label table exists.
   */
  protected int getHeightOfTallestLabel()
  {
    int tallest = 0;
    Component label;

    if (slider.getLabelTable() == null)
      return 0;
    Dimension pref;
    for (Enumeration list = slider.getLabelTable().elements();
         list.hasMoreElements();)
      {
        Object comp = list.nextElement();
        if (! (comp instanceof Component))
          continue;
        label = (Component) comp;
        pref = label.getPreferredSize();
        if (pref != null && pref.height > tallest)
          tallest = pref.height;
      }
    return tallest;
  }

  /**
   * Returns the width of the label whose key has the highest value, or 0 if
   * there are no labels.
   *
   * @return The width of the label whose key has the highest value.
   * 
   * @see #getHighestValueLabel()
   */
  protected int getWidthOfHighValueLabel()
  {
    Component highValueLabel = getHighestValueLabel();
    if (highValueLabel != null)
      return highValueLabel.getPreferredSize().width;
    else
      return 0;
  }

  /**
   * Returns the width of the label whose key has the lowest value, or 0 if
   * there are no labels.
   *
   * @return The width of the label whose key has the lowest value.
   * 
   * @see #getLowestValueLabel()
   */
  protected int getWidthOfLowValueLabel()
  {
    Component lowValueLabel = getLowestValueLabel();
    if (lowValueLabel != null)
      return lowValueLabel.getPreferredSize().width;
    else
      return 0;
  }

  /**
   * Returns the height of the label whose key has the highest value, or 0 if
   * there are no labels.
   *
   * @return The height of the high value label or 0 if no label table exists.
   */
  protected int getHeightOfHighValueLabel()
  {
    Component highValueLabel = getHighestValueLabel();
    if (highValueLabel != null)
      return highValueLabel.getPreferredSize().height;
    else
      return 0;
  }

  /**
   * Returns the height of the label whose key has the lowest value, or 0 if
   * there are no labels.
   *
   * @return The height of the low value label or 0 if no label table exists.
   */
  protected int getHeightOfLowValueLabel()
  {
    Component lowValueLabel = getLowestValueLabel();
    if (lowValueLabel != null)
      return lowValueLabel.getPreferredSize().height;
    else
      return 0;
  }

  /**
   * Returns <code>true</code> if the slider scale is to be drawn inverted,
   * and <code>false</code> if not.
   *
   * @return <code>true</code> if the slider is to be drawn inverted.
   */
  protected boolean drawInverted()
  {
    return slider.getInverted();
  }

  /**
   * This method returns the label whose key has the lowest value.
   *
   * @return The low value label or null if no label table exists.
   */
  protected Component getLowestValueLabel()
  {
    Integer key = new Integer(Integer.MAX_VALUE);
    Integer tmpKey;
    Dictionary labelTable = slider.getLabelTable();

    if (labelTable == null)
      return null;

    for (Enumeration list = labelTable.keys(); list.hasMoreElements();)
      {
        Object value = list.nextElement();
        if (! (value instanceof Integer))
          continue;
        tmpKey = (Integer) value;
        if (tmpKey.intValue() < key.intValue())
          key = tmpKey;
      }
    Object comp = labelTable.get(key);
    if (! (comp instanceof Component))
      return null;
    return (Component) comp;
  }

  /**
   * Returns the label whose key has the highest value.
   *
   * @return The label whose key has the highest value or <code>null</code> if 
   *     no label table exists.
   */
  protected Component getHighestValueLabel()
  {
    Integer key = new Integer(Integer.MIN_VALUE);
    Integer tmpKey;
    Dictionary labelTable = slider.getLabelTable();

    if (labelTable == null)
      return null;

    for (Enumeration list = labelTable.keys(); list.hasMoreElements();)
      {
        Object value = list.nextElement();
        if (! (value instanceof Integer))
          continue;
        tmpKey = (Integer) value;
        if (tmpKey.intValue() > key.intValue())
          key = tmpKey;
      }
    Object comp = labelTable.get(key);
    if (! (comp instanceof Component))
      return null;
    return (Component) comp;
  }

  /**
   * This method is used to paint the {@link JSlider}. It delegates all its
   * duties to the various paint methods like paintTicks(),  paintTrack(),
   * paintThumb(), etc.
   *
   * @param g The {@link Graphics} object to paint with.
   * @param c The {@link JComponent} that is being painted.
   */
  public void paint(Graphics g, JComponent c)
  {
    recalculateIfInsetsChanged();
    recalculateIfOrientationChanged();
    if (slider.getPaintTrack() && hitClip(g, trackRect))
      paintTrack(g);
    if (slider.getPaintTicks() && hitClip(g, tickRect))
      paintTicks(g);
    if (slider.getPaintLabels() && hitClip(g, labelRect))
      paintLabels(g);
    if (slider.hasFocus() && hitClip(g, focusRect))
      paintFocus(g);
    if (hitClip(g, thumbRect))
      paintThumb(g);
  }

  /**
   * This method recalculates any rectangles that need to be recalculated
   * after the insets of the component have changed.
   */
  protected void recalculateIfInsetsChanged()
  {
    Insets insets = slider.getInsets();
    if (! insets.equals(insetCache))
      {
        insetCache = insets;
        calculateGeometry();
      }
  }

  /**
   * This method recalculates any rectangles that need to be recalculated
   * after the orientation of the slider changes.
   */
  protected void recalculateIfOrientationChanged()
  {
    // Examining a test program shows that either Sun calls private
    // methods that we don't know about, or these don't do anything.  
    calculateThumbSize();
    calculateTrackBuffer();
    calculateTrackRect();
    calculateThumbLocation();

    calculateTickRect();
    calculateLabelRect();
  }

  /**
   * This method is called during a repaint if the slider has focus. It draws
   * an outline of the  focusRect using the color returned by
   * getFocusColor().
   *
   * @param g The {@link Graphics} object to draw with.
   */
  public void paintFocus(Graphics g)
  {
    Color saved_color = g.getColor();

    g.setColor(getFocusColor());
    
    g.drawRect(focusRect.x, focusRect.y, focusRect.width, focusRect.height);

    g.setColor(saved_color);
  }

  /**
   * <p>
   * This method is called during a repaint if the  track is to be drawn. It
   * draws a 3D rectangle to  represent the track. The track is not the size
   * of the trackRect. The top and left edges of the track should be outlined
   * with the shadow color. The bottom and right edges should be outlined
   * with the highlight color.
   * </p>
   * <pre>
   *    a---d   
   *    |   |   
   *    |   |   a------------------------d
   *    |   |   |                        |
   *    |   |   b------------------------c
   *    |   |
   *    |   |   
   *    b---c
   * </pre>
   * 
   * <p>
   * The b-a-d path needs to be drawn with the shadow color and the b-c-d path
   * needs to be drawn with the highlight color.
   * </p>
   *
   * @param g The {@link Graphics} object to draw with.
   */
  public void paintTrack(Graphics g)
  {
    Color saved_color = g.getColor();
    int width;
    int height;

    Point a = new Point(trackRect.x, trackRect.y + 1);
    Point b = new Point(a);
    Point c = new Point(a);
    Point d = new Point(a);

    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        width = trackRect.width;
        height = (thumbRect.height / 4 == 0) ? 1 : thumbRect.height / 4;

        a.translate(0, (trackRect.height / 2) - (height / 2));
        b.translate(0, (trackRect.height / 2) + (height / 2));
        c.translate(trackRect.width, (trackRect.height / 2) + (height / 2));
        d.translate(trackRect.width, (trackRect.height / 2) - (height / 2));
      }
    else
      {
        width = (thumbRect.width / 4 == 0) ? 1 : thumbRect.width / 4;
        height = trackRect.height;

        a.translate((trackRect.width / 2) - (width / 2), 0);
        b.translate((trackRect.width / 2) - (width / 2), trackRect.height);
        c.translate((trackRect.width / 2) + (width / 2), trackRect.height);
        d.translate((trackRect.width / 2) + (width / 2), 0);
      }
    g.setColor(Color.GRAY);
    g.fillRect(a.x, a.y, width, height);

    g.setColor(getHighlightColor());
    g.drawLine(b.x, b.y, c.x, c.y);
    g.drawLine(c.x, c.y, d.x, d.y);

    g.setColor(getShadowColor());
    g.drawLine(b.x, b.y, a.x, a.y);
    g.drawLine(a.x, a.y, d.x, d.y);

    g.setColor(saved_color);
  }

  /**
   * This method is called during a repaint if the ticks are to be drawn. This
   * method must still verify that the majorTickSpacing and minorTickSpacing
   * are greater than zero before drawing the ticks.
   *
   * @param g The {@link Graphics} object to draw with.
   */
  public void paintTicks(Graphics g)
  {
    int max = slider.getMaximum();
    int min = slider.getMinimum();
    int majorSpace = slider.getMajorTickSpacing();
    int minorSpace = slider.getMinorTickSpacing();

    if (majorSpace > 0)
      {
        if (slider.getOrientation() == JSlider.HORIZONTAL)
          {
            g.translate(0, tickRect.y);
            for (int i = min; i <= max; i += majorSpace)
              paintMajorTickForHorizSlider(g, tickRect, xPositionForValue(i));
            g.translate(0, -tickRect.y);
          }
        else // JSlider.VERTICAL
          {
            g.translate(tickRect.x, 0);
            for (int i = min; i <= max; i += majorSpace)
              paintMajorTickForVertSlider(g, tickRect, yPositionForValue(i));
            g.translate(-tickRect.x, 0);
          }
      }
    if (minorSpace > 0)
      {
        if (slider.getOrientation() == JSlider.HORIZONTAL)
          {
            g.translate(0, tickRect.y);
            for (int i = min; i <= max; i += minorSpace)
              paintMinorTickForHorizSlider(g, tickRect, xPositionForValue(i));
            g.translate(0, -tickRect.y);
          }
        else
          {
            g.translate(tickRect.x, 0);
            for (int i = min; i <= max; i += minorSpace)
              paintMinorTickForVertSlider(g, tickRect, yPositionForValue(i));
            g.translate(-tickRect.x, 0);
          }
      }
  }

  /* Minor ticks start at 1/4 of the height (or width) of the tickRect and 
     extend to 1/2 of the tickRect.

     Major ticks start at 1/4 of the height and extend to 3/4.
   */

  /**
   * This method paints a minor tick for a horizontal slider at the given x
   * value. x represents the x coordinate to paint at.
   *
   * @param g The {@link Graphics} object to draw with.
   * @param tickBounds The tickRect rectangle.
   * @param x The x coordinate to draw the tick at.
   */
  protected void paintMinorTickForHorizSlider(Graphics g,
                                              Rectangle tickBounds, int x)
  {
    int y = tickRect.height / 4;
    Color saved = g.getColor();
    g.setColor(Color.BLACK);

    g.drawLine(x, y, x, y + tickRect.height / 4);
    g.setColor(saved);
  }

  /**
   * This method paints a major tick for a horizontal slider at the given x
   * value. x represents the x coordinate to paint at.
   *
   * @param g The {@link Graphics} object to draw with.
   * @param tickBounds The tickRect rectangle.
   * @param x The x coordinate to draw the tick at.
   */
  protected void paintMajorTickForHorizSlider(Graphics g,
                                              Rectangle tickBounds, int x)
  {
    int y = tickRect.height / 4;
    Color saved = g.getColor();
    g.setColor(Color.BLACK);

    g.drawLine(x, y, x, y + tickRect.height / 2);
    g.setColor(saved);
  }

  /**
   * This method paints a minor tick for a vertical slider at the given y
   * value. y represents the y coordinate to paint at.
   *
   * @param g The {@link Graphics} object to draw with.
   * @param tickBounds The tickRect rectangle.
   * @param y The y coordinate to draw the tick at.
   */
  protected void paintMinorTickForVertSlider(Graphics g, Rectangle tickBounds,
                                             int y)
  {
    int x = tickRect.width / 4;
    Color saved = g.getColor();
    g.setColor(Color.BLACK);

    g.drawLine(x, y, x + tickRect.width / 4, y);
    g.setColor(saved);
  }

  /**
   * This method paints a major tick for a vertical slider at the given y
   * value. y represents the y coordinate to paint at.
   *
   * @param g The {@link Graphics} object to draw with.
   * @param tickBounds The tickRect rectangle.
   * @param y The y coordinate to draw the tick at.
   */
  protected void paintMajorTickForVertSlider(Graphics g, Rectangle tickBounds,
                                             int y)
  {
    int x = tickRect.width / 4;
    Color saved = g.getColor();
    g.setColor(Color.BLACK);

    g.drawLine(x, y, x + tickRect.width / 2, y);
    g.setColor(saved);
  }

  /**
   * This method paints all the labels from the slider's label table. This
   * method must make sure that the label table is not null before painting
   * the labels. Each entry in the label table is a (integer, component)
   * pair. Every label is painted at the value of the integer.
   *
   * @param g The {@link Graphics} object to draw with.
   */
  public void paintLabels(Graphics g)
  {
    Dictionary table = slider.getLabelTable();
    if (table != null)
      {
        int min = slider.getMinimum();
        int max = slider.getMaximum();
        for (Enumeration list = table.keys(); list.hasMoreElements();)
          {
            Integer key = (Integer) list.nextElement();
            int value = key.intValue();
            if (value >= min && value <= max)
              {
                Component label = (Component) table.get(key);
                if (slider.getOrientation() == JSlider.HORIZONTAL)
                  {
                    g.translate(0, labelRect.y);
                    paintHorizontalLabel(g, value, label);
                    g.translate(0, -labelRect.y);
                  }
                else
                  {
                    g.translate(labelRect.x, 0);
                    paintVerticalLabel(g, value, label);
                    g.translate(-labelRect.x, 0);
                  }
              }
          }
      }
  }

  /**
   * This method paints the label on the horizontal slider at the value
   * specified. The value is not a coordinate. It is a value within the range
   * of the  slider. If the value is not within the range of the slider, this
   * method will do nothing. This method should not paint outside the
   * boundaries of the labelRect.
   *
   * @param g The {@link Graphics} object to draw with.
   * @param value The value to paint at.
   * @param label The label to paint.
   */
  protected void paintHorizontalLabel(Graphics g, int value, Component label)
  {
    int center = xPositionForValue(value);
    int left = center - label.getPreferredSize().width / 2;
    g.translate(left, 0);
    label.paint(g);
    g.translate(-left, 0);
  }

  /**
   * This method paints the label on the vertical slider at the value
   * specified. The value is not a coordinate. It is a value within the range
   * of the  slider. If the value is not within the range of the slider, this
   * method will do nothing. This method should not paint outside the
   * boundaries of the labelRect.
   *
   * @param g The {@link Graphics} object to draw with.
   * @param value The value to paint at.
   * @param label The label to paint.
   */
  protected void paintVerticalLabel(Graphics g, int value, Component label)
  {
    int center = yPositionForValue(value);
    int top = center - label.getPreferredSize().height / 2;
    g.translate(0, top);
    label.paint(g);
    g.translate(0, -top);
  }

  /**
   * <p>
   * This method paints a thumb. There are two types of thumb:
   * </p>
   * <pre>
   *   Vertical         Horizontal
   *    a---b            a-----b
   *    |   |            |      \
   *    e   c            |       c
   *     \ /             |      /
   *      d              e-----d
   *  </pre>
   * 
   * <p>
   * In the case of vertical thumbs, we highlight the path b-a-e-d and shadow
   * the path b-c-d. In the case of horizontal thumbs, we highlight the path
   * c-b-a-e and shadow the path c-d-e. In both cases we fill the path
   * a-b-c-d-e before shadows and highlights are drawn.
   * </p>
   *
   * @param g The graphics object to paint with
   */
  public void paintThumb(Graphics g)
  {
    Color saved_color = g.getColor();

    Point a = new Point(thumbRect.x, thumbRect.y);
    Point b = new Point(a);
    Point c = new Point(a);
    Point d = new Point(a);
    Point e = new Point(a);

    Polygon bright;
    Polygon light; // light shadow
    Polygon dark; // dark shadow
    Polygon all;

    // This will be in X-dimension if the slider is inverted and y if it isn't.
    int turnPoint;

    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
        turnPoint = thumbRect.height * 3 / 4;

        b.translate(thumbRect.width - 1, 0);
        c.translate(thumbRect.width - 1, turnPoint);
        d.translate(thumbRect.width / 2 - 1, thumbRect.height - 1);
        e.translate(0, turnPoint);

        bright = new Polygon(new int[] { b.x - 1, a.x, e.x, d.x },
                             new int[] { b.y, a.y, e.y, d.y }, 4);

        dark = new Polygon(new int[] { b.x, c.x, d.x + 1 }, new int[] { b.y,
                                                                       c.y - 1,
                                                                       d.y }, 3);

        light = new Polygon(new int[] { b.x - 1, c.x - 1, d.x + 1 },
                            new int[] { b.y + 1, c.y - 1, d.y - 1 }, 3);

        all = new Polygon(
                          new int[] { a.x + 1, b.x - 2, c.x - 2, d.x, e.x + 1 },
                          new int[] { a.y + 1, b.y + 1, c.y - 1, d.y - 1, e.y },
                          5);
      }
    else
      {
        turnPoint = thumbRect.width * 3 / 4 - 1;

        b.translate(turnPoint, 0);
        c.translate(thumbRect.width - 1, thumbRect.height / 2);
        d.translate(turnPoint, thumbRect.height - 1);
        e.translate(0, thumbRect.height - 1);

        bright = new Polygon(new int[] { c.x - 1, b.x, a.x, e.x },
                             new int[] { c.y - 1, b.y, a.y, e.y - 1 }, 4);

        dark = new Polygon(new int[] { c.x, d.x, e.x }, new int[] { c.y, d.y,
                                                                   e.y }, 3);

        light = new Polygon(new int[] { c.x - 1, d.x, e.x + 1 },
                            new int[] { c.y, d.y - 1, e.y - 1 }, 3);
        all = new Polygon(new int[] { a.x + 1, b.x, c.x - 2, c.x - 2, d.x,
                                     e.x + 1 }, new int[] { a.y + 1, b.y + 1,
                                                           c.y - 1, c.y,
                                                           d.y - 2, e.y - 2 },
                          6);
      }

    g.setColor(Color.WHITE);
    g.drawPolyline(bright.xpoints, bright.ypoints, bright.npoints);

    g.setColor(Color.BLACK);
    g.drawPolyline(dark.xpoints, dark.ypoints, dark.npoints);

    g.setColor(Color.GRAY);
    g.drawPolyline(light.xpoints, light.ypoints, light.npoints);

    g.setColor(Color.LIGHT_GRAY);
    g.drawPolyline(all.xpoints, all.ypoints, all.npoints);
    g.fillPolygon(all);

    g.setColor(saved_color);
  }

  /**
   * This method sets the position of the thumbRect.
   *
   * @param x The new x position.
   * @param y The new y position.
   */
  public void setThumbLocation(int x, int y)
  {
    Rectangle union = new Rectangle(thumbRect);
    thumbRect.setLocation(x, y);
    SwingUtilities.computeUnion(thumbRect.x, thumbRect.y, thumbRect.width,
                                thumbRect.height, union);
    slider.repaint(union);
  }

  /**
   * Moves the thumb one block in the direction specified (a block is 1/10th
   * of the slider range).   If the slider snaps to ticks, this method is 
   * responsible for snapping it to a tick after the thumb has been moved.
   *
   * @param direction  the direction (positive values increment the thumb 
   *   position by one block, zero/negative values decrement the thumb position
   *   by one block).
   */
  public void scrollByBlock(int direction)
  {
    int unit = (slider.getMaximum() - slider.getMinimum()) / 10;
    int moveTo = slider.getValue();
    if (direction > 0)
      moveTo += unit;
    else
      moveTo -= unit;

    if (slider.getSnapToTicks())
      moveTo = findClosestTick(moveTo);

    slider.setValue(moveTo);
  }

  /**
   * Moves the thumb one unit in the specified direction. If the slider snaps 
   * to ticks, this method is responsible for snapping it to a tick after the 
   * thumb has been moved.
   *
   * @param direction  the direction (positive values increment the thumb 
   *   position by one, zero/negative values decrement the thumb position by
   *   one).
   */
  public void scrollByUnit(int direction)
  {
    int moveTo = slider.getValue();
    if (direction > 0)
      moveTo++;
    else
      moveTo--;

    if (slider.getSnapToTicks())
      moveTo = findClosestTick(moveTo);

    slider.setValue(moveTo);
  }

  /**
   * This method is called when there has been a click in the track and the
   * thumb needs to be scrolled  on regular intervals. This method is only
   * responsible  for starting the timer and not for stopping it.
   *
   * @param dir The direction to move in.
   */
  protected void scrollDueToClickInTrack(int dir)
  {
    scrollTimer.stop();

    scrollListener.setDirection(dir);
    scrollListener.setScrollByBlock(true);

    scrollTimer.start();
  }

  /**
   * Returns the x-coordinate (relative to the component) for the given slider 
   * value.  This method assumes that the <code>trackRect</code> field is
   * set up.
   *
   * @param value  the slider value.
   *
   * @return The x-coordinate.
   */
  protected int xPositionForValue(int value)
  {
    int min = slider.getMinimum();
    int max = slider.getMaximum();
    int len = trackRect.width;
    double range = max - min;
    double pixPerVal = len / range;
    int left = trackRect.x;
    int right = left + trackRect.width - 1;
    int xpos;
    if (! drawInverted())
      xpos = left + (int) Math.round(pixPerVal * ((double) value - min));
    else
      xpos = right - (int) Math.round(pixPerVal * ((double) value - min));
    xpos = Math.max(left, xpos);
    xpos = Math.min(right, xpos);
    return xpos;
  }

  /**
   * Returns the y-coordinate (relative to the component) for the given slider 
   * value.  This method assumes that the <code>trackRect</code> field is 
   * set up.
   *
   * @param value  the slider value.
   *
   * @return The y-coordinate.
   */
  protected int yPositionForValue(int value)
  {
    int min = slider.getMinimum();
    int max = slider.getMaximum();
    int len = trackRect.height;
    double range = max - min;
    double pixPerVal = len / range;
    int top = trackRect.y;
    int bottom = top + trackRect.height - 1;
    int ypos;
    if (! drawInverted())
      ypos = top + (int) Math.round(pixPerVal * ((double) max - value));
    else
      ypos = top + (int) Math.round(pixPerVal * ((double) value - min));
    ypos = Math.max(top, ypos);
    ypos = Math.min(bottom, ypos);
    return ypos;
  }

  /**
   * This method returns the value in the slider's range given the y
   * coordinate. If the value is out of range, it will  return the closest
   * legal value.
   *
   * @param yPos The y coordinate to calculate a value for.
   *
   * @return The value for the y coordinate.
   */
  public int valueForYPosition(int yPos)
  {
    int min = slider.getMinimum();
    int max = slider.getMaximum();
    int len = trackRect.height;

    int value;

    // If the length is 0, you shouldn't be able to even see where the slider 
    // is.  This really shouldn't ever happen, but just in case, we'll return 
    // the middle.
    if (len == 0)
      return (max - min) / 2;

    if (! drawInverted())
      value = (len - (yPos - trackRect.y)) * (max - min) / len + min;
    else
      value = (yPos - trackRect.y) * (max - min) / len + min;

    // If this isn't a legal value, then we'll have to move to one now.
    if (value > max)
      value = max;
    else if (value < min)
      value = min;
    return value;
  }

  /**
   * This method returns the value in the slider's range given the x
   * coordinate. If the value is out of range, it will return the closest
   * legal value.
   *
   * @param xPos The x coordinate to calculate a value for.
   *
   * @return The value for the x coordinate.
   */
  public int valueForXPosition(int xPos)
  {
    int min = slider.getMinimum();
    int max = slider.getMaximum();
    int len = trackRect.width;

    int value;

    // If the length is 0, you shouldn't be able to even see where the slider 
    // is.  This really shouldn't ever happen, but just in case, we'll return 
    // the middle.
    if (len == 0)
      return (max - min) / 2;

    if (! drawInverted())
      value = (xPos - trackRect.x) * (max - min) / len + min;
    else
      value = (len - (xPos - trackRect.x)) * (max - min) / len + min;

    // If this isn't a legal value, then we'll have to move to one now.
    if (value > max)
      value = max;
    else if (value < min)
      value = min;
    return value;
  }

  /**
   * This method finds the closest value that has a tick associated with it.
   * This is package-private to avoid an accessor method.
   *
   * @param value The value to search from.
   *
   * @return The closest value that has a tick associated with it.
   */
  int findClosestTick(int value)
  {
    int min = slider.getMinimum();
    int max = slider.getMaximum();
    int majorSpace = slider.getMajorTickSpacing();
    int minorSpace = slider.getMinorTickSpacing();

    // The default value to return is value + minor or
    // value + major. 
    // Initializing at min - value leaves us with a default
    // return value of min, which always has tick marks
    // (if ticks are painted).
    int minor = min - value;
    int major = min - value;

    // If there are no major tick marks or minor tick marks 
    // e.g. snap is set to true but no ticks are set, then
    // we can just return the value.
    if (majorSpace <= 0 && minorSpace <= 0)
      return value;

    // First check the major ticks.
    if (majorSpace > 0)
      {
        int lowerBound = (value - min) / majorSpace;
        int majLower = majorSpace * lowerBound + min;
        int majHigher = majorSpace * (lowerBound + 1) + min;

        if (majHigher <= max && majHigher - value <= value - majLower)
          major = majHigher - value;
        else
          major = majLower - value;
      }

    if (minorSpace > 0)
      {
        int lowerBound = value / minorSpace;
        int minLower = minorSpace * lowerBound;
        int minHigher = minorSpace * (lowerBound + 1);

        if (minHigher <= max && minHigher - value <= value - minLower)
          minor = minHigher - value;
        else
          minor = minLower - value;
      }

    // Give preference to minor ticks
    if (Math.abs(minor) > Math.abs(major))
      return value + major;
    else
      return value + minor;
  }
  
  InputMap getInputMap(int condition) 
  {
    if (condition == JComponent.WHEN_FOCUSED)
      return (InputMap) UIManager.get("Slider.focusInputMap");
    return null;
  }

  /**
   * Returns the action map for the {@link JSlider}.  All sliders share
   * a single action map which is created the first time this method is 
   * called, then stored in the UIDefaults table for subsequent access.
   * 
   * @return The shared action map.
   */
  ActionMap getActionMap() 
  {
    ActionMap map = (ActionMap) UIManager.get("Slider.actionMap");

    if (map == null) // first time here
      {
        map = createActionMap();
        if (map != null)
          UIManager.put("Slider.actionMap", map);
      }
    return map;
  }

  /**
   * Creates the action map shared by all {@link JSlider} instances.
   * This method is called once by {@link #getActionMap()} when it 
   * finds no action map in the UIDefaults table...after the map is 
   * created, it gets added to the defaults table so that subsequent 
   * calls to {@link #getActionMap()} will return the same shared 
   * instance.
   * 
   * @return The action map.
   */
  ActionMap createActionMap()
  {
    ActionMap map = new ActionMapUIResource();
    map.put("positiveUnitIncrement", 
            new AbstractAction("positiveUnitIncrement") {
              public void actionPerformed(ActionEvent event)
              {
                JSlider slider = (JSlider) event.getSource();
                BasicSliderUI ui = (BasicSliderUI) slider.getUI();
                if (slider.getInverted())
                  ui.scrollByUnit(BasicSliderUI.NEGATIVE_SCROLL);
                else
                  ui.scrollByUnit(BasicSliderUI.POSITIVE_SCROLL);
              }
            }
    );
    map.put("negativeUnitIncrement", 
            new AbstractAction("negativeUnitIncrement") {
              public void actionPerformed(ActionEvent event)
              {
                JSlider slider = (JSlider) event.getSource();
                BasicSliderUI ui = (BasicSliderUI) slider.getUI();
                if (slider.getInverted())
                  ui.scrollByUnit(BasicSliderUI.POSITIVE_SCROLL);
                else
                  ui.scrollByUnit(BasicSliderUI.NEGATIVE_SCROLL);
              }
            }
    );
    map.put("positiveBlockIncrement", 
            new AbstractAction("positiveBlockIncrement") {
              public void actionPerformed(ActionEvent event)
              {
                JSlider slider = (JSlider) event.getSource();
                BasicSliderUI ui = (BasicSliderUI) slider.getUI();
                if (slider.getInverted())
                  ui.scrollByBlock(BasicSliderUI.NEGATIVE_SCROLL);
                else
                  ui.scrollByBlock(BasicSliderUI.POSITIVE_SCROLL);
              }
            }
    );
    map.put("negativeBlockIncrement", 
            new AbstractAction("negativeBlockIncrement") {
              public void actionPerformed(ActionEvent event)
              {
                JSlider slider = (JSlider) event.getSource();
                BasicSliderUI ui = (BasicSliderUI) slider.getUI();
                if (slider.getInverted())
                  ui.scrollByBlock(BasicSliderUI.POSITIVE_SCROLL);
                else
                  ui.scrollByBlock(BasicSliderUI.NEGATIVE_SCROLL);
              }
            }
    );
    map.put("minScroll", 
            new AbstractAction("minScroll") {
              public void actionPerformed(ActionEvent event)
              {
                JSlider slider = (JSlider) event.getSource();
                if (slider.getInverted())
                  slider.setValue(slider.getMaximum());
                else
                  slider.setValue(slider.getMinimum());   
              }
            }
    );
    map.put("maxScroll", 
            new AbstractAction("maxScroll") {
              public void actionPerformed(ActionEvent event)
              {
                JSlider slider = (JSlider) event.getSource();
                if (slider.getInverted())
                  slider.setValue(slider.getMinimum());
                else
                  slider.setValue(slider.getMaximum());                  
              }
            }
    );
    return map;
  }

  /**
   * Small utility method to save me from typing the hell out of myself in
   * paint().
   */
  private boolean hitClip(Graphics g, Rectangle r)
  {
    return g.hitClip(r.x, r.y, r.width, r.height);
  }
}
