/* BasicSliderUI.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
import java.awt.ComponentOrientation;
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

import javax.swing.BoundedRangeModel;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JSlider;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MouseInputAdapter;
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
   */
  protected class ComponentHandler extends ComponentAdapter
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

      slider.revalidate();
      slider.repaint();
    }
  }

  /**
   * Helper class that listens for focus events.
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
      // FIXME: implement.
    }

    /**
     * Called when the {@link JSlider} has lost focus. It  should repaint the
     * slider without the focus drawn.
     *
     * @param e A {@link FocusEvent}.
     */
    public void focusLost(FocusEvent e)
    {
      // FIXME: implement.
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
      if (e.getPropertyName().equals("orientation"))
	recalculateIfOrientationChanged();
      else if (e.getPropertyName().equals("model"))
        {
	  BoundedRangeModel oldModel = (BoundedRangeModel) e.getOldValue();
	  oldModel.removeChangeListener(changeListener);
	  slider.getModel().addChangeListener(changeListener);
	  calculateThumbLocation();
        }

      // elif the componentOrientation changes (this is a bound property,
      // just undocumented) we change leftToRightCache. In Sun's 
      // implementation, the LTR cache changes on a repaint. This is strange
      // since there is no need to do so. We could events here and 
      // update the cache. 
      // elif the border/insets change, we recalculateInsets.
      slider.repaint();
    }
  }

  /**
   * Helper class that listens to our swing timer. This class is responsible
   * for listening to the timer and moving the thumb in the proper direction
   * every interval.
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
      currentMouseX = e.getX();
      currentMouseY = e.getY();

      int value;
      if (slider.getOrientation() == JSlider.HORIZONTAL)
	value = valueForXPosition(currentMouseX);
      else
	value = valueForYPosition(currentMouseY);

      if (slider.getSnapToTicks())
	value = findClosestTick(value);

      // If the thumb is hit, then we don't need to set the timers to move it. 
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

    /**
     * Called when the mouse is released.  This should stop the timer that
     * scrolls the thumb.
     *
     * @param e A {@link MouseEvent}
     */
    public void mouseReleased(MouseEvent e)
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
	return (value > slider.getValue());
      else
	return (value < slider.getValue());
    }
  }

  /** The preferred height of the thumb. */
  private transient int thumbHeight;

  /** The preferred width of the thumb. */
  private transient int thumbWidth;

  /** The preferred height of the tick rectangle. */
  private transient int tickHeight;

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
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    slider.setForeground(defaults.getColor("Slider.foreground"));
    slider.setBackground(defaults.getColor("Slider.background"));
    shadowColor = defaults.getColor("Slider.shadow");
    highlightColor = defaults.getColor("Slider.highlight");
    focusColor = defaults.getColor("Slider.focus");
    slider.setBorder(defaults.getBorder("Slider.border"));
    slider.setOpaque(true);

    thumbHeight = defaults.getInt("Slider.thumbHeight");
    thumbWidth = defaults.getInt("Slider.thumbWidth");
    tickHeight = defaults.getInt("Slider.tickHeight");

    focusInsets = defaults.getInsets("Slider.focusInsets");
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
    // FIXME: implement.
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
    // FIXME: implement.
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
    Insets insets = slider.getInsets();

    // The width should cover all the labels (which are usually the
    // deciding factor of the width)
    int width = getWidthOfWidestLabel() * (slider.getLabelTable() == null ? 0
                                                                          : slider.getLabelTable()
                                                                                  .size());

    // If there are not enough labels.
    // This number is pretty much arbitrary, but it looks nice.
    if (width < 200)
      width = 200;

    // We can only draw inside of the focusRectangle, so we have to
    // pad it with insets.
    width += insets.left + insets.right + focusInsets.left + focusInsets.right;

    // Height is determined by the thumb, the ticks and the labels.
    int height = thumbHeight;

    if (slider.getPaintTicks() && slider.getMajorTickSpacing() > 0
        || slider.getMinorTickSpacing() > 0)
      height += tickHeight;

    if (slider.getPaintLabels())
      height += getHeightOfTallestLabel();

    height += insets.top + insets.bottom + focusInsets.top
    + focusInsets.bottom;

    return new Dimension(width, height);
  }

  /**
   * This method returns the preferred size when the slider is vertically
   * oriented.
   *
   * @return The dimensions of the preferred vertical size.
   */
  public Dimension getPreferredVerticalSize()
  {
    Insets insets = slider.getInsets();

    int height = getHeightOfTallestLabel() * (slider.getLabelTable() == null
                                              ? 0 : slider.getLabelTable()
                                                          .size());

    if (height < 200)
      height = 200;

    height += insets.top + insets.bottom + focusInsets.top
    + focusInsets.bottom;

    int width = thumbHeight;

    if (slider.getPaintTicks() && slider.getMajorTickSpacing() > 0
        || slider.getMinorTickSpacing() > 0)
      width += tickHeight;

    if (slider.getPaintLabels())
      width += getWidthOfWidestLabel();

    width += insets.left + insets.right + focusInsets.left + focusInsets.right;

    return new Dimension(width, height);
  }

  /**
   * This method returns the minimum size when the slider is horizontally
   * oriented.
   *
   * @return The dimensions of the minimum horizontal size.
   */
  public Dimension getMinimumHorizontalSize()
  {
    return getPreferredHorizontalSize();
  }

  /**
   * This method returns the minimum size of the slider when it  is vertically
   * oriented.
   *
   * @return The dimensions of the minimum vertical size.
   */
  public Dimension getMinimumVerticalSize()
  {
    return getPreferredVerticalSize();
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
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      return getPreferredHorizontalSize();
    else
      return getPreferredVerticalSize();
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
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      return getPreferredHorizontalSize();
    else
      return getPreferredVerticalSize();
  }

  /**
   * This method returns the maximum size for this {@link JSlider} for this
   * look and feel. If it returns null, then it is up to the Layout Manager
   * to give the {@link JComponent} a size.
   *
   * @param c The {@link JComponent} to find a maximum size for.
   *
   * @return The dimensions of the maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      return getPreferredHorizontalSize();
    else
      return getPreferredVerticalSize();
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
    insetCache = slider.getInsets();
    focusRect = SwingUtilities.calculateInnerArea(slider, focusRect);

    if (focusRect.width < 0)
      focusRect.width = 0;
    if (focusRect.height < 0)
      focusRect.height = 0;
  }

  /**
   * This method calculates the size but not the position of the thumbRect. It
   * must take into account the orientation of the slider.
   */
  protected void calculateThumbSize()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
	if (thumbWidth > contentRect.width)
	  thumbRect.width = contentRect.width / 4;
	else
	  thumbRect.width = thumbWidth;
	if (thumbHeight > contentRect.height)
	  thumbRect.height = contentRect.height;
	else
	  thumbRect.height = thumbHeight;
      }
    else
      {
	// The thumb gets flipped when inverted, so thumbWidth 
	// actually is the height and vice versa.
	if (thumbWidth > contentRect.height)
	  thumbRect.height = contentRect.height / 4;
	else
	  thumbRect.height = thumbWidth;
	if (thumbHeight > contentRect.width)
	  thumbRect.width = contentRect.width;
	else
	  thumbRect.width = thumbHeight;
      }
  }

  /**
   * This method calculates the size and position of the contentRect. This
   * method does not need to be  called if the orientation changes.
   */
  protected void calculateContentRect()
  {
    contentRect.x = focusRect.x + focusInsets.left;
    contentRect.y = focusRect.y + focusInsets.top;
    contentRect.width = focusRect.width - focusInsets.left - focusInsets.right;
    contentRect.height = focusRect.height - focusInsets.top
                         - focusInsets.bottom;

    if (contentRect.width < 0)
      contentRect.width = 0;
    if (contentRect.height < 0)
      contentRect.height = 0;
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
	thumbRect.y = contentRect.y;
      }
    else
      {
	thumbRect.x = contentRect.x;
	thumbRect.y = yPositionForValue(value) - thumbRect.height / 2;
      }
  }

  /**
   * Calculates the gap size between the left edge of the contentRect and the
   * left edge of the trackRect.
   */
  protected void calculateTrackBuffer()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      trackBuffer = thumbRect.width;
    else
      trackBuffer = thumbRect.height;
  }

  /**
   * This method returns the size of the thumbRect.
   *
   * @return The dimensions of the thumb.
   */
  protected Dimension getThumbSize()
  {
    // This is really just the bounds box for the thumb.
    // The thumb will actually be pointed (like a rectangle + triangle at bottom)
    return thumbRect.getSize();
  }

  /**
   * Calculates the size and position of the trackRect. It must take into
   * account the orientation of the slider.
   */
  protected void calculateTrackRect()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
	trackRect.x = contentRect.x + trackBuffer;
	trackRect.y = contentRect.y;
	trackRect.width = contentRect.width - 2 * trackBuffer;
	trackRect.height = thumbRect.height;
      }
    else
      {
	trackRect.x = contentRect.x;
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
    return tickHeight;
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

	if (tickRect.y + tickRect.height > contentRect.y + contentRect.height)
	  tickRect.height = contentRect.y + contentRect.height - tickRect.y;
      }
    else
      {
	tickRect.x = trackRect.x + trackRect.width;
	tickRect.y = trackRect.y;
	tickRect.width = getTickLength();
	tickRect.height = trackRect.height;

	if (tickRect.x + tickRect.width > contentRect.x + contentRect.width)
	  tickRect.width = contentRect.x + contentRect.width - tickRect.x;
      }
  }

  /**
   * This method calculates the size and position of the labelRect. It must
   * take into account the orientation of the slider.
   */
  protected void calculateLabelRect()
  {
    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
	labelRect.x = contentRect.x;
	labelRect.y = tickRect.y + tickRect.height;
	labelRect.width = contentRect.width;
	labelRect.height = contentRect.height - labelRect.y;
      }
    else
      {
	labelRect.x = tickRect.x + tickRect.width;
	labelRect.y = contentRect.y;
	labelRect.width = contentRect.width - labelRect.x;
	labelRect.height = contentRect.height;
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
	if (pref != null && pref.width > widest)
	  widest = pref.width;
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
   * This method returns the width of the label whose key has the highest
   * value.
   *
   * @return The width of the high value label or 0 if no label table exists.
   */
  protected int getWidthOfHighValueLabel()
  {
    Component highValueLabel = getHighestValueLabel();
    if (highValueLabel != null)
      return highValueLabel.getWidth();
    else
      return 0;
  }

  /**
   * This method returns the width of the label whose key has the lowest
   * value.
   *
   * @return The width of the low value label or 0 if no label table exists.
   */
  protected int getWidthOfLowValueLabel()
  {
    Component lowValueLabel = getLowestValueLabel();
    if (lowValueLabel != null)
      return lowValueLabel.getWidth();
    else
      return 0;
  }

  /**
   * This method returns the height of the label whose key has the highest
   * value.
   *
   * @return The height of the high value label or 0 if no label table exists.
   */
  protected int getHeightOfHighValueLabel()
  {
    Component highValueLabel = getHighestValueLabel();
    if (highValueLabel != null)
      return highValueLabel.getHeight();
    else
      return 0;
  }

  /**
   * This method returns the height of the label whose key has the lowest
   * value.
   *
   * @return The height of the low value label or 0 if no label table exists.
   */
  protected int getHeightOfLowValueLabel()
  {
    Component lowValueLabel = getLowestValueLabel();
    if (lowValueLabel != null)
      return lowValueLabel.getHeight();
    else
      return 0;
  }

  /**
   * This method returns whether the slider is to be drawn inverted.
   *
   * @return True is the slider is to be drawn inverted.
   */
  protected boolean drawInverted()
  {
    return ! (slider.getInverted() ^ leftToRightCache);
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
   * This method returns the label whose  key has the highest value.
   *
   * @return The high value label or null if no label table exists.
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
    // FIXME: Move this to propertyChangeEvent handler, when we get those.
    leftToRightCache = slider.getComponentOrientation() != ComponentOrientation.RIGHT_TO_LEFT;
    // FIXME: This next line is only here because the above line is here.
    calculateThumbLocation();

    if (slider.getPaintTrack())
      paintTrack(g);
    if (slider.getPaintTicks())
      paintTicks(g);
    if (slider.getPaintLabels())
      paintLabels(g);

    //FIXME: Paint focus.
    paintThumb(g);
  }

  /**
   * This method recalculates any rectangles that need to be recalculated
   * after the insets of the component have changed.
   */
  protected void recalculateIfInsetsChanged()
  {
    // Examining a test program shows that either Sun calls private
    // methods that we don't know about, or these don't do anything.
    calculateFocusRect();

    calculateContentRect();
    calculateThumbSize();
    calculateTrackBuffer();
    calculateTrackRect();
    calculateThumbLocation();

    calculateTickRect();
    calculateLabelRect();
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

    Point a = new Point(trackRect.x, trackRect.y);
    Point b = new Point(a);
    Point c = new Point(a);
    Point d = new Point(a);

    Polygon high;
    Polygon shadow;

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
	    double loc = tickRect.x;
	    double increment = (max == min) ? 0
	                                    : majorSpace * (double) tickRect.width / (max
	                                    - min);
	    if (drawInverted())
	      {
		loc += tickRect.width;
		increment *= -1;
	      }
	    for (int i = min; i <= max; i += majorSpace)
	      {
		paintMajorTickForHorizSlider(g, tickRect, (int) loc);
		loc += increment;
	      }
	  }
	else
	  {
	    double loc = tickRect.height + tickRect.y;
	    double increment = (max == min) ? 0
	                                    : -majorSpace * (double) tickRect.height / (max
	                                    - min);
	    if (drawInverted())
	      {
		loc = tickRect.y;
		increment *= -1;
	      }
	    for (int i = min; i <= max; i += majorSpace)
	      {
		paintMajorTickForVertSlider(g, tickRect, (int) loc);
		loc += increment;
	      }
	  }
      }
    if (minorSpace > 0)
      {
	if (slider.getOrientation() == JSlider.HORIZONTAL)
	  {
	    double loc = tickRect.x;
	    double increment = (max == min) ? 0
	                                    : minorSpace * (double) tickRect.width / (max
	                                    - min);
	    if (drawInverted())
	      {
		loc += tickRect.width;
		increment *= -1;
	      }
	    for (int i = min; i <= max; i += minorSpace)
	      {
		paintMinorTickForHorizSlider(g, tickRect, (int) loc);
		loc += increment;
	      }
	  }
	else
	  {
	    double loc = tickRect.height + tickRect.y;
	    double increment = (max == min) ? 0
	                                    : -minorSpace * (double) tickRect.height / (max
	                                    - min);
	    if (drawInverted())
	      {
		loc = tickRect.y;
		increment *= -1;
	      }
	    for (int i = min; i <= max; i += minorSpace)
	      {
		paintMinorTickForVertSlider(g, tickRect, (int) loc);
		loc += increment;
	      }
	  }
      }
  }

  /* Minor ticks start at 1/4 of the height (or width) of the tickRect and extend
     to 1/2 of the tickRect.

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
    int y = tickRect.y + tickRect.height / 4;
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
    int y = tickRect.y + tickRect.height / 4;
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
    int x = tickRect.x + tickRect.width / 4;
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
    int x = tickRect.x + tickRect.width / 4;
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
    if (slider.getLabelTable() != null)
      {
	Dictionary table = slider.getLabelTable();
	Integer tmpKey;
	Object key;
	Object element;
	Component label;
	if (slider.getOrientation() == JSlider.HORIZONTAL)
	  {
	    for (Enumeration list = table.keys(); list.hasMoreElements();)
	      {
		key = list.nextElement();
		if (! (key instanceof Integer))
		  continue;
		tmpKey = (Integer) key;
		element = table.get(tmpKey);
		// We won't paint them if they're not
		// JLabels so continue anyway
		if (! (element instanceof JLabel))
		  continue;
		label = (Component) element;
		paintHorizontalLabel(g, tmpKey.intValue(), label);
	      }
	  }
	else
	  {
	    for (Enumeration list = table.keys(); list.hasMoreElements();)
	      {
		key = list.nextElement();
		if (! (key instanceof Integer))
		  continue;
		tmpKey = (Integer) key;
		element = table.get(tmpKey);
		// We won't paint them if they're not
		// JLabels so continue anyway
		if (! (element instanceof JLabel))
		  continue;
		label = (Component) element;
		paintVerticalLabel(g, tmpKey.intValue(), label);
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
    // This relies on clipping working properly or we'll end up
    // painting all over the place. If our preferred size is ignored, then
    // the labels may not fit inside the slider's bounds. Rather than mucking 
    // with font sizes and possible icon sizes, we'll set the bounds for
    // the label and let it get clipped.
    Dimension dim = label.getPreferredSize();
    int w = (int) dim.getWidth();
    int h = (int) dim.getHeight();

    int max = slider.getMaximum();
    int min = slider.getMinimum();

    if (value > max || value < min)
      return;

    //           value
    //             |
    //        ------------
    //        |          |
    //        |          |
    //        |          |
    //  The label must move w/2 to the right to fit directly under the value.
    int xpos = xPositionForValue(value) - w / 2;
    int ypos = labelRect.y;

    // We want to center the label around the xPositionForValue
    // So we use xpos - w / 2. However, if value is min and the label 
    // is large, we run the risk of going out of bounds. So we bring it back
    // to 0 if it becomes negative.
    if (xpos < 0)
      xpos = 0;

    // If the label + starting x position is greater than
    // the x space in the label rectangle, we reset it to the largest
    // amount possible in the rectangle. This means ugliness.
    if (xpos + w > labelRect.x + labelRect.width)
      w = labelRect.x + labelRect.width - xpos;

    // If the label is too tall. We reset it to the height of the label
    // rectangle.
    if (h > labelRect.height)
      h = labelRect.height;

    label.setBounds(xpos, ypos, w, h);
    javax.swing.SwingUtilities.paintComponent(g, label, null, label.getBounds());
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
    Dimension dim = label.getPreferredSize();
    int w = (int) dim.getWidth();
    int h = (int) dim.getHeight();

    int max = slider.getMaximum();
    int min = slider.getMinimum();

    if (value > max || value < min)
      return;

    int xpos = labelRect.x;
    int ypos = yPositionForValue(value) - h / 2;

    if (ypos < 0)
      ypos = 0;

    if (ypos + h > labelRect.y + labelRect.height)
      h = labelRect.y + labelRect.height - ypos;

    if (w > labelRect.width)
      w = labelRect.width;

    label.setBounds(xpos, ypos, w, h);
    javax.swing.SwingUtilities.paintComponent(g, label, null, label.getBounds());
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

    Polygon thumb = new Polygon();

    Point a = new Point(thumbRect.x, thumbRect.y);
    Point b = new Point(a);
    Point c = new Point(a);
    Point d = new Point(a);
    Point e = new Point(a);

    Polygon bright;
    Polygon dark;
    Polygon all;

    // This will be in X-dimension if the slider is inverted and y if it isn't.	  	  
    int turnPoint;

    if (slider.getOrientation() == JSlider.HORIZONTAL)
      {
	turnPoint = thumbRect.height * 3 / 4;

	b.translate(thumbRect.width, 0);
	c.translate(thumbRect.width, turnPoint);
	d.translate(thumbRect.width / 2, thumbRect.height);
	e.translate(0, turnPoint);

	bright = new Polygon(new int[] { b.x, a.x, e.x, d.x },
	                     new int[] { b.y, a.y, e.y, d.y }, 4);

	dark = new Polygon(new int[] { b.x, c.x, d.x },
	                   new int[] { b.y, c.y, d.y }, 3);
	all = new Polygon(new int[] { a.x + 1, b.x, c.x, d.x, e.x + 1 },
	                  new int[] { a.y + 1, b.y + 1, c.y, d.y + 1, e.y }, 5);
      }
    else
      {
	turnPoint = thumbRect.width * 3 / 4;

	b.translate(turnPoint, 0);
	c.translate(thumbRect.width, thumbRect.height / 2);
	d.translate(turnPoint, thumbRect.height);
	e.translate(0, thumbRect.height);

	bright = new Polygon(new int[] { c.x, b.x, a.x, e.x },
	                     new int[] { c.y, b.y, a.y, e.y }, 4);

	dark = new Polygon(new int[] { c.x, d.x, e.x + 1 },
	                   new int[] { c.y, d.y, e.y }, 3);

	all = new Polygon(new int[] { a.x + 1, b.x, c.x - 1, d.x, e.x + 1 },
	                  new int[] { a.y + 1, b.y + 1, c.y, d.y, e.y }, 5);
      }

    g.setColor(Color.WHITE);
    g.drawPolyline(bright.xpoints, bright.ypoints, bright.npoints);

    g.setColor(Color.BLACK);
    g.drawPolyline(dark.xpoints, dark.ypoints, dark.npoints);

    g.setColor(Color.GRAY);
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
    thumbRect.x = x;
    thumbRect.y = y;
  }

  /**
   * This method is used to move the thumb one  block in the direction
   * specified. If the slider  snaps to ticks, this method is responsible for
   * snapping it to a tick after the thumb  has been moved.
   *
   * @param direction The direction to move in.
   */
  public void scrollByBlock(int direction)
  {
    // The direction is -1 for backwards and 1 for forwards.
    int unit = direction * (slider.getMaximum() - slider.getMinimum()) / 10;

    int moveTo = slider.getValue() + unit;

    if (slider.getSnapToTicks())
      moveTo = findClosestTick(moveTo);

    slider.setValue(moveTo);
  }

  /**
   * This method is used to move the thumb one unit in the direction
   * specified. If the slider snaps to ticks, this method is responsible for
   * snapping it to a tick after the thumb has been moved.
   *
   * @param direction The direction to move in.
   */
  public void scrollByUnit(int direction)
  {
    // The direction is -1 for backwards and 1 for forwards.
    int moveTo = slider.getValue() + direction;

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
   * This method returns the X coordinate for the value passed in.
   *
   * @param value The value to calculate an x coordinate for.
   *
   * @return The x coordinate for the value.
   */
  protected int xPositionForValue(int value)
  {
    int min = slider.getMinimum();
    int max = slider.getMaximum();
    int extent = slider.getExtent();
    int len = trackRect.width;

    int xPos = (max == min) ? 0 : (value - min) * len / (max - min);

    if (! drawInverted())
      xPos += trackRect.x;
    else
      {
	xPos = trackRect.width - xPos;
	xPos += trackRect.x;
      }
    return xPos;
  }

  /**
   * This method returns the y coordinate for the value passed in.
   *
   * @param value The value to calculate a y coordinate for.
   *
   * @return The y coordinate for the value.
   */
  protected int yPositionForValue(int value)
  {
    int min = slider.getMinimum();
    int max = slider.getMaximum();
    int extent = slider.getExtent();
    int len = trackRect.height;

    int yPos = (max == min) ? 0 : (value - min) * len / (max - min);

    if (! drawInverted())
      {
	yPos = trackRect.height - yPos;
	yPos += trackRect.y;
      }
    else
      yPos += trackRect.y;
    return yPos;
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

    // If the length is 0, you shouldn't be able to even see where the slider is.
    // This really shouldn't ever happen, but just in case, we'll return the middle.
    if (len == 0)
      return ((max - min) / 2);

    if (! drawInverted())
      value = ((len - (yPos - trackRect.y)) * (max - min) / len + min);
    else
      value = ((yPos - trackRect.y) * (max - min) / len + min);

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

    // If the length is 0, you shouldn't be able to even see where the slider is.
    // This really shouldn't ever happen, but just in case, we'll return the middle.
    if (len == 0)
      return ((max - min) / 2);

    if (! drawInverted())
      value = ((xPos - trackRect.x) * (max - min) / len + min);
    else
      value = ((len - (xPos - trackRect.x)) * (max - min) / len + min);

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
}
