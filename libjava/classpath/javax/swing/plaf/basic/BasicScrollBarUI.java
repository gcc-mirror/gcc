/* BasicScrollBarUI.java --
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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BoundedRangeModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.LookAndFeel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ScrollBarUI;

/**
 * The Basic Look and Feel UI delegate for JScrollBar.
 */
public class BasicScrollBarUI extends ScrollBarUI implements LayoutManager,
                                                             SwingConstants
{
  /**
   * A helper class that listens to the two JButtons on each end of the
   * JScrollBar.
   */
  protected class ArrowButtonListener extends MouseAdapter
  {
   
    /**
     * Move the thumb in the direction specified by the  button's arrow. If
     * this button is held down, then it should keep moving the thumb.
     *
     * @param e The MouseEvent fired by the JButton.
     */
    public void mousePressed(MouseEvent e)
    {
      scrollTimer.stop();
      scrollListener.setScrollByBlock(false);
      if (e.getSource() == incrButton)
          scrollListener.setDirection(POSITIVE_SCROLL);
      else if (e.getSource() == decrButton)
          scrollListener.setDirection(NEGATIVE_SCROLL);
      scrollTimer.setDelay(100);
      scrollTimer.start();
    }

    /**
     * Stops the thumb when the JButton is released.
     *
     * @param e The MouseEvent fired by the JButton.
     */
    public void mouseReleased(MouseEvent e)
    {
      scrollTimer.stop();
      scrollTimer.setDelay(300);
      if (e.getSource() == incrButton)
          scrollByUnit(POSITIVE_SCROLL);
      else if (e.getSource() == decrButton)
        scrollByUnit(NEGATIVE_SCROLL);
    }
  }

  /**
   * A helper class that listens to the ScrollBar's model for ChangeEvents.
   */
  protected class ModelListener implements ChangeListener
  {
    /**
     * Called when the model changes.
     *
     * @param e The ChangeEvent fired by the model.
     */
    public void stateChanged(ChangeEvent e)
    {
      calculatePreferredSize();
      updateThumbRect();
      scrollbar.repaint();
    }
  }

  /**
   * A helper class that listens to the ScrollBar's properties.
   */
  public class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * Called when one of the ScrollBar's properties change.
     *
     * @param e The PropertyChangeEvent fired by the ScrollBar.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals("model"))
        {
          ((BoundedRangeModel) e.getOldValue()).removeChangeListener(modelListener);
          scrollbar.getModel().addChangeListener(modelListener);
          updateThumbRect();
        }
      else if (e.getPropertyName().equals("orientation"))
        {
          uninstallListeners();
          uninstallComponents();
          uninstallDefaults();
          installDefaults();
          installComponents();
          installListeners();
        }
      else if (e.getPropertyName().equals("enabled"))
        {
          Boolean b = (Boolean) e.getNewValue();
          if (incrButton != null)
            incrButton.setEnabled(b.booleanValue());
          if (decrButton != null)
            decrButton.setEnabled(b.booleanValue());
        }
    }
  }

  /**
   * A helper class that listens for events from the timer that is used to
   * move the thumb.
   */
  protected class ScrollListener implements ActionListener
  {
    /** The direction the thumb moves in. */
    private transient int direction;

    /** Whether movement will be in blocks. */
    private transient boolean block;

    /**
     * Creates a new ScrollListener object. The default is scrolling
     * positively with block movement.
     */
    public ScrollListener()
    {
      direction = POSITIVE_SCROLL;
      block = true;
    }

    /**
     * Creates a new ScrollListener object using the given direction and
     * block.
     *
     * @param dir The direction to move in.
     * @param block Whether movement will be in blocks.
     */
    public ScrollListener(int dir, boolean block)
    {
      direction = dir;
      this.block = block;
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
     * Sets whether scrolling will be done in blocks.
     *
     * @param block Whether scrolling will be in blocks.
     */
    public void setScrollByBlock(boolean block)
    {
      this.block = block;
    }

    /**
     * Called every time the timer reaches its interval.
     *
     * @param e The ActionEvent fired by the timer.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (block)
        {
          // Only need to check it if it's block scrolling
          // We only block scroll if the click occurs
          // in the track.
          if (!trackListener.shouldScroll(direction))
            {
              trackHighlight = NO_HIGHLIGHT;
              scrollbar.repaint();
              return;
            }
            scrollByBlock(direction);
        }
      else
        scrollByUnit(direction);
    }
  }

  /**
   * Helper class that listens for movement on the track.
   */
  protected class TrackListener extends MouseAdapter
    implements MouseMotionListener
  {
    /** The current X coordinate of the mouse. */
    protected int currentMouseX;

    /** The current Y coordinate of the mouse. */
    protected int currentMouseY;

    /**
     * The offset between the current mouse cursor and the  current value of
     * the scrollbar.
     */
    protected int offset;

    /**
     * This method is called when the mouse is being dragged.
     *
     * @param e The MouseEvent given.
     */
    public void mouseDragged(MouseEvent e)
    {
      currentMouseX = e.getX();
      currentMouseY = e.getY();
      if (scrollbar.getValueIsAdjusting())
        {
	  int value;
	  if (scrollbar.getOrientation() == SwingConstants.HORIZONTAL)
	    value = valueForXPosition(currentMouseX) - offset;
	  else
	    value = valueForYPosition(currentMouseY) - offset;

	  scrollbar.setValue(value);
        }
    }

    /**
     * This method is called when the mouse is moved.
     *
     * @param e The MouseEvent given.
     */
    public void mouseMoved(MouseEvent e)
    {
      // Not interested in where the mouse
      // is unless it is being dragged.
    }

    /**
     * This method is called when the mouse is pressed. When it is pressed,
     * the thumb should move in blocks towards the cursor.
     *
     * @param e The MouseEvent given.
     */
    public void mousePressed(MouseEvent e)
    {
      currentMouseX = e.getX();
      currentMouseY = e.getY();

      int value;
      if (scrollbar.getOrientation() == SwingConstants.HORIZONTAL)
	value = valueForXPosition(currentMouseX);
      else
	value = valueForYPosition(currentMouseY);

      if (! thumbRect.contains(e.getPoint()))
        {
	  scrollTimer.stop();
	  scrollListener.setScrollByBlock(true);
	  if (value > scrollbar.getValue())
	    {
	      trackHighlight = INCREASE_HIGHLIGHT;
	      scrollListener.setDirection(POSITIVE_SCROLL);
	    }
	  else
	    {
	      trackHighlight = DECREASE_HIGHLIGHT;
	      scrollListener.setDirection(NEGATIVE_SCROLL);
	    }
      scrollTimer.setDelay(100);
	  scrollTimer.start();
        }
      else
        {
	  // We'd like to keep track of where the cursor
	  // is inside the thumb.
	  // This works because the scrollbar's value represents 
	  // "lower" edge of the thumb. The value at which
	  // the cursor is at must be greater or equal
	  // to that value.

      scrollListener.setScrollByBlock(false);
	  scrollbar.setValueIsAdjusting(true);
      offset = value - scrollbar.getValue();
        }
      scrollbar.repaint();
    }

    /**
     * This method is called when the mouse is released. It should stop
     * movement on the thumb
     *
     * @param e The MouseEvent given.
     */
    public void mouseReleased(MouseEvent e)
    {
      scrollTimer.stop();
      scrollTimer.setDelay(300);
      currentMouseX = e.getX();
      currentMouseY = e.getY();

      if (shouldScroll(POSITIVE_SCROLL))
        scrollByBlock(POSITIVE_SCROLL);
      else if (shouldScroll(NEGATIVE_SCROLL))
        scrollByBlock(NEGATIVE_SCROLL);

      trackHighlight = NO_HIGHLIGHT;
      scrollListener.setScrollByBlock(false);
      scrollbar.setValueIsAdjusting(true);
      scrollbar.repaint();
    }

    /**
     * A helper method that decides whether we should keep scrolling in the
     * given direction.
     *
     * @param direction The direction to check for.
     *
     * @return Whether the thumb should keep scrolling.
     */
    public boolean shouldScroll(int direction)
    {
      int value;
      if (scrollbar.getOrientation() == HORIZONTAL)
	value = valueForXPosition(currentMouseX);
      else
	value = valueForYPosition(currentMouseY);

      if (thumbRect.contains(currentMouseX, currentMouseY))
        return false;
      
      if (direction == POSITIVE_SCROLL)
	return (value > scrollbar.getValue());
      else
	return (value < scrollbar.getValue());
    }
  }

  /** The listener that listens to the JButtons. */
  protected ArrowButtonListener buttonListener;

  /** The listener that listens to the model. */
  protected ModelListener modelListener;

  /** The listener that listens to the scrollbar for property changes. */
  protected PropertyChangeListener propertyChangeListener;

  /** The listener that listens to the timer. */
  protected ScrollListener scrollListener;

  /** The listener that listens for MouseEvents on the track. */
  protected TrackListener trackListener;

  /** The JButton that decrements the scrollbar's value. */
  protected JButton decrButton;

  /** The JButton that increments the scrollbar's value. */
  protected JButton incrButton;

  /** The dimensions of the maximum thumb size. */
  protected Dimension maximumThumbSize;

  /** The dimensions of the minimum thumb size. */
  protected Dimension minimumThumbSize;

  /** The color of the thumb. */
  protected Color thumbColor;

  /** The outer shadow of the thumb. */
  protected Color thumbDarkShadowColor;

  /** The top and left edge color for the thumb. */
  protected Color thumbHighlightColor;

  /** The outer light shadow for the thumb. */
  protected Color thumbLightShadowColor;

  /** The color that is used when the mouse press occurs in the track. */
  protected Color trackHighlightColor;

  /** The color of the track. */
  protected Color trackColor;

  /** The size and position of the track. */
  protected Rectangle trackRect;

  /** The size and position of the thumb. */
  protected Rectangle thumbRect;

  /** Indicates that the decrease highlight should be painted. */
  protected static final int DECREASE_HIGHLIGHT = 1;

  /** Indicates that the increase highlight should be painted. */
  protected static final int INCREASE_HIGHLIGHT = 2;

  /** Indicates that no highlight should be painted. */
  protected static final int NO_HIGHLIGHT = 0;

  /** Indicates that the scrolling direction is positive. */
  private static final int POSITIVE_SCROLL = 1;

  /** Indicates that the scrolling direction is negative. */
  private static final int NEGATIVE_SCROLL = -1;

  /** The cached preferred size for the scrollbar. */
  private transient Dimension preferredSize;

  /** The current highlight status. */
  protected int trackHighlight;

  /** FIXME: Use this for something (presumably mouseDragged) */
  protected boolean isDragging;

  /** The timer used to move the thumb when the mouse is held. */
  protected Timer scrollTimer;

  /** The scrollbar this UI is acting for. */
  protected JScrollBar scrollbar;

  /**
   * This method adds a component to the layout.
   *
   * @param name The name to associate with the component that is added.
   * @param child The Component to add.
   */
  public void addLayoutComponent(String name, Component child)
  {
    // You should not be adding stuff to this component.
    // The contents are fixed.
  }

  /**
   * This method configures the scrollbar's colors. This can be  done by
   * looking up the standard colors from the Look and Feel defaults.
   */
  protected void configureScrollBarColors()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    trackColor = defaults.getColor("ScrollBar.track");
    trackHighlightColor = defaults.getColor("ScrollBar.trackHighlight");
    thumbColor = defaults.getColor("ScrollBar.thumb");
    thumbHighlightColor = defaults.getColor("ScrollBar.thumbHighlight");
    thumbDarkShadowColor = defaults.getColor("ScrollBar.thumbDarkShadow");
    thumbLightShadowColor = defaults.getColor("ScrollBar.thumbShadow");
  }

  /**
   * This method creates an ArrowButtonListener.
   *
   * @return A new ArrowButtonListener.
   */
  protected ArrowButtonListener createArrowButtonListener()
  {
    return new ArrowButtonListener();
  }

  /**
   * This method creates a new JButton with the appropriate icon for the
   * orientation.
   *
   * @param orientation The orientation this JButton uses.
   *
   * @return The increase JButton.
   */
  protected JButton createIncreaseButton(int orientation)
  {
    return new BasicArrowButton(orientation);
  }

  /**
   * This method creates a new JButton with the appropriate icon for the
   * orientation.
   *
   * @param orientation The orientation this JButton uses.
   *
   * @return The decrease JButton.
   */
  protected JButton createDecreaseButton(int orientation)
  {
    return new BasicArrowButton(orientation);
  }

  /**
   * This method creates a new ModelListener.
   *
   * @return A new ModelListener.
   */
  protected ModelListener createModelListener()
  {
    return new ModelListener();
  }

  /**
   * This method creates a new PropertyChangeListener.
   *
   * @return A new PropertyChangeListener.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeHandler();
  }

  /**
   * This method creates a new ScrollListener.
   *
   * @return A new ScrollListener.
   */
  protected ScrollListener createScrollListener()
  {
    return new ScrollListener();
  }

  /**
   * This method creates a new TrackListener.
   *
   * @return A new TrackListener.
   */
  protected TrackListener createTrackListener()
  {
    return new TrackListener();
  }

  /**
   * This method returns a new BasicScrollBarUI.
   *
   * @param c The JComponent to create a UI for.
   *
   * @return A new BasicScrollBarUI.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicScrollBarUI();
  }

  /**
   * This method returns the maximum size for this JComponent.
   *
   * @param c The JComponent to measure the maximum size for.
   *
   * @return The maximum size for the component.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
  }

  /**
   * This method returns the maximum thumb size.
   *
   * @return The maximum thumb size.
   */
  protected Dimension getMaximumThumbSize()
  {
    return maximumThumbSize;
  }

  /**
   * This method returns the minimum size for this JComponent.
   *
   * @param c The JComponent to measure the minimum size for.
   *
   * @return The minimum size for the component.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the minimum thumb size.
   *
   * @return The minimum thumb size.
   */
  protected Dimension getMinimumThumbSize()
  {
    return minimumThumbSize;
  }

  /**
   * This method calculates the preferred size since calling
   * getPreferredSize() returns a cached value.
   * This is package-private to avoid an accessor method.
   */
  void calculatePreferredSize()
  {
    int height;
    int width;
    height = width = 0;

    if (scrollbar.getOrientation() == SwingConstants.HORIZONTAL)
      {
	width += incrButton.getPreferredSize().getWidth();
	width += decrButton.getPreferredSize().getWidth();

	width += (scrollbar.getMaximum() - scrollbar.getMinimum());

	height = Math.max(incrButton.getPreferredSize().height,
	                  decrButton.getPreferredSize().height);
	height = Math.max(getMinimumThumbSize().height, height);
	height = Math.min(getMaximumThumbSize().height, height);
      }
    else
      {
	height += incrButton.getPreferredSize().getHeight();
	height += decrButton.getPreferredSize().getHeight();

	height += (scrollbar.getMaximum() - scrollbar.getMinimum());

	width = Math.max(incrButton.getPreferredSize().width,
	                 decrButton.getPreferredSize().width);
	width = Math.max(getMinimumThumbSize().width, width);
	width = Math.min(getMaximumThumbSize().width, width);
      }

    Insets insets = scrollbar.getInsets();

    height += insets.top + insets.bottom;
    width += insets.left + insets.right;

    preferredSize = new Dimension(width, height);
  }

  /**
   * This method returns a cached value of the preferredSize. The only
   * restrictions are: If the scrollbar is horizontal, the height should be
   * the maximum of the height of the JButtons and  the minimum width of the
   * thumb. For vertical scrollbars, the  calculation is similar (swap width
   * for height and vice versa).
   *
   * @param c The JComponent to measure.
   *
   * @return The preferredSize.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    calculatePreferredSize();
    return preferredSize;
  }

  /**
   * This method returns the thumb's bounds based on the  current value of the
   * scrollbar. This method updates the cached value and returns that.
   *
   * @return The thumb bounds.
   */
  protected Rectangle getThumbBounds()
  {
    return thumbRect;
  }

  /**
   * This method calculates the bounds of the track. This method updates the
   * cached value and returns it.
   *
   * @return The track's bounds.
   */
  protected Rectangle getTrackBounds()
  {
    return trackRect;
  }

  /**
   * This method installs any addition Components that  are a part of or
   * related to this scrollbar.
   */
  protected void installComponents()
  {
    if (incrButton != null)
      scrollbar.add(incrButton);
    if (decrButton != null)
      scrollbar.add(decrButton);
  }

  /**
   * This method installs the defaults for the scrollbar specified by the
   * Basic Look and Feel.
   */
  protected void installDefaults()
  {
    int orientation = scrollbar.getOrientation();
    switch (orientation)
      {
      case (JScrollBar.HORIZONTAL):
        incrButton = createIncreaseButton(EAST);
        decrButton = createDecreaseButton(WEST);
        break;
      default:
        incrButton = createIncreaseButton(SOUTH);
        decrButton = createDecreaseButton(NORTH);
        break;
      }

    LookAndFeel.installColors(scrollbar, "ScrollBar.background",
                              "ScrollBar.foreground");
    LookAndFeel.installBorder(scrollbar, "ScrollBar.border");
    scrollbar.setOpaque(true);
    scrollbar.setLayout(this);

    thumbColor = UIManager.getColor("ScrollBar.thumb");
    thumbDarkShadowColor = UIManager.getColor("ScrollBar.thumbDarkShadow");
    thumbHighlightColor = UIManager.getColor("ScrollBar.thumbHighlight");
    thumbLightShadowColor = UIManager.getColor("ScrollBar.thumbShadow");

    maximumThumbSize = UIManager.getDimension("ScrollBar.maximumThumbSize");
    minimumThumbSize = UIManager.getDimension("ScrollBar.minimumThumbSize");
  }

  /**
   * This method installs the keyboard actions for the scrollbar.
   */
  protected void installKeyboardActions()
  {
    // FIXME: implement.
  }

  /**
   * This method installs any listeners for the scrollbar. This method also
   * installs listeners for things such as the JButtons and the timer.
   */
  protected void installListeners()
  {
    scrollListener = createScrollListener();
    trackListener = createTrackListener();
    buttonListener = createArrowButtonListener();
    modelListener = createModelListener();
    propertyChangeListener = createPropertyChangeListener();

    scrollbar.addMouseMotionListener(trackListener);
    scrollbar.addMouseListener(trackListener);

    incrButton.addMouseListener(buttonListener);
    decrButton.addMouseListener(buttonListener);

    scrollbar.addPropertyChangeListener(propertyChangeListener);
    scrollbar.getModel().addChangeListener(modelListener);

    scrollTimer.addActionListener(scrollListener);
  }

  /**
   * This method installs the UI for the component. This can include setting
   * up listeners, defaults,  and components. This also includes initializing
   * any data objects.
   *
   * @param c The JComponent to install.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    if (c instanceof JScrollBar)
      {
	scrollbar = (JScrollBar) c;

	trackRect = new Rectangle();
	thumbRect = new Rectangle();

	scrollTimer = new Timer(300, null);

        installDefaults();
	installComponents();
	configureScrollBarColors();
	installListeners();

	calculatePreferredSize();
      }
  }

  /**
   * This method lays out the scrollbar.
   *
   * @param scrollbarContainer The Container to layout.
   */
  public void layoutContainer(Container scrollbarContainer)
  {
    if (scrollbarContainer instanceof JScrollBar)
      {
	if (scrollbar.getOrientation() == SwingConstants.HORIZONTAL)
	  layoutHScrollbar((JScrollBar) scrollbarContainer);
	else
	  layoutVScrollbar((JScrollBar) scrollbarContainer);
      }
  }

  /**
   * This method lays out the scrollbar horizontally.
   *
   * @param sb The JScrollBar to layout.
   */
  protected void layoutHScrollbar(JScrollBar sb)
  {
    Rectangle vr = new Rectangle();
    SwingUtilities.calculateInnerArea(scrollbar, vr);

    Dimension incrDims = incrButton.getPreferredSize();
    Dimension decrDims = decrButton.getPreferredSize();
    
    // calculate and update the track bounds
    SwingUtilities.calculateInnerArea(scrollbar, trackRect);
    trackRect.width -= incrDims.getWidth();
    trackRect.width -= decrDims.getWidth();
    trackRect.x += decrDims.getWidth();

    updateThumbRect();
    
    decrButton.setBounds(vr.x, vr.y, decrDims.width, trackRect.height);
    incrButton.setBounds(trackRect.x + trackRect.width, vr.y, incrDims.width,
                         trackRect.height);
  }

  /**
   * This method lays out the scrollbar vertically.
   *
   * @param sb The JScrollBar to layout.
   */
  protected void layoutVScrollbar(JScrollBar sb)
  {
    Rectangle vr = new Rectangle();
    SwingUtilities.calculateInnerArea(scrollbar, vr);

    Dimension incrDims = incrButton.getPreferredSize();
    Dimension decrDims = decrButton.getPreferredSize();
    
    // Update rectangles
    SwingUtilities.calculateInnerArea(scrollbar, trackRect);
    trackRect.height -= incrDims.getHeight();
    trackRect.height -= decrDims.getHeight();
    trackRect.y += decrDims.getHeight();
    
    updateThumbRect();

    decrButton.setBounds(vr.x, vr.y, trackRect.width, decrDims.height);
    incrButton.setBounds(vr.x, trackRect.y + trackRect.height,
                         trackRect.width, incrDims.height);
  }

  /**
   * Updates the thumb rect.
   */
  void updateThumbRect()
  {
    int max = scrollbar.getMaximum();
    int min = scrollbar.getMinimum();
    int value = scrollbar.getValue();
    int extent = scrollbar.getVisibleAmount();
    if (max - extent <= min)
      {
        if (scrollbar.getOrientation() == JScrollBar.HORIZONTAL)
          {
            thumbRect.x = trackRect.x;
            thumbRect.y = trackRect.y;
            thumbRect.width = getMinimumThumbSize().width;
            thumbRect.height = trackRect.height;
          }
        else
          {
            thumbRect.x = trackRect.x;
            thumbRect.y = trackRect.y;
            thumbRect.width = trackRect.width;
            thumbRect.height = getMinimumThumbSize().height;
          }
      }
    else
      {
        if (scrollbar.getOrientation() == JScrollBar.HORIZONTAL)
          {
            thumbRect.x = trackRect.x;
            thumbRect.width = Math.max(extent * trackRect.width / (max - min),
                getMinimumThumbSize().width);
            int availableWidth = trackRect.width - thumbRect.width;
            thumbRect.x += (value - min) * availableWidth / (max - min - extent);
            thumbRect.y = trackRect.y;
            thumbRect.height = trackRect.height;
          }
        else
          {
            thumbRect.x = trackRect.x;
            thumbRect.height = Math.max(extent * trackRect.height / (max - min),
                    getMinimumThumbSize().height);
            int availableHeight = trackRect.height - thumbRect.height;
            thumbRect.y = trackRect.y 
              + (value - min) * availableHeight / (max - min - extent);
            thumbRect.width = trackRect.width;
          }
      }

  }
  
  /**
   * This method returns the minimum size required for the layout.
   *
   * @param scrollbarContainer The Container that is laid out.
   *
   * @return The minimum size.
   */
  public Dimension minimumLayoutSize(Container scrollbarContainer)
  {
    return preferredLayoutSize(scrollbarContainer);
  }

  /**
   * This method is called when the component is painted.
   *
   * @param g The Graphics object to paint with.
   * @param c The JComponent to paint.
   */
  public void paint(Graphics g, JComponent c)
  {
    paintTrack(g, c, getTrackBounds());
    paintThumb(g, c, getThumbBounds());

    if (trackHighlight == INCREASE_HIGHLIGHT)
      paintIncreaseHighlight(g);
    else if (trackHighlight == DECREASE_HIGHLIGHT)
      paintDecreaseHighlight(g);
  }

  /**
   * This method is called when repainting and the mouse is  pressed in the
   * track. It paints the track below the thumb with the trackHighlight
   * color.
   *
   * @param g The Graphics object to paint with.
   */
  protected void paintDecreaseHighlight(Graphics g)
  {
    Color saved = g.getColor();

    g.setColor(trackHighlightColor);
    if (scrollbar.getOrientation() == HORIZONTAL)
      g.fillRect(trackRect.x, trackRect.y, thumbRect.x - trackRect.x,
                 trackRect.height);
    else
      g.fillRect(trackRect.x, trackRect.y, trackRect.width,
                 thumbRect.y - trackRect.y);
    g.setColor(saved);
  }

  /**
   * This method is called when repainting and the mouse is  pressed in the
   * track. It paints the track above the thumb with the trackHighlight
   * color.
   *
   * @param g The Graphics objet to paint with.
   */
  protected void paintIncreaseHighlight(Graphics g)
  {
    Color saved = g.getColor();

    g.setColor(trackHighlightColor);
    if (scrollbar.getOrientation() == HORIZONTAL)
      g.fillRect(thumbRect.x + thumbRect.width, trackRect.y,
                 trackRect.x + trackRect.width - thumbRect.x - thumbRect.width,
                 trackRect.height);
    else
      g.fillRect(trackRect.x, thumbRect.y + thumbRect.height, trackRect.width,
                 trackRect.y + trackRect.height - thumbRect.y
                 - thumbRect.height);
    g.setColor(saved);
  }

  /**
   * This method paints the thumb.
   *
   * @param g The Graphics object to paint with.
   * @param c The Component that is being painted.
   * @param thumbBounds The thumb bounds.
   */
  protected void paintThumb(Graphics g, JComponent c, Rectangle thumbBounds)
  {
    g.setColor(thumbColor);
    g.fillRect(thumbBounds.x, thumbBounds.y, thumbBounds.width,
               thumbBounds.height);

    BasicGraphicsUtils.drawBezel(g, thumbBounds.x, thumbBounds.y,
                                 thumbBounds.width, thumbBounds.height,
                                 false, false, thumbDarkShadowColor,
                                 thumbDarkShadowColor, thumbHighlightColor,
                                 thumbHighlightColor);
  }

  /**
   * This method paints the track.
   *
   * @param g The Graphics object to paint with.
   * @param c The JComponent being painted.
   * @param trackBounds The track's bounds.
   */
  protected void paintTrack(Graphics g, JComponent c, Rectangle trackBounds)
  {
    Color saved = g.getColor();
    g.setColor(trackColor);
    g.fill3DRect(trackBounds.x, trackBounds.y, trackBounds.width,
                 trackBounds.height, false);
    g.setColor(saved);
  }

  /**
   * This method returns the preferred size for the layout.
   *
   * @param scrollbarContainer The Container to find a size for.
   *
   * @return The preferred size for the layout.
   */
  public Dimension preferredLayoutSize(Container scrollbarContainer)
  {
    if (scrollbarContainer instanceof JComponent)
      return getPreferredSize((JComponent) scrollbarContainer);
    else
      return null;
  }

  /**
   * This method removes a child component from the layout.
   *
   * @param child The child to remove.
   */
  public void removeLayoutComponent(Component child)
  {
    // You should not be removing stuff from this component.
  }

  /**
   * The method scrolls the thumb by a block in the  direction specified.
   *
   * @param direction The direction to scroll.
   */
  protected void scrollByBlock(int direction)
  {
    scrollbar.setValue(scrollbar.getValue()
                       + scrollbar.getBlockIncrement(direction));
  }

  /**
   * The method scrolls the thumb by a unit in the direction specified.
   *
   * @param direction The direction to scroll.
   */
  protected void scrollByUnit(int direction)
  {
    scrollbar.setValue(scrollbar.getValue()
                       + scrollbar.getUnitIncrement(direction));
  }

  /**
   * This method sets the thumb's bounds.
   *
   * @param x The X position of the thumb.
   * @param y The Y position of the thumb.
   * @param width The width of the thumb.
   * @param height The height of the thumb.
   */
  protected void setThumbBounds(int x, int y, int width, int height)
  {
    thumbRect.x = x;
    thumbRect.y = y;
    thumbRect.width = width;
    thumbRect.height = height;
  }

  /**
   * This method uninstalls any components that  are a part of or related to
   * this scrollbar.
   */
  protected void uninstallComponents()
  {
    if (incrButton != null)
      scrollbar.remove(incrButton);
    if (decrButton != null)
      scrollbar.remove(decrButton);
  }

  /**
   * This method uninstalls any defaults that this scrollbar acquired from the
   * Basic Look and Feel defaults.
   */
  protected void uninstallDefaults()
  {
    scrollbar.setForeground(null);
    scrollbar.setBackground(null);
    LookAndFeel.uninstallBorder(scrollbar);
    incrButton = null;
    decrButton = null;
  }

  /**
   * This method uninstalls any keyboard actions this scrollbar acquired
   * during install.
   */
  protected void uninstallKeyboardActions()
  {
    // FIXME: implement.
  }

  /**
   * This method uninstalls any listeners that were registered during install.
   */
  protected void uninstallListeners()
  {
    if (scrollTimer != null)
      scrollTimer.removeActionListener(scrollListener);

    if (scrollbar != null)
      {
        scrollbar.getModel().removeChangeListener(modelListener);
        scrollbar.removePropertyChangeListener(propertyChangeListener);
        scrollbar.removeMouseListener(trackListener);
        scrollbar.removeMouseMotionListener(trackListener);
      }

    if (decrButton != null)
      decrButton.removeMouseListener(buttonListener);
    if (incrButton != null)
      incrButton.removeMouseListener(buttonListener);
    
    propertyChangeListener = null;
    modelListener = null;
    buttonListener = null;
    trackListener = null;
    scrollListener = null;
  }

  /**
   * This method uninstalls the UI. This includes removing any defaults,
   * listeners, and components that this UI may have initialized. It also
   * nulls any instance data.
   *
   * @param c The Component to uninstall for.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallDefaults();
    uninstallComponents();

    scrollTimer = null;

    thumbRect = null;
    trackRect = null;

    trackColor = null;
    trackHighlightColor = null;
    thumbColor = null;
    thumbHighlightColor = null;
    thumbDarkShadowColor = null;
    thumbLightShadowColor = null;

    scrollbar = null;
  }

  /**
   * This method returns the value in the scrollbar's range given the y
   * coordinate. If the value is out of range, it will return the closest
   * legal value.
   * This is package-private to avoid an accessor method.
   *
   * @param yPos The y coordinate to calculate a value for.
   *
   * @return The value for the y coordinate.
   */
  int valueForYPosition(int yPos)
  {
    int min = scrollbar.getMinimum();
    int max = scrollbar.getMaximum();
    int len = trackRect.height;

    int value;

    // If the length is 0, you shouldn't be able to even see where the thumb is.
    // This really shouldn't ever happen, but just in case, we'll return the middle.
    if (len == 0)
      return ((max - min) / 2);

    value = ((yPos - trackRect.y) * (max - min) / len + min);

    // If this isn't a legal value, then we'll have to move to one now.
    if (value > max)
      value = max;
    else if (value < min)
      value = min;
    return value;
  }

  /**
   * This method returns the value in the scrollbar's range given the x
   * coordinate. If the value is out of range, it will return the closest
   * legal value.
   * This is package-private to avoid an accessor method.
   *
   * @param xPos The x coordinate to calculate a value for.
   *
   * @return The value for the x coordinate.
   */
  int valueForXPosition(int xPos)
  {
    int min = scrollbar.getMinimum();
    int max = scrollbar.getMaximum();
    int len = trackRect.width;

    int value;

    // If the length is 0, you shouldn't be able to even see where the slider is.
    // This really shouldn't ever happen, but just in case, we'll return the middle.
    if (len == 0)
      return ((max - min) / 2);

    value = ((xPos - trackRect.x) * (max - min) / len + min);

    // If this isn't a legal value, then we'll have to move to one now.
    if (value > max)
      value = max;
    else if (value < min)
      value = min;
    return value;
  }
}
