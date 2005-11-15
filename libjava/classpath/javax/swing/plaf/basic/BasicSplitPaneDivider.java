/* BasicSplitPaneDivider.java --
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JButton;
import javax.swing.JSplitPane;
import javax.swing.SwingConstants;
import javax.swing.border.Border;

/**
 * The divider that separates the two parts of a JSplitPane in the Basic look
 * and feel.
 * 
 * <p>
 * Implementation status: We do not have a real implementation yet. Currently,
 * it is mostly a stub to allow compiling other parts of the
 * javax.swing.plaf.basic package, although some parts are already
 * functional.
 * </p>
 *
 * @author Sascha Brawer (brawer_AT_dandelis.ch)
 */
public class BasicSplitPaneDivider extends Container
  implements PropertyChangeListener
{
  /**
   * Determined using the <code>serialver</code> tool of Apple/Sun JDK 1.3.1
   * on MacOS X 10.1.5.
   */
  static final long serialVersionUID = 1463404307042803342L;

  /**
   * The width and height of the little buttons for showing and hiding parts
   * of a JSplitPane in a single mouse click.
   */
  protected static final int ONE_TOUCH_SIZE = 6;

  /** The distance the one touch buttons will sit from the divider's edges. */
  protected static final int ONE_TOUCH_OFFSET = 2;

  /**
   * An object that performs the tasks associated with an ongoing drag
   * operation, or <code>null</code> if the user is currently not dragging
   * the divider.
   */
  protected DragController dragger;

  /**
   * The delegate object that is responsible for the UI of the
   * <code>JSplitPane</code> that contains this divider.
   */
  protected BasicSplitPaneUI splitPaneUI;

  /** The thickness of the divider in pixels. */
  protected int dividerSize;

  /** A divider that is used for layout purposes. */
  protected Component hiddenDivider;

  /** The JSplitPane containing this divider. */
  protected JSplitPane splitPane;

  /**
   * The listener for handling mouse events from both the divider and the
   * containing <code>JSplitPane</code>.
   * 
   * <p>
   * The reason for also handling MouseEvents from the containing
   * <code>JSplitPane</code> is that users should be able to start a drag
   * gesture from inside the JSplitPane, but slightly outisde the divider.
   * </p>
   */
  protected MouseHandler mouseHandler = new MouseHandler();

  /**
   * The current orientation of the containing <code>JSplitPane</code>, which
   * is either {@link javax.swing.JSplitPane#HORIZONTAL_SPLIT} or {@link
   * javax.swing.JSplitPane#VERTICAL_SPLIT}.
   */
  protected int orientation;

  /**
   * The button for showing and hiding the left (or top) component of the
   * <code>JSplitPane</code>.
   */
  protected JButton leftButton;

  /**
   * The button for showing and hiding the right (or bottom) component of the
   * <code>JSplitPane</code>.
   */
  protected JButton rightButton;

  /**
   * The border of this divider. Typically, this will be an instance of {@link
   * javax.swing.plaf.basic.BasicBorders.SplitPaneDividerBorder}.
   *
   * @see #getBorder()
   * @see #setBorder(javax.swing.border.Border)
   */
  private Border border;

  // This is not a pixel count.
  // This int should be able to take 3 values.
  // left (top), middle, right(bottom)
  //    0          1          2

  /**
   * Keeps track of where the divider should be placed when using one touch
   * expand buttons.
   * This is package-private to avoid an accessor method.
   */
  transient int currentDividerLocation = 1;

  /** DOCUMENT ME! */
  private transient Border tmpBorder = new Border()
    {
      public Insets getBorderInsets(Component c)
      {
	return new Insets(2, 2, 2, 2);
      }

      public boolean isBorderOpaque()
      {
	return false;
      }

      public void paintBorder(Component c, Graphics g, int x, int y,
                              int width, int height)
      {
	Color saved = g.getColor();
	g.setColor(Color.BLACK);

	g.drawRect(x + 2, y + 2, width - 4, height - 4);

	g.setColor(saved);
      }
    };

  /**
   * Constructs a new divider.
   *
   * @param ui the UI delegate of the enclosing <code>JSplitPane</code>.
   */
  public BasicSplitPaneDivider(BasicSplitPaneUI ui)
  {
    setLayout(new DividerLayout());
    setBasicSplitPaneUI(ui);
    setDividerSize(splitPane.getDividerSize());
    setBorder(tmpBorder);
  }

  /**
   * Sets the delegate object that is responsible for the UI of the {@link
   * javax.swing.JSplitPane} containing this divider.
   *
   * @param newUI the UI delegate, or <code>null</code> to release the
   *        connection to the current delegate.
   */
  public void setBasicSplitPaneUI(BasicSplitPaneUI newUI)
  {
    /* Remove the connection to the existing JSplitPane. */
    if (splitPane != null)
      {
	splitPane.removePropertyChangeListener(this);
	splitPane.removeMouseListener(mouseHandler);
	splitPane.removeMouseMotionListener(mouseHandler);
	removeMouseListener(mouseHandler);
	removeMouseMotionListener(mouseHandler);
	splitPane = null;
	hiddenDivider = null;
      }

    /* Establish the connection to the new JSplitPane. */
    splitPaneUI = newUI;
    if (splitPaneUI != null)
      splitPane = newUI.getSplitPane();
    if (splitPane != null)
      {
	splitPane.addPropertyChangeListener(this);
	splitPane.addMouseListener(mouseHandler);
	splitPane.addMouseMotionListener(mouseHandler);
	addMouseListener(mouseHandler);
	addMouseMotionListener(mouseHandler);
	hiddenDivider = splitPaneUI.getNonContinuousLayoutDivider();
	orientation = splitPane.getOrientation();
	oneTouchExpandableChanged();
      }
  }

  /**
   * Returns the delegate object that is responsible for the UI of the {@link
   * javax.swing.JSplitPane} containing this divider.
   *
   * @return The UI for the JSplitPane.
   */
  public BasicSplitPaneUI getBasicSplitPaneUI()
  {
    return splitPaneUI;
  }

  /**
   * Sets the thickness of the divider.
   *
   * @param newSize the new width or height in pixels.
   */
  public void setDividerSize(int newSize)
  {
    this.dividerSize = newSize;
  }

  /**
   * Retrieves the thickness of the divider.
   *
   * @return The thickness of the divider.
   */
  public int getDividerSize()
  {
    return dividerSize;
  }

  /**
   * Sets the border of this divider.
   *
   * @param border the new border. Typically, this will be an instance of
   *        {@link
   *        javax.swing.plaf.basic.BasicBorders.SplitPaneBorder}.
   *
   * @since 1.3
   */
  public void setBorder(Border border)
  {
    if (border != this.border)
      {
	Border oldValue = this.border;
	this.border = border;
	firePropertyChange("border", oldValue, border);
      }
  }

  /**
   * Retrieves the border of this divider.
   *
   * @return the current border, or <code>null</code> if no border has been
   *         set.
   *
   * @since 1.3
   */
  public Border getBorder()
  {
    return border;
  }

  /**
   * Retrieves the insets of the divider. If a border has been installed on
   * the divider, the result of calling its <code>getBorderInsets</code>
   * method is returned. Otherwise, the inherited implementation will be
   * invoked.
   *
   * @see javax.swing.border.Border#getBorderInsets(java.awt.Component)
   */
  public Insets getInsets()
  {
    if (border != null)
      return border.getBorderInsets(this);
    else
      return super.getInsets();
  }

  /**
   * Returns the preferred size of this divider, which is
   * <code>dividerSize</code> by <code>dividerSize</code> pixels.
   *
   * @return The preferred size of the divider.
   */
  public Dimension getPreferredSize()
  {
    return getLayout().preferredLayoutSize(this);
  }

  /**
   * Returns the minimal size of this divider, which is
   * <code>dividerSize</code> by <code>dividerSize</code> pixels.
   *
   * @return The minimal size of the divider.
   */
  public Dimension getMinimumSize()
  {
    return getPreferredSize();
  }

  /**
   * Processes events from the <code>JSplitPane</code> that contains this
   * divider.
   *
   * @param e The PropertyChangeEvent.
   */
  public void propertyChange(PropertyChangeEvent e)
  {
    if (e.getPropertyName().equals(JSplitPane.ONE_TOUCH_EXPANDABLE_PROPERTY))
      oneTouchExpandableChanged();
    else if (e.getPropertyName().equals(JSplitPane.ORIENTATION_PROPERTY))
      {
	orientation = splitPane.getOrientation();
	if (splitPane.isOneTouchExpandable())
	  {
	    layout();
	    repaint();
	  }
      }
    else if (e.getPropertyName().equals(JSplitPane.DIVIDER_SIZE_PROPERTY))
      dividerSize = splitPane.getDividerSize();
  }

  /**
   * Paints the divider by painting its border.
   *
   * @param g The Graphics Object to paint with.
   */
  public void paint(Graphics g)
  {
    Dimension dividerSize;

    super.paint(g);
    if (border != null)
      {
	dividerSize = getSize();
	border.paintBorder(this, g, 0, 0, dividerSize.width, dividerSize.height);
      }
  }

  /**
   * Reacts to changes of the <code>oneToughExpandable</code> property of the
   * containing <code>JSplitPane</code>.
   */
  protected void oneTouchExpandableChanged()
  {
    if (splitPane.isOneTouchExpandable())
      {
	leftButton = createLeftOneTouchButton();
	rightButton = createRightOneTouchButton();
	add(leftButton);
	add(rightButton);

	leftButton.addMouseListener(mouseHandler);
	rightButton.addMouseListener(mouseHandler);

	// Set it to 1.
	currentDividerLocation = 1;
      }
    else
      {
	if (leftButton != null && rightButton != null)
	  {
	    leftButton.removeMouseListener(mouseHandler);
	    rightButton.removeMouseListener(mouseHandler);

	    remove(leftButton);
	    remove(rightButton);
	    leftButton = null;
	    rightButton = null;
	  }
      }
    layout();
    repaint();
  }

  /**
   * Creates a button for showing and hiding the left (or top) part of a
   * <code>JSplitPane</code>.
   *
   * @return The left one touch button.
   */
  protected JButton createLeftOneTouchButton()
  {
    int dir = SwingConstants.WEST;
    if (orientation == JSplitPane.VERTICAL_SPLIT)
      dir = SwingConstants.NORTH;
    JButton button = new BasicArrowButton(dir);
    button.setBorder(null);

    return button;
  }

  /**
   * Creates a button for showing and hiding the right (or bottom) part of a
   * <code>JSplitPane</code>.
   *
   * @return The right one touch button.
   */
  protected JButton createRightOneTouchButton()
  {
    int dir = SwingConstants.EAST;
    if (orientation == JSplitPane.VERTICAL_SPLIT)
      dir = SwingConstants.SOUTH;
    JButton button = new BasicArrowButton(dir);
    button.setBorder(null);
    return button;
  }

  /**
   * Prepares the divider for dragging by calling the
   * <code>startDragging</code> method of the UI delegate of the enclosing
   * <code>JSplitPane</code>.
   *
   * @see BasicSplitPaneUI#startDragging()
   */
  protected void prepareForDragging()
  {
    if (splitPaneUI != null)
      splitPaneUI.startDragging();
  }

  /**
   * Drags the divider to a given location by calling the
   * <code>dragDividerTo</code> method of the UI delegate of the enclosing
   * <code>JSplitPane</code>.
   *
   * @param location the new location of the divider.
   *
   * @see BasicSplitPaneUI#dragDividerTo(int location)
   */
  protected void dragDividerTo(int location)
  {
    if (splitPaneUI != null)
      splitPaneUI.dragDividerTo(location);
  }

  /**
   * Finishes a dragging gesture by calling the <code>finishDraggingTo</code>
   * method of the UI delegate of the enclosing <code>JSplitPane</code>.
   *
   * @param location the new, final location of the divider.
   *
   * @see BasicSplitPaneUI#finishDraggingTo(int location)
   */
  protected void finishDraggingTo(int location)
  {
    if (splitPaneUI != null)
      splitPaneUI.finishDraggingTo(location);
  }

  /**
   * This helper method moves the divider to one of the  three locations when
   * using one touch expand buttons. Location 0 is the left (or top) most
   * location. Location 1 is the middle. Location 2 is the right (or bottom)
   * most location.
   * This is package-private to avoid an accessor method.
   *
   * @param locationIndex The location to move to.
   */
  void moveDividerTo(int locationIndex)
  {
    Insets insets = splitPane.getInsets();
    switch (locationIndex)
      {
      case 1:
	splitPane.setDividerLocation(splitPane.getLastDividerLocation());
	break;
      case 0:
	int top = (orientation == JSplitPane.HORIZONTAL_SPLIT) ? insets.left
	                                                       : insets.top;
	splitPane.setDividerLocation(top);
	break;
      case 2:
	int bottom;
	if (orientation == JSplitPane.HORIZONTAL_SPLIT)
	  bottom = splitPane.getBounds().width - insets.right - dividerSize;
	else
	  bottom = splitPane.getBounds().height - insets.bottom - dividerSize;
	splitPane.setDividerLocation(bottom);
	break;
      }
  }

  /**
   * The listener for handling mouse events from both the divider and the
   * containing <code>JSplitPane</code>.
   * 
   * <p>
   * The reason for also handling MouseEvents from the containing
   * <code>JSplitPane</code> is that users should be able to start a drag
   * gesture from inside the JSplitPane, but slightly outisde the divider.
   * </p>
   *
   * @author Sascha Brawer (brawer_AT_dandelis.ch)
   */
  protected class MouseHandler extends MouseAdapter
    implements MouseMotionListener
  {
    /** Keeps track of whether a drag is occurring. */
    private transient boolean isDragging;

    /**
     * This method is called when the mouse is pressed.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      if (splitPane.isOneTouchExpandable())
        {
	  if (e.getSource() == leftButton)
	    {
	      currentDividerLocation--;
	      if (currentDividerLocation < 0)
		currentDividerLocation = 0;
	      moveDividerTo(currentDividerLocation);
	      return;
	    }
	  else if (e.getSource() == rightButton)
	    {
	      currentDividerLocation++;
	      if (currentDividerLocation > 2)
		currentDividerLocation = 2;
	      moveDividerTo(currentDividerLocation);
	      return;
	    }
        }
      isDragging = true;
      currentDividerLocation = 1;
      if (orientation == JSplitPane.HORIZONTAL_SPLIT)
	dragger = new DragController(e);
      else
	dragger = new VerticalDragController(e);
      prepareForDragging();
    }

    /**
     * This method is called when the mouse is released.
     *
     * @param e The MouseEvent.
     */
    public void mouseReleased(MouseEvent e)
    {
      if (isDragging)
	dragger.completeDrag(e);
      isDragging = false;
    }

    /**
     * Repeatedly invoked when the user is dragging the mouse cursor while
     * having pressed a mouse button.
     *
     * @param e The MouseEvent.
     */
    public void mouseDragged(MouseEvent e)
    {
      if (dragger != null)
	dragger.continueDrag(e);
    }

    /**
     * Repeatedly invoked when the user is dragging the mouse cursor without
     * having pressed a mouse button.
     *
     * @param e The MouseEvent.
     */
    public void mouseMoved(MouseEvent e)
    {
      // Do nothing.
    }
  }

  /**
   * Performs the tasks associated with an ongoing drag operation.
   *
   * @author Sascha Brawer (brawer_AT_dandelis.ch)
   */
  protected class DragController
  {
    /**
     * The difference between where the mouse is clicked and the  initial
     * divider location.
     */
    transient int offset;

    /**
     * Creates a new DragController object.
     *
     * @param e The MouseEvent to initialize with.
     */
    protected DragController(MouseEvent e)
    {
      offset = e.getX();
    }

    /**
     * This method returns true if the divider can move.
     *
     * @return True if dragging is allowed.
     */
    protected boolean isValid()
    {
      // Views can always be resized?
      return true;
    }

    /**
     * Returns a position for the divider given the MouseEvent.
     *
     * @param e MouseEvent.
     *
     * @return The position for the divider to move to.
     */
    protected int positionForMouseEvent(MouseEvent e)
    {
      return e.getX() + getX() - offset;
    }

    /**
     * This method returns one of the two paramters for the orientation. In
     * this case, it returns x.
     *
     * @param x The x coordinate.
     * @param y The y coordinate.
     *
     * @return The x coordinate.
     */
    protected int getNeededLocation(int x, int y)
    {
      return x;
    }

    /**
     * This method is called to pass on the drag information to the UI through
     * dragDividerTo.
     *
     * @param newX The x coordinate of the MouseEvent.
     * @param newY The y coordinate of the MouseEvent.
     */
    protected void continueDrag(int newX, int newY)
    {
      if (isValid())
	dragDividerTo(adjust(newX, newY));
    }

    /**
     * This method is called to pass on the drag information  to the UI
     * through dragDividerTo.
     *
     * @param e The MouseEvent.
     */
    protected void continueDrag(MouseEvent e)
    {
      if (isValid())
	dragDividerTo(positionForMouseEvent(e));
    }

    /**
     * This method is called to finish the drag session  by calling
     * finishDraggingTo.
     *
     * @param x The x coordinate of the MouseEvent.
     * @param y The y coordinate of the MouseEvent.
     */
    protected void completeDrag(int x, int y)
    {
      finishDraggingTo(adjust(x, y));
    }

    /**
     * This method is called to finish the drag session  by calling
     * finishDraggingTo.
     *
     * @param e The MouseEvent.
     */
    protected void completeDrag(MouseEvent e)
    {
      finishDraggingTo(positionForMouseEvent(e));
    }

    /**
     * This is a helper method that includes the offset in the needed
     * location.
     *
     * @param x The x coordinate of the MouseEvent.
     * @param y The y coordinate of the MouseEvent.
     *
     * @return The needed location adjusted by the offsets.
     */
    int adjust(int x, int y)
    {
      return getNeededLocation(x, y) + getX() - offset;
    }
  }

  /**
   * This is a helper class that controls dragging when  the orientation is
   * VERTICAL_SPLIT.
   */
  protected class VerticalDragController extends DragController
  {
    /**
     * Creates a new VerticalDragController object.
     *
     * @param e The MouseEvent to initialize with.
     */
    protected VerticalDragController(MouseEvent e)
    {
      super(e);
      offset = e.getY();
    }

    /**
     * This method returns one of the two parameters given the orientation. In
     * this case, it returns y.
     *
     * @param x The x coordinate of the MouseEvent.
     * @param y The y coordinate of the MouseEvent.
     *
     * @return The y coordinate.
     */
    protected int getNeededLocation(int x, int y)
    {
      return y;
    }

    /**
     * This method returns the new location of the divider given a MouseEvent.
     *
     * @param e The MouseEvent.
     *
     * @return The new location of the divider.
     */
    protected int positionForMouseEvent(MouseEvent e)
    {
      return e.getY() + getY() - offset;
    }

    /**
     * This is a helper method that includes the offset in the needed
     * location.
     *
     * @param x The x coordinate of the MouseEvent.
     * @param y The y coordinate of the MouseEvent.
     *
     * @return The needed location adjusted by the offsets.
     */
    int adjust(int x, int y)
    {
      return getNeededLocation(x, y) + getY() - offset;
    }
  }

  /**
   * This helper class acts as the Layout Manager for the divider.
   */
  protected class DividerLayout implements LayoutManager
  {
    /**
     * Creates a new DividerLayout object.
     */
    protected DividerLayout()
    {
      // Nothing to do here.
    }

    /**
     * This method is called when a Component is added.
     *
     * @param string The constraints string.
     * @param c The Component to add.
     */
    public void addLayoutComponent(String string, Component c)
    {
      // Do nothing.
    }

    /**
     * This method is called to lay out the container.
     *
     * @param c The container to lay out.
     */
    public void layoutContainer(Container c)
    {
      if (splitPane.isOneTouchExpandable())
        {
	  changeButtonOrientation();
	  positionButtons();
        }
    }

    /**
     * This method returns the minimum layout size.
     *
     * @param c The container to calculate for.
     *
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container c)
    {
      return preferredLayoutSize(c);
    }

    /**
     * This method returns the preferred layout size.
     *
     * @param c The container to calculate for.
     *
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container c)
    {
      return new Dimension(dividerSize, dividerSize);
    }

    /**
     * This method is called when a component is removed.
     *
     * @param c The component to remove.
     */
    public void removeLayoutComponent(Component c)
    {
      // Do nothing.
    }

    /**
     * This method changes the button orientation when the orientation of the
     * SplitPane changes.
     */
    private void changeButtonOrientation()
    {
      if (orientation == JSplitPane.HORIZONTAL_SPLIT)
        {
	  ((BasicArrowButton) rightButton).setDirection(SwingConstants.EAST);
	  ((BasicArrowButton) leftButton).setDirection(SwingConstants.WEST);
        }
      else
        {
	  ((BasicArrowButton) rightButton).setDirection(SwingConstants.SOUTH);
	  ((BasicArrowButton) leftButton).setDirection(SwingConstants.NORTH);
        }
    }

    /**
     * This method sizes and positions the buttons.
     */
    private void positionButtons()
    {
      int w = 0;
      int h = 0;
      if (orientation == JSplitPane.HORIZONTAL_SPLIT)
        {
	  rightButton.setLocation(ONE_TOUCH_OFFSET, ONE_TOUCH_OFFSET);
	  leftButton.setLocation(ONE_TOUCH_OFFSET,
	                         ONE_TOUCH_OFFSET + 2 * ONE_TOUCH_SIZE);
	  w = dividerSize - 2 * ONE_TOUCH_OFFSET;
	  h = 2 * ONE_TOUCH_SIZE;
        }
      else
        {
	  leftButton.setLocation(ONE_TOUCH_OFFSET, ONE_TOUCH_OFFSET);
	  rightButton.setLocation(ONE_TOUCH_OFFSET + 2 * ONE_TOUCH_SIZE,
	                          ONE_TOUCH_OFFSET);
	  h = dividerSize - 2 * ONE_TOUCH_OFFSET;
	  w = 2 * ONE_TOUCH_SIZE;
        }
      Dimension dims = new Dimension(w, h);
      leftButton.setSize(dims);
      rightButton.setSize(dims);
    }
  }
}
