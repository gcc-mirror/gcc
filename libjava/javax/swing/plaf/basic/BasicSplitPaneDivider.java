/* BasicSplitPaneDivider.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JButton;
import javax.swing.JSplitPane;
import javax.swing.border.Border;


/**
 * The divider that separates the two parts of a JSplitPane in the
 * Basic look and feel.
 *
 * <p>Implementation status: We do not have a real implementation yet.
 * Currently, it is mostly a stub to allow compiling other parts of
 * the javax.swing.plaf.basic package, although some parts are already
 * functional.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class BasicSplitPaneDivider
  extends Container
  implements PropertyChangeListener
{
  /**
   * Determined using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = 1463404307042803342L;


  /**
   * The width and height of the little buttons for showing and
   * hiding parts of a JSplitPane in a single mouse click.
   */
  protected static final int ONE_TOUCH_SIZE = 6;


  // FIXME: Javadoc.
  protected static final int ONE_TOUCH_OFFSET = 2;


  /**
   * An object that performs the tasks associated with an ongoing drag
   * operation, or <code>null</code> if the user is currently not
   * dragging the divider.
   */
  protected DragController dragger;


  /**
   * The delegate object that is responsible for the UI of the
   * <code>JSplitPane</code> that contains this divider.
   */
  protected BasicSplitPaneUI splitPaneUI;


  /**
   * The thickness of the divider in pixels.
   */
  protected int dividerSize;
  

  /**
   * A divider that is used for layout purposes.
   */
  protected Component hiddenDivider;


  /**
   * The JSplitPane containing this divider.
   */
  protected JSplitPane splitPane;


  /**
   * The listener for handling mouse events from both the divider
   * and the containing <code>JSplitPane</code>.
   *
   * <p>The reason for also handling MouseEvents from the containing
   * <code>JSplitPane</code> is that users should be able to start
   * a drag gesture from inside the JSplitPane, but slightly outisde
   * the divider.
   */
  protected MouseHandler mouseHandler = new MouseHandler();


  /**
   * The current orientation of the containing <code>JSplitPane</code>,
   * which is either {@link javax.swing.JSplitPane#HORIZONTAL_SPLIT}
   * or {@link javax.swing.JSplitPane#VERTICAL_SPLIT}.
   */
  protected int orientation;


  /**
   * The button for showing and hiding the left (or top) component
   * of the <code>JSplitPane</code>.
   */
  protected JButton leftButton;


  /**
   * The button for showing and hiding the right (or bottom) component
   * of the <code>JSplitPane</code>.
   */
  protected JButton rightButton;


  /**
   * The border of this divider. Typically, this will be an instance of
   * {@link javax.swing.plaf.basic.BasicBorders.SplitPaneDividerBorder}.
   *
   * @see #getBorder()
   * @see #setBorder(javax.swing.border.Border)
   */
  private Border border;


  /**
   * Constructs a new divider.
   *
   * @param ui the UI delegate of the enclosing
   *           <code>JSplitPane</code>.
   */
  public BasicSplitPaneDivider(BasicSplitPaneUI ui)
  {
    setBasicSplitPaneUI(ui);
  }


  /**
   * Sets the delegate object that is responsible for the UI of the
   * {@link javax.swing.JSplitPane} containing this divider.
   *
   * @param newUI the UI delegate, or <code>null</code> to release
   *        the connection to the current delegate.
   */
  public void setBasicSplitPaneUI(BasicSplitPaneUI newUI)
  {
    /* Remove the connection to the existing JSplitPane. */
    if (splitPane != null)
    {
      splitPane.removePropertyChangeListener(this);
      splitPane.removeMouseListener(mouseHandler);
      splitPane.removeMouseMotionListener(mouseHandler);
      splitPane = null;
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
      orientation = splitPane.getOrientation();
    }
  }
  

  /**
   * Returns the delegate object that is responsible for the UI of the
   * {@link javax.swing.JSplitPane} containing this divider.
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
   */
  public int getDividerSize()
  {
    return dividerSize;
  }
  
  
  /**
   * Sets the border of this divider.
   *
   * @param border the new border. Typically, this will be an instance of
   * {@link javax.swing.plaf.basic.BasicBorders.SplitPaneDividerBorder}.
   *
   * @since 1.3
   */
  public void setBorder(Border border)
  {
    Border oldValue = this.border;
    this.border = border;
    firePropertyChange("border", oldValue, border);
  }


  /**
   * Retrieves the border of this divider.
   *
   * @return the current border, or <code>null</code> if no border
   *         has been set.
   *
   * @since 1.3
   */
  public Border getBorder()
  {
    return border;
  }
  

  /**
   * Retrieves the insets of the divider. If a border has been
   * installed on the divider, the result of calling its
   * <code>getBorderInsets</code> method is returned. Otherwise,
   * the inherited implementation will be invoked.
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
   * <code>dividerSize</code> by <code>dividerSize</code>
   * pixels.
   */
  public Dimension getPreferredSize()
  {
    return new Dimension(dividerSize, dividerSize);
  }


  /**
   * Returns the minimal size of this divider, which is
   * <code>dividerSize</code> by <code>dividerSize</code>
   * pixels.
   */
  public Dimension getMinimumSize()
  {
    return getPreferredSize();
  }
  
  
  /**
   * Processes events from the <code>JSplitPane</code> that contains
   * this divider.
   */
  public void propertyChange(PropertyChangeEvent e)
  {
    // FIXME: Not yet implemented.
  }


  /**
   * Paints the divider by painting its border.
   */
  public void paint(Graphics g)
  {
    Dimension dividerSize;

    super.paint(g);
    if (border != null)
    {
      dividerSize = getSize();
      border.paintBorder(this, g, 0, 0, dividerSize.width, dividerSize.height);
      //System.out.println(dividerSize);
      //g.setColor(java.awt.Color.white);
      //g.drawRect(0, 0, 5, 5);
    }
  }


  /**
   * Reacts to changes of the <code>oneToughExpandable</code>
   * property of the containing <code>JSplitPane</code>.
   */
  protected void oneTouchExpandableChanged()
  {
    // FIXME: Not yet implemented.
  }


  /**
   * Creates a button for showing and hiding the left (or top)
   * part of a <code>JSplitPane</code>.
   */
  protected JButton createLeftOneTouchButton()
  {
    return new OneTouchButton(/* left */ true);
  }


  /**
   * Creates a button for showing and hiding the right (or bottom)
   * part of a <code>JSplitPane</code>.
   */
  protected JButton createRightOneTouchButton()
  {
    return new OneTouchButton(/* left */ false);
  }
  

  /**
   * Prepares the divider for dragging by calling the
   * <code>startDragging</code> method of the UI delegate of the
   * enclosing <code>JSplitPane</code>.
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
   * <code>dragDividerTo</code> method of the UI delegate of the
   * enclosing <code>JSplitPane</code>.
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
   * Finishes a dragging gesture by calling the
   * <code>finishDraggingTo</code> method of the UI delegate of the
   * enclosing <code>JSplitPane</code>.
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
   * The listener for handling mouse events from both the divider
   * and the containing <code>JSplitPane</code>.
   *
   * <p>The reason for also handling MouseEvents from the containing
   * <code>JSplitPane</code> is that users should be able to start
   * a drag gesture from inside the JSplitPane, but slightly outisde
   * the divider. 
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  protected class MouseHandler
    extends MouseAdapter
    implements MouseMotionListener
  {
    
    public void mousePressed(MouseEvent e)
    {
      // FIXME: Not yet implemented.
    }


    public void mouseReleased(MouseEvent e)
    {
      // FIXME: Not yet implemented.
    }


    /**
     * Repeatedly invoked when the user is dragging the mouse cursor
     * while having pressed a mouse button.
     */
    public void mouseDragged(MouseEvent e)
    {
      // FIXME: Not yet implemented.
    }


    /**
     * Repeatedly invoked when the user is dragging the mouse cursor
     * without having pressed a mouse button.
     */
    public void mouseMoved(MouseEvent e)
    {
      // FIXME: Not yet implemented.
    }
  }


  /**
   * A small button for showing and hiding parts of a
   * <code>JSplitPane</code> with a single mouse click.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static class OneTouchButton
    extends JButton
  {
    OneTouchButton(boolean left)
    {
      // FIXME: Set various properties of the button.
      // Make  sure it looks identical to the small
      // buttons of the Sun reference implementation.
      // The size should also be the same.
      if (left)
        setText("<");
      else
        setText(">");

      Dimension butSize = new Dimension(ONE_TOUCH_SIZE, ONE_TOUCH_SIZE);
      setMinimumSize(butSize);
      setMaximumSize(butSize);
      setPreferredSize(butSize);

      setBorderPainted(false);
    }
  }


  /**
   * Performs the tasks associated with an ongoing drag
   * operation.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  protected class DragController
  {
    // FIXME: Not yet implemented.
    protected DragController(MouseEvent e)
    {
    }

    protected boolean isValid()
    {
      // FIXME: Not yet implemented.
      return true;
    }

    protected int positionForMouseEvent(MouseEvent e)
    {
      return 0;
    }

    protected int getNeededLocation(int x, int y)
    {
      return 0;
    }

    protected void continueDrag(int newX, int newY)
    {
    }

    protected void completeDrag(int x, int y)
    {
    }

    protected void completeDrag(MouseEvent e)
    {
    }
  }
}
