/* BasicInternalFrameUI.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

import javax.swing.DefaultDesktopManager;
import javax.swing.DesktopManager;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.AbstractBorder;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InternalFrameUI;
import javax.swing.plaf.UIResource;

/**
 * This is the UI delegate for the Basic look and feel for JInternalFrames.
 */
public class BasicInternalFrameUI extends InternalFrameUI
{
  /**
   * This is a helper class that listens to the JInternalFrame for
   * InternalFrameEvents.
   */
  protected class BasicInternalFrameListener implements InternalFrameListener
  {
    /**
     * This method is called when the JInternalFrame is activated.
     *
     * @param e The InternalFrameEvent.
     */
    public void internalFrameActivated(InternalFrameEvent e)
    {
      // FIXME: Implement.
    }

    /**
     * This method is called when the JInternalFrame is closed.
     *
     * @param e The InternalFrameEvent.
     */
    public void internalFrameClosed(InternalFrameEvent e)
    {
      // FIXME: Implement.
    }

    /**
     * This method is called when the JInternalFrame is closing.
     *
     * @param e The InternalFrameEvent.
     */
    public void internalFrameClosing(InternalFrameEvent e)
    {
      // FIXME: Implement.
    }

    /**
     * This method is called when the JInternalFrame is deactivated.
     *
     * @param e The InternalFrameEvent.
     */
    public void internalFrameDeactivated(InternalFrameEvent e)
    {
      // FIXME: Implement.
    }

    /**
     * This method is called when the JInternalFrame is  deiconified.
     *
     * @param e The InternalFrameEvent.
     */
    public void internalFrameDeiconified(InternalFrameEvent e)
    {
      // FIXME: Implement.
    }

    /**
     * This method is called when the JInternalFrame is  iconified.
     *
     * @param e The InternalFrameEvent.
     */
    public void internalFrameIconified(InternalFrameEvent e)
    {
      // FIXME: Implement.
    }

    /**
     * This method is called when the JInternalFrame is opened.
     *
     * @param e The InternalFrameEvent.
     */
    public void internalFrameOpened(InternalFrameEvent e)
    {
      // FIXME: Implement.
    }
  }

  /**
   * This helper class listens to the edges of the JInternalFrame and the
   * TitlePane for mouse events. It is responsible for dragging  and resizing
   * the JInternalFrame in response to the MouseEvents.
   */
  protected class BorderListener extends MouseInputAdapter
    implements SwingConstants
  {
    /** FIXME: Use for something. */
    protected final int RESIZE_NONE = 0;

    /** The x offset from the top left corner of the JInternalFrame. */
    private transient int xOffset = 0;

    /** The y offset from the top left corner of the JInternalFrame. */
    private transient int yOffset = 0;

    /** The direction that the resize is occuring in. */
    private transient int direction = -1;

    /** Cache rectangle that can be reused. */
    private transient Rectangle cacheRect = new Rectangle();

    /**
     * This method is called when the mouse is clicked.
     *
     * @param e The MouseEvent.
     */
    public void mouseClicked(MouseEvent e)
    {
      // There is nothing to do when the mouse is clicked
      // on the border.
    }

    /**
     * This method is called when the mouse is dragged. This method is
     * responsible for resizing or dragging the JInternalFrame.
     *
     * @param e The MouseEvent.
     */
    public void mouseDragged(MouseEvent e)
    {
      // If the frame is maximized, there is nothing that
      // can be dragged around.
      if (frame.isMaximum())
        return;
      DesktopManager dm = getDesktopManager();
      Rectangle b = frame.getBounds();
      Dimension min = frame.getMinimumSize();
      if (min == null)
        min = new Dimension(0, 0);
      Insets insets = frame.getInsets();
      int x = e.getX();
      int y = e.getY();
      if (e.getSource() == frame && frame.isResizable())
        {
          switch (direction)
            {
            case NORTH:
              cacheRect.setBounds(b.x, Math.min(b.y + y, b.y + b.height
                                                         - min.height),
                                  b.width, b.height - y);
              break;
            case NORTH_EAST:
              cacheRect.setBounds(b.x, Math.min(b.y + y, b.y + b.height
                                                         - min.height), x,
                                  b.height - y);
              break;
            case EAST:
              cacheRect.setBounds(b.x, b.y, x, b.height);
              break;
            case SOUTH_EAST:
              cacheRect.setBounds(b.x, b.y, x, y);
              break;
            case SOUTH:
              cacheRect.setBounds(b.x, b.y, b.width, y);
              break;
            case SOUTH_WEST:
              cacheRect.setBounds(Math.min(b.x + x, b.x + b.width - min.width),
                                  b.y, b.width - x, y);
              break;
            case WEST:
              cacheRect.setBounds(Math.min(b.x + x, b.x + b.width - min.width),
                                  b.y, b.width - x, b.height);
              break;
            case NORTH_WEST:
              cacheRect.setBounds(
                                  Math.min(b.x + x, b.x + b.width - min.width),
                                  Math.min(b.y + y, b.y + b.height - min.height),
                                  b.width - x, b.height - y);
              break;
            }
          dm.resizeFrame(frame, cacheRect.x, cacheRect.y,
                         Math.max(min.width, cacheRect.width),
                         Math.max(min.height, cacheRect.height));
        }
      else if (e.getSource() == titlePane)
        {
          Rectangle fBounds = frame.getBounds();

          dm.dragFrame(frame, e.getX() - xOffset + b.x, e.getY() - yOffset
                                                        + b.y);
        }
    }

    /**
     * This method is called when the mouse exits the JInternalFrame.
     *
     * @param e The MouseEvent.
     */
    public void mouseExited(MouseEvent e)
    {
      // There is nothing to do when the mouse exits 
      // the border area.
    }

    /**
     * This method is called when the mouse is moved inside the
     * JInternalFrame.
     *
     * @param e The MouseEvent.
     */
    public void mouseMoved(MouseEvent e)
    {
      // There is nothing to do when the mouse moves
      // over the border area.
    }

    /**
     * This method is called when the mouse is pressed.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      activateFrame(frame);
      DesktopManager dm = getDesktopManager();
      int x = e.getX();
      int y = e.getY();
      Insets insets = frame.getInsets();

      if (e.getSource() == frame && frame.isResizable())
        {
          direction = sectionOfClick(x, y);
          dm.beginResizingFrame(frame, direction);
        }
      else if (e.getSource() == titlePane)
        {
          Rectangle tBounds = titlePane.getBounds();

          xOffset = e.getX() - tBounds.x + insets.left;
          yOffset = e.getY() - tBounds.y + insets.top;

          dm.beginDraggingFrame(frame);
        }
    }

    /**
     * This method is called when the mouse is released.
     *
     * @param e The MouseEvent.
     */
    public void mouseReleased(MouseEvent e)
    {
      DesktopManager dm = getDesktopManager();
      xOffset = 0;
      yOffset = 0;
      if (e.getSource() == frame && frame.isResizable())
        dm.endResizingFrame(frame);
      else if (e.getSource() == titlePane)
        dm.endDraggingFrame(frame);
    }

    /**
     * This method determines the direction of the resize based on the
     * coordinates and the size of the JInternalFrame.
     *
     * @param x The x coordinate of the MouseEvent.
     * @param y The y coordinate of the MouseEvent.
     *
     * @return The direction of the resize (a SwingConstant direction).
     */
    private int sectionOfClick(int x, int y)
    {
      Insets insets = frame.getInsets();
      Rectangle b = frame.getBounds();
      if (x < insets.left && y < insets.top)
        return NORTH_WEST;
      else if (x > b.width - insets.right && y < insets.top)
        return NORTH_EAST;
      else if (x > b.width - insets.right && y > b.height - insets.bottom)
        return SOUTH_EAST;
      else if (x < insets.left && y > b.height - insets.bottom)
        return SOUTH_WEST;
      else if (y < insets.top)
        return NORTH;
      else if (x < insets.left)
        return WEST;
      else if (y > b.height - insets.bottom)
        return SOUTH;
      else if (x > b.width - insets.right)
        return EAST;

      return -1;
    }
  }

  /**
   * This helper class listens to the JDesktopPane that parents this
   * JInternalFrame and listens for resize events and resizes the
   * JInternalFrame appropriately.
   */
  protected class ComponentHandler implements ComponentListener
  {
    /**
     * This method is called when the JDesktopPane is hidden.
     * 
     * @param e
     *          The ComponentEvent fired.
     */
    public void componentHidden(ComponentEvent e)
    {
      // Do nothing.
    }

    /**
     * This method is called when the JDesktopPane is moved.
     * 
     * @param e
     *          The ComponentEvent fired.
     */
    public void componentMoved(ComponentEvent e)
    {
      // Do nothing.
    }

    /**
     * This method is called when the JDesktopPane is resized.
     * 
     * @param e
     *          The ComponentEvent fired.
     */
    public void componentResized(ComponentEvent e)
    {
      if (frame.isMaximum())
        {
          JDesktopPane pane = (JDesktopPane) e.getSource();
          Insets insets = pane.getInsets();
          Rectangle bounds = pane.getBounds();

          frame.setBounds(bounds.x + insets.left, bounds.y + insets.top,
                          bounds.width - insets.left - insets.right,
                          bounds.height - insets.top - insets.bottom);
          frame.revalidate();
          frame.repaint();
        }

      // Sun also resizes the icons. but it doesn't seem to do anything.
    }

    /**
     * This method is called when the JDesktopPane is shown.
     * 
     * @param e
     *          The ComponentEvent fired.
     */
    public void componentShown(ComponentEvent e)
    {
      // Do nothing.
    }
  }

  /**
   * This helper class acts as the LayoutManager for JInternalFrames.
   */
  public class InternalFrameLayout implements LayoutManager
  {
    /**
     * This method is called when the given Component is added to the
     * JInternalFrame.
     * 
     * @param name
     *          The name of the Component.
     * @param c
     *          The Component added.
     */
    public void addLayoutComponent(String name, Component c)
    {
      // Nothing to do here.
    }

    /**
     * This method is used to set the bounds of the children of the
     * JInternalFrame.
     * 
     * @param c
     *          The Container to lay out.
     */
    public void layoutContainer(Container c)
    {
      Dimension dims = frame.getSize();
      Insets insets = frame.getInsets();

      dims.width -= insets.left + insets.right;
      dims.height -= insets.top + insets.bottom;

      frame.getRootPane().getGlassPane().setBounds(0, 0, dims.width,
                                                   dims.height);
      int nh = 0;
      int sh = 0;
      int ew = 0;
      int ww = 0;

      if (northPane != null)
        {
          Dimension nDims = northPane.getPreferredSize();
          nh = Math.min(nDims.height, dims.height);

          northPane.setBounds(insets.left, insets.top, dims.width, nh);
        }

      if (southPane != null)
        {
          Dimension sDims = southPane.getPreferredSize();
          sh = Math.min(sDims.height, dims.height - nh);

          southPane.setBounds(insets.left, insets.top + dims.height - sh,
                              dims.width, sh);
        }

      int remHeight = dims.height - sh - nh;

      if (westPane != null)
        {
          Dimension wDims = westPane.getPreferredSize();
          ww = Math.min(dims.width, wDims.width);

          westPane.setBounds(insets.left, insets.top + nh, ww, remHeight);
        }

      if (eastPane != null)
        {
          Dimension eDims = eastPane.getPreferredSize();
          ew = Math.min(eDims.width, dims.width - ww);

          eastPane.setBounds(insets.left + dims.width - ew, insets.top + nh,
                             ew, remHeight);
        }

      int remWidth = dims.width - ww - ew;

      frame.getRootPane().setBounds(insets.left + ww, insets.top + nh,
                                    remWidth, remHeight);
    }

    /**
     * This method returns the minimum layout size.
     * 
     * @param c
     *          The Container to find a minimum layout size for.
     * @return The minimum dimensions for the JInternalFrame.
     */
    public Dimension minimumLayoutSize(Container c)
    {
      return getSize(c, true);
    }

    /**
     * This method returns the maximum layout size.
     * 
     * @param c
     *          The Container to find a maximum layout size for.
     * @return The maximum dimensions for the JInternalFrame.
     */
    public Dimension maximumLayoutSize(Container c)
    {
      return preferredLayoutSize(c);
    }

    /**
     * Th8is method returns the preferred layout size.
     * 
     * @param c
     *          The Container to find a preferred layout size for.
     * @return The preferred dimensions for the JInternalFrame.
     */
    public Dimension preferredLayoutSize(Container c)
    {
      return getSize(c, false);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param c
     *          DOCUMENT ME!
     * @param min
     *          DOCUMENT ME!
     * @return DOCUMENT ME!
     */
    private Dimension getSize(Container c, boolean min)
    {
      Insets insets = frame.getInsets();

      Dimension contentDims = frame.getContentPane().getPreferredSize();
      if (min)
        contentDims.width = contentDims.height = 0;
      int nWidth = 0;
      int nHeight = 0;
      int sWidth = 0;
      int sHeight = 0;
      int eWidth = 0;
      int eHeight = 0;
      int wWidth = 0;
      int wHeight = 0;
      Dimension dims;

      if (northPane != null)
        {
          dims = northPane.getPreferredSize();
          if (dims != null)
            {
              nWidth = dims.width;
              nHeight = dims.height;
            }
        }

      if (southPane != null)
        {
          dims = southPane.getPreferredSize();
          if (dims != null)
            {
              sWidth = dims.width;
              sHeight = dims.height;
            }
        }

      if (eastPane != null)
        {
          dims = eastPane.getPreferredSize();
          if (dims != null)
            {
              sWidth = dims.width;
              sHeight = dims.height;
            }
        }

      if (westPane != null)
        {
          dims = westPane.getPreferredSize();
          if (dims != null)
            {
              wWidth = dims.width;
              wHeight = dims.height;
            }
        }

      int width = Math.max(sWidth, nWidth);
      width = Math.max(width, contentDims.width + eWidth + wWidth);

      int height = Math.max(eHeight, wHeight);
      height = Math.max(height, contentDims.height);
      height += nHeight + sHeight;

      width += insets.left + insets.right;
      height += insets.top + insets.bottom;

      return new Dimension(width, height);
    }

    /**
     * This method is called when a Component is removed from the
     * JInternalFrame.
     *
     * @param c The Component that was removed.
     */
    public void removeLayoutComponent(Component c)
    {
      // Nothing to do here.
    }
  }

  /**
   * This helper class is used to listen to the JDesktopPane's glassPane for
   * MouseEvents. The JInternalFrame can then be selected if a click is
   * detected on its children.
   */
  protected class GlassPaneDispatcher implements MouseInputListener
  {
    /** The MouseEvent target. */
    private transient Component mouseEventTarget;

    /** The component pressed. */
    private transient Component pressedComponent;

    /** The last component entered. */
    private transient Component lastComponentEntered;

    /** Used to store/reset lastComponentEntered. */
    private transient Component tempComponent;

    /** The number of presses. */
    private transient int pressCount;

    /**
     * This method is called when the mouse enters the glass pane.
     * 
     * @param e
     *          The MouseEvent.
     */
    public void mouseEntered(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * This method is called when the mouse is clicked on the glass pane.
     * 
     * @param e
     *          The MouseEvent.
     */
    public void mouseClicked(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * This method is called when the mouse is dragged in the glass pane.
     * 
     * @param e
     *          The MouseEvent.
     */
    public void mouseDragged(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * This method is called when the mouse exits the glass pane.
     * 
     * @param e
     *          The MouseEvent.
     */
    public void mouseExited(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * This method is called when the mouse is moved in the glass pane.
     * 
     * @param e
     *          The MouseEvent.
     */
    public void mouseMoved(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * This method is called when the mouse is pressed in the glass pane.
     * 
     * @param e
     *          The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      activateFrame(frame);
      handleEvent(e);
    }

    /**
     * This method is called when the mouse is released in the glass pane.
     * 
     * @param e
     *          The MouseEvent.
     */
    public void mouseReleased(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * This method acquires a candidate component to dispatch the MouseEvent to.
     * 
     * @param me
     *          The MouseEvent to acquire a component for.
     */
    private void acquireComponentForMouseEvent(MouseEvent me)
    {
      int x = me.getX();
      int y = me.getY();

      // Find the candidate which should receive this event.
      Component parent = frame.getLayeredPane();
      if (parent == null)
        return;
      Component candidate = null;
      Point p = me.getPoint();
      while (candidate == null && parent != null)
        {
          candidate = SwingUtilities.getDeepestComponentAt(parent, p.x, p.y);
          if (candidate == null)
            {
              p = SwingUtilities.convertPoint(parent, p.x, p.y,
                                              parent.getParent());
              parent = parent.getParent();
            }
        }

      // If the only candidate we found was the native container itself,
      // don't dispatch any event at all. We only care about the lightweight
      // children here.
      if (candidate == frame.getContentPane())
        candidate = null;

      // If our candidate is new, inform the old target we're leaving.
      if (lastComponentEntered != null && lastComponentEntered.isShowing()
          && lastComponentEntered != candidate)
        {
          Point tp = SwingUtilities.convertPoint(frame.getContentPane(), x, y,
                                                 lastComponentEntered);
          MouseEvent exited = new MouseEvent(lastComponentEntered,
                                             MouseEvent.MOUSE_EXITED,
                                             me.getWhen(), me.getModifiersEx(),
                                             tp.x, tp.y, me.getClickCount(),
                                             me.isPopupTrigger(),
                                             me.getButton());
          tempComponent = lastComponentEntered;
          lastComponentEntered = null;
          tempComponent.dispatchEvent(exited);
        }

      // If we have a candidate, maybe enter it.
      if (candidate != null)
        {
          mouseEventTarget = candidate;
          if (candidate.isLightweight() && candidate.isShowing()
              && candidate != frame.getContentPane()
              && candidate != lastComponentEntered)
            {
              lastComponentEntered = mouseEventTarget;
              Point cp = SwingUtilities.convertPoint(frame.getContentPane(), x,
                                                     y, lastComponentEntered);
              MouseEvent entered = new MouseEvent(lastComponentEntered,
                                                  MouseEvent.MOUSE_ENTERED,
                                                  me.getWhen(),
                                                  me.getModifiersEx(), cp.x,
                                                  cp.y, me.getClickCount(),
                                                  me.isPopupTrigger(),
                                                  me.getButton());
              lastComponentEntered.dispatchEvent(entered);
            }
        }

      if (me.getID() == MouseEvent.MOUSE_RELEASED
          || me.getID() == MouseEvent.MOUSE_PRESSED && pressCount > 0
          || me.getID() == MouseEvent.MOUSE_DRAGGED)
        // If any of the following events occur while a button is held down,
        // they should be dispatched to the same component to which the
        // original MOUSE_PRESSED event was dispatched:
        // - MOUSE_RELEASED
        // - MOUSE_PRESSED: another button pressed while the first is held down
        // - MOUSE_DRAGGED
        mouseEventTarget = pressedComponent;
      else if (me.getID() == MouseEvent.MOUSE_CLICKED)
        {
          // Don't dispatch CLICKED events whose target is not the same as the
          // target for the original PRESSED event.
          if (candidate != pressedComponent)
            mouseEventTarget = null;
          else if (pressCount == 0)
            pressedComponent = null;
        }
    }

    /**
     * This is a helper method that dispatches the GlassPane MouseEvents to the
     * proper component.
     * 
     * @param e
     *          The AWTEvent to be dispatched. Usually an instance of
     *          MouseEvent.
     */
    private void handleEvent(AWTEvent e)
    {
      if (e instanceof MouseEvent)
        {
          MouseEvent me = (MouseEvent) e;
          acquireComponentForMouseEvent(me);

          //If there is no target, return
          if (mouseEventTarget == null)
            return;
          
          //Avoid re-dispatching to ourselves and causing an infinite loop
          if (mouseEventTarget.equals(frame.getGlassPane()))
            return;

          // Avoid dispatching ENTERED and EXITED events twice.
          if (mouseEventTarget.isShowing()
              && e.getID() != MouseEvent.MOUSE_ENTERED
              && e.getID() != MouseEvent.MOUSE_EXITED)
            {
              MouseEvent newEvt = SwingUtilities.convertMouseEvent(
                                                                   frame.getGlassPane(),
                                                                   me,
                                                                   mouseEventTarget);
              mouseEventTarget.dispatchEvent(newEvt);

              switch (e.getID())
                {
                case MouseEvent.MOUSE_PRESSED:
                  if (pressCount++ == 0)
                    pressedComponent = mouseEventTarget;
                  break;
                case MouseEvent.MOUSE_RELEASED:
                  // Clear our memory of the original PRESSED event, only if
                  // we're not expecting a CLICKED event after this. If
                  // there is a CLICKED event after this, it will do clean up.
                  if (--pressCount == 0 && mouseEventTarget != pressedComponent)
                    pressedComponent = null;
                  break;
                }
            }
        }
    }
  }

  /**
   * This helper class listens for PropertyChangeEvents from the
   * JInternalFrame.
   */
  public class InternalFramePropertyChangeListener implements
      PropertyChangeListener, VetoableChangeListener
  {

    /**
     * This method is called when one of the JInternalFrame's properties change.
     * This method is to allow JInternalFrame to veto an attempt to close the
     * internal frame. This allows JInternalFrame to honour its
     * defaultCloseOperation if that is DO_NOTHING_ON_CLOSE.
     */
    public void vetoableChange(PropertyChangeEvent e)
        throws PropertyVetoException
    {
      if (e.getPropertyName().equals(JInternalFrame.IS_CLOSED_PROPERTY))
        {
          if (frame.getDefaultCloseOperation() == JInternalFrame.HIDE_ON_CLOSE)
            {
              frame.setVisible(false);
              frame.getDesktopPane().repaint();
              throw new PropertyVetoException(
                                              "close operation is HIDE_ON_CLOSE\n",
                                              e);
            }
          else if (frame.getDefaultCloseOperation() == JInternalFrame.DISPOSE_ON_CLOSE)
            closeFrame(frame);
          else
            throw new PropertyVetoException(
                                            "close operation is DO_NOTHING_ON_CLOSE\n",
                                            e);
        }
    }

    /**
     * This method is called when one of the JInternalFrame's properties change.
     * 
     * @param evt
     *          The PropertyChangeEvent.
     */
    public void propertyChange(PropertyChangeEvent evt)
    {
      if (evt.getPropertyName().equals(JInternalFrame.IS_MAXIMUM_PROPERTY))
        {
          if (frame.isMaximum())
            maximizeFrame(frame);
          else
            minimizeFrame(frame);
        }
      else if (evt.getPropertyName().equals(JInternalFrame.IS_ICON_PROPERTY))
        {
          if (frame.isIcon())
            iconifyFrame(frame);
          else
            deiconifyFrame(frame);
        }
      else if (evt.getPropertyName().equals(JInternalFrame.IS_SELECTED_PROPERTY))
        {
          if (frame.isSelected())
            activateFrame(frame);
          else
            deactivateFrame(frame);
        }
      else if (evt.getPropertyName().equals(JInternalFrame.ROOT_PANE_PROPERTY)
               || evt.getPropertyName().equals(
                                               JInternalFrame.GLASS_PANE_PROPERTY))
        {
          Component old = (Component) evt.getOldValue();
          old.removeMouseListener(glassPaneDispatcher);
          old.removeMouseMotionListener(glassPaneDispatcher);

          Component newPane = (Component) evt.getNewValue();
          newPane.addMouseListener(glassPaneDispatcher);
          newPane.addMouseMotionListener(glassPaneDispatcher);

          frame.revalidate();
        }
      /*
       * FIXME: need to add ancestor properties to JComponents. else if
       * (evt.getPropertyName().equals(JComponent.ANCESTOR_PROPERTY)) { if
       * (desktopPane != null)
       * desktopPane.removeComponentListener(componentListener); desktopPane =
       * frame.getDesktopPane(); if (desktopPane != null)
       * desktopPane.addComponentListener(componentListener); }
       */
    }
  }

  /**
   * This helper class is the border for the JInternalFrame.
   */
  private class InternalFrameBorder extends AbstractBorder implements
      UIResource
  {
    /** The width of the border. */
    private static final int bSize = 5;

    /** The size of the corners. */
    private static final int offset = 10;

    /**
     * This method returns whether the border is opaque.
     * 
     * @return Whether the border is opaque.
     */
    public boolean isBorderOpaque()
    {
      return true;
    }

    /**
     * This method returns the insets of the border.
     * 
     * @param c
     *          The Component to find border insets for.
     * @return The border insets.
     */
    public Insets getBorderInsets(Component c)
    {
      return new Insets(bSize, bSize, bSize, bSize);
    }

    /**
     * This method paints the border.
     * 
     * @param c
     *          The Component that owns the border.
     * @param g
     *          The Graphics object to paint with.
     * @param x
     *          The x coordinate to paint at.
     * @param y
     *          The y coordinate to paint at.
     * @param width
     *          The width of the Component.
     * @param height
     *          The height of the Component.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int width,
                            int height)
    {
      g.translate(x, y);
      Color saved = g.getColor();
      Rectangle b = frame.getBounds();

      Color d = c.getBackground();
      g.setColor(d);
      g.fillRect(0, 0, bSize, b.height);
      g.fillRect(0, 0, b.width, bSize);
      g.fillRect(0, b.height - bSize, b.width, bSize);
      g.fillRect(b.width - bSize, 0, bSize, b.height);

      int x1 = 0;
      int x2 = bSize;
      int x3 = b.width - bSize;
      int x4 = b.width;

      int y1 = 0;
      int y2 = bSize;
      int y3 = b.height - bSize;
      int y4 = b.height;

      g.setColor(Color.GRAY);
      g.fillRect(0, 0, bSize, y4);
      g.fillRect(0, 0, x4, bSize);
      g.fillRect(0, y3, b.width, bSize);
      g.fillRect(x3, 0, bSize, b.height);

      g.fill3DRect(0, offset, bSize, b.height - 2 * offset, false);
      g.fill3DRect(offset, 0, b.width - 2 * offset, bSize, false);
      g.fill3DRect(offset, b.height - bSize, b.width - 2 * offset, bSize, false);
      g.fill3DRect(b.width - bSize, offset, bSize, b.height - 2 * offset, false);

      g.translate(-x, -y);
      g.setColor(saved);
    }
  }

  /**
   * The MouseListener that is responsible for dragging and resizing the
   * JInternalFrame in response to MouseEvents.
   */
  protected MouseInputAdapter borderListener;

  /**
   * The ComponentListener that is responsible for resizing the JInternalFrame
   * in response to ComponentEvents from the JDesktopPane.
   */
  protected ComponentListener componentListener;

  /**
   * The MouseListener that is responsible for activating the JInternalFrame
   * when the mouse press activates one of its descendents.
   */
  protected MouseInputListener glassPaneDispatcher;

  /**
   * The PropertyChangeListener that is responsible for listening to
   * PropertyChangeEvents from the JInternalFrame.
   */
  protected PropertyChangeListener propertyChangeListener;

  /**
   * The VetoableChangeListener.  Listens to PropertyChangeEvents
   * from the JInternalFrame and allows the JInternalFrame to 
   * veto attempts to close it.
   */
  private VetoableChangeListener internalFrameVetoableChangeListener;

  /** The InternalFrameListener that listens to the JInternalFrame. */
  private transient BasicInternalFrameListener internalFrameListener;

  /** The JComponent placed at the east region of the JInternalFrame. */
  protected JComponent eastPane;

  /** The JComponent placed at the north region of the JInternalFrame. */
  protected JComponent northPane;

  /** The JComponent placed at the south region of the JInternalFrame. */
  protected JComponent southPane;

  /** The JComponent placed at the west region of the JInternalFrame. */
  protected JComponent westPane;

  /**
   * The Keystroke bound to open the menu.
   * @deprecated
   */
  protected KeyStroke openMenuKey;

  /** The TitlePane displayed at the top of the JInternalFrame. */
  protected BasicInternalFrameTitlePane titlePane;

  /** The JInternalFrame this UI is responsible for. */
  protected JInternalFrame frame;

  /** The LayoutManager used in the JInternalFrame. */
  protected LayoutManager internalFrameLayout;

  /** The JDesktopPane that is the parent of the JInternalFrame. */
  private transient JDesktopPane desktopPane;

  /**
   * Creates a new BasicInternalFrameUI object.
   *
   * @param b The JInternalFrame this UI will represent.
   */
  public BasicInternalFrameUI(JInternalFrame b)
  {
    // Nothing to do here.
  }

  /**
   * This method will create a new BasicInternalFrameUI for the given
   * JComponent.
   *
   * @param b The JComponent to create a BasicInternalFrameUI for.
   *
   * @return A new BasicInternalFrameUI.
   */
  public static ComponentUI createUI(JComponent b)
  {
    return new BasicInternalFrameUI((JInternalFrame) b);
  }

  /**
   * This method installs a UI for the JInternalFrame.
   *
   * @param c The JComponent to install this UI on.
   */
  public void installUI(JComponent c)
  {
    if (c instanceof JInternalFrame)
      {
        frame = (JInternalFrame) c;

        ((JComponent) frame.getRootPane().getGlassPane()).setOpaque(false);
        frame.getRootPane().getGlassPane().setVisible(true);

        installDefaults();
        installListeners();
        installComponents();
        installKeyboardActions();

        frame.setOpaque(true);
        frame.invalidate();
      }
  }

  /**
   * This method reverses the work done by installUI.
   *
   * @param c The JComponent to uninstall this UI for.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallKeyboardActions();
    uninstallComponents();
    uninstallListeners();
    uninstallDefaults();

    ((JComponent) frame.getRootPane().getGlassPane()).setOpaque(true);
    frame.getRootPane().getGlassPane().setVisible(false);

    frame = null;
  }

  /**
   * This method installs the defaults specified by the look and feel.
   */
  protected void installDefaults()
    {
      internalFrameLayout = createLayoutManager();
      frame.setLayout(internalFrameLayout);
      LookAndFeel.installBorder(frame, "InternalFrame.border");
      frame.setFrameIcon(UIManager.getIcon("InternalFrame.icon"));
      // InternalFrames are invisible by default.
      frame.setVisible(false);
  }

  /**
   * This method installs the keyboard actions for the JInternalFrame.
   */
  protected void installKeyboardActions()
  {
    // FIXME: Implement.
  }

  /**
   * This method installs the Components for the JInternalFrame.
   */
  protected void installComponents()
  {
    setNorthPane(createNorthPane(frame));
    setSouthPane(createSouthPane(frame));
    setEastPane(createEastPane(frame));
    setWestPane(createWestPane(frame));
  }

  /**
   * This method installs the listeners for the JInternalFrame.
   */
  protected void installListeners()
  {
    glassPaneDispatcher = createGlassPaneDispatcher();
    createInternalFrameListener();
    borderListener = createBorderListener(frame);
    componentListener = createComponentListener();
    propertyChangeListener = createPropertyChangeListener();
    internalFrameVetoableChangeListener = new InternalFramePropertyChangeListener();

    frame.addMouseListener(borderListener);
    frame.addMouseMotionListener(borderListener);
    frame.addInternalFrameListener(internalFrameListener);
    frame.addPropertyChangeListener(propertyChangeListener);
    frame.addVetoableChangeListener(internalFrameVetoableChangeListener);
    frame.getRootPane().getGlassPane().addMouseListener(glassPaneDispatcher);
    frame.getRootPane().getGlassPane().addMouseMotionListener(glassPaneDispatcher);
  }

  /**
   * This method uninstalls the defaults for the JInternalFrame.
   */
  protected void uninstallDefaults()
  {
    frame.setBorder(null);
    frame.setLayout(null);
    internalFrameLayout = null;
  }

  /**
   * This method uninstalls the Components for the JInternalFrame.
   */
  protected void uninstallComponents()
  {
    setNorthPane(null);
    setSouthPane(null);
    setEastPane(null);
    setWestPane(null);
  }

  /**
   * This method uninstalls the listeners for the JInternalFrame.
   */
  protected void uninstallListeners()
  {
    if (desktopPane != null)
      desktopPane.removeComponentListener(componentListener);

    frame.getRootPane().getGlassPane().removeMouseMotionListener(glassPaneDispatcher);
    frame.getRootPane().getGlassPane().removeMouseListener(glassPaneDispatcher);

    frame.removePropertyChangeListener(propertyChangeListener);
    frame.removeInternalFrameListener(internalFrameListener);
    frame.removeMouseMotionListener(borderListener);
    frame.removeMouseListener(borderListener);

    propertyChangeListener = null;
    componentListener = null;
    borderListener = null;
    internalFrameListener = null;
    glassPaneDispatcher = null;
  }

  /**
   * This method uninstalls the keyboard actions for the JInternalFrame.
   */
  protected void uninstallKeyboardActions()
  {
    // FIXME: Implement.
  }

  /**
   * This method creates a new LayoutManager for the JInternalFrame.
   *
   * @return A new LayoutManager for the JInternalFrame.
   */
  protected LayoutManager createLayoutManager()
  {
    return new InternalFrameLayout();
  }

  /**
   * This method creates a new PropertyChangeListener for the JInternalFrame.
   *
   * @return A new PropertyChangeListener for the JInternalFrame.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new InternalFramePropertyChangeListener();
  }

  /**
   * This method returns the preferred size of the given JComponent.
   *
   * @param x The JComponent to find a preferred size for.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize(JComponent x)
  {
    Dimension pref = null;
    LayoutManager layout = frame.getLayout();
    if (frame == x && layout != null)
      pref = layout.preferredLayoutSize(frame);
    else
      pref = new Dimension(100, 100);
    return pref;
  }

  /**
   * This method returns the minimum size of the given JComponent.
   *
   * @param x The JComponent to find a minimum size for.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent x)
  {
    Dimension min = null;
    LayoutManager layout = frame.getLayout();
    if (frame == x && layout != null)
      min = layout.minimumLayoutSize(frame);
    else
      min = new Dimension(0, 0);
    return min;
  }

  /**
   * This method returns the maximum size of the given JComponent.
   *
   * @param x The JComponent to find a maximum size for.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize(JComponent x)
  {
    Dimension max = null;
    LayoutManager layout = frame.getLayout();
    if (frame == x && layout != null && layout instanceof LayoutManager2)
      max = ((LayoutManager2) layout).maximumLayoutSize(frame);
    else
      max = new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
    return max;
  }

  /**
   * This method replaces the currentPane with the newPane. When replacing it
   * also removes the MouseHandlers for the old pane and installs  them on
   * the new pane.
   *
   * @param currentPane The old pane to remove.
   * @param newPane The new pane to install.
   */
  protected void replacePane(JComponent currentPane, JComponent newPane)
  {
    if (currentPane != null)
      {
        deinstallMouseHandlers(currentPane);
        frame.remove(currentPane);
      }

    if (newPane != null)
      {
        installMouseHandlers(newPane);
        frame.add(newPane);
      }
  }

  /**
   * This method removes the necessary MouseListeners from the given
   * JComponent.
   *
   * @param c The JComponent to remove MouseListeners from.
   */
  protected void deinstallMouseHandlers(JComponent c)
  {
    c.removeMouseListener(borderListener);
    c.removeMouseMotionListener(borderListener);
  }

  /**
   * This method installs the necessary MouseListeners from the given
   * JComponent.
   *
   * @param c The JComponent to install MouseListeners on.
   */
  protected void installMouseHandlers(JComponent c)
  {
    c.addMouseListener(borderListener);
    c.addMouseMotionListener(borderListener);
  }

  /**
   * This method creates the north pane used in the JInternalFrame.
   *
   * @param w The JInternalFrame to create a north pane for.
   *
   * @return The north pane.
   */
  protected JComponent createNorthPane(JInternalFrame w)
  {
    titlePane = new BasicInternalFrameTitlePane(w);
    return titlePane;
  }

  /**
   * This method creates the west pane used in the JInternalFrame.
   *
   * @param w The JInternalFrame to create a west pane for.
   *
   * @return The west pane.
   */
  protected JComponent createWestPane(JInternalFrame w)
  {
    return null;
  }

  /**
   * This method creates the south pane used in the JInternalFrame.
   *
   * @param w The JInternalFrame to create a south pane for.
   *
   * @return The south pane.
   */
  protected JComponent createSouthPane(JInternalFrame w)
  {
    return null;
  }

  /**
   * This method creates the east pane used in the JInternalFrame.
   *
   * @param w The JInternalFrame to create an east pane for.
   *
   * @return The east pane.
   */
  protected JComponent createEastPane(JInternalFrame w)
  {
    return null;
  }

  /**
   * This method returns a new BorderListener for the given JInternalFrame.
   *
   * @param w The JIntenalFrame to create a BorderListener for.
   *
   * @return A new BorderListener.
   */
  protected MouseInputAdapter createBorderListener(JInternalFrame w)
  {
    return new BorderListener();
  }

  /**
   * This method creates a new InternalFrameListener for the JInternalFrame.
   */
  protected void createInternalFrameListener()
  {
    internalFrameListener = new BasicInternalFrameListener();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected final boolean isKeyBindingRegistered()
  {
    // FIXME: Implement.
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
   */
  protected final void setKeyBindingRegistered(boolean b)
  {
    // FIXME: Implement.
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public final boolean isKeyBindingActive()
  {
    // FIXME: Implement.
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
   */
  protected final void setKeyBindingActive(boolean b)
  {
    // FIXME: Implement.
  }

  /**
   * DOCUMENT ME!
   */
  protected void setupMenuOpenKey()
  {
    // FIXME: Implement.
  }

  /**
   * DOCUMENT ME!
   */
  protected void setupMenuCloseKey()
  {
    // FIXME: Implement.
  }

  /**
   * This method returns the north pane.
   *
   * @return The north pane.
   */
  public JComponent getNorthPane()
  {
    return northPane;
  }

  /**
   * This method sets the north pane to be the given JComponent.
   *
   * @param c The new north pane.
   */
  public void setNorthPane(JComponent c)
  {
    replacePane(northPane, c);
    northPane = c;
  }

  /**
   * This method returns the south pane.
   *
   * @return The south pane.
   */
  public JComponent getSouthPane()
  {
    return southPane;
  }

  /**
   * This method sets the south pane to be the given JComponent.
   *
   * @param c The new south pane.
   */
  public void setSouthPane(JComponent c)
  {
    replacePane(southPane, c);
    southPane = c;
  }

  /**
   * This method sets the east pane to be the given JComponent.
   *
   * @param c The new east pane.
   */
  public void setEastPane(JComponent c)
  {
    replacePane(eastPane, c);
    eastPane = c;
  }

  /**
   * This method returns the east pane.
   *
   * @return The east pane.
   */
  public JComponent getEastPane()
  {
    return eastPane;
  }

  /**
   * This method sets the west pane to be the given JComponent.
   *
   * @param c The new west pane.
   */
  public void setWestPane(JComponent c)
  {
    replacePane(westPane, c);
    westPane = c;
  }

  /**
   * This method returns the west pane.
   *
   * @return The west pane.
   */
  public JComponent getWestPane()
  {
    return westPane;
  }

  /**
   * This method returns the DesktopManager to use with the JInternalFrame.
   *
   * @return The DesktopManager to use with the JInternalFrame.
   */
  protected DesktopManager getDesktopManager()
  {
    DesktopManager value = null;
    JDesktopPane pane = frame.getDesktopPane();
    if (pane != null)
      value = frame.getDesktopPane().getDesktopManager();
    if (value == null)
      value = createDesktopManager();
    return value;
  }

  /**
   * This method returns a default DesktopManager that can be used with this
   * JInternalFrame.
   *
   * @return A default DesktopManager that can be used with this
   *         JInternalFrame.
   */
  protected DesktopManager createDesktopManager()
  {
    return new DefaultDesktopManager();
  }

  /**
   * This is a convenience method that closes the JInternalFrame.
   *
   * @param f The JInternalFrame to close.
   */
  protected void closeFrame(JInternalFrame f)
  {
    getDesktopManager().closeFrame(f);
  }

  /**
   * This is a convenience method that maximizes the JInternalFrame.
   *
   * @param f The JInternalFrame to maximize.
   */
  protected void maximizeFrame(JInternalFrame f)
  {
    getDesktopManager().maximizeFrame(f);
  }

  /**
   * This is a convenience method that minimizes the JInternalFrame.
   *
   * @param f The JInternalFrame to minimize.
   */
  protected void minimizeFrame(JInternalFrame f)
  {
    getDesktopManager().minimizeFrame(f);
  }

  /**
   * This is a convenience method that iconifies the JInternalFrame.
   *
   * @param f The JInternalFrame to iconify.
   */
  protected void iconifyFrame(JInternalFrame f)
  {
    getDesktopManager().iconifyFrame(f);
  }

  /**
   * This is a convenience method that deiconifies the JInternalFrame.
   *
   * @param f The JInternalFrame to deiconify.
   */
  protected void deiconifyFrame(JInternalFrame f)
  {
    getDesktopManager().deiconifyFrame(f);
  }

  /**
   * This is a convenience method that activates the JInternalFrame.
   *
   * @param f The JInternalFrame to activate.
   */
  protected void activateFrame(JInternalFrame f)
  {
    getDesktopManager().activateFrame(f);
  }

  /**
   * This is a convenience method that deactivates the JInternalFrame.
   *
   * @param f the JInternalFrame to deactivate
   */
  protected void deactivateFrame(JInternalFrame f)
  {
    getDesktopManager().deactivateFrame(f);
  }

  /**
   * This method returns a new ComponentListener for the JDesktopPane.
   *
   * @return A new ComponentListener.
   */
  protected ComponentListener createComponentListener()
  {
    return new ComponentHandler();
  }

  /**
   * This method returns a new GlassPaneDispatcher.
   *
   * @return A new GlassPaneDispatcher.
   */
  protected MouseInputListener createGlassPaneDispatcher()
  {
    return new GlassPaneDispatcher();
  }
}
