/* BasicInternalFrameUI.java --
   Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

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
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
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
import javax.swing.plaf.ActionMapUIResource;
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
      frame.getGlassPane().setVisible(false);
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
      frame.getGlassPane().setVisible(true);
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
    /**
     * The current shape of the cursor. 
     */
    transient int showingCursor;
    
    /** FIXME: Use for something. */
    protected final int RESIZE_NONE = 0;

    /** The x offset from the top left corner of the JInternalFrame. */
    private transient int xOffset;

    /** The y offset from the top left corner of the JInternalFrame. */
    private transient int yOffset;

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
      // Do minimization/maximization when double-clicking in the title pane.
      if (e.getSource() == titlePane && e.getClickCount() == 2)
        try
          {
            if (frame.isMaximizable() && ! frame.isMaximum())
              frame.setMaximum(true);
            else if (frame.isMaximum())
              frame.setMaximum(false);
          }
        catch (PropertyVetoException pve)
          {
            // We do nothing if the attempt has been vetoed.
          }
        
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
            case Cursor.N_RESIZE_CURSOR:
              cacheRect.setBounds(b.x, Math.min(b.y + y, b.y + b.height
                                                         - min.height),
                                  b.width, b.height - y);
              break;
            case Cursor.NE_RESIZE_CURSOR:
              cacheRect.setBounds(b.x, Math.min(b.y + y, b.y + b.height
                                                         - min.height), x + 1,
                                  b.height - y);
              break;
            case Cursor.E_RESIZE_CURSOR:
              cacheRect.setBounds(b.x, b.y, x + 1, b.height);
              break;
            case Cursor.SE_RESIZE_CURSOR:
              cacheRect.setBounds(b.x, b.y, x + 1, y + 1);
              break;
            case Cursor.S_RESIZE_CURSOR:
              cacheRect.setBounds(b.x, b.y, b.width, y + 1);
              break;
            case Cursor.SW_RESIZE_CURSOR:
              cacheRect.setBounds(Math.min(b.x + x, b.x + b.width - min.width),
                                  b.y, b.width - x, y + 1);
              break;
            case Cursor.W_RESIZE_CURSOR:
              cacheRect.setBounds(Math.min(b.x + x, b.x + b.width - min.width),
                                  b.y, b.width - x, b.height);
              break;
            case Cursor.NW_RESIZE_CURSOR:
              cacheRect.setBounds(
                                  Math.min(b.x + x, b.x + b.width - min.width),
                                  Math.min(b.y + y, b.y + b.height - min.height),
                                  b.width - x, b.height - y);
              break;
            }
          dm.resizeFrame(frame, cacheRect.x, cacheRect.y,
                         Math.max(min.width, cacheRect.width),
                         Math.max(min.height, cacheRect.height));
          setCursor(e);
        }
      else if (e.getSource() == titlePane)
        {
          Rectangle fBounds = frame.getBounds();
          frame.putClientProperty("bufferedDragging", Boolean.TRUE);
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
      if (showingCursor != Cursor.DEFAULT_CURSOR)
        {
          frame.setCursor(Cursor.getDefaultCursor());
          showingCursor = Cursor.DEFAULT_CURSOR;
        }
    }

    /**
     * This method is called when the mouse is moved inside the JInternalFrame.
     * 
     * @param e The MouseEvent.
     */
    public void mouseMoved(MouseEvent e)
    {
      // Turn off the resize cursor if we are in the frame header.
      if (showingCursor != Cursor.DEFAULT_CURSOR && e.getSource() != frame)
        {
          frame.setCursor(Cursor.getDefaultCursor());
          showingCursor = Cursor.DEFAULT_CURSOR;
        }
      else if (e.getSource() == frame && frame.isResizable())
        {
          setCursor(e);
        }
    }
    
    /**
     * Set the mouse cursor, how applicable.
     * 
     * @param e the current mouse event.
     */
    void setCursor(MouseEvent e)
    {
      int cursor = sectionOfClick(e.getX(), e.getY());
      if (cursor != showingCursor)
        {
          Cursor resize = Cursor.getPredefinedCursor(cursor);
          frame.setCursor(resize);
          showingCursor = cursor;
        }
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
        {
          dm.endDraggingFrame(frame);
          frame.putClientProperty("bufferedDragging", null);
        }
      
      setCursor(e);
    }

    /**
     * This method determines the direction of the resize based on the
     * coordinates and the size of the JInternalFrame.
     *
     * @param x The x coordinate of the MouseEvent.
     * @param y The y coordinate of the MouseEvent.
     *
     * @return The cursor constant, determining the resizing direction.
     */
    private int sectionOfClick(int x, int y)
    {
      Rectangle b = frame.getBounds();
      int corner = InternalFrameBorder.cornerSize;
      
      if (x < corner && y < corner)
        return Cursor.NW_RESIZE_CURSOR;
      else if (x > b.width - corner && y < corner)
        return Cursor.NE_RESIZE_CURSOR;
      else if (x > b.width - corner && y > b.height - corner)
        return Cursor.SE_RESIZE_CURSOR;
      else if (x < corner && y > b.height - corner)
        return Cursor.SW_RESIZE_CURSOR;
      else if (y < corner)
        return Cursor.N_RESIZE_CURSOR;
      else if (x < corner)
        return Cursor.W_RESIZE_CURSOR;
      else if (y > b.height - corner)
        return Cursor.S_RESIZE_CURSOR;
      else if (x > b.width - corner)
        return Cursor.E_RESIZE_CURSOR;

      return Cursor.DEFAULT_CURSOR;
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
          Container parent = frame.getParent();
          Insets i = parent.getInsets();
          int width = parent.getWidth() - i.left - i.right;
          int height = parent.getHeight() - i.top - i.bottom;
          frame.setBounds(0, 0, width, height);
        }
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

    private Component dragTarget;

    /**
     * Indicates if we are currently in a dragging operation or not.
     */
    private boolean isDragging;

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
      // Experiments show that this seems to call the
      // borderListener.mousePressed() method to activate the frame.
      if (borderListener != null)
        borderListener.mousePressed(e);
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
     * This is a helper method that dispatches the GlassPane MouseEvents to the
     * proper component.
     * 
     * @param e the mouse event to be dispatched
     */
    private void handleEvent(MouseEvent e)
    {
      // Find candidate component inside the JInternalFrame.
      Component target = frame.getLayeredPane().findComponentAt(e.getX(),
                                                                e.getY());

      // Now search upwards to find a component that actually has
      // a MouseListener attached.
      while (target != null
             && target.getMouseListeners().length == 0
             && target.getMouseMotionListeners().length == 0
             && target.getMouseWheelListeners().length == 0)
        {
          target = target.getParent();
        }

      if (target != null)
        {
          int id = e.getID();
          switch (id)
          {
            case MouseEvent.MOUSE_ENTERED:
              // Now redispatch the thing.
              if (! isDragging || frame.isSelected())
                {
                  mouseEventTarget = target;
                  redispatch(id, e, mouseEventTarget);
                }
              break;
            case MouseEvent.MOUSE_EXITED:
              if (! isDragging || frame.isSelected())
                {
                  redispatch(id, e, mouseEventTarget);
                }
              break;
            case MouseEvent.MOUSE_PRESSED:
              mouseEventTarget = target;
              redispatch(id, e, mouseEventTarget);
              // Start dragging.
              dragTarget = target;
              break;
            case MouseEvent.MOUSE_RELEASED:
              if (isDragging)
                {
                  redispatch(id, e, dragTarget);
                  isDragging = false;
                }
              else
                redispatch(id, e, mouseEventTarget);
              break;
            case MouseEvent.MOUSE_CLICKED:
              redispatch(id, e, mouseEventTarget);
              break;
            case MouseEvent.MOUSE_MOVED:
              if (target != mouseEventTarget)
                {
                  // Create additional MOUSE_EXITED/MOUSE_ENTERED pairs.
                  redispatch(MouseEvent.MOUSE_EXITED, e, mouseEventTarget);
                  mouseEventTarget = target;
                  redispatch(MouseEvent.MOUSE_ENTERED, e, mouseEventTarget);
                }
              redispatch(id, e, mouseEventTarget);
              break;
            case MouseEvent.MOUSE_DRAGGED:
              if (! isDragging)
                isDragging = true;
              redispatch(id, e, mouseEventTarget);
              break;
            case MouseEvent.MOUSE_WHEEL:
              redispatch(id, e, mouseEventTarget);
              break;
            default:
              assert false : "Must not reach here";
          }
        }
    }

    /**
     * Redispatches the event to the real target with the specified id.
     *
     * @param id the new event ID
     * @param e the original event
     * @param target the real event target
     */
    private void redispatch(int id, MouseEvent e, Component target)
    {
      Point p = SwingUtilities.convertPoint(frame.getLayeredPane(), e.getX(),
                                            e.getY(), target);
      MouseEvent ev = new MouseEvent(target, id, e.getWhen(),
                                     e.getModifiers() | e.getModifiersEx(),
                                     p.x, p.y, e.getClickCount(),
                                     e.isPopupTrigger());
      target.dispatchEvent(ev);
    }
  }

  /**
   * This helper class listens for PropertyChangeEvents from the
   * JInternalFrame.
   */
  public class InternalFramePropertyChangeListener
    implements PropertyChangeListener
  {

    /**
     * This method is called when one of the JInternalFrame's properties change.
     * 
     * @param evt
     *          The PropertyChangeEvent.
     */
    public void propertyChange(PropertyChangeEvent evt)
    {
      String property = evt.getPropertyName();
      if (property.equals(JInternalFrame.IS_MAXIMUM_PROPERTY))
        {
          if (frame.isMaximum())
            maximizeFrame(frame);
          else
            minimizeFrame(frame);
        }
      else if (property.equals(JInternalFrame.IS_ICON_PROPERTY))
        {
          if (frame.isIcon())
            iconifyFrame(frame);
          else
            deiconifyFrame(frame);
        }
      else if (property.equals(JInternalFrame.IS_SELECTED_PROPERTY))
        {
          Component glassPane = frame.getGlassPane();
          if (frame.isSelected())
            {
              activateFrame(frame);
              glassPane.setVisible(false);
            }
          else
            {
              deactivateFrame(frame);
              glassPane.setVisible(true);
            }
        }
      else if (property.equals(JInternalFrame.ROOT_PANE_PROPERTY)
               || property.equals(JInternalFrame.GLASS_PANE_PROPERTY))
        {
          Component old = (Component) evt.getOldValue();
          if (old != null)
            {
              old.removeMouseListener(glassPaneDispatcher);
              old.removeMouseMotionListener(glassPaneDispatcher);
            }

          Component newPane = (Component) evt.getNewValue();
          if (newPane != null)
            {
              newPane.addMouseListener(glassPaneDispatcher);
              newPane.addMouseMotionListener(glassPaneDispatcher);
            }

          frame.revalidate();
        }
      else if (property.equals(JInternalFrame.IS_CLOSED_PROPERTY))
        {
          if (evt.getNewValue() == Boolean.TRUE)
            {
              Container parent = frame.getParent();
              if (parent != null)
                parent.removeComponentListener(componentListener);
              closeFrame(frame);
            }
        }
      else if (property.equals("ancestor"))
        {
          Container newParent = (Container) evt.getNewValue();
          Container oldParent = (Container) evt.getOldValue();
          if (newParent != null)
            {
              newParent.addComponentListener(componentListener);
            }
          else if (oldParent != null)
            {
              oldParent.removeComponentListener(componentListener);
            }
        }
    }
  }

  /**
   * This helper class is the border for the JInternalFrame.
   */
  class InternalFrameBorder extends AbstractBorder implements
      UIResource
  {
    /** 
     * The width of the border. 
     */
    static final int bSize = 5;

    /**
     * The size of the corners (also used by the mouse listener).
     */
    static final int cornerSize = 10;

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

      g.fill3DRect(0, cornerSize, bSize, b.height - 2 * cornerSize, false);
      g.fill3DRect(cornerSize, 0, b.width - 2 * cornerSize, bSize, false);
      g.fill3DRect(cornerSize, b.height - bSize, b.width - 2 * cornerSize, 
                   bSize, false);
      g.fill3DRect(b.width - bSize, cornerSize, bSize, 
                   b.height - 2 * cornerSize, false);

      g.translate(-x, -y);
      g.setColor(saved);
    }
  }

  /**
   * This action triggers the system menu.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class ShowSystemMenuAction
    extends AbstractAction
  {
    public void actionPerformed(ActionEvent e)
    {
      if (titlePane != null)
        {
          titlePane.showSystemMenu();
        }
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

        installDefaults();
        installListeners();
        installComponents();
        installKeyboardActions();

        if (! frame.isSelected())
          frame.getGlassPane().setVisible(true);
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

      // Let the content pane inherit the background color from its
      // frame by setting the background to null.
      Component contentPane = frame.getContentPane();
      if (contentPane != null
          && contentPane.getBackground() instanceof UIResource)
        {
          contentPane.setBackground(null);
        }
  }

  /**
   * This method installs the keyboard actions for the JInternalFrame.
   */
  protected void installKeyboardActions()
  {
    ActionMapUIResource am = new ActionMapUIResource();
    am.put("showSystemMenu", new ShowSystemMenuAction());

    // The RI impl installs the audio actions as parent of the UI action map,
    // so do we.
    BasicLookAndFeel blaf = (BasicLookAndFeel) UIManager.getLookAndFeel();
    ActionMap audioActionMap = blaf.getAudioActionMap();
    am.setParent(audioActionMap);

    SwingUtilities.replaceUIActionMap(frame, am);
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

    frame.addMouseListener(borderListener);
    frame.addMouseMotionListener(borderListener);
    frame.addInternalFrameListener(internalFrameListener);
    frame.addPropertyChangeListener(propertyChangeListener);
    frame.getRootPane().getGlassPane().addMouseListener(glassPaneDispatcher);
    frame.getRootPane().getGlassPane().addMouseMotionListener(glassPaneDispatcher);

    Container parent = frame.getParent();
    if (parent != null)
      {
        parent.addComponentListener(componentListener);
      }
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

    Container parent = frame.getParent();
    if (parent != null)
      {
        parent.removeComponentListener(componentListener);
      }
    componentListener = null;

    frame.getRootPane().getGlassPane().removeMouseMotionListener(glassPaneDispatcher);
    frame.getRootPane().getGlassPane().removeMouseListener(glassPaneDispatcher);

    frame.removePropertyChangeListener(propertyChangeListener);
    frame.removeInternalFrameListener(internalFrameListener);
    frame.removeMouseMotionListener(borderListener);
    frame.removeMouseListener(borderListener);

    propertyChangeListener = null;

    borderListener = null;
    internalFrameListener = null;
    glassPaneDispatcher = null;
  }

  /**
   * This method uninstalls the keyboard actions for the JInternalFrame.
   */
  protected void uninstallKeyboardActions()
  {
    SwingUtilities.replaceUIActionMap(frame, null);
    SwingUtilities.replaceUIInputMap(frame, JComponent.WHEN_IN_FOCUSED_WINDOW,
                                     null);
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
    // the following is needed to make internal frames draggable when using
    // the JGoodies PlasticLookAndFeel, because it overrides the 
    // createNorthPane() method and doesn't assign anything to the titlePane
    // field.  It is possible there is another way to make this work, but
    // I didn't find it...
    if (c instanceof BasicInternalFrameTitlePane)
      titlePane = (BasicInternalFrameTitlePane) c;
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
