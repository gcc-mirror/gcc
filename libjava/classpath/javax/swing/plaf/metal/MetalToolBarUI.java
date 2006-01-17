/* MetalToolBarUI.java
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

import java.awt.Point;
import java.awt.event.ContainerListener;
import java.awt.event.MouseEvent;

import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JToolBar;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicToolBarUI;

/**
 * A UI delegate for the {@link JToolBar} component.
 */
public class MetalToolBarUI extends BasicToolBarUI
{
  
  /**
   * A listener (no longer used) that responds when components are added to or 
   * removed from the {@link JToolBar}.  The required behaviour is now
   * handled in the super class. 
   * 
   * @see MetalToolBarUI#createContainerListener()
   */
  protected class MetalContainerListener
    extends BasicToolBarUI.ToolBarContListener
  {
    /**
     * Creates a new instance.
     */
    protected MetalContainerListener()
    {
      // Nothing to do here.
    }
  }

  /**
   * A listener (no longer used) that responds to property change events in a
   * {@link JToolBar} component.  The required behaviour is now handled in the 
   * super class. 
   * 
   * @see MetalToolBarUI#createRolloverListener()
   */
  protected class MetalRolloverListener
    extends BasicToolBarUI.PropertyListener
  {
    /**
     * Creates a new instance.
     */
    protected MetalRolloverListener()
    {
      // Nothing to do here.
    }
  }
  
  /** 
   * The container listener (an implementation specific field, according to the
   * spec, and not used in GNU Classpath).
   */
  protected ContainerListener contListener;
  
  /** 
   * The rollover listener (an implementation specific field, according to the
   * spec, and not used in GNU Classpath). 
   */
  protected PropertyChangeListener rolloverListener;

  /**
   * Creates a new instance of this UI delegate.
   */
  public MetalToolBarUI()
  {
    super();
  }

  /**
   * Returns a new instance of <code>MetalToolBarUI</code>.
   *
   * @param component  the component for which we return an UI instance
   *
   * @return A new instance of <code>MetalToolBarUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalToolBarUI();
  }
  
  /**
   * Returns <code>null</code> as permitted by recent versions of the API
   * specification.  Originally it seems this method returned a new instance of 
   * {@link MetalRolloverListener}, but this is now redundant.
   * 
   * @return <code>null</code>.
   */
  protected PropertyChangeListener createRolloverListener()
  {
    return null;
  }
  
  /**
   * Returns <code>null</code> as permitted by recent versions of the API
   * specification.  Originally it seems this method returned a new instance of 
   * {@link MetalContainerListener}, but this is now redundant.
   * 
   * @return <code>null</code>.
   */
  protected ContainerListener createContainerListener()
  {
    return null;
  }
  
  /**
   * Returns a border with no rollover effect for buttons in the tool bar.
   * 
   * @return A border.
   * 
   * @see MetalBorders#getToolbarButtonBorder()
   */
  protected Border createNonRolloverBorder()
  {
    return MetalBorders.getToolbarButtonBorder();   
  }
  
  /**
   * Sets the offset for the window used for dragging the toolbar.
   * It is set as long as the window is not null (it has been installed).
   */
  protected void setDragOffset(Point p)
  {
    if (dragWindow != null)
      dragWindow.setOffset(p);
  }
  
  /** 
   * Creates and returns an instance of MetalDockingListener.
   * 
   * @return an instance of MetalDockingListener.
   */
  protected MouseInputListener createDockingListener()
  {
    return new MetalDockingListener(toolBar);
  }
  
  /**
   * This is the MouseHandler class that allows the user to drag the JToolBar
   * in and out of the parent and dock it if it can.
   */
  protected class MetalDockingListener extends BasicToolBarUI.DockingListener
  {    
    /**
     * Creates a new DockingListener object.
     *
     * @param t The JToolBar this DockingListener is being used for.
     */
    public MetalDockingListener(JToolBar t)
    {
      super(t);
    }
    
    /**
     * This method is called when the mouse is pressed in the JToolBar. If the
     * press doesn't occur in a place where it causes the JToolBar to be
     * dragged, it returns. Otherwise, it starts a drag session.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      super.mousePressed(e);
      setDragOffset(new Point(e.getX(), e.getY()));
    }
    
    /**
     * This method is called when the mouse is dragged. It delegates the drag
     * painting to the dragTo method.
     *
     * @param e The MouseEvent.
     */
    public void mouseDragged(MouseEvent e)
    {
      // Does not do anything differently than dragging 
      // BasicToolBarUI.DockingListener
      super.mouseDragged(e);
    }
  }
}
