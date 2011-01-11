/* SwingContainerPeer.java -- A Swing based peer for AWT containers
   Copyright (C)  2006, 2007  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.swing;

import gnu.classpath.SystemProperties;

import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * A peer for Container to be used with the Swing based AWT peers.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingContainerPeer
  extends SwingComponentPeer
  implements ContainerPeer
{

  /**
   * Stores all heavyweight descendents of the container. This is used
   * in {@link #peerPaintChildren(Graphics)}.
   */
  private LinkedList heavyweightDescendents;

  /**
   * The backbuffer used for painting UPDATE events.
   */
  private Image backbuffer;

  /**
   * Creates a new SwingContainerPeer.
   *
   * @param awtCont
   */
  public SwingContainerPeer(Container awtCont)
  {
    heavyweightDescendents = new LinkedList();
  }

  /**
   * Registers a heavyweight descendent. This is then painted by
   * {@link #peerPaintChildren(Graphics)}.
   *
   * @param comp the descendent to register
   *
   * @see #peerPaintChildren(Graphics)
   * @see #removeHeavyweightDescendent(Component)
   */
  protected synchronized void addHeavyweightDescendent(Component comp)
  {
    heavyweightDescendents.add(comp);
    focusOwner = null;
  }

  /**
   * Unregisters a heavyweight descendent.
   *
   * @param comp the descendent to unregister
   *
   * @see #peerPaintChildren(Graphics)
   * @see #addHeavyweightDescendent(Component)
   */
  protected synchronized void removeHeavyweightDescendent(Component comp)
  {
    heavyweightDescendents.remove(comp);
    focusOwner = null;
  }

  /**
   * Returns an array of all registered heavyweight descendents.
   *
   * @return all registered heavyweight descendents
   */
  protected Component[] getHeavyweightDescendents()
  {
    Component[] heavyweights = new Component[heavyweightDescendents.size()];
    heavyweights = (Component[]) heavyweightDescendents.toArray(heavyweights);
    return heavyweights;
  }

  /**
   * Returns the insets of the container.
   *
   * This is implemented to return the insets of the Swing container.
   *
   * @return the insets of the container
   */
  public Insets insets()
  {
    Insets retVal;
    if (swingComponent != null)
      retVal = swingComponent.getJComponent().getInsets();
    else
      retVal = new Insets(0, 0, 0, 0);
    return retVal;
  }

  /**
   * Returns the insets of the container.
   *
   * This is implemented to return the insets of the Swing container.
   *
   * @return the insets of the container
   */
  public Insets getInsets()
  {
    return insets();
  }

  /**
   * Called before the validation of this containers begins.
   */
  public void beginValidate()
  {
    // Nothing to do here.
  }

  /**
   * Called after the validation of this containers ended.
   */
  public void endValidate()
  {
    // Nothing to do here.
  }

  /**
   * Called before the layout of this containers begins.
   */
  public void beginLayout()
  {
    // Nothing to do here.
  }

  /**
   * Called after the layout of this containers ended.
   */
  public void endLayout()
  {
    // Nothing to do here.
  }

  /**
   * Returns <code>false</code> unconditionally. This method is not used at
   * the moment.
   *
   * @return <code>false</code>
   */
  public boolean isPaintPending()
  {
    return false;
  }

  /**
   * Returns <code>false</code> unconditionally. This method is not used at
   * the moment.
   *
   * @return <code>false</code>
   */
  public boolean isRestackSupported()
  {
    return false;
  }

  /**
   * This method is not used at the moment.
   */
  public void cancelPendingPaint(int x, int y, int width, int height)
  {
    // Nothing to do here.
  }

  /**
   * This method is not used at the moment.
   */
  public void restack()
  {
    // Nothing to do here.
  }

  /**
   * Performs the super behaviour (call peerPaintComponent() and
   * awtComponent.paint()), and forwards the paint request to the heavyweight
   * descendents of the container.
   */
  protected void peerPaint(Graphics g, boolean update)
  {
    if (isDoubleBuffering())
      {
        int width = awtComponent.getWidth();
        int height = awtComponent.getHeight();
        if (backbuffer == null
            || backbuffer.getWidth(awtComponent) < width
            || backbuffer.getHeight(awtComponent) < height)
          backbuffer = awtComponent.createImage(width, height);
        Graphics g2 = backbuffer.getGraphics();
        Rectangle clip = g.getClipRect();
        try
          {
            g2.setClip(clip);
            super.peerPaint(g2, update);
            peerPaintChildren(g2);
          }
        finally
          {
            g2.dispose();
          }
        g.drawImage(backbuffer, 0, 0, awtComponent);
      }
    else
      {
        super.peerPaint(g, update);
        peerPaintChildren(g);
      }
  }

  /**
   * Determines if we should do double buffering or not.
   *
   * @return if we should do double buffering or not
   */
  private boolean isDoubleBuffering()
  {
    Object prop =
      SystemProperties.getProperty("gnu.awt.swing.doublebuffering", "false");
    return prop.equals("true");
  }

  /**
   * Paints any heavyweight child components.
   *
   * @param g the graphics to use for painting
   */
  protected synchronized void peerPaintChildren(Graphics g)
  {
    // TODO: Is this the right painting order?
    for (Iterator i = heavyweightDescendents.iterator(); i.hasNext();)
      {
        Component child = (Component) i.next();
        ComponentPeer peer = child.getPeer();

        if (peer instanceof SwingComponentPeer && child.isVisible())
          {
            // TODO: The translation here doesn't work for deeper
            // nested children. Fix this!
            Graphics g2 = g.create(child.getX(), child.getY(),
                                   child.getWidth(), child.getHeight());
            try
              {
                // update() is only called for the topmost component if
                // necessary, all other components only get paint() called.
                ((SwingComponentPeer) peer).peerPaint(g2, false);
              }
            finally
              {
                g2.dispose();
              }
          }
      }
  }

  /**
   * Handles mouse events by dispatching it to the correct component.
   *
   * @param ev the mouse event
   */
  protected void handleMouseEvent(MouseEvent ev)
  {
    Component comp = awtComponent.getComponentAt(ev.getPoint());
    if(comp == null) comp = awtComponent;
    ComponentPeer peer = comp.getPeer();
    if (awtComponent != comp && !comp.isLightweight() && peer instanceof SwingComponentPeer)
      {
        ev.translatePoint(comp.getX(), comp.getY());
        ev.setSource(comp);
        ((SwingComponentPeer) peer).handleMouseEvent(ev);
      }
  }

  /**
   * Handles mouse events by dispatching it to the correct component.
   *
   * @param ev the mouse event
   */
  protected void handleMouseMotionEvent(MouseEvent ev)
  {
    Component comp = awtComponent.getComponentAt(ev.getPoint());
    if (comp != null)
      {
        ComponentPeer peer = comp.getPeer();
        if (awtComponent != comp && !comp.isLightweight() && peer instanceof SwingComponentPeer)
          {
            ev.translatePoint(comp.getX(), comp.getY());
            ((SwingComponentPeer) peer).handleMouseMotionEvent(ev);
          }
      }
  }

  /**
   * Handles key events on the component. This is usually forwarded to the
   * SwingComponent's processKeyEvent() method.
   *
   * @param e the key event
   */
  protected void handleKeyEvent(KeyEvent e)
  {
    Component owner = getFocusOwner();
    if(owner != null)
      owner.getPeer().handleEvent(e);
    else
      super.handleKeyEvent(e);
  }

  private Component focusOwner = null;

  private Component getFocusOwner()
  {
      if(focusOwner == null)
      {
        for(Iterator iter=heavyweightDescendents.iterator(); iter.hasNext();)
        {
           Component child = (Component) iter.next();
          if(child.isFocusable())
          {
              focusOwner = child;
            break;
          }
      }
      }
      return focusOwner;
  }

}
