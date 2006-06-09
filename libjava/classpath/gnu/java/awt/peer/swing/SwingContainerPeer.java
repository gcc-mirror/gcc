/* SwingContainerPeer.java -- A Swing based peer for AWT containers
   Copyright (C)  2006  Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;

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
   * Creates a new SwingContainerPeer.
   *
   * @param awtCont
   */
  public SwingContainerPeer(Component awtCont)
  {
    init(awtCont, null);
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
    Insets retVal;
    if (swingComponent != null)
      retVal = swingComponent.getJComponent().getInsets();
    else
      retVal = new Insets(0, 0, 0, 0);
    return retVal;
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
   * Triggers painting of a component. This calls peerPaint on all the child
   * components of this container.
   *
   * @param g the graphics context to paint to
   */
  protected void peerPaint(Graphics g)
  {
    Container c = (Container) awtComponent;
    Component[] children = c.getComponents();
    for (int i = children.length - 1; i >= 0; --i)
      {
        Component child = children[i];
        ComponentPeer peer = child.getPeer();
        boolean translated = false;
        boolean clipped = false;
        Shape oldClip = g.getClip();
        try
        {
          g.translate(child.getX(), child.getY());
          translated = true;
          g.setClip(0, 0, child.getWidth(), child.getHeight());
          clipped = true;
          if (peer instanceof SwingComponentPeer)
            ((SwingComponentPeer) peer).peerPaint(g);
        }
        finally
        {
          if (translated)
            g.translate(- child.getX(), - child.getY());
          if (clipped)
            g.setClip(oldClip);
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
    if(comp == null)
      comp = awtComponent;
    if (comp != null)
      {
        ComponentPeer peer = comp.getPeer();
        if (awtComponent != comp && !comp.isLightweight() && peer instanceof SwingComponentPeer)
          {
            ev.translatePoint(comp.getX(), comp.getY());
            ev.setSource(comp);
            ((SwingComponentPeer) peer).handleMouseEvent(ev);
          }
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
}
