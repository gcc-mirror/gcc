/* QtRepaintThread.java -- Repaint thread implementation
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

/**
 * This class does repainting of Component back-buffers. It is undesirable to
 * do this directly from the paint callback in QtComponentPeer, because that
 * is executed from the main thread. Thus, if a call is made at the same time
 * which requires execution by the main thread, and this is sharing a lock with
 * paint(), then a deadlock will occur, which must be avoided. In general,
 * the main Qt thread should avoid calling into java code as far as possible.
 *
 */
public class QtRepaintThread extends Thread
{
  static class RepaintComponent
  {
    public QtComponentPeer curr;
    public RepaintComponent next;
    public boolean paintAll;
    public int x, y, w, h;

    public RepaintComponent(QtComponentPeer p)
    {
      curr = p;
      next = null;
      paintAll = true;
    }

    public RepaintComponent(QtComponentPeer p, int x, int y, int w, int h)
    {
      this(p);
      paintAll = false;
      this.x = x;
      this.y = y;
      this.w = w;
      this.h = h;
    }
  }

  RepaintComponent component;
  boolean busy;

  public QtRepaintThread()
  {
    component = null;
  }

  public void run()
  {
    while( true )
      {
        try
          {
            busy = false;
            // Wait for a repaint
            sleep(100);
            busy = true;
          }
        catch (InterruptedException ie)
          {
            while( component != null )
              {
                try
                  {
                    if( component.paintAll )
                      {
                        // update the back-buffer.
                        component.curr.paintBackBuffer();
                        component.curr.QtUpdate(); // trigger a native repaint event
                      }
                    else
                      {
                        component.curr.paintBackBuffer(component.x, component.y,
                                                       component.w, component.h);
                        component.curr.QtUpdateArea(component.x, component.y,
                                                    component.w, component.h);
                      }
                  }
                catch (InterruptedException e)
                  {
                  }
                component = component.next;
              }
          }
      }
  }

  /**
   * Enqueue a component for repainting.
   */
  public synchronized void queueComponent(QtComponentPeer p)
  {
    if( component == null )
      component = new RepaintComponent(p);
    else
      {
        RepaintComponent r = component;
        while( r.next != null ) r = r.next;
        r.next = new RepaintComponent(p);
      }
    interrupt();
  }

  /**
   * Enqueue a component for repainting.
   */
  public synchronized void queueComponent(QtComponentPeer p, int x, int y,
                                          int w, int h)
  {
    if( component == null )
      component = new RepaintComponent(p, x, y, w, h);
    else
      {
        RepaintComponent r = component;
        while( r.next != null ) r = r.next;
        r.next = new RepaintComponent(p, x, y, w, h);
      }
    interrupt();
  }
}
