/* EventDispatcher.java -- Dispatch events for prefs
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.util.prefs;

import java.util.ArrayList;

/**
 * This is a helper class used for dispatching events for
 * the prefs package.
 */
public class EventDispatcher extends Thread
{
  // This is a singleton class.  We dispatch all events via a
  // new Thread which is created on demand.
  private static final Thread dispatchThread = new EventDispatcher();

  // This is a queue of events to dispatch.  This thread waits on
  // the queue and when notified will remove events until the queue
  // is empty.
  private static final ArrayList<Runnable> queue = new ArrayList<Runnable>();

  // FIXME: this thread probably ought to go in some classpath-internal
  // ThreadGroup.  But we don't have that yet.
  private EventDispatcher()
  {
    setDaemon(true);
    start();
  }

  public void run()
  {
    while (true)
      {
        Runnable r;
        synchronized (queue)
          {
            while (queue.size() == 0)
              {
                try
                  {
                    queue.wait();
                  }
                catch (InterruptedException _)
                  {
                    // Ignore.
                  }
              }
            r = queue.remove(0);
          }
        // Invoke outside the synchronization, so that 
        // we aren't blocking other threads from posting events.
        try
          {
            r.run();
          }
        catch (Throwable _)
          {
            // Ignore.
          }
      }
  }

  /**
   * Add a new runnable to the event dispatch queue.  The
   * runnable will be invoked in the event dispatch queue
   * without any locks held.
   * @param runner the Runnable to dispatch
   */
  public static void dispatch(Runnable runner)
  {
    synchronized (queue)
      {
        queue.add(runner);
	queue.notify();
      }
  }
}
