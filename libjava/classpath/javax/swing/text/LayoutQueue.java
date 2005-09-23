/* LayoutQueue.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package javax.swing.text;

import java.util.LinkedList;

/**
 * This is a queue which holds {@link Runnable} objects.  It is
 * intended for deferring layout operations.
 */
public class LayoutQueue
{
  // The default layout queue.
  private static LayoutQueue defaultQueue = new LayoutQueue();

  // The queue of tasks.
  private LinkedList list = new LinkedList();

  /**
   * Create a new layout queue.
   */
  public LayoutQueue()
  {
  }

  /**
   * Add a layout task to the queue.
   */
  public void addTask(Runnable task)
  {
    synchronized (list)
      {
	list.addLast(task);
	list.notify();
      }
  }

  /**
   * Called by a worker thread to retrieve the next layout task.  This
   * will block until a new task is available.  This method will
   * return null if the thread is interrupted while waiting.
   */
  protected Runnable waitForWork()
  {
    synchronized (list)
      {
	while (list.size() == 0)
	  {
	    try
	      {
		list.wait();
	      }
	    catch (InterruptedException _)
	      {
		// This seemed like a good idea, but it has not been
		// tested on the JDK.
		return null;
	      }
	  }
	return (Runnable) list.removeFirst();
      }
  }

  /**
   * Return the default layout queue.
   */
  public static synchronized LayoutQueue getDefaultQueue()
  {
    return defaultQueue;
  }

  /**
   * Set the default layout queue.
   */
  public static synchronized void setDefaultQueue(LayoutQueue q)
  {
    defaultQueue = q;
  }
}
