/* Copyright (C) 1999, 2000, 2001, 2002, 2003  Free Software Foundation

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


package java.awt;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.InvocationEvent;
import java.lang.reflect.InvocationTargetException;
import java.util.EmptyStackException;

/* Written using on-line Java 2 Platform Standard Edition v1.3 API 
 * Specification, as well as "The Java Class Libraries", 2nd edition 
 * (Addison-Wesley, 1998).
 * Status:  Believed complete, but untested.
 */

/**
 * This class manages a queue of <code>AWTEvent</code> objects that
 * are posted to it.  The AWT system uses only one event queue for all
 * events.
 *
 * @author Bryce McKinlay
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class EventQueue
{
  private static final int INITIAL_QUEUE_DEPTH = 8;
  private AWTEvent[] queue = new AWTEvent[INITIAL_QUEUE_DEPTH];

  private int next_in = 0; // Index where next event will be added to queue
  private int next_out = 0; // Index of next event to be removed from queue

  private EventQueue next;
  private EventQueue prev;
  private AWTEvent currentEvent;
  private long lastWhen = System.currentTimeMillis();

  private EventDispatchThread dispatchThread = new EventDispatchThread(this);

  /**
   * Initializes a new instance of <code>EventQueue</code>.
   */
  public EventQueue()
  {
  }

  /**
   * Returns the next event in the queue.  This method will block until
   * an event is available or until the thread is interrupted.
   *
   * @return The next event in the queue.
   *
   * @exception InterruptedException If this thread is interrupted while
   * waiting for an event to be posted to the queue.
   */
  public synchronized AWTEvent getNextEvent()
    throws InterruptedException
  {
    if (next != null)
      return next.getNextEvent();

    while (next_in == next_out)
      wait();

    AWTEvent res = queue[next_out];

    if (++next_out == queue.length)
      next_out = 0;
    return res;
  }

  /**
   * Returns the next event in the queue without removing it from the queue.
   * This method will block until an event is available or until the thread
   * is interrupted.
   *
   * @return The next event in the queue.
   * @specnote Does not block. Returns null if there are no events on the 
   *            queue. 
   */ 
  public synchronized AWTEvent peekEvent()
  {
    if (next != null)
      return next.peekEvent();

    if (next_in != next_out)
      return queue[next_out];
    else
      return null;
  }

  /**
   * Returns the next event in the queue that has the specified id
   * without removing it from the queue.
   * This method will block until an event is available or until the thread
   * is interrupted.
   *
   * @param id The event id to return.
   *
   * @return The next event in the queue.
   *
   * @specnote Does not block. Returns null if there are no matching events 
   *            on the queue. 
   */ 
  public synchronized AWTEvent peekEvent(int id)
  {
    if (next != null)
      return next.peekEvent(id);

    int i = next_out;
    while (i != next_in)
      {
        AWTEvent qevt = queue[i];
        if (qevt.id == id)
          return qevt;
      }
    return null;
  }

  /**
   * Posts a new event to the queue.
   *
   * @param event The event to post to the queue.
   *
   * @exception NullPointerException If event is null.
   */
  public synchronized void postEvent(AWTEvent evt)
  {
    if (next != null)
      {
        next.postEvent(evt);
        return;
      }

    /* Check for any events already on the queue with the same source 
       and ID. */	
    int i = next_out;
    while (i != next_in)
      {
        AWTEvent qevt = queue[i];
        Object src;
        if (qevt.id == evt.id
            && (src = qevt.getSource()) == evt.getSource()
            && src instanceof Component)
          {
            /* If there are, call coalesceEvents on the source component 
               to see if they can be combined. */
            Component srccmp = (Component) src;
            AWTEvent coalesced_evt = srccmp.coalesceEvents(qevt, evt);
            if (coalesced_evt != null)
              {
                /* Yes. Replace the existing event with the combined event. */
                queue[i] = coalesced_evt;
                return;
              }
            break;
          }
        if (++i == queue.length)
          i = 0;
      }

    queue[next_in] = evt;    
    if (++next_in == queue.length)
      next_in = 0;

    if (next_in == next_out)
      {
        /* Queue is full. Extend it. */
        AWTEvent[] oldQueue = queue;
        queue = new AWTEvent[queue.length * 2];

        int len = oldQueue.length - next_out;
        System.arraycopy(oldQueue, next_out, queue, 0, len);
        if (next_out != 0)
          System.arraycopy(oldQueue, 0, queue, len, next_out);

        next_out = 0;
        next_in = oldQueue.length;
      }
    notify();
  }

  /**
   * Causes runnable to have its run method called in the dispatch thread of the
   * EventQueue. This will happen after all pending events are processed. The
   * call blocks until this has happened. This method will throw an Error if
   * called from the event dispatcher thread.
   *
   * @exception InterruptedException If another thread has interrupted
   * this thread.
   * @exception InvocationTargetException If an exception is thrown when running
   * runnable.
   *
   * @since 1.2
   */
  public static void invokeAndWait(Runnable runnable)
    throws InterruptedException, InvocationTargetException
  {
    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue(); 
    Thread current = Thread.currentThread();
    if (current == eq.dispatchThread)
      throw new Error("Can't call invokeAndWait from event dispatch thread");

    InvocationEvent ie = 
      new InvocationEvent(eq, runnable, current, true);

    synchronized (current)
      {
        eq.postEvent(ie);
        current.wait();
      }

    Exception exception;

    if ((exception = ie.getException()) != null)
      throw new InvocationTargetException(exception);
  }

  /**
   * This arranges for runnable to have its run method called in the
   * dispatch thread of the EventQueue.  This will happen after all
   * pending events are processed.
   *
   * @since 1.2
   */
  public static void invokeLater(Runnable runnable)
  {
    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue(); 

    InvocationEvent ie = 
      new InvocationEvent(eq, runnable, null, false);

    eq.postEvent(ie);
  }

  /**
   * Return true if the current thread is the current AWT event dispatch
   * thread.
   */
  public static boolean isDispatchThread()
  {
    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue();
    
    /* Find last EventQueue in chain */ 
    while (eq.next != null)
      eq = eq.next;

    return (Thread.currentThread() == eq.dispatchThread);
  }

  /**
   * Return the event currently being dispatched by the event
   * dispatch thread.  If the current thread is not the event
   * dispatch thread, this method returns null.
   *
   * @since 1.4
   */
  public static AWTEvent getCurrentEvent()
  {
    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue(); 
    if (Thread.currentThread() != eq.dispatchThread)
      return null;
    return eq.currentEvent;
  }

  /**
   * Allows a custom EventQueue implementation to replace this one. 
   * All pending events are transferred to the new queue. Calls to postEvent,
   * getNextEvent, and peekEvent and others are forwarded to the pushed queue
   * until it is removed with a pop().
   *
   * @exception NullPointerException if newEventQueue is null.
   */
  public synchronized void push(EventQueue newEventQueue)
  {
    if (newEventQueue == null)
      throw new NullPointerException ();

    /* Make sure we are at the top of the stack because callers can
       only get a reference to the one at the bottom using
       Toolkit.getDefaultToolkit().getSystemEventQueue() */
    if (next != null)
      {
        next.push (newEventQueue);
        return;
      }

    /* Make sure we have a live dispatch thread to drive the queue */
    if (dispatchThread == null)
      dispatchThread = new EventDispatchThread(this);

    int i = next_out;
    while (i != next_in)
      {
        newEventQueue.postEvent(queue[i]);
        next_out = i;
        if (++i == queue.length)
          i = 0;
      }

    next = newEventQueue;
    newEventQueue.prev = this;    
  }

  /** Transfer any pending events from this queue back to the parent queue that
    * was previously push()ed. Event dispatch from this queue is suspended.
    *
    * @exception EmptyStackException If no previous push was made on this
    * EventQueue.
    */
  protected void pop() throws EmptyStackException
  {
    if (prev == null)
      throw new EmptyStackException();

    // Don't synchronize both this and prev at the same time, or deadlock could
    // occur.
    synchronized (prev)
      {
        prev.next = null;
      }

    synchronized (this)
      {
        int i = next_out;
        while (i != next_in)
          {
            prev.postEvent(queue[i]);
            next_out = i;
            if (++i == queue.length)
              i = 0;
          }
	// Empty the queue so it can be reused
	next_in = 0;
	next_out = 0;

        // Tell our EventDispatchThread that it can end execution
        dispatchThread.interrupt ();
	dispatchThread = null;
      }
  }

  /**
   * Dispatches an event. The manner in which the event is dispatched depends
   * upon the type of the event and the type of the event's source object.
   *
   * @exception NullPointerException If event is null.
   */
  protected void dispatchEvent(AWTEvent evt)
  {
    currentEvent = evt;

    if (evt instanceof InputEvent)
      lastWhen = ((InputEvent) evt).getWhen();
    else if (evt instanceof ActionEvent)
      lastWhen = ((ActionEvent) evt).getWhen();
    else if (evt instanceof InvocationEvent)
      lastWhen = ((InvocationEvent) evt).getWhen();

    if (evt instanceof ActiveEvent)
      {
        ActiveEvent active_evt = (ActiveEvent) evt;
        active_evt.dispatch();
      }
    else
      {
        Object source = evt.getSource();

        if (source instanceof Component)
          {
            Component srccmp = (Component) source;
            srccmp.dispatchEvent(evt);
          }
        else if (source instanceof MenuComponent)
          {
            MenuComponent srccmp = (MenuComponent) source;
            srccmp.dispatchEvent(evt);
          }
      }
  }

  /**
   * Returns the timestamp of the most recent event that had a timestamp, or
   * the initialization time of the event queue if no events have been fired.
   * At present, only <code>InputEvent</code>s, <code>ActionEvent</code>s,
   * <code>InputMethodEvent</code>s, and <code>InvocationEvent</code>s have
   * timestamps, but this may be added to other events in future versions.
   * If this is called by the event dispatching thread, it can be any
   * (sequential) value, but to other threads, the safest bet is to return
   * System.currentTimeMillis().
   *
   * @return the most recent timestamp
   * @see InputEvent#getWhen()
   * @see ActionEvent#getWhen()
   * @see InvocationEvent#getWhen()
   * @see InputMethodEvent#getWhen()
   * @since 1.4
   */
  public static long getMostRecentEventTime()
  {
    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue(); 
    if (Thread.currentThread() != eq.dispatchThread)
      return System.currentTimeMillis();
    return eq.lastWhen;
  }
}
