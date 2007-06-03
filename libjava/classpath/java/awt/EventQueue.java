/* EventQueue.java --
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2005  Free Software Foundation

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


package java.awt;

import gnu.java.awt.LowPriorityEvent;
import gnu.java.awt.peer.NativeEventLoopRunningEvent;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.InputMethodEvent;
import java.awt.event.InvocationEvent;
import java.awt.event.PaintEvent;
import java.awt.peer.ComponentPeer;
import java.awt.peer.LightweightPeer;
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
  /**
   * Indicates events that are processed with normal priority. This is normally
   * all events except PaintEvents.
   */
  private static final int NORM_PRIORITY = 0;

  /**
   * Indicates events that are processed with lowes priority. This is normally
   * all PaintEvents and LowPriorityEvents.
   */
  private static final int LOW_PRIORITY = 1;

  /**
   * Implements the actual queue. EventQueue has 2 internal queues for
   * different priorities:
   * 1 PaintEvents are always dispatched with low priority.
   * 2. All other events are dispatched with normal priority.
   *
   * This makes sure that the actual painting (output) is performed _after_ all
   * available input has been processed and that the paint regions are
   * coalesced as much as possible.
   */
  private class Queue
  {
    /**
     * The first item in the queue. This is where events are popped from.
     */
    AWTEvent queueHead;

    /**
     * The last item. This is where events are posted to.
     */
    AWTEvent queueTail;
  }

  /**
   * The three internal event queues.
   *
   * @see Queue
   */
  private Queue[] queues;

  private EventQueue next;
  private EventQueue prev;
  private AWTEvent currentEvent;
  private long lastWhen = System.currentTimeMillis();

  private EventDispatchThread dispatchThread = new EventDispatchThread(this);
  private boolean nativeLoopRunning = false;

  private boolean isShutdown ()
  {
    // This is the exact self-shutdown condition specified in J2SE:
    // http://java.sun.com/j2se/1.4.2/docs/api/java/awt/doc-files/AWTThreadIssues.html

    if (nativeLoopRunning)
      return false;

    if (peekEvent() != null)
      return false;

    if (Frame.hasDisplayableFrames())
      return false;

    return true;
  }

  /**
   * Initializes a new instance of <code>EventQueue</code>.
   */
  public EventQueue()
  {
    queues = new Queue[2];
    queues[NORM_PRIORITY] = new Queue();
    queues[LOW_PRIORITY] = new Queue();
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

    AWTEvent res = getNextEventImpl(true);

    while (res == null)
      {
        if (isShutdown())
          {
            // Explicitly set dispathThread to null.  If we don't do
            // this, there is a race condition where dispatchThread
            // can be != null even after the event dispatch thread has
            // stopped running.  If that happens, then the
            // dispatchThread == null check in postEventImpl will
            // fail, and a new event dispatch thread will not be
            // created, leaving invokeAndWaits waiting indefinitely.
            dispatchThread = null;

            // Interrupt the event dispatch thread.
            throw new InterruptedException();
          }

        wait();
        res = getNextEventImpl(true);
      }

    return res;
  }

  /**
   * Fetches and possibly removes the next event from the internal queues.
   * This method returns immediately. When all queues are empty, this returns
   * <code>null</code>:
   *
   * @param remove <true> when the event should be removed from the queue,
   *        <code>false</code> otherwise
   *
   * @return the next event or <code>null</code> when all internal queues
   *         are empty
   */
  private AWTEvent getNextEventImpl(boolean remove)
  {
    AWTEvent next = null;
    for (int i = 0; i < queues.length && next == null; i++)
      {
        Queue q = queues[i];
        if (q.queueHead != null)
          {
            // Got an event, remove it.
            next = q.queueHead;
            if (remove)
              {
                // Unlink event from the queue.
                q.queueHead = next.queueNext;
                if (q.queueHead == null)
                  q.queueTail = null;
                next.queueNext = null;
              }
          }
      }
    return next;
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

    return getNextEventImpl(false);
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

    AWTEvent evt = null;
    for (int i = 0; i < queues.length && evt == null; i++)
      {
        Queue q = queues[i];
        evt = q.queueHead;
        while (evt != null && evt.id != id)
          evt = evt.queueNext;
        // At this point we either have found an event (evt != null -> exit
        // for loop), or we have found no event (evt == null -> search next
        // internal queue).
      }
    return evt;
  }

  /**
   * Posts a new event to the queue.
   *
   * @param evt The event to post to the queue.
   *
   * @exception NullPointerException If event is null.
   */
  public void postEvent(AWTEvent evt)
  {
    postEventImpl(evt);
  }

  /**
   * Sorts events to their priority and calls
   * {@link #postEventImpl(AWTEvent, int)}.
   *
   * @param evt the event to post
   */
  private synchronized final void postEventImpl(AWTEvent evt)
  {
    int priority = NORM_PRIORITY;
    if (evt instanceof PaintEvent || evt instanceof LowPriorityEvent)
      priority = LOW_PRIORITY;
    // TODO: Maybe let Swing RepaintManager events also be processed with
    // low priority.
    if (evt instanceof NativeEventLoopRunningEvent)
      {
        nativeLoopRunning = ((NativeEventLoopRunningEvent) evt).isRunning();
        notify();
        return;
      }
    postEventImpl(evt, priority);
  }

  /**
   * Actually performs the event posting. This is needed because the
   * RI doesn't use the public postEvent() method when transferring events
   * between event queues in push() and pop().
   * 
   * @param evt the event to post
   * @param priority the priority of the event
   */
  private final void postEventImpl(AWTEvent evt, int priority)
  {
    if (evt == null)
      throw new NullPointerException();

    if (next != null)
      {
        next.postEvent(evt);
        return;
      }

    Object source = evt.getSource();

    Queue q = queues[priority];
    if (source instanceof Component)
      {
        // For PaintEvents, ask the ComponentPeer to coalesce the event
        // when the component is heavyweight.
        Component comp = (Component) source;
        ComponentPeer peer = comp.peer;
        if (peer != null && evt instanceof PaintEvent
            && ! (peer instanceof LightweightPeer))
          peer.coalescePaintEvent((PaintEvent) evt);

        // Check for any events already on the queue with the same source
        // and ID.
        AWTEvent previous = null;
        for (AWTEvent qevt = q.queueHead; qevt != null; qevt = qevt.queueNext)
          {
            Object src = qevt.getSource();
            if (qevt.id == evt.id && src == comp)
              {
                // If there are, call coalesceEvents on the source component 
                // to see if they can be combined.
                Component srccmp = (Component) src;
                AWTEvent coalescedEvt = srccmp.coalesceEvents(qevt, evt);
                if (coalescedEvt != null)
                  {
                    // Yes. Replace the existing event with the combined event.
                    if (qevt != coalescedEvt)
                      {
                        if (previous != null)
                          {
                            assert previous.queueNext == qevt;
                            previous.queueNext = coalescedEvt;
                          }
                        else
                          {
                            assert q.queueHead == qevt;
                            q.queueHead = coalescedEvt;
                          }
                        coalescedEvt.queueNext = qevt.queueNext;
                        if (q.queueTail == qevt)
                          q.queueTail = coalescedEvt;
                        qevt.queueNext = null;
                      }
                    return;
                  }
              }
            previous = qevt;
          }
      }

    if (q.queueHead == null)
      {
        // We have an empty queue. Set this event both as head and as tail.
        q.queueHead = evt;
        q.queueTail = evt;
      }
    else
      {
        // Note: queueTail should not be null here.
        q.queueTail.queueNext = evt;
        q.queueTail = evt;
      }

    if (dispatchThread == null || !dispatchThread.isAlive())
      {
        dispatchThread = new EventDispatchThread(this);
        dispatchThread.start();
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
    if (isDispatchThread ())
      throw new Error("Can't call invokeAndWait from event dispatch thread");

    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue(); 
    Object notifyObject = new Object();

    InvocationEvent ie =
      new InvocationEvent(eq, runnable, notifyObject, true);

    synchronized (notifyObject)
      {
        eq.postEvent(ie);
        notifyObject.wait();
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
    Thread ct = Thread.currentThread();
    
    /* Find out if this thread is the dispatch thread for any of the
       EventQueues in the chain */ 
    while (ct != eq.dispatchThread)
      {
        // Try next EventQueue, if any
        if (eq.next == null)
           return null;  // Not an event dispatch thread
        eq = eq.next;
      }

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

    synchronized (newEventQueue)
      {
        // The RI transfers the events without calling the new eventqueue's
        // push(), but using getNextEvent().
        while (peekEvent() != null)
          {
            try
              {
                newEventQueue.postEventImpl(getNextEvent());
              }
            catch (InterruptedException ex)
              {
                // What should we do with this?
                ex.printStackTrace();
              }
          }
        newEventQueue.prev = this;
      }

    next = newEventQueue;
  }

  /** Transfer any pending events from this queue back to the parent queue that
    * was previously push()ed. Event dispatch from this queue is suspended.
    *
    * @exception EmptyStackException If no previous push was made on this
    * EventQueue.
    */
  protected void pop() throws EmptyStackException
  {
    /* The order is important here, we must get the prev lock first,
       or deadlock could occur as callers usually get here following
       prev's next pointer, and thus obtain prev's lock before trying
       to get this lock. */
    EventQueue previous = prev;
    if (previous == null)
      throw new EmptyStackException();
    synchronized (previous)
      {
        synchronized (this)
          {
            EventQueue nextQueue = next;
            if (nextQueue != null)
              {
                nextQueue.pop();
              }
            else
              {
                previous.next = null;

                // The RI transfers the events without calling the new eventqueue's
                // push(), so this should be OK and most effective.
                while (peekEvent() != null)
                  {
                    try
                      {
                        previous.postEventImpl(getNextEvent());
                      }
                    catch (InterruptedException ex)
                      {
                        // What should we do with this?
                        ex.printStackTrace();
                      }
                  }
                prev = null;
                // Tell our EventDispatchThread that it can end
                // execution.
                if (dispatchThread != null)
                  {
                    dispatchThread.interrupt();
                    dispatchThread = null;
                  }
              }
          }
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
