/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.event.*;
import java.util.EmptyStackException;
import java.lang.reflect.InvocationTargetException;

/* Written using on-line Java 2 Platform Standard Edition v1.3 API 
 * Specification, as well as "The Java Class Libraries", 2nd edition 
 * (Addison-Wesley, 1998).
 * Status:  Believed complete, but untested. Check FIXME's.
 */

/** @author Bryce McKinlay */

public class EventQueue
{
  private static final int INITIAL_QUEUE_DEPTH = 8;
  private AWTEvent[] queue = new AWTEvent[INITIAL_QUEUE_DEPTH];
  
  private int next_in = 0; // Index where next event will be added to queue
  private int next_out = 0; // Index of next event to be removed from queue

  private EventQueue next;
  private EventQueue prev;

  private EventDispatchThread dispatchThread = new EventDispatchThread(this);

  public EventQueue()
  {
  }
  
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
  
  /** @specnote Does not block. Returns null if there are no events on the 
   *            queue. 
   */ 
  public synchronized AWTEvent peekEvent()
  {
    if (next != null)
      return next.peekEvent();
      
    if (next_in != next_out)
      return queue[next_out];
    else return null;
  }
  
  /** @specnote Does not block. Returns null if there are no matching events 
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
  
  public synchronized void postEvent(AWTEvent evt)
  {
    if (next != null)
      {
        next.postEvent(evt);
	return;
      }
    // FIXME: Security checks?
    
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
  
  /** @since JDK1.2 */
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
  
  /** @since JDK1.2 */
  public static void invokeLater(Runnable runnable)
  {
    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue(); 

    InvocationEvent ie = 
      new InvocationEvent(eq, runnable, null, false);

    eq.postEvent(ie);
  }
  
  public static boolean isDispatchThread()
  {
    EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue(); 
    return (Thread.currentThread() == eq.dispatchThread);
  }
  
  /** Allows a custom EventQueue implementation to replace this one. 
    * All pending events are transferred to the new queue. Calls to postEvent,
    * getNextEvent, and peekEvent are forwarded to the pushed queue until it
    * is removed with a pop().
    */
  public synchronized void push(EventQueue newEventQueue)
  {
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
    * was previously push()ed. Event dispatch from this queue is suspended. */
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
      }
  }
  
  protected void dispatchEvent(AWTEvent evt)
  {
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
}
