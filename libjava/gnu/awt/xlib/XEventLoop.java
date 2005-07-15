package gnu.awt.xlib;

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

import java.awt.*;

import gnu.awt.LightweightRedirector;
import gnu.gcj.xlib.Display;
import gnu.gcj.xlib.XAnyEvent;
import gnu.gcj.xlib.XExposeEvent;
import gnu.gcj.xlib.XButtonEvent;
import gnu.gcj.xlib.XConfigureEvent;
import java.awt.event.PaintEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.Vector;

public class XEventLoop implements Runnable
{
  Display display;
  EventQueue queue;
  XAnyEvent anyEvent;
  private Thread eventLoopThread;
  
  LightweightRedirector lightweightRedirector = new LightweightRedirector();
    
  public XEventLoop(Display display, EventQueue queue)
  {
    this.display = display;
    this.queue = queue;
    
    anyEvent = new XAnyEvent(display);
    eventLoopThread = new Thread(this, "AWT thread for XEventLoop");
    eventLoopThread.start();
  }

  public void run ()
  {
    // FIXME: do we need an interrupt mechanism for window shutdown?
    while (true)
      postNextEvent (true);
  }
  
  /** If there's an event available, post it.
   * @return true if an event was posted
   */
  boolean postNextEvent(boolean block)
  {
    AWTEvent evt = getNextEvent(block);
    if (evt != null)
      queue.postEvent(evt);
    return evt != null;
  }
    
  /** Get the next event.
   * @param block If true, block until an event becomes available
   */
  public AWTEvent getNextEvent(boolean block)
  {
    // ASSERT:
    if (isIdle())
      throw new Error("should not be idle");
    
    AWTEvent event = null;
    if (loadNextEvent(block))
      {
        event = createEvent(); 
        event = lightweightRedirector.redirect(event);
      }
    return event;
  }

  boolean loadNextEvent(boolean block)
  {
    boolean gotEvent = false;
    try
      {
	setIdle(true);
	
	/* The code below will result in an XFlush(). However,
	   while we are waiting for events after calling XFlush(),
	   new X requests issued on other threads will not
	   automatically be flushed. This can lead to a deadlock
	   since XFlush() will not be called before after the
	   processing of the next event, and new events arriving
	   might be dependent on the delivery of the X
	   requests. 
	   
	   Code that issues X requests should therefore call
	   flushIfIdle() after they're done, to ensure that the
	   requests are delivered in a timely manner.  XFlush is not
	   run if event processing is underway, since we are assured
	   that the event loop execution will return to this point,
	   where requests are flushed again, before waiting for new
	   events.

	   Alternatively, do the work on the AWT thread, since the
	   XEventQueue knows how to flush the display when it runs out
	   of events. */
	
	//display.flush(); // implicit?
	gotEvent = anyEvent.loadNext(block);
      }
    catch (RuntimeException re)
      {
	System.err.println("Exception thrown on event thread:" + re);
      }
    finally
      {
	setIdle(false);
      }
    return gotEvent;
  }
    
  /**
   * @returns an AWT event created based on the current XEvent.
   * Returns null if the current XEvent does not map to any perticular
   * AWT event.
   */
    
  AWTEvent createEvent ()
  {
    int type = anyEvent.getType ();
    // Ignore some events without further processing
    switch (type)
    {
      // ignore "no expose" events, which are generated whenever a  pixmap
      // is copied to copied to a window which is entirely unobscured
      case XAnyEvent.TYPE_NO_EXPOSE:
      case XAnyEvent.TYPE_UNMAP_NOTIFY:     // ignore for now
      case XAnyEvent.TYPE_MAP_NOTIFY:       // ignore for now
      case XAnyEvent.TYPE_REPARENT_NOTIFY:  // ignore for now
        return null;
      default:
        break;  // continue processing events not in ignore list
    }
    /* avoid attempting to get client data before client data has
       been set. */
    Object peer;
    synchronized (this)
    {
      peer = anyEvent.getWindow ().getClientData ();
    }
    
    Component source = null;
    
    // Try to identify source component
    
    if (peer instanceof XCanvasPeer)
    {
      source = ((XCanvasPeer) peer).getComponent ();
    }
    
    if (source == null)
    {
      String msg = "unable to locate source for event (" +
      anyEvent + "): peer=" + peer;
      throw new RuntimeException (msg);
    }
    
    /* if a mapping from anyEvent to AWTEvent is possible, construct a
       new AWTEvent and return it. */
    
    switch (type)
    {
      case XAnyEvent.TYPE_EXPOSE:
        return createPaintEvent (source);
      case XAnyEvent.TYPE_BUTTON_PRESS:
      case XAnyEvent.TYPE_BUTTON_RELEASE:
        return createMouseEvent (type, source);
      case XAnyEvent.TYPE_CONFIGURE_NOTIFY:
        configureNotify (peer);
        return null;
        
      default:
        String msg = "Do not know how to handle event (" + anyEvent + ")";
        throw new RuntimeException (msg);
    }
  }
  
  AWTEvent createPaintEvent(Component src)
  {
    XExposeEvent expose = new XExposeEvent(anyEvent);
    PaintEvent pe = new PaintEvent(src, PaintEvent.PAINT,
				   expose.getBounds());
    return pe;
  }
    
  AWTEvent createMouseEvent(int type, Component src)
  {    
    XButtonEvent buttonEvt = new XButtonEvent(anyEvent);
    
    int modifiers = 0; //buttonToModifierMap[buttonEvt.button];
    
    /* Warning: this makes assumptions on the contents of
       X.h... Button1 = 1, Button2 = 2, etc... */
    switch (buttonEvt.button)
      {
      case 1:
	modifiers = InputEvent.BUTTON1_DOWN_MASK;
	break;
      case 2:
	modifiers = InputEvent.BUTTON2_DOWN_MASK;
	break;
      case 3:
	modifiers = InputEvent.BUTTON2_DOWN_MASK;
	break;
      }
    
    int state = buttonEvt.state;
    
    // remap bits from state to modifiers:
    
    if ((state & XButtonEvent.MASK_SHIFT) != 0)
      modifiers |= InputEvent.SHIFT_MASK;
	
	
    if ((state & XButtonEvent.MASK_CONTROL) != 0)
      modifiers |= InputEvent.CTRL_MASK;
	
    
    /* FIXME: we need additional X code to properly map MODn states to
       input modifiers */
	
    int clickCount = 1; // FIXME... Can't get this from X.
    boolean popupTrigger = false; // FIXME: look up policy somewhere
	
    int x = buttonEvt.x;
    int y = buttonEvt.y;

    int id = (type == XAnyEvent.TYPE_BUTTON_PRESS) ?
      MouseEvent.MOUSE_PRESSED :
      MouseEvent.MOUSE_RELEASED;
	
    MouseEvent me = new MouseEvent(src,
				   id,
				   buttonEvt.time, modifiers,
				   buttonEvt.x, buttonEvt.y,
				   clickCount, popupTrigger);
    return me;
  }

  void configureNotify(Object peerObj)
  {
    XConfigureEvent configEvent = new XConfigureEvent(anyEvent);
    XFramePeer peer = (XFramePeer)  peerObj;
    
    peer.configureNotify(configEvent);
  }
    
  public void flushIfIdle()
  {
    if (isIdle())
      display.flush();
  }
  
  volatile boolean idle = false;

  final synchronized void setIdle(boolean idle)
  {
    this.idle = idle;
  }

  final synchronized boolean isIdle()
  {
    return idle;
  }
}
