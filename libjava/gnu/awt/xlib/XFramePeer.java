/* Copyright (C) 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.*;
import java.awt.peer.*;
import java.awt.image.*;
import gnu.gcj.xlib.WMSizeHints;
import gnu.gcj.xlib.WindowAttributes;
import gnu.gcj.xlib.Display;
import gnu.gcj.xlib.Visual;
import gnu.gcj.xlib.Screen;
import gnu.gcj.xlib.XConfigureEvent;

/** FIXME: a lot of the code here should be moved down to XWindowPeer. */

public class XFramePeer extends XCanvasPeer implements FramePeer
{

  public XFramePeer(Frame frame)
  {
    super(frame);
    
    // Set some defaults for a toplevel component:
    if (frame.getFont() == null)
      frame.setFont(new Font("helvetica", Font.PLAIN, 12));

    if (frame.getBackground() == null)
      frame.setBackground(Color.lightGray);

    if (frame.getForeground() == null)
      frame.setForeground(Color.black);
  }

  /** Find parent window for toplevel window, ie. root window of
      selected screen. Bounds are not changed. */
  gnu.gcj.xlib.Window locateParentWindow(Rectangle bounds)
  {
    Screen screen = config.getVisual().getScreen();
    return screen.getRootWindow();
  }
	
  void initWindowProperties()
  {
    Frame frame = (Frame) component;
    setResizable(frame.isResizable());
    String title = frame.getTitle();
    if (!title.equals("")) setTitle(title);
  }

  long getBasicEventMask()
  {
    return super.getBasicEventMask() |
      WindowAttributes.MASK_STRUCTURE_NOTIFY;
  }

  void configureNotify(XConfigureEvent configEvent)
  {
    component.setBounds(configEvent.getBounds());
    
    /* FIXME: Validation should probably not be done here.  The best
       strategy is probably to validate on the AWT thread in response
       to an ComponentEvent.  This will make it possible to coalesce
       resize validations. */
    component.validate();
  }

  /* Overridden to ignore request to set bounds if the request occurs
     on the X event loop thread.  It is assumed that all requests that
     occur on the X event loop thread are results of XConfigureNotify
     events, in which case the X window already has the desired
     bounds.  */
  public void setBounds(int x, int y, int width, int height)
  {
    if (Thread.currentThread() == getXToolkit().eventLoop.eventLoopThread)
      return;
    
    super.setBounds(x, y, width, height);
  }
 
  // Implementing ContainerPeer:

  static final Insets INSETS_0_PROTOTYPE = new Insets(0, 0, 0, 0);

  public Insets getInsets()
  {
    return (Insets) INSETS_0_PROTOTYPE.clone();
  }

  public Insets insets ()
  {
    return getInsets ();
  }

  public void beginValidate()
  {
  }

  public void endValidate()
  {
    // reassert sizing hints
    Frame frame = (Frame) component;
    setResizable(frame.isResizable());
  }
    

  // Implementing WindowPeer:

  public void toBack()
  {
    throw new UnsupportedOperationException("not implemented yet");	
  }
  
  public void toFront()
  {
    throw new UnsupportedOperationException("not implemented yet");	
  }


  // Implementing FramePeer:

  public void setIconImage(Image image)
  {
    throw new UnsupportedOperationException("not implemented yet");	
  }
  
  public void setMenuBar(MenuBar mb)
  {
    throw new UnsupportedOperationException("not implemented yet");	
  }


  public void setTitle(String title)
  {
    synchronized (window.getDisplay())
      {
	// Oh, what a nice implementation :-)
	window.setProperty("WM_NAME", "STRING", title);
	
	ensureFlush();
      }
  }

  public void setResizable(boolean resizable)
  {
    Frame frame = (Frame) component;
    
    WMSizeHints sizeHints = new WMSizeHints();
    if (resizable)
      {
	Dimension minSize = frame.getMinimumSize();
	sizeHints.setMinSize(minSize.width, minSize.height);
	
	Dimension maxSize = frame.getMaximumSize();
	
	if ((maxSize.width < Short.MAX_VALUE) ||
	    (maxSize.height < Short.MAX_VALUE))
	  {
	    maxSize.width  = Math.min(maxSize.width,  Short.MAX_VALUE);
	    maxSize.height = Math.min(maxSize.height, Short.MAX_VALUE);
	    sizeHints.setMaxSize(maxSize.width, maxSize.height);
	  }
      }
    else
      {
	// lock resizing to current bounds
	Dimension size = frame.getSize();
	sizeHints.setMinSize(size.width, size.height);
	sizeHints.setMaxSize(size.width, size.height);
      }
    sizeHints.applyNormalHints(window);
  }
}
