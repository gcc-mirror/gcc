/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.peer.WindowPeer;
import java.awt.peer.ComponentPeer;
import java.util.Locale;

/* A very incomplete placeholder. */

public class Window extends Container
{
  public Window (Frame parent)
  {
    this.parent = parent;
    // FIXME: compiler bug
    // this.layoutMgr = new BorderLayout ();
  }

  public void addNotify ()
  {
    if (peer == null)
      peer = (ComponentPeer) getToolkit ().createWindow (this);
    super.addNotify ();
  }

  public synchronized void addWindowListener (WindowListener listener)
  {
    windowListener = AWTEventMulticaster.add (windowListener, listener);
  }

  public void dispose ()
  {
  }

  public Component getFocusOwner ()
  {
    return null;		// FIXME
  }

  public Locale getLocale ()
  {
    return locale == null ? Locale.getDefault () : locale;
  }

  public String getWarningString ()
  {
    return warningString;
  }

  public void pack ()
  {
    addNotify ();
    // FIXME
  }

  public boolean postEvent (Event evt)
  {
    return false;		// FIXME
  }

  protected void processEvent (AWTEvent evt)
  {
    if (evt instanceof WindowEvent)
      processWindowEvent ((WindowEvent) evt);
    else
      super.processEvent (evt);
  }

  protected void processWindowEvent (WindowEvent evt)
  {
    if (windowListener != null)
      {
	switch (evt.getID ())
	  {
	  case WindowEvent.WINDOW_ACTIVATED:
	    windowListener.windowActivated (evt);
	    break;
	  case WindowEvent.WINDOW_CLOSED:
	    windowListener.windowClosed (evt);
	    break;
	  case WindowEvent.WINDOW_CLOSING:
	    windowListener.windowClosing (evt);
	    break;
	  case WindowEvent.WINDOW_DEACTIVATED:
	    windowListener.windowDeactivated (evt);
	    break;
	  case WindowEvent.WINDOW_DEICONIFIED:
	    windowListener.windowDeiconified (evt);
	    break;
	  case WindowEvent.WINDOW_ICONIFIED:
	    windowListener.windowIconified (evt);
	    break;
	  case WindowEvent.WINDOW_OPENED:
	    windowListener.windowOpened (evt);
	    break;
	  }
      }
  }

  public synchronized void removeWindowListener (WindowListener listener)
  {
    windowListener = AWTEventMulticaster.remove (windowListener, listener);
  }

  public void show ()
  {
    addNotify ();
    validate ();
    setVisible (true);
    // FIXME: is there more to it?
  }

  public void toBack ()
  {
    if (peer != null)
      {
	WindowPeer wp = (WindowPeer) peer;
	wp.toBack ();
      }
  }

  public void toFront ()
  {
    if (peer != null)
      {
	WindowPeer wp = (WindowPeer) peer;
	wp.toFront ();
      }
  }

  // Serialized fields, from Sun's serialization spec.
  // private FocusManager focusMgr;  // FIXME: what is this?
  private int state;
  private String warningString;

  private transient WindowListener windowListener;
}
