/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

   Copyright (C) 1999 Free Software Foundation, Inc.

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
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.peer.WindowPeer;
import java.awt.peer.ComponentPeer;
import java.util.EventListener;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 * This class represents a top-level window with no decorations.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy  <warrenl@cygnus.com>
 */
public class Window extends Container
{
  // Serialized fields, from Sun's serialization spec.
  // private FocusManager focusMgr;  // FIXME: what is this?  
  private String warningString = null;
  private int state = 0;
  private int windowSerializedDataVersion = 0; // FIXME

  private transient WindowListener windowListener;
  private transient GraphicsConfiguration graphicsConfiguration;

  /** 
   * This (package access) constructor is used by subclasses that want
   * to build windows that do not have parents.  Eg. toplevel
   * application frames.  Subclasses cannot call super(null), since
   * null is an illegal argument.
   */
  Window()
  {
    setVisible(false);
    setLayout((LayoutManager) new BorderLayout());
  }

  Window(GraphicsConfiguration gc)
  {
    this();
    graphicsConfiguration = gc;
  }
    
  /**
   * Initializes a new instance of <code>Window</code> with the specified
   * parent.  The window will initially be invisible.
   *
   * @param parent The owning <code>Frame</code> of this window.
   */
  public Window(Frame owner)
  {
    this((Window) owner);
  }

  /** @since 1.2 */
  public Window(Window owner)
  {
    this();
    if (owner == null)
      throw new IllegalArgumentException("owner must not be null");
    
    this.parent = owner;

    // FIXME: add to owner's "owned window" list
    //owner.owned.add(this); // this should be a weak reference
  }
  
  /** @since 1.3 */
  public Window(Window owner, GraphicsConfiguration gc)
  {
    this(owner);

    /*  FIXME: Security check
    SecurityManager.checkTopLevelWindow(...)

    if (gc != null
        && gc.getDevice().getType() != GraphicsDevice.TYPE_RASTER_SCREEN)
      throw new IllegalArgumentException ("gc must be from a screen device");

    if (gc == null)
      graphicsConfiguration = GraphicsEnvironment.getLocalGraphicsEnvironment()
			     .getDefaultScreenDevice()
			     .getDefaultConfiguration();
    else
    */    
    graphicsConfiguration = gc;
  }

  GraphicsConfiguration getGraphicsConfigurationImpl()
  {
    if (graphicsConfiguration != null)
	return graphicsConfiguration;

    return super.getGraphicsConfigurationImpl();
  }

  protected void finalize() throws Throwable
  {
    // FIXME: remove from owner's "owned window" list (Weak References)
    super.finalize();
  }

  /**
   * Creates the native peer for this window.
   */
  public void addNotify()
  {
    if (peer == null)
      peer = getToolkit ().createWindow (this);
    super.addNotify ();
  }

  /**
   * Relays out this window's child components at their preferred size.
   *
   * @specnote pack() doesn't appear to be called internally by show(), so
   *             we duplicate some of the functionality.
   */
  public void pack()
  {
    if (parent != null
        && !parent.isDisplayable())
      parent.addNotify();
    if (peer == null)
      addNotify();

    setSize(getPreferredSize());
    
    validate();
  }

  /**
   * Makes this window visible and brings it to the front.
   */
  public void show ()
  {
    if (peer == null)
      addNotify();

    super.show();
    toFront();
  }

  public void hide()
  {
    // FIXME: call hide() on amy "owned" children here.
    super.hide();
  }

  /**
   * Called to free any resource associated with this window.
   */
  public void dispose()
  {
    hide();

    Window[] list = getOwnedWindows();
    for (int i=0; i<list.length; i++)
      list[i].dispose();

    for (int i = 0; i < ncomponents; ++i)
      component[i].removeNotify();
    this.removeNotify();
  }

  /**
   * Sends this window to the back so that all other windows display in
   * front of it.
   */
  public void toBack ()
  {
    if (peer != null)
      {
	WindowPeer wp = (WindowPeer) peer;
	wp.toBack ();
      }
  }

  /**
   * Brings this window to the front so that it displays in front of
   * any other windows.
   */
  public void toFront ()
  {
    if (peer != null)
      {
	WindowPeer wp = (WindowPeer) peer;
	wp.toFront ();
      }
  }

  /**
   * Returns the toolkit used to create this window.
   *
   * @return The toolkit used to create this window.
   *
   * @specnote Unlike Component.getToolkit, this implementation always 
   *           returns the value of Toolkit.getDefaultToolkit().
   */
  public Toolkit getToolkit()
  {
    return Toolkit.getDefaultToolkit ();    
  }

  /**
   * Returns the warning string that will be displayed if this window is
   * popped up by an unsecure applet or application.
   *
   * @return The unsecure window warning message.
   */
  public final String getWarningString()
  {
    boolean secure = true;
    /* boolean secure = SecurityManager.checkTopLevelWindow(...) */

    if (!secure)
      {
        if (warningString != null)
	  return warningString;
	else
	  {
	    String warning = System.getProperty("awt.appletWarning");
	    return warning;
	  }
      }
    return null;
  }

  /**
   * Returns the locale that this window is configured for.
   *
   * @return The locale this window is configured for.
   */
  public Locale getLocale ()
  {
    return locale == null ? Locale.getDefault () : locale;
  }

  /*
  /** @since 1.2
  public InputContext getInputContext()
  {
    // FIXME
  }
  */

  /**
   * Sets the cursor for this window to the specifiec cursor.
   *
   * @param cursor The new cursor for this window.
   */
  public void setCursor(Cursor cursor)
  {
    super.setCursor(cursor);
  }

  public Window getOwner()
  {
    return (Window) parent;
  }

  /** @since 1.2 */
  public Window[] getOwnedWindows()
  {
    // FIXME: return array containing all the windows this window currently 
    // owns.
    return new Window[0];
  }

  /**
   * Adds the specified listener to the list of <code>WindowListeners</code>
   * that will receive events for this window.
   *
   * @param listener The <code>WindowListener</code> to add.
   */
  public synchronized void addWindowListener (WindowListener listener)
  {
    windowListener = AWTEventMulticaster.add (windowListener, listener);
  }

  /**
   * Removes the specified listener from the list of
   * <code>WindowListeners</code> that will receive events for this window.
   *
   * @param listener The <code>WindowListener</code> to remove.
   */
  public synchronized void removeWindowListener (WindowListener listener)
  {
    windowListener = AWTEventMulticaster.remove (windowListener, listener);
  }

  /** @since 1.3 */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == WindowListener.class)
      return getListenersImpl(listenerType, windowListener);
    else return super.getListeners(listenerType);
  }

  void dispatchEventImpl(AWTEvent e)
  {
    // Make use of event id's in order to avoid multiple instanceof tests.
    if (e.id <= WindowEvent.WINDOW_LAST 
        && e.id >= WindowEvent.WINDOW_FIRST
        && (windowListener != null 
	    || (eventMask & AWTEvent.WINDOW_EVENT_MASK) != 0))
      processEvent(e);
    else
      super.dispatchEventImpl(e);
  }

  /**
   * Processes the specified event for this window.  If the event is an
   * instance of <code>WindowEvent</code>, then
   * <code>processWindowEvent()</code> is called to process the event,
   * otherwise the superclass version of this method is invoked.
   *
   * @param event The event to process.
   */
  protected void processEvent (AWTEvent evt)
  {
    if (evt instanceof WindowEvent)
      processWindowEvent ((WindowEvent) evt);
    else
      super.processEvent (evt);
  }

  /**
   * Dispatches this event to any listeners that are listening for
   * <code>WindowEvents</code> on this window.  This method only gets
   * invoked if it is enabled via <code>enableEvents()</code> or if
   * a listener has been added.
   *
   * @param event The event to process.
   */
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

  /**
   * Returns the child window that has focus if this window is active.
   * This method returns <code>null</code> if this window is not active
   * or no children have focus.
   *
   * @return The component that has focus, or <code>null</code> if no
   * component has focus.
   */
  public Component getFocusOwner()
  {
    // FIXME
    return null;
  }

  /**
   * Post a Java 1.0 event to the event queue.
   *
   * @param event The event to post.
   */
  public boolean postEvent(Event e)
  {
    // FIXME
    return false;
  }

  /**
   * Tests whether or not this window is visible on the screen.
   *
   * @return <code>true</code> if this window is visible, <code>false</code>
   * otherwise.
   */
  public boolean isShowing()
  {
    return super.isShowing();
  }

  /** @since 1.2 */
  public void applyResourceBundle(ResourceBundle rb)
  {
    // FIXME
  }

  /** @since 1.2 */
  public void applyResourceBundle(String rbName)
  {
    ResourceBundle rb = ResourceBundle.getBundle(rbName);
    if (rb != null)
      applyResourceBundle(rb);    
  }

  /*
  public AccessibleContext getAccessibleContext()
  {
    // FIXME
  }
  */

  /** 
   * Get graphics configuration.  The implementation for Window will
   * not ask any parent containers, since Window is a toplevel
   * window and not actually embedded in the parent component.
   */
  public GraphicsConfiguration getGraphicsConfiguration()
  {
    if (graphicsConfiguration != null) return graphicsConfiguration;
    if (peer != null) return peer.getGraphicsConfiguration();
    return null;
  }

}
