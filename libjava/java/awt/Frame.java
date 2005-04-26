/* Frame.java -- AWT toplevel window
   Copyright (C) 1999, 2000, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.peer.FramePeer;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;

/**
  * This class is a top-level window with a title bar and window
  * decorations.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Frame extends Window implements MenuContainer
{
/**
  * Constant for the default cursor.
  * @deprecated Replaced by <code>Cursor.DEFAULT_CURSOR</code> instead.
  */
public static final int DEFAULT_CURSOR = Cursor.DEFAULT_CURSOR;

/**
  * Constant for a cross-hair cursor.
  * @deprecated Use <code>Cursor.CROSSHAIR_CURSOR</code> instead.
  */
public static final int CROSSHAIR_CURSOR = Cursor.CROSSHAIR_CURSOR;

/**
  * Constant for a cursor over a text field.
  * @deprecated Use <code>Cursor.TEXT_CURSOR</code> instead.
  */
public static final int TEXT_CURSOR = Cursor.TEXT_CURSOR;

/**
  * Constant for a cursor to display while waiting for an action to complete.
  * @deprecated Use <code>Cursor.WAIT_CURSOR</code>.
  */
public static final int WAIT_CURSOR = Cursor.WAIT_CURSOR;

/**
  * Cursor used over SW corner of window decorations.
  * @deprecated Use <code>Cursor.SW_RESIZE_CURSOR</code> instead.
  */
public static final int SW_RESIZE_CURSOR = Cursor.SW_RESIZE_CURSOR;

/**
  * Cursor used over SE corner of window decorations.
  * @deprecated Use <code>Cursor.SE_RESIZE_CURSOR</code> instead.
  */
public static final int SE_RESIZE_CURSOR = Cursor.SE_RESIZE_CURSOR;

/**
  * Cursor used over NW corner of window decorations.
  * @deprecated Use <code>Cursor.NW_RESIZE_CURSOR</code> instead.
  */
public static final int NW_RESIZE_CURSOR = Cursor.NW_RESIZE_CURSOR;

/**
  * Cursor used over NE corner of window decorations.
  * @deprecated Use <code>Cursor.NE_RESIZE_CURSOR</code> instead.
  */
public static final int NE_RESIZE_CURSOR = Cursor.NE_RESIZE_CURSOR;

/**
  * Cursor used over N edge of window decorations.
  * @deprecated Use <code>Cursor.N_RESIZE_CURSOR</code> instead.
  */
public static final int N_RESIZE_CURSOR = Cursor.N_RESIZE_CURSOR;

/**
  * Cursor used over S edge of window decorations.
  * @deprecated Use <code>Cursor.S_RESIZE_CURSOR</code> instead.
  */
public static final int S_RESIZE_CURSOR = Cursor.S_RESIZE_CURSOR;

/**
  * Cursor used over E edge of window decorations.
  * @deprecated Use <code>Cursor.E_RESIZE_CURSOR</code> instead.
  */
public static final int E_RESIZE_CURSOR = Cursor.E_RESIZE_CURSOR;

/**
  * Cursor used over W edge of window decorations.
  * @deprecated Use <code>Cursor.W_RESIZE_CURSOR</code> instead.
  */
public static final int W_RESIZE_CURSOR = Cursor.W_RESIZE_CURSOR;

/**
  * Constant for a hand cursor.
  * @deprecated Use <code>Cursor.HAND_CURSOR</code> instead.
  */
public static final int HAND_CURSOR = Cursor.HAND_CURSOR;

/**
  * Constant for a cursor used during window move operations.
  * @deprecated Use <code>Cursor.MOVE_CURSOR</code> instead.
  */
public static final int MOVE_CURSOR = Cursor.MOVE_CURSOR;

public static final int ICONIFIED = 1;
public static final int MAXIMIZED_BOTH = 6;
public static final int MAXIMIZED_HORIZ = 2;
public static final int MAXIMIZED_VERT = 4;
public static final int NORMAL = 0;

// Serialization version constant
private static final long serialVersionUID = 2673458971256075116L;

/**
  * @serial The version of the class data being serialized
  * // FIXME: what is this value?
  */
private int frameSerializedDataVersion;

/**
  * @serial Image used as the icon when this frame is minimized.
  */
private Image icon;

/**
  * @serial Constant used by the JDK Motif peer set.  Not used in
  * this implementation.
  */
private boolean mbManagement;

/**
  * @serial The menu bar for this frame.
  */
//private MenuBar menuBar = new MenuBar();
private MenuBar menuBar;

/**
  * @serial A list of other top-level windows owned by this window.
  */
Vector ownedWindows = new Vector();

/**
  * @serial Indicates whether or not this frame is resizable.
  */
private boolean resizable = true;

/**
  * @serial The state of this frame.
  * // FIXME: What are the values here?
  * This is package-private to avoid an accessor method.
  */
int state;

/**
  * @serial The title of the frame.
  */
private String title = "";

  /**
   * Maximized bounds for this frame.
   */
  private Rectangle maximizedBounds;

  /**
   * This field indicates whether the frame is undecorated or not.
   */
  private boolean undecorated = false;

  /*
   * The number used to generate the name returned by getName.
   */
  private static transient long next_frame_number;

/**
  * Initializes a new instance of <code>Frame</code> that is not visible
  * and has no title.
  */
public
Frame()
{
  this("");
  noteFrame(this);
}

/**
  * Initializes a new instance of <code>Frame</code> that is not visible
  * and has the specified title.
  *
  * @param title The title of this frame.
  */
public
Frame(String title)
{
  super();
  this.title = title;
  // Top-level frames are initially invisible.
  visible = false;
  noteFrame(this);
}

public
Frame(GraphicsConfiguration gc)
{
  super(gc);
  visible = false;
  noteFrame(this);
}

public
Frame(String title, GraphicsConfiguration gc)
{
  super(gc);
  setTitle(title);
  visible = false;
  noteFrame(this);
}

/**
  * Returns this frame's title string.
  *
  * @return This frame's title string.
  */
public String
getTitle()
{
  return(title);
}

/*
 * Sets this frame's title to the specified value.
 *
 * @param title The new frame title.
 */
public synchronized void
setTitle(String title)
{
  this.title = title;
  if (peer != null)
    ((FramePeer) peer).setTitle(title);
}

/**
  * Returns this frame's icon.
  *
  * @return This frame's icon, or <code>null</code> if this frame does not
  * have an icon.
  */
public Image
getIconImage()
{
  return(icon);
}

/**
  * Sets this frame's icon to the specified value.
  *
  * @icon The new icon for this frame.
  */
public synchronized void
setIconImage(Image icon)
{
  this.icon = icon;
  if (peer != null)
    ((FramePeer) peer).setIconImage(icon);
}

/**
  * Returns this frame's menu bar.
  *
  * @return This frame's menu bar, or <code>null</code> if this frame
  * does not have a menu bar.
  */
public MenuBar
getMenuBar()
{
  return(menuBar);
}

/**
  * Sets this frame's menu bar.
  *
  * @param menuBar The new menu bar for this frame.
  */
public synchronized void
setMenuBar(MenuBar menuBar)
{
  if (peer != null)
  {
    if (this.menuBar != null)
      this.menuBar.removeNotify();  
    if (menuBar != null)
      menuBar.addNotify();
    invalidateTree ();
    ((FramePeer) peer).setMenuBar(menuBar);
  }
  this.menuBar = menuBar;
}

/**
  * Tests whether or not this frame is resizable.  This will be 
  * <code>true</code> by default.
  *
  * @return <code>true</code> if this frame is resizable, <code>false</code>
  * otherwise.
  */
public boolean
isResizable()
{
  return(resizable);
}

/**
  * Sets the resizability of this frame to the specified value.
  *
  * @param resizable <code>true</code> to make the frame resizable,
  * <code>false</code> to make it non-resizable.
  */
public synchronized void
setResizable(boolean resizable)
{
  this.resizable = resizable;
  if (peer != null)
    ((FramePeer) peer).setResizable(resizable);
}

/**
  * Returns the cursor type of the cursor for this window.  This will
  * be one of the constants in this class.
  *
  * @return The cursor type for this frame.
  *
  * @deprecated Use <code>Component.getCursor()</code> instead.
  */
public int
getCursorType()
{
  return(getCursor().getType());
}

/**
  * Sets the cursor for this window to the specified type.  The specified
  * type should be one of the constants in this class.
  *
  * @param type The cursor type.
  *
  * @deprecated Use <code>Component.setCursor(Cursor)</code> instead.
  */
public void
setCursor(int type)
{
  setCursor(new Cursor(type));
}

/**
  * Removes the specified component from this frame's menu.
  *
  * @param menu The menu component to remove.
  */
public void
remove(MenuComponent menu)
{
  menuBar.remove(menu);
}

/**
  * Notifies this frame that it should create its native peer.
  */
private static void fireDummyEvent()
{
  EventQueue.invokeLater(new Runnable()
    {
      public void run()
      {
	// Do nothing here.
      }
    });
}

public void
addNotify()
{
  if (menuBar != null)
    menuBar.addNotify();
  if (peer == null)
    peer = getToolkit ().createFrame (this);

  // We now know there's a Frame (us) with a live peer, so we can start the
  // fundamental queue and dispatch thread, by inserting a dummy event.
  if (parent != null && parent.isDisplayable())
    fireDummyEvent();
  
  super.addNotify();
}

public void removeNotify()
{
  if (menuBar != null)
    menuBar.removeNotify();
  super.removeNotify();

  // By now we've been disconnected from the peer, and the peer set to
  // null.  This is formally the same as saying "we just became
  // un-displayable", so we wake up the event queue with a dummy event to
  // see if it's time to shut down.
  fireDummyEvent();
}

  /**
   * Returns a debugging string describing this window.
   *
   * @return A debugging string describing this window.
   */
  protected String paramString ()
  {
    String title = getTitle ();

    String resizable = "";
    if (isResizable ())
      resizable = ",resizable";

    String state = "";
    switch (getState ())
      {
      case NORMAL:
        state = ",normal";
        break;
      case ICONIFIED:
        state = ",iconified";
        break;
      case MAXIMIZED_BOTH:
        state = ",maximized-both";
        break;
      case MAXIMIZED_HORIZ:
        state = ",maximized-horiz";
        break;
      case MAXIMIZED_VERT:
        state = ",maximized-vert";
        break;
      }

    return super.paramString () + ",title=" + title + resizable + state;
  }

private static ArrayList weakFrames = new ArrayList();

private static void noteFrame(Frame f)
{
  weakFrames.add(new WeakReference(f));
}

public static Frame[] getFrames()
{
  int n = 0;
  synchronized (weakFrames)
    {
      Iterator i = weakFrames.iterator();
      while (i.hasNext())
        {
          WeakReference wr = (WeakReference) i.next();
          if (wr.get() != null)
            ++n;
        }
      if (n == 0)
        return new Frame[0];
      else
        {
          Frame[] frames = new Frame[n];
          n = 0;
          i = weakFrames.iterator();
          while (i.hasNext())
            {
              WeakReference wr = (WeakReference) i.next();
              if (wr.get() != null)
                frames[n++] = (Frame) wr.get();
            }
          return frames;
        }
    }
}

  public void setState (int state)
  {
    int current_state = getExtendedState ();

    if (state == NORMAL
        && (current_state & ICONIFIED) != 0)
      setExtendedState (current_state | ICONIFIED);
    
    if (state == ICONIFIED
        && (current_state & ~ICONIFIED) == 0)
      setExtendedState (current_state & ~ICONIFIED);
  }

  public int getState ()
  {
    /* FIXME: State might have changed in the peer... Must check. */
  
    return (state & ICONIFIED) != 0 ? ICONIFIED : NORMAL;
  }

  /**
   * @since 1.4
   */
  public void setExtendedState (int state)
  {
    this.state = state;
  }

  /**
   * @since 1.4
   */
  public int getExtendedState ()
  {
    return state;
  }

  /**
   * @since 1.4
   */
  public void setMaximizedBounds (Rectangle maximizedBounds)
  {
    this.maximizedBounds = maximizedBounds;
  }

  /**
   * Returns the maximized bounds of this frame.
   *
   * @return the maximized rectangle, may be null.
   *
   * @since 1.4
   */
  public Rectangle getMaximizedBounds ()
  {
    return maximizedBounds;
  }

  /**
   * Returns whether this frame is undecorated or not.
   * 
   * @since 1.4
   */
  public boolean isUndecorated ()
  {
    return undecorated;
  }

  /**
   * Disables or enables decorations for this frame. This method can only be
   * called while the frame is not displayable.
   * 
   * @exception IllegalComponentStateException If this frame is displayable.
   * 
   * @since 1.4
   */
  public void setUndecorated (boolean undecorated)
  {
    if (!isDisplayable ())
      throw new IllegalComponentStateException ();

    this.undecorated = undecorated;
  }

  /**
   * Generate a unique name for this frame.
   *
   * @return A unique name for this frame.
   */
  String generateName ()
  {
    return "frame" + getUniqueLong ();
  }

  private static synchronized long getUniqueLong ()
  {
    return next_frame_number++;
  }
  
  protected class AccessibleAWTFrame extends AccessibleAWTWindow
  {
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.FRAME;
    }
    
    public AccessibleStateSet getAccessibleState()
    {
      AccessibleStateSet states = super.getAccessibleStateSet();
      if (isResizable())
        states.add(AccessibleState.RESIZABLE);
      if ((state & ICONIFIED) != 0)
        states.add(AccessibleState.ICONIFIED);
      return states;
    }
  }
  
  /**
   * Gets the AccessibleContext associated with this <code>Frame</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTFrame();
    return accessibleContext;
  }

}
