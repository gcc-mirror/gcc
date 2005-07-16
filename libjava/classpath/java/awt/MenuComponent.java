/* MenuComponent.java -- Superclass of all AWT menu components
   Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.peer.MenuComponentPeer;
import java.io.Serializable;
import java.util.Locale;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.accessibility.AccessibleStateSet;

/**
  * This is the superclass of all menu AWT widgets. 
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
  */
public abstract class MenuComponent implements Serializable
{

/*
 * Static Variables
 */

// Serialization Constant
private static final long serialVersionUID = -4536902356223894379L;

/*************************************************************************/
  
/*
 * Instance Variables
 */

/**
 * The font for this component.
 *
 * @see #getFont()
 * @see #setFont(java.awt.Font)
 * @serial the component's font.
 */
  private Font font;

  /**
   * The name of the component.
   *
   * @see #getName()
   * @see #setName(String)
   * @serial the component's name.
   */
  private String name;

  /**
   * The parent of this component.
   *
   * @see #getParent()
   * @see #setParent(java.awt.MenuContainer)
   * @serial ignored.
   */
  transient MenuContainer parent;

  /**
   * The native peer for this component.
   *
   * @see #getPeer()
   * @see #setPeer(java.awt.peer.MenuComponentPeer)
   * @serial ignored.
   */
  transient MenuComponentPeer peer;

  /**
   * The synchronization locking object for this component.
   *
   * @serial ignored.
   */
  private transient Object tree_lock = this;

  /**
   * The toolkit for this object.
   *
   * @see #getToolkit()
   * @serial ignored.
   */
  private static transient Toolkit toolkit = Toolkit.getDefaultToolkit();

  /**
   * The accessible context for this component.
   *
   * @see #getAccessibleContext()
   * @serial the accessibility information for this component.
   */
  AccessibleContext accessibleContext;

  /**
   * Was the name of the component set?  This value defaults
   * to false and becomes true after a call to <code>setName()</code>.
   * Please note that this does not guarantee that name will then
   * be non-null, as this may be the value passed to <code>setName()</code>.
   *
   * @see #setName(String)
   * @serial true if the name value has been explicitly set by calling
   *         <code>setName()</code>.
   */
  private boolean nameExplicitlySet;

  /**
   * Does this component handle new events?  Events will be handled
   * by this component if this is true.  Otherwise, they will be forwarded
   * up the component hierarchy.  This implementation does not use this
   * variable; it is merely provided for serialization compatability.
   *
   * @see #dispatchEvent(AWTEvent)
   * @serial true if events are to be processed locally.  Unused.
   */
  private boolean newEventsOnly;

  /**
   * The focus listener chain handler which deals with focus events for
   * the accessible context of this component.
   *
   * @see AccessibleAWTMenuComponent#addFocusListener(java.awt.event.FocusListener)
   * @serial ignored.
   * This is package-private to avoid an accessor method.
   */
  transient FocusListener focusListener;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Default constructor for subclasses.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
MenuComponent()
{
  if (GraphicsEnvironment.isHeadless())
    throw new HeadlessException ();
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the font in use for this component.
  *
  * @return The font for this component.
  */
public Font
getFont()
{
  if (font != null)
    return font;

  if (parent != null)
    return parent.getFont ();

  return null;
}

/*************************************************************************/

/**
  * Sets the font for this component to the specified font.
  *
  * @param font The new font for this component.
  */
public void
setFont(Font font)
{
  this.font = font;
}

/*************************************************************************/

/**
  * Returns the name of this component.
  *
  * @return The name of this component.
  */
public String
getName()
{
  return(name);
}

/*************************************************************************/

/**
  * Sets the name of this component to the specified name.
  *
  * @param name The new name of this component.
  */
public void
setName(String name)
{
  this.name = name;
  nameExplicitlySet = true;
}

/*************************************************************************/

/**
  * Returns the parent of this component.
  * 
  * @return The parent of this component.
  */
public MenuContainer
getParent()
{
  return(parent);
} 

/*************************************************************************/

// Sets the parent of this component.
final void
setParent(MenuContainer parent)
{
  this.parent = parent;
}

/*************************************************************************/

/**
  * Returns the native windowing system peer for this component.
  *
  * @return The peer for this component.
  *
  * @deprecated
  */
public MenuComponentPeer
getPeer()
{
  return(peer);
}

/*************************************************************************/

// Sets the peer for this component.
final void
setPeer(MenuComponentPeer peer)
{
  this.peer = peer;
}

/*************************************************************************/

/**
  * Destroys this component's native peer
  */
public void
removeNotify()
{
  if (peer != null)
    peer.dispose();
  peer = null;
}

/*************************************************************************/

/**
  * Returns the toolkit in use for this component.
  *
  * @return The toolkit for this component.
  */
final Toolkit
getToolkit()
{
  return(toolkit);
}

/*************************************************************************/

/**
  * Returns the object used for synchronization locks on this component
  * when performing tree and layout functions.
  *
  * @return The synchronization lock for this component.
  */
protected final Object
getTreeLock()
{
  return(tree_lock);
}

/*************************************************************************/

// The sync lock object for this component.
final void
setTreeLock(Object tree_lock)
{
  this.tree_lock = tree_lock;
}

/*************************************************************************/

/**
  * AWT 1.0 event dispatcher.
  *
  * @deprecated Deprecated in favor of <code>dispatchEvent()</code>.
  * @return true if the event was dispatched, false otherwise.
  */
public boolean
postEvent(Event event)
{
  // This is overridden by subclasses that support events.
  return false;
}
/*************************************************************************/

/**
  * Sends this event to this component or a subcomponent for processing.
  *
  * @param event The event to dispatch
  */
public final void dispatchEvent(AWTEvent event)
{
  // See comment in Component.dispatchEvent().
  dispatchEventImpl(event);
}


/**
 * Implementation of dispatchEvent. Allows trusted package classes
 * to dispatch additional events first.  This implementation first
 * translates <code>event</code> to an AWT 1.0 event and sends the
 * result to {@link #postEvent}.  The event is then
 * passed on to {@link #processEvent} for local processing.
 *
 * @param event the event to dispatch.
 */
void dispatchEventImpl(AWTEvent event)
{
  Event oldStyleEvent;

  // This is overridden by subclasses that support events.
  /* Convert AWT 1.1 event to AWT 1.0 event */
  oldStyleEvent = Component.translateEvent(event);
  if (oldStyleEvent != null)
    {
      postEvent(oldStyleEvent);
    }
  /* Do local processing */
  processEvent(event);
}

/*************************************************************************/

/**
  * Processes the specified event.  In this class, this method simply
  * calls one of the more specific event handlers.
  * 
  * @param event The event to process.
  */
protected void
processEvent(AWTEvent event)
{
  /* 
     Pass a focus event to the focus listener for
     the accessibility context.
  */
  if (event instanceof FocusEvent)
    {
      if (focusListener != null)
        {
          switch (event.id)
            {
            case FocusEvent.FOCUS_GAINED:
              focusListener.focusGained((FocusEvent) event);
              break;
            case FocusEvent.FOCUS_LOST:
              focusListener.focusLost((FocusEvent) event);
              break;
            }
        }
    }
}

/*************************************************************************/

/**
  * Returns a string representation of this component.
  *
  * @return A string representation of this component
  */
public String
toString()
{
  return this.getClass().getName() + "[" + paramString() + "]";
}

/*************************************************************************/

/**
 * Returns a debugging string for this component
 */
protected String
paramString()
{
  return "name=" + getName();
}

/**
 * Gets the AccessibleContext associated with this <code>MenuComponent</code>.
 * As an abstract class, we return null.  Concrete subclasses should return
 * their implementation of the accessibility context.
 *
 * @return null.
 */

public AccessibleContext getAccessibleContext()
{
  return null;
}

/**
 * This class provides a base for the accessibility support of menu
 * components.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
protected abstract class AccessibleAWTMenuComponent
  extends AccessibleContext
  implements Serializable, AccessibleComponent, AccessibleSelection
{

  /**
   * Compatible with JDK 1.4.2 revision 5
   */
  private static final long serialVersionUID = -4269533416223798698L;

  /**
   * This is the default constructor.  It should be called by
   * concrete subclasses to ensure necessary groundwork is completed.
   */
  protected AccessibleAWTMenuComponent()
  {
  }

  /**
   * Replaces or supplements the component's selection with the
   * <code>Accessible</code> child at the supplied index.  If
   * the component supports multiple selection, the child is
   * added to the current selection.  Otherwise, the current
   * selection becomes the specified child.  If the child is
   * already selected, nothing happens.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   *
   * @param index the index of the specified child within a
   *        zero-based list of the component's children.
   */
  public void addAccessibleSelection(int index)
  {
    /* Subclasses with children should implement this */
  }

  /**
   * Registers the specified focus listener to receive
   * focus events from this component.
   *
   * @param listener the new focus listener.
   */
  public void addFocusListener(FocusListener listener)
  {
    /*
     * Chain the new focus listener to the existing chain
     * of focus listeners.  Each new focus listener is
     * coupled via multicasting to the existing chain.
     */
    focusListener = AWTEventMulticaster.add(focusListener, listener);
  }

  /**
   * Clears the component's current selection.  Following
   * the calling of this method, no children of the component
   * will be selected.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   */
  public void clearAccessibleSelection()
  {
  }

  /**
   * Returns true if the specified point lies within the
   * component.  The supplied co-ordinates are assumed to
   * be relative to the co-ordinate system of the component
   * itself.  Thus, the point (0,0) is the upper left corner
   * of this component.
   * <br />
   * <br />
   * Please note that this method depends on a correctly implemented
   * version of the <code>getBounds()</code> method.  Subclasses
   * must provide the bounding rectangle via <code>getBounds()</code>
   * in order for this method to work.  
   *
   * @param point the point to check against this component.
   * @return true if the point is within this component.
   * @see #getBounds()
   */
  public boolean contains(Point point)
  {
    /* 
       We can simply return the result of a
       test for containment in the bounding rectangle 
    */
    return getBounds().contains(point);
  }

  /**
   * Returns the <code>Accessible</code> child of this component present
   * at the specified point.  The supplied co-ordinates are
   * assumed to be relative to the co-ordinate system of this
   * component (the parent of any returned accessible).  Thus,
   * the point (0,0) is the upper left corner of this menu
   * component.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   * 
   * @param point the point at which the returned accessible
   *        is located.
   * @return null.
   */
  public Accessible getAccessibleAt(Point point)
  {
    return null;
  }

  /**
   * Returns the <code>Accessible</code> child at the supplied
   * index within the list of children of this component.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   *
   * @param index the index of the <code>Accessible</code> child
   *        to retrieve.
   * @return null.
   */
  public Accessible getAccessibleChild(int index)
  {
    return null;
  }

  /**
   * Returns the number of children of this component which
   * implement the <code>Accessible</code> interface.  If
   * all children of this component are accessible, then
   * the returned value will be the same as the number of
   * children.
   * <br />
   * <br />
   *
   * @return 0.
   */
  public int getAccessibleChildrenCount()
  {
    return 0;
  }

  /**
   * Retrieves the <code>AccessibleComponent</code> associated
   * with this accessible context and its component.  As the
   * context itself implements <code>AccessibleComponent</code>,
   * this is the return value.
   *
   * @return the context itself.
   */
  public AccessibleComponent getAccessibleComponent()
  {
    return this;
  }

  /**
   * Returns the accessible name for this menu component.  This
   * is the name given to the component, which may be null if
   * not set using <code>setName()</code>.
   * <br />
   * <br />
   * The name is not the most appropriate description of this
   * object.  Subclasses should preferably provide a more
   * accurate description.  For example, a File menu could
   * have the description `Lists commands related to the
   * file system'.
   *
   * @return a description of the component.  Currently,
   *         this is just the contents of the name property.
   * @see MenuComponent#setName(String)
   */
  public String getAccessibleDescription()
  {
    return MenuComponent.this.getName();
  }

  /**
   * Retrieves the index of this component within its parent.
   * If no parent exists, -1 is returned.
   *
   * @return -1 as the parent, a <code>MenuContainer</code>
   *         is not <code>Accessible</code>.
   */
  public int getAccessibleIndexInParent()
  {
    return -1;
  }

  /**
   * Returns the accessible name of this component.  This
   * is the name given to the component, which may be null if
   * not set using <code>setName()</code>.
   * <br />
   * <br />
   * The name property is not the most suitable string to return
   * for this method.  The string should be localized, and
   * relevant to the operation of the component.  For example,
   * it could be the text of a menu item.  However, this can
   * not be used at this level of abstraction, so it is the
   * responsibility of subclasses to provide a more appropriate
   * name.
   *
   * @return a localized name for this component.  Currently, this
   *         is just the contents of the name property.
   * @see MenuComponent#setName(String)
   */
  public String getAccessibleName()
  {
    return MenuComponent.this.getName();
  }

  /**
   * Returns the <code>Accessible</code> parent of this component.
   * As the parent of a <code>MenuComponent</code> is a
   * <code>MenuContainer</code>, which doesn't implement
   * <code>Accessible</code>, this method returns null.
   *
   * @return null.
   */
  public Accessible getAccessibleParent()
  {
    return null;
  }

  /**
   * Returns the accessible role of this component.
   * <br />
   * <br />
   * The abstract implementation of this method returns
   * <code>AccessibleRole.AWT_COMPONENT</code>,
   * as the abstract component has no specific role.  This
   * method should be overridden by concrete subclasses, so
   * as to return an appropriate role for the component.
   *
   * @return <code>AccessibleRole.AWT_COMPONENT</code>.
   */
  public AccessibleRole getAccessibleRole()
  {
    return AccessibleRole.AWT_COMPONENT;
  }

  /**
   * Retrieves the <code>AccessibleSelection</code> associated
   * with this accessible context and its component.  As the
   * context itself implements <code>AccessibleSelection</code>,
   * this is the return value.
   *
   * @return the context itself.
   */
  public AccessibleSelection getAccessibleSelection()
  {
    return this;
  }

  /**
   * Retrieves the <code>Accessible</code> selected child
   * at the specified index.  If there are no selected children
   * or the index is outside the range of selected children,
   * null is returned.  Please note that the index refers
   * to the index of the child in the list of <strong>selected
   * children</strong>, and not the index of the child in
   * the list of all <code>Accessible</code> children.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   *
   * @param index the index of the selected <code>Accessible</code>
   *        child.
   */
  public Accessible getAccessibleSelection(int index)
  {
    return null;
  }

  /**
   * Returns a count of the number of <code>Accessible</code>
   * children of this component which are currently selected.
   * If there are no children currently selected, 0 is returned.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   *
   * @return 0.
   */
  public int getAccessibleSelectionCount()
  {
    return 0;
  }

  /**
   * Retrieves the current state of this component
   * in an accessible form.  For example, a given component
   * may be visible, selected, disabled, etc.
   * <br />
   * <br />
   * As this class tells us virtually nothing about the component,
   * except for its name and font, no state information can be
   * provided.  This implementation thus returns an empty
   * state set, and it is left to concrete subclasses to provide
   * a more acceptable and relevant state set.  Changes to these
   * properties also need to be handled using
   * <code>PropertyChangeListener</code>s.
   *
   * @return an empty <code>AccessibleStateSet</code>.
   */
  public AccessibleStateSet getAccessibleStateSet()
  {
    return new AccessibleStateSet();
  }

  /**
   * Returns the background color of the component, or null
   * if this property is unsupported.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply returns the
   * default system background color used for rendering menus.
   * Concrete subclasses which handle the drawing of an onscreen
   * menu component should override this method and provide
   * the appropriate information.
   *
   * @return the default system background color for menus.
   * @see #setBackground(java.awt.Color)
   */
  public Color getBackground()
  {
    return SystemColor.menu;
  }

  /**
   * Returns a <code>Rectangle</code> which represents the
   * bounds of this component.  The returned rectangle has the
   * height and width of the component's bounds, and is positioned
   * at a location relative to this component's parent, the
   * <code>MenuContainer</code>.  null is returned if bounds
   * are not supported by the component.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply returns null.
   * Concrete subclasses which handle the drawing of an onscreen
   * menu component should override this method and provide
   * the appropriate information.
   *
   * @return null.
   * @see #setBounds(java.awt.Rectangle)
   */
  public Rectangle getBounds()
  {
    return null;
  }

  /**
   * Returns the <code>Cursor</code> displayed when the pointer
   * is positioned over this component.  Alternatively, null
   * is returned if the component doesn't support the cursor
   * property.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply returns the default
   * system cursor.  Concrete subclasses which handle the drawing
   * of an onscreen menu component may override this method and provide
   * the appropriate information.
   *
   * @return the default system cursor.
   * @see #setCursor(java.awt.Cursor)
   */
  public Cursor getCursor()
  {
    return Cursor.getDefaultCursor();
  }

  /**
   * Returns the <code>Font</code> used for text created by this component.
   *
   * @return the current font.
   * @see #setFont(java.awt.Font)
   */
  public Font getFont()
  {
    return MenuComponent.this.getFont();
  }

  /**
   * Retrieves information on the rendering and metrics of the supplied
   * font.  If font metrics are not supported by this component, null
   * is returned.
   * <br />
   * <br />
   * The abstract implementation of this method simply uses the toolkit
   * to obtain the <code>FontMetrics</code>.  Concrete subclasses may
   * find it more efficient to invoke their peer class directly, if one
   * is available.
   *
   * @param font the font about which to retrieve rendering and metric
   *        information.
   * @return the metrics of the given font, as provided by the system
   *         toolkit.
   * @throws NullPointerException if the supplied font was null.
   */
  public FontMetrics getFontMetrics(Font font)
  {
    return MenuComponent.this.getToolkit().getFontMetrics(font);
  }

  /**
   * Returns the foreground color of the component, or null
   * if this property is unsupported.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply returns the
   * default system text color used for rendering menus.
   * Concrete subclasses which handle the drawing of an onscreen
   * menu component should override this method and provide
   * the appropriate information.
   *
   * @return the default system text color for menus.
   * @see #setForeground(java.awt.Color)
   */
  public Color getForeground()
  {
    return SystemColor.menuText;
  }

  /**
   * Returns the locale currently in use by this component.
   * <br />
   * <br />
   * This abstract class has no property relating to the
   * locale used by the component, so this method simply
   * returns the default locale for the current instance
   * of the Java Virtual Machine (JVM).  Concrete subclasses
   * which maintain such a property should override this method
   * and provide the locale information more accurately.
   *
   * @return the default locale for this JVM instance.
   */
  public Locale getLocale()
  {
    return Locale.getDefault();
  }

  /**
   * Returns the location of the component, with co-ordinates
   * relative to the parent component and using the co-ordinate
   * space of the screen.  Thus, the point (0,0) is the upper
   * left corner of the parent component.
   * <br />
   * <br />
   * Please note that this method depends on a correctly implemented
   * version of the <code>getBounds()</code> method.  Subclasses
   * must provide the bounding rectangle via <code>getBounds()</code>
   * in order for this method to work.  
   *
   * @return the location of the component, relative to its parent.
   * @see #setLocation(java.awt.Point)
   */
  public Point getLocation()
  {
    /* Simply return the location of the bounding rectangle */
    return getBounds().getLocation();
  }

  /**
   * Returns the location of the component, with co-ordinates
   * relative to the screen.  Thus, the point (0,0) is the upper
   * left corner of the screen.  null is returned if the component
   * is either not on screen or if this property is unsupported.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply returns null.
   * Concrete subclasses which handle the drawing of an onscreen
   * menu component should override this method and provide
   * the appropriate information.
   *
   * @return the location of the component, relative to the screen.
   */
  public Point getLocationOnScreen()
  {
    return null;
  }

  /**
   * Returns the size of the component.
   * <br />
   * <br />
   * Please note that this method depends on a correctly implemented
   * version of the <code>getBounds()</code> method.  Subclasses
   * must provide the bounding rectangle via <code>getBounds()</code>
   * in order for this method to work.  
   *
   * @return the size of the component.
   * @see #setSize(java.awt.Dimension)
   */
  public Dimension getSize()
  {
    /* Simply return the size of the bounding rectangle */
    return getBounds().getSize();
  }

  /**
   * Returns true if the accessible child specified by the supplied index
   * is currently selected.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   *
   * @param index the index of the accessible child to check for selection.
   * @return false.
   */
  public boolean isAccessibleChildSelected(int index)
  {
    return false;
  }

  /**
   * Returns true if this component is currently enabled.
   * <br />
   * <br />
   * As this abstract component has no properties related to
   * its enabled or disabled state, the implementation of this
   * method is left to subclasses.
   *
   * @return false.
   * @see #setEnabled(boolean)
   */
  public boolean isEnabled()
  {
    return false;
  }

  /**
   * Returns true if this component is included in the traversal
   * of the current focus from one component to the other.
   * <br />
   * <br />
   * As this abstract component has no properties related to
   * its ability to accept the focus, the implementation of this
   * method is left to subclasses.
   *
   * @return false.
   */
  public boolean isFocusTraversable()
  {
    return false;
  }

  /**
   * Returns true if the component is being shown on screen.
   * A component is determined to be shown if it is visible,
   * and each parent component is also visible.  Please note
   * that, even when a component is showing, it may still be
   * obscured by other components in front.  This method only
   * determines if the component is being drawn on the screen.
   * <br />
   * <br />
   * As this abstract component and its parent have no properties
   * relating to visibility, the implementation of this method is
   * left to subclasses.
   *
   * @return false.
   * @see #isVisible()
   */
  public boolean isShowing()
  {
    return false;
  }

  /**
   * Returns true if the component is visible.  A component may
   * be visible but not drawn on the screen if one of its parent
   * components is not visible.  To determine if the component is
   * actually drawn on screen, <code>isShowing()</code> should be
   * used.
   * <br />
   * <br />
   * As this abstract component has no properties relating to its
   * visibility, the implementation of this method is left to subclasses.
   *
   * @return false.
   * @see #isShowing()
   * @see #setVisible(boolean)
   */
  public boolean isVisible()
  {
    return false;
  }

  /**
   * Removes the accessible child specified by the supplied index from
   * the list of currently selected children.  If the child specified
   * is not selected, nothing happens.
   * <br />
   * <br />
   * As the existence of children can not be determined from
   * this abstract class, the implementation of this method
   * is left to subclasses.
   *
   * @param index the index of the <code>Accessible</code> child.
   */
  public void removeAccessibleSelection(int index)
  {
    /* Subclasses with children should implement this */
  }

  /**
   * Removes the specified focus listener from the list of registered
   * focus listeners for this component.
   *
   * @param listener the listener to remove.
   */
  public void removeFocusListener(FocusListener listener)
  {
    /* Remove the focus listener from the chain */
    focusListener = AWTEventMulticaster.remove(focusListener, listener);
  }

  /**
   * Requests that this component gains focus.  This depends on the
   * component being focus traversable.
   * <br />
   * <br />
   * As this abstract component has no properties relating to its
   * focus traversability, or access to a peer with request focusing
   * abilities, the implementation of this method is left to subclasses.
   */
  public void requestFocus()
  {
    /* Ignored */
  }

  /**
   * Selects all <code>Accessible</code> children of this component which
   * it is possible to select.  The component needs to support multiple
   * selections.
   * <br />
   * <br />
   * This abstract component provides a simplistic implementation of this
   * method, which ignores the ability of the component to support multiple
   * selections and simply uses <code>addAccessibleSelection</code> to
   * add each <code>Accessible</code> child to the selection.  The last
   * <code>Accessible</code> component is thus selected for components
   * which don't support multiple selections.  Concrete implementations should
   * override this with a more appopriate and efficient implementation, which
   * properly takes into account the ability of the component to support multiple
   * selections. 
   */
  public void selectAllAccessibleSelection()
  {
    /* Simply call addAccessibleSelection() on all accessible children */
    for (int a = 0; a < getAccessibleChildrenCount(); ++a)
      {
        addAccessibleSelection(a);
      }
  }

  /**
   * Sets the background color of the component to that specified.
   * Unspecified behaviour occurs when null is given as the new
   * background color.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply ignores the supplied
   * color and continues to use the default system color.   
   * Concrete subclasses which handle the drawing of an onscreen
   * menu component should override this method and provide
   * the appropriate information.
   *
   * @param color the new color to use for the background.
   * @see getBackground()
   */
  public void setBackground(Color color)
  {
    /* Ignored */
  }

  /**
   * Sets the height and width of the component, and its position
   * relative to this component's parent, to the values specified
   * by the supplied rectangle.  Unspecified behaviour occurs when
   * null is given as the new bounds.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply ignores the new
   * rectangle and continues to return null from <code>getBounds()</code>.
   * Concrete subclasses which handle the drawing of an onscreen
   * menu component should override this method and provide
   * the appropriate information.
   *
   * @param rectangle a rectangle which specifies the new bounds of
   *        the component.
   * @see #getBounds()
   */
  public void setBounds(Rectangle rectangle)
  {
    /* Ignored */
  }

  /**
   * Sets the <code>Cursor</code> used when the pointer is positioned over the
   * component.  Unspecified behaviour occurs when null is given as the new
   * cursor.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply ignores the new cursor
   * and continues to return the default system cursor.  Concrete
   * subclasses which handle the drawing of an onscreen menu component
   * may override this method and provide the appropriate information.
   *
   * @param cursor the new cursor to use.
   * @see #getCursor()
   */
  public void setCursor(Cursor cursor)
  {
    /* Ignored */
  }

  /**
   * Sets the enabled/disabled state of this component.
   * <br />
   * <br />
   * As this abstract component has no properties related to
   * its enabled or disabled state, the implementation of this
   * method is left to subclasses.
   *
   * @param enabled true if the component should be enabled,
   *        false otherwise.
   * @see #getEnabled()
   */
  public void setEnabled(boolean enabled)
  {
    /* Ignored */
  }

  /**
   * Sets the <code>Font</code> used for text created by this component.
   * Unspecified behaviour occurs when null is given as the new
   * font.
   *
   * @param font the new font to use for text.
   * @see #getFont()
   */
  public void setFont(Font font)
  {
    /* Call the method of the enclosing component */
    MenuComponent.this.setFont(font);
  }

  /**
   * Sets the foreground color of the component to that specified.
   * Unspecified behaviour occurs when null is given as the new
   * background color.
   * <br />
   * <br />
   * This abstract class knows nothing about how the component
   * is drawn on screen, so this method simply ignores the supplied
   * color and continues to return the default system text color used
   * for rendering menus.
   * Concrete subclasses which handle the drawing of an onscreen
   * menu component should override this method and provide
   * the appropriate information.
   *
   * @param color the new foreground color.
   * @see #getForeground()
   */
  public void setForeground(Color color)
  {
    /* Ignored */
  }

  /**
   * Sets the location of the component, with co-ordinates
   * relative to the parent component and using the co-ordinate
   * space of the screen.  Thus, the point (0,0) is the upper
   * left corner of the parent component.
   * <br />
   * <br />
   * Please note that this method depends on a correctly implemented
   * version of the <code>getBounds()</code> method.  Subclasses
   * must provide the bounding rectangle via <code>getBounds()</code>
   * in order for this method to work.  
   *
   * @param point the location of the component, relative to its parent.
   * @see #getLocation()
   */
  public void setLocation(Point point)
  {
    getBounds().setLocation(point);
  }

  /**
   * Sets the size of the component.
   * <br />
   * <br />
   * Please note that this method depends on a correctly implemented
   * version of the <code>getBounds()</code> method.  Subclasses
   * must provide the bounding rectangle via <code>getBounds()</code>
   * in order for this method to work.  
   *
   * @param size the new size of the component.
   * @see #getSize()
   */
  public void setSize(Dimension size)
  {
    getBounds().setSize(size);
  }

  /**
   * Sets the visibility state of the component.  A component may
   * be visible but not drawn on the screen if one of its parent
   * components is not visible.  To determine if the component is
   * actually drawn on screen, <code>isShowing()</code> should be
   * used.
   * <br />
   * <br />
   * As this abstract component has no properties relating to its
   * visibility, the implementation of this method is left to subclasses.
   *
   * @param visibility the new visibility of the component -- true if
   *        the component is visible, false if not.
   * @see #isShowing()
   * @see #isVisible()
   */
  public void setVisible(boolean visibility)
  {
    /* Ignored */
  }

} /* class AccessibleAWTMenuComponent */
          

} // class MenuComponent
