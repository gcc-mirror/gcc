/* CheckboxMenuItem.java -- A menu option with a checkbox on it.
   Copyright (C) 1999, 2000, 2001, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.peer.CheckboxMenuItemPeer;
import java.util.EventListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleValue;

/**
  * This class implements a menu item that has a checkbox on it indicating
  * the selected state of some option.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@redhat.com)
  */
public class CheckboxMenuItem extends MenuItem
  implements ItemSelectable, Accessible
{

/*
 * Static Variables
 */

// Serialization constant
private static final long serialVersionUID = 6190621106981774043L;

/*
 * Instance Variables
 */

/**
  * @serial The state of the checkbox, with <code>true</code> being on and
  * <code>false</code> being off.
  */
private boolean state;

// List of registered ItemListeners
private transient ItemListener item_listeners;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>CheckboxMenuItem</code> with no
  * label and an initial state of off.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless()
  * returns true.
  */
public
CheckboxMenuItem()
{
  this("", false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>CheckboxMenuItem</code> with the
  * specified label and an initial state of off.
  *
  * @param label The label of the menu item.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless()
  * returns true.
  */
public
CheckboxMenuItem(String label)
{
  this(label, false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>CheckboxMenuItem</code> with the
  * specified label and initial state.
  *
  * @param label The label of the menu item.
  * @param state The initial state of the menu item, where <code>true</code>
  * is on, and <code>false</code> is off.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless()
  * returns true.
  */
public
CheckboxMenuItem(String label, boolean state)
{
  super(label);
  this.state = state;

  if (GraphicsEnvironment.isHeadless())
    throw new HeadlessException ();
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the state of this menu item.
  *
  * @return The state of this menu item.
  */
public boolean
getState()
{
  return(state);
}

/*************************************************************************/

/**
  * Sets the state of this menu item.
  *
  * @param state The initial state of the menu item, where <code>true</code>
  * is on, and <code>false</code> is off.
  */
public synchronized void
setState(boolean state)
{
  this.state = state;
  if (peer != null)
    {
      CheckboxMenuItemPeer cp = (CheckboxMenuItemPeer) peer;
      cp.setState (state);
    }
}

/*************************************************************************/

/**
  * Returns an array of length 1 with the menu item label for this object
  * if the state is on.  Otherwise <code>null</code> is returned.
  *
  * @return An array with this menu item's label if it has a state of on,
  * or <code>null</code> otherwise.
  */
public Object[]
getSelectedObjects()
{
  if (state == false)
    return(null);

  Object[] obj = new Object[1];
  obj[0] = getLabel();

  return(obj);
}

/*************************************************************************/

/**
  * Create's this object's native peer
  */
public synchronized void
addNotify()
{
  if (peer == null)
    peer = getToolkit().createCheckboxMenuItem(this);

  super.addNotify ();
}

/*************************************************************************/

/**
  * Adds the specified listener to the list of registered item listeners
  * for this object.
  *
  * @param listener The listener to add.
  */
public synchronized void
addItemListener(ItemListener listener)
{
  item_listeners = AWTEventMulticaster.add(item_listeners, listener);

  enableEvents(AWTEvent.ITEM_EVENT_MASK);
}

/*************************************************************************/

/**
  * Removes the specified listener from the list of registered item
  * listeners for this object.
  *
  * @param listener The listener to remove.
  */
public synchronized void
removeItemListener(ItemListener listener)
{
  item_listeners = AWTEventMulticaster.remove(item_listeners, listener);
}

/*************************************************************************/

/**
  * Processes the specified event by calling <code>processItemEvent()</code>
  * if it is an instance of <code>ItemEvent</code> or calling the superclass
  * method otherwise.
  *
  * @param event The event to process.
  */
protected void
processEvent(AWTEvent event)
{
  if (event instanceof ItemEvent)
    processItemEvent((ItemEvent)event);
  else
    super.processEvent(event);
}

/*************************************************************************/

/**
  * Processes the specified event by dispatching it to any registered listeners.
  *
  * @param event The event to process.
  */
protected void
processItemEvent(ItemEvent event)
{
  if (item_listeners != null)
    item_listeners.itemStateChanged(event);
}

void
dispatchEventImpl(AWTEvent e)
{
  if (e instanceof ItemEvent)
    {
      synchronized (this)
        {
          state = (((ItemEvent) e).getStateChange() == ItemEvent.SELECTED);
        }
    }

  if (e.id <= ItemEvent.ITEM_LAST 
      && e.id >= ItemEvent.ITEM_FIRST
      && (item_listeners != null 
	  || (eventMask & AWTEvent.ITEM_EVENT_MASK) != 0))
    processEvent(e);
  else
    super.dispatchEventImpl(e);
}

/*************************************************************************/

/**
  * Returns a debugging string for this object.
  *
  * @return A debugging string for this object.
  */
public String
paramString()
{
  return ("label=" + getLabel() + ",state=" + state
	  + "," + super.paramString());
}

  /**
   * Returns an array of all the objects currently registered as FooListeners
   * upon this <code>CheckboxMenuItem</code>. FooListeners are registered using
   * the addFooListener method.
   *
   * @exception ClassCastException If listenerType doesn't specify a class or
   * interface that implements java.util.EventListener.
   */
  public EventListener[] getListeners (Class listenerType)
  {
    if (listenerType == ItemListener.class)
      return AWTEventMulticaster.getListeners (item_listeners, listenerType); 
	      
    return super.getListeners (listenerType);
  }

  /**
   * Returns an aray of all item listeners currently registered to this
   * <code>CheckBoxMenuItem</code>.
   */
  public ItemListener[] getItemListeners ()
  {
    return (ItemListener[]) getListeners (ItemListener.class);
  }


  protected class AccessibleAWTCheckboxMenuItem extends AccessibleAWTMenuItem
    implements AccessibleAction, AccessibleValue
  {
    // I think the base class provides the necessary implementation
  }
  
  /**
   * Gets the AccessibleContext associated with this <code>CheckboxMenuItem</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTCheckboxMenuItem();
    return accessibleContext;
  }

} // class CheckboxMenuItem

