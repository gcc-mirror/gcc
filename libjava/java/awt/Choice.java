/* Choice.java -- Java choice button widget.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.peer.ChoicePeer;
import java.io.Serializable;
import java.util.EventListener;
import java.util.Vector;

/**
  * This class implements a drop down choice list.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Choice extends Component implements ItemSelectable, Serializable
{

/*
 * Static Variables
 */

// Serialization constant
private static final long serialVersionUID = -4075310674757313071L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial A list of items for the choice box, which can be <code>null</code>.
  */
private Vector pItems = new Vector();

/**
  * @serial The index of the selected item in the choice box.
  */
private int selectedIndex = -1;

// Listener chain
private ItemListener item_listeners;

/*************************************************************************/

/*
 * Constructors
 */

  /**
   * Initializes a new instance of <code>Choice</code>.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true
   */
  public Choice()
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException ();
  }

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the number of items in the list.
  *
  * @return The number of items in the list.
  */
public int
getItemCount()
{
  return countItems ();
}

/*************************************************************************/

/**
  * Returns the number of items in the list.
  *
  * @return The number of items in the list.
  *
  * @deprecated This method is deprecated in favor of <code>getItemCount</code>.
  */
public int
countItems()
{
  return(pItems.size());
}

/*************************************************************************/

/**
  * Returns the item at the specified index in the list.
  *
  * @param index The index into the list to return the item from.
  *
  * @exception ArrayIndexOutOfBoundsException If the index is invalid.
  */
public String
getItem(int index)
{
  return((String)pItems.elementAt(index));
}

/*************************************************************************/

/**
  * Adds the specified item to this choice box.
  *
  * @param item The item to add.
  *
  * @exception NullPointerException If the item's value is null
  *
  * @since 1.1
  */
public synchronized void
add(String item)
{
  if (item == null)
    throw new NullPointerException ("item must be non-null");

  pItems.addElement(item);

  int i = pItems.size () - 1;
  if (peer != null)
    {
      ChoicePeer cp = (ChoicePeer) peer;
      cp.add (item, i);
    }
}

/*************************************************************************/

/**
  * Adds the specified item to this choice box.
  *
  * This method is oboslete since Java 2 platform 1.1. Please use @see add
  * instead.
  *
  * @param item The item to add.
  *
  * @exception NullPointerException If the item's value is equal to null
  */
public synchronized void
addItem(String item)
{
  add(item);
}

/*************************************************************************/

/** Inserts an item into this Choice.  Existing items are shifted
 * upwards.  If the new item is the only item, then it is selected.
 * If the currently selected item is shifted, then the first item is
 * selected.  If the currently selected item is not shifted, then it
 * remains selected.
 *
 * @param item The item to add.
 * @param index The index at which the item should be inserted.
 *
 * @exception IllegalArgumentException If index is less than 0
 */
public synchronized void
insert(String item, int index)
{
  if (index < 0)
    throw new IllegalArgumentException ("index may not be less then 0");

  if (index > getItemCount ())
    index = getItemCount ();

  pItems.insertElementAt(item, index);

  if (peer != null)
    {
      ChoicePeer cp = (ChoicePeer) peer;
      cp.add (item, index);
    }
}

/*************************************************************************/

/**
  * Removes the specified item from the choice box.
  *
  * @param item The item to remove.
  *
  * @exception IllegalArgumentException If the specified item doesn't exist.
  */
public synchronized void
remove(String item)
{
  int index = pItems.indexOf(item);
  if (index == -1)
    throw new IllegalArgumentException ("item \""
					+ item + "\" not found in Choice");
  remove(index);
}

/*************************************************************************/

/**
  * Removes the item at the specified index from the choice box.
  *
  * @param index The index of the item to remove.
  *
  * @exception IndexOutOfBoundsException If the index is not valid.
  */
public synchronized void
remove(int index)
{
  if ((index < 0) || (index > getItemCount()))
    throw new IllegalArgumentException("Bad index: " + index);

  pItems.removeElementAt(index);

  if (peer != null)
    {
      ChoicePeer cp = (ChoicePeer) peer;
      cp.remove (index);
    }

  if (selectedIndex > index)
    --selectedIndex;
}

/*************************************************************************/

/**
  * Removes all of the objects from this choice box.
  */
public synchronized void
removeAll()
{
  if (getItemCount() <= 0)
    return;
  
  pItems.removeAllElements ();

  if (peer != null)
    {
      ChoicePeer cp = (ChoicePeer) peer;
      cp.removeAll ();
    }

  selectedIndex = -1;
}

/*************************************************************************/

/**
  * Returns the currently selected item, or null if no item is
  * selected.
  *
  * @return The currently selected item.
  */
public synchronized String
getSelectedItem()
{
  return (selectedIndex == -1
	  ? null
	  : ((String)pItems.elementAt(selectedIndex)));
}

/*************************************************************************/

/**
  * Returns an array with one row containing the selected item.
  *
  * @return An array containing the selected item.
  */
public synchronized Object[]
getSelectedObjects()
{
  if (selectedIndex == -1)
    return null;

  Object[] objs = new Object[1];
  objs[0] = pItems.elementAt(selectedIndex);

  return(objs);
}

/*************************************************************************/

/**
  * Returns the index of the selected item.
  *
  * @return The index of the selected item.
  */
public int
getSelectedIndex()
{
  return(selectedIndex);
}

/*************************************************************************/

/**
  * Forces the item at the specified index to be selected.
  *
  * @param index The index of the row to make selected.
  *
  * @exception IllegalArgumentException If the specified index is invalid.
  */
public synchronized void
select(int index)
{
  if ((index < 0) || (index > getItemCount()))
    throw new IllegalArgumentException("Bad index: " + index);

  this.selectedIndex = index;
  if (peer != null)
    {
      ChoicePeer cp = (ChoicePeer) peer;
      cp.select (index);
    }
}

/*************************************************************************/

/**
  * Forces the named item to be selected.
  *
  * @param item The item to be selected.
  *
  * @exception IllegalArgumentException If the specified item does not exist.
  */
public synchronized void
select(String item)
{
  int index = pItems.indexOf(item);
  if (index >= 0)
    select(index);
}

/*************************************************************************/

/**
  * Creates the native peer for this object.
  */
public void
addNotify()
{
  if (peer == null)
    peer = getToolkit ().createChoice (this);
  super.addNotify ();
}

/*************************************************************************/

/**
  * Adds the specified listener to the list of registered listeners for
  * this object.
  *
  * @param listener The listener to add.
  */
public synchronized void
addItemListener(ItemListener listener)
{
  item_listeners = AWTEventMulticaster.add(item_listeners, listener);
}

/*************************************************************************/

/**
  * Removes the specified listener from the list of registered listeners for
  * this object.
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
  * Processes this event by invoking <code>processItemEvent()</code> if the
  * event is an instance of <code>ItemEvent</code>, otherwise the event
  * is passed to the superclass.
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
  * Processes item event by dispatching to any registered listeners.
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
protected String
paramString()
{
  return ("selectedIndex=" + selectedIndex + "," + super.paramString());
}

  /**
   * Returns an array of all the objects currently registered as FooListeners
   * upon this Choice. FooListeners are registered using the addFooListener
   * method.
   *
   * @exception ClassCastException If listenerType doesn't specify a class or
   * interface that implements java.util.EventListener.
   *
   * @since 1.3
   */
  public EventListener[] getListeners (Class listenerType)
  {
    if (listenerType == ItemListener.class)
      return AWTEventMulticaster.getListeners (item_listeners, listenerType);
    
    return super.getListeners (listenerType);
  }

  /**
   * Returns all registered item listeners.
   *
   * @since 1.4
   */
  public ItemListener[] getItemListeners ()
  {
    return (ItemListener[]) getListeners (ItemListener.class);
  }
} // class Choice 
