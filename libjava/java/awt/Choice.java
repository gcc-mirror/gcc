/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;
import java.awt.peer.ChoicePeer;
import java.util.ArrayList;

/** This component lets the user choose an item from a list of
 * Strings.
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 25, 2000
 */
public class Choice extends Component implements ItemSelectable
{
  /** Create a new Choice object.  */
  public Choice ()
  {
    items = new ArrayList ();
    selected = -1;
  }

  /** Add a new item to this Choice object.  If the item is the first
   * item on the list, then it is selected.
   * @param item The new item; must be non-null.
   */
  public synchronized void add (String item)
  {
    if (item == null)
      throw new IllegalArgumentException ("item must be non-null");
    items.add (item);

    int i = items.size () - 1;
    if (peer != null)
      {
	ChoicePeer cp = (ChoicePeer) peer;
	cp.add (item, i);
      }

    if (i == 0)
      select (0);
  }

  /** Add a new item to this Choice object.  This is the same as the
   * add method.  */
  public void addItem (String item)
  {
    add (item);
  }

  /** Add a listener for item events.
   * @param listener The listener to add.
   */
  public synchronized void addItemListener (ItemListener listener)
  {
    listeners = AWTEventMulticaster.add (listeners, listener);
  }

  /** This creates the component's peer.  */
  public void addNotify ()
  {
    if (peer == null)
      peer = getToolkit ().createChoice (this);
    super.addNotify ();
  }

  /** Returns number of items.
   * @deprecated
   */
  public int countItems ()
  {
    return getItemCount ();
  }

  /** Returns an item from this choice.
   * @param index Index of the item.  Indices start at zero.
   */
  public String getItem (int index)
  {
    return (String) items.get (index);
  }

  /** Returns number of items in Choice.  */
  public int getItemCount ()
  {
    return items.size ();
  }

  /** Returns index of selected item; -1 if no item is selected.  */
  public int getSelectedIndex ()
  {
    return selected;
  }

  /** Returns currently selected item; null if no item is selected.  */
  public synchronized String getSelectedItem ()
  {
    return selected == -1 ? null : (String) items.get (selected);
  }

  /** Returns the currently selected item.  */
  public synchronized Object[] getSelectedObjects ()
  {
    // The JCL says this can return null but that breaks the contract
    // for ItemSelectable.
    Object[] r;
    if (selected != -1)
      {
	r = new Object[1];
	r[0] = items.get (selected);
      }
    else
      r = new Object[0];
    return r;
  }

  /** Inserts an item into this Choice.  Existing items are shifted
   * upwards.  If the new item is the only item, then it is selected.
   * If the currently selected item is shifted, then the first item is
   * selected.  If the currently selected item is not shifted, then it
   * remains selected.
   * @param item The new item
   * @param index The position at which to insert it.
   */
  public synchronized void insert (String item, int index)
  {
    if (index > items.size ())
      index = items.size ();
    items.add (index, item);

    if (peer != null)
      {
	ChoicePeer cp = (ChoicePeer) peer;
	cp.add (item, index);
      }

    if (items.size () == 1 || selected >= index)
      select (0);
  }

  /** Generates a String representation of this Choice's state.  */
  protected String paramString ()
  {
    return ("Choice["
	    + "selected=" + selected
	    + "]");
  }

  /** Process an event for this Choice
   * @param event The event the process.
   */
  protected void processEvent (AWTEvent event)
  {
    if (event instanceof ItemEvent)
      processItemEvent ((ItemEvent) event);
    else
      super.processEvent (event);
  }

  /** Process an item event for this Choice.
   * @param event The ItemEvent to process
   */
  protected void processItemEvent (ItemEvent event)
  {
    if (listeners != null)
      listeners.itemStateChanged (event);
  }

  /** Remove an item from this Choice.  If several matches exist, the
   * first one is removed.  If the removed item is selected, the the
   * first item is selected.
   * @param item The item string.
   */
  public synchronized void remove (String item)
  {
    int size = items.size ();
    for (int i = 0; i < size; ++i)
      {
	if (item.equals (items.get (i)))
	  {
	    remove (i);
	    break;
	  }
      }
    throw new IllegalArgumentException ("item \"" + item + "\" not in Choice");
  }

  /** Remove an item from this Choice.  If the removed item is
   * selected, the the first item is selected.
   * @param index Index of the item to remove
   */
  public synchronized void remove (int index)
  {
    items.remove (index);

    if (peer != null)
      {
	ChoicePeer cp = (ChoicePeer) peer;
	cp.remove (index);
      }

    if (index == selected)
      select (0);
    else if (selected > index)
      --selected;
  }

  /** Remove all items from this choice.  */
  public synchronized void removeAll ()
  {
    int oldsize = items.size ();
    items.clear ();
    selected = -1;

    if (peer != null)
      {
	ChoicePeer cp = (ChoicePeer) peer;
	for (int i = 0; i < oldsize; ++i)
	  {
	    // Always remove item 0.
	    cp.remove (0);
	  }
      }
  }

  /** Remove an item listener.
   * @param listener Item listener to remove.
   */
  public synchronized void removeItemListener (ItemListener listener)
  {
    listeners = AWTEventMulticaster.remove (listeners, listener);
  }

  /** Select an item in this Choice.
   * @param item Name of the item to select.
   */
  public synchronized void select (String item)
  {
    int size = items.size ();
    for (int i = 0; i < size; ++i)
      {
	if (item.equals (items.get (i)))
	  {
	    select (i);
	    break;
	  }
      }
  }

  /** Select an item in this choice.
   * @param index Index of item to select.
   */
  public synchronized void select (int index)
  {
    if (index < 0 || index > items.size ())
      throw new IllegalArgumentException ("index out of range");
    selected = index;
    if (peer != null)
      {
	ChoicePeer cp = (ChoicePeer) peer;
	cp.select (index);
      }
  }

  private ItemListener listeners;

  // List of items.
  ArrayList items;
  // Index of selected item.
  int selected;
}
