/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.peer.ListPeer;
import java.awt.event.*;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date April 17, 2001
 * Status: incomplete
 */

public class List extends Component implements ItemSelectable
{
  /** Creates a new scrolling list with four rows.
   * Initially, multiple selections are not allowed.
   */
  public List ()
  {
    this (4, false);
  }

  /** Create a new scrolling list with the indicated number of rows.
   * Initially, multiple selections are not allowed.
   * @param rows Number of rows
   */
  public List (int rows)
  {
    this (rows, false);
  }

  /** Create a new scrolling list with the indicated number of rows.
   * @param rows Number of rows
   * @param multiple True if multiple selections allowed
   */
  public List (int rows, boolean multiple)
  {
    this.rows = rows;
    this.multipleMode = multiple;
  }

  /** Create the peer if it does not already exist.  */
  public void addNotify ()
  {
    if (peer != null)
      peer = getToolkit ().createList (this);
    super.addNotify ();
  }

  public int getItemCount ()
  {
    return items.size ();
  }

  /** @deprecated Use getItemCount() instead.  */
  public int countItems ()
  {
    return getItemCount ();
  }

  public String getItem (int index)
  {
    return (String) items.elementAt (index);
  }

  public String[] getItems ()
  {
    String[] els = new String[items.size ()];
    items.copyInto (els);
    return els;
  }

  public void add (String item)
  {
    add (item, items.size ());
  }

  /** @deprecated Use add() instead.  */
  public void addItem (String item)
  {
    add (item);
  }

  public void add (String item, int index)
  {
    items.insertElementAt (item, index);
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.add (item, index);
      }
  }

  /** @deprecated Use add() instead.  */
  public void addItem (String item, int index)
  {
    add (item, index);
  }

  public void replaceItem (String item, int index)
  {
    items.setElementAt (item, index);
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.delItems (index, index);
	l.add (item, index);
      }
  }

  public void removeAll ()
  {
    items.clear ();
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.removeAll ();
      }
  }

  /** @deprecated Use removeAll() instead.  */
  public void clear ()
  {
    removeAll ();
  }

  public void remove (String item)
  {
    remove (items.indexOf (item));
  }

  public void remove (int index)
  {
    items.removeElementAt (index);
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.delItems (index, index);
      }
  }

  /** @deprecated Use remove() instead.  */
  public void delItem (int index)
  {
    remove (index);
  }

  public int getSelectedIndex ()
  {
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	selected = l.getSelectedIndexes ();
      }

    if (selected == null || selected.length > 1)
      return -1;
    return selected[0];
  }

  public int[] getSelectedIndexes ()
  {
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	selected = l.getSelectedIndexes ();
      }
    return selected;
  }

  public String getSelectedItem ()
  {
    int i = getSelectedIndex ();
    return i == -1 ? null : (String) items.elementAt (i);
  }

  public String[] getSelectedItems ()
  {
    int[] is = getSelectedIndexes ();
    if (is == null)
      return null;
    String[] r = new String[is.length];
    for (int i = 0; i < is.length; ++i)
      r[i] = (String) items.elementAt (is[i]);
    return r;
  }

  public Object[] getSelectedObjects ()
  {
    return getSelectedItems ();
  }

  public void select (int index)
  {
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.select (index);
      }
    else if (selected == null)
      {
	selected = new int[1];
	selected[0] = index;
      }
    else
      {
	int i;
	for (i = 0; i < selected.length; ++i)
	  {
	    if (selected[i] == index)
	      return;
	    if (selected[i] > index)
	      break;
	  }

	int[] ns = new int[selected.length + 1];
	System.arraycopy (selected, 0, ns, 0, i);
	ns[i] = index;
	System.arraycopy (selected, i, ns, i + 1, selected.length - i);

	selected = ns;
      }
  }

  public void deselect (int index)
  {
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.deselect (index);
      }
    else if (selected != null)
      {
	int i;
	for (i = 0; i < selected.length; ++i)
	  {
	    if (selected[i] == index)
	      break;
	  }
	if (i < selected.length)
	  {
	    int[] ns = new int[selected.length - 1];
	    System.arraycopy (selected, 0, ns, 0, i);
	    System.arraycopy (selected, i + 1, ns, i, selected.length - i);
	    selected = ns;
	  }
      }
  }

  public boolean isIndexSelected (int index)
  {
    int[] is = getSelectedIndexes ();
    for (int i = 0; i < is.length; ++i)
      {
	if (is[i] == index)
	  return true;
      }
    return false;
  }

  /** @deprecated Use isIndexSelected().  */
  public boolean isSelected (int index)
  {
    return isIndexSelected (index);
  }

  public int getRows ()
  {
    return rows;
  }

  public boolean isMultipleMode ()
  {
    return multipleMode;
  }

  /** @deprecated Use isMultipleMode().  */
  public boolean allowsMultipleSelections ()
  {
    return isMultipleMode ();
  }

  public void setMultipleMode (boolean multiple)
  {
    this.multipleMode = multiple;
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.setMultipleMode (multiple);
      }
  }

  /** @deprecated Use setMultipleMode().  */
  public void setMultipleSelections (boolean multiple)
  {
    setMultipleMode (multiple);
  }

  public int getVisibleIndex ()
  {
    return visibleIndex;
  }

  public void makeVisible (int index)
  {
    visibleIndex = index;
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.makeVisible (index);
      }
  }

  public Dimension getPreferredSize (int rows)
  {
    return null;		// FIXME
  }

  /** @deprecated Use getPreferredSize(int).  */
  public Dimension preferredSize (int rows)
  {
    return getPreferredSize (rows);
  }

  public Dimension getPreferredSize ()
  {
    return null;		// FIXME
  }

  /** @deprecated Use getPreferredSize().  */
  public Dimension preferredSize ()
  {
    return getPreferredSize ();
  }

  public Dimension getMinimumSize (int rows)
  {
    return null;		// FIXME
  }

  /** @deprecated Use getMinimumSize(int).  */
  public Dimension minimumSize (int rows)
  {
    return getMinimumSize (rows);
  }
  
  public Dimension getMinimumSize ()
  {
    return null;		// FIXME
  }

  /** @deprecated Use getMinimumSize().  */
  public Dimension minimumSize ()
  {
    return getMinimumSize ();
  }

  public void addItemListener (ItemListener listen)
  {
    item_listeners = AWTEventMulticaster.add (item_listeners, listen);
  }

  public void removeItemListener (ItemListener listen)
  {
    item_listeners = AWTEventMulticaster.remove (item_listeners, listen);
  }

  public void addActionListener (ActionListener listen)
  {
    action_listeners = AWTEventMulticaster.add (action_listeners, listen);
  }

  public void removeActionListener (ActionListener listen)
  {
    action_listeners = AWTEventMulticaster.remove (action_listeners, listen);
  }

  protected void processEvent (AWTEvent e)
  {
    if (e instanceof ItemEvent)
      processItemEvent ((ItemEvent) e);
    else if (e instanceof ActionEvent)
      processActionEvent ((ActionEvent) e);
    else
      super.processEvent (e);
  }

  protected void processItemEvent (ItemEvent e)
  {
    if (item_listeners != null)
      item_listeners.itemStateChanged (e);
  }

  protected void processActionEvent (ActionEvent e)
  {
    if (action_listeners != null)
      action_listeners.actionPerformed (e);
  }

  protected String paramString ()
  {
    return ("List[multiple=" + multipleMode
	    + ",rows=" + rows
	    + "]");
  }

  /** @deprecated */
  public void delItems (int start, int end)
  {
    for (int i = end; i >= start; --i)
      items.removeElementAt (i);
    if (peer != null)
      {
	ListPeer l = (ListPeer) peer;
	l.delItems (start, end);
      }
  }

  // Vector of items in the list.
  private Vector items;
  // True if multiple selection mode enabled.
  private boolean multipleMode;
  // Number of rows.
  private int rows;
  // Array of indices of selected items.  When there is no peer, we
  // maintain this in place.  When there is a peer, the peer maintains
  // the list and we ask for it whenever needed.
  private int[] selected;
  // Value used by makeVisible().
  private int visibleIndex;

  // Listeners.
  private ActionListener action_listeners;
  private ItemListener item_listeners;
}
