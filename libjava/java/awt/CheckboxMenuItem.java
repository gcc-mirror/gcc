/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;
import java.awt.peer.CheckboxMenuItemPeer;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;

/** This implements a menu item which keeps track of a boolean state.
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 25, 2000
 */
public class CheckboxMenuItem extends MenuItem implements ItemSelectable
{
  /** Create a new CheckboxMenuItem.
   * @param label The checkbox label.  A null value is the same as "";
   *              null is the default.
   * @param state The initial check state; defaults to false.
   */
  public CheckboxMenuItem ()
  {
    this (null, false);
  }

  public CheckboxMenuItem (String label)
  {
    this (label, false);
  }

  public CheckboxMenuItem (String label, boolean state)
  {
    this.label = label;
    this.state = state;
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
    if (peer != null)
      {
	// This choice of toolkit seems unsatisfying, but I'm not sure
	// what else to do.
	peer = Toolkit.getDefaultToolkit ().createCheckboxMenuItem (this);
      }
    super.addNotify ();
  }

  /** Returns this checkbox's label if this checkbox is selected.  */
  public Object[] getSelectedObjects ()
  {
    Object[] r;
    if (state)
      {
	r = new Object[1];
	r[0] = label;
      }
    else
      r = new Object[0];
    return r;
  }

  /** Returns the current state of this checkbox.  */
  public boolean getState ()
  {
    return state;
  }

  /** Generates a String representation of this Checkbox's state.  */
  public String paramString ()
  {
    return ("[" + getClass ().getName ()
	    + "state=" + state + ","
	    + "label=" + label + "]");
  }

  /** Process an event for this Checkbox.
   * @param event The event the process.
   */
  protected void processEvent (AWTEvent event) 
  {
    if (event instanceof ItemEvent)
      processItemEvent ((ItemEvent) event);
    else
      super.processEvent (event);
  }

  /** Process an item event for this Checkbox.
   * @param event The ItemEvent to process
   */
  protected void processItemEvent (ItemEvent event)
  {
    if (listeners != null)
      listeners.itemStateChanged (event);
  }

  /** Remove an item listener.
   * @param listener Item listener to remove.
   */
  public synchronized void removeItemListener (ItemListener listener)
  {
    listeners = AWTEventMulticaster.remove (listeners, listener);
  }

  /** Set the checkbox's state.
   * @param state The new state.
   */
  public void setState (boolean state)
  {
    this.state = state;
    if (peer != null)
      {
	CheckboxMenuItemPeer cp = (CheckboxMenuItemPeer) peer;
	cp.setState (state);
      }
  }

  // Private state.
  String label;
  boolean state;
  ItemListener listeners;
}
