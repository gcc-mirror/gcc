/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;
import java.awt.peer.CheckboxPeer;

/** This class implements a component which has an on/off state.  Two
 * or more Checkboxes can be grouped by a CheckboxGroup.
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 25, 2000
 */
public class Checkbox extends Component implements ItemSelectable
{
  /** Create a new checkbox.
   * @param label The checkbox label.  A null value is the same as "";
   *              this is the default.
   * @param state The initial check state; defaults to false.
   * @param group The CheckboxGroup.  Defaults to null.
   */
  public Checkbox ()
  {
    this (null, null, false);
  }

  public Checkbox (String label)
  {
    this (label, null, false);
  }

  public Checkbox (String label, boolean state)
  {
    this (label, null, state);
  }

  public Checkbox (String label, boolean state, CheckboxGroup group)
  {
    this (label, group, state);
  }

  public Checkbox (String label, CheckboxGroup group, boolean state)
  {
    this.label = label;
    this.group = group;
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
    if (peer == null)
      peer = getToolkit ().createCheckbox (this);
    super.addNotify ();
  }

  /** Returns the current CheckboxGroup associated with this
   * Checkbox.  */
  public CheckboxGroup getCheckboxGroup ()
  {
    return group;
  }

  /** Returns the current label; might be null.  */
  public String getLabel ()
  {
    return label;
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
  protected String paramString ()
  {
    return ("Checkbox["
	    + "state=" + state + ","
	    + "label=" + label + ","
	    + "group=" + group + "]");
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

  /** Set this checkbox's group.
   * @param group The new group.  null means remove the Checkbox from
   *              its group.
   */
  public void setCheckboxGroup (CheckboxGroup group)
  {
    this.group = group;
    if (peer != null)
      {
	CheckboxPeer cp = (CheckboxPeer) peer;
	cp.setCheckboxGroup (group);
      }
  }

  /** Set the checkbox's label.
   * @param label The new label
   */
  public synchronized void setLabel (String label)
  {
    this.label = label;
    if (peer != null)
      {
	CheckboxPeer cp = (CheckboxPeer) peer;
	// FIXME: unclear what to do here; we err on the side of
	// caution.
	cp.setLabel (label == null ? "" : label);
      }
  }

  /** Set the checkbox's state.
   * @param state The new state.
   */
  public void setState (boolean state)
  {
    this.state = state;
    if (peer != null)
      {
	CheckboxPeer cp = (CheckboxPeer) peer;
	cp.setState (state);
      }
  }

  private ItemListener listeners;

  String label;
  CheckboxGroup group;
  boolean state;
}
