/* Checkbox.java -- An AWT checkbox widget
   Copyright (C) 1999, 2000, 2001, 2002, 2005  Free Software Foundation, Inc.

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
import java.awt.peer.CheckboxPeer;
import java.io.Serializable;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;

/**
 * This class implements a component which has an on/off state.  Two
 * or more Checkboxes can be grouped by a CheckboxGroup.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@redhat.com)
 */
public class Checkbox extends Component
  implements ItemSelectable, Accessible, Serializable
{

// FIXME: Need readObject/writeObject for this.

/*
 * Static Variables
 */

// Serialization Constant
private static final long serialVersionUID = 7270714317450821763L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The checkbox group for this checkbox.
  */
private CheckboxGroup group;

/**
  * @serial The label on this checkbox.
  */
private String label;

/**
  * @serial The state of this checkbox.
  * This is package-private to avoid an accessor method.
  */
boolean state;

// The list of listeners for this object.
private transient ItemListener item_listeners;

  /*
   * The number used to generate the name returned by getName.
   */
  private static transient long next_checkbox_number;

/**
 * This class provides accessibility support for the
 * checkbox.
 *
 * @author Jerry Quinn  (jlquinn@optonline.net)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
protected class AccessibleAWTCheckbox
  extends AccessibleAWTComponent
  implements ItemListener, AccessibleAction, AccessibleValue
{
  /**
   * Serialization constant to match JDK 1.5
   */
  private static final long serialVersionUID = 7881579233144754107L;

  /**
   * Default constructor which simply calls the
   * super class for generic component accessibility
   * handling.
   */
  public AccessibleAWTCheckbox()
  {
    super();
  }

  /**
   * Captures changes to the state of the checkbox and
   * fires appropriate accessible property change events.
   *
   * @param event the event fired.
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  public void itemStateChanged(ItemEvent event)
  {
    firePropertyChange(ACCESSIBLE_STATE_PROPERTY,
		       state ? null : AccessibleState.CHECKED,
                       state ? AccessibleState.CHECKED : null);
  }
  
  /**
   * Returns an implementation of the <code>AccessibleAction</code>
   * interface for this accessible object.  In this case, the
   * current instance is simply returned (with a more appropriate
   * type), as it also implements the accessible action as well as
   * the context.
   *
   * @return the accessible action associated with this context.
   * @see javax.accessibility.AccessibleAction
   */
  public AccessibleAction getAccessibleAction()
  {
    return this;
  }
  
  /**
   * Returns an implementation of the <code>AccessibleValue</code>
   * interface for this accessible object.  In this case, the
   * current instance is simply returned (with a more appropriate
   * type), as it also implements the accessible value as well as
   * the context.
   *
   * @return the accessible value associated with this context.
   * @see javax.accessibility.AccessibleValue
   */
  public AccessibleValue getAccessibleValue()
  {
    return this;
  }
  
  /* 
   * The following methods are implemented in the JDK (up to
   * 1.5) as stubs.  We do likewise here.
   */

  /**
   * Returns the number of actions associated with this accessible
   * object.  This default implementation returns 0.
   *
   * @return the number of accessible actions available.
   * @see javax.accessibility.AccessibleAction#getAccessibleActionCount()
   */
  public int getAccessibleActionCount()
  {
    // 1.4.1 and 1.5 do this
    return 0;
  }

  /**
   * Returns a description of the action with the supplied id.
   * This default implementation always returns null.
   *
   * @param i the id of the action whose description should be
   *          retrieved.
   * @return a <code>String</code> describing the action.
   * @see javax.accessibility.AccessibleAction#getAccessibleActionDescription(int)
   */
  public String getAccessibleActionDescription(int i)
  {
    // 1.5 does this
    return null;
  }

  /**
   * Executes the action with the specified id.  This
   * default implementation simply returns false.
   *
   * @param i the id of the action to perform.
   * @return true if the action was performed.
   * @see javax.accessibility.AccessibleAction#doAccessibleAction(int)
   */
  public boolean doAccessibleAction(int i)
  {
    // 1.5 does this
    return false;
  }

  /**
   * Returns the current value of this accessible object.
   * If no value has been set, null is returned.  This
   * default implementation always returns null, regardless.
   *
   * @return the numeric value of this object, or null if
   *         no value has been set.
   * @see javax.accessibility.AccessibleValue#getCurrentAccessibleValue()
   */
  public Number getCurrentAccessibleValue()
  {
    // 1.5 does this
    return null;
  }

  /**
   * Sets the current value of this accessible object
   * to that supplied.  In this default implementation,
   * the value is never set and the method always returns
   * false.
   *
   * @param number the new accessible value.
   * @return true if the value was set.
   * @see javax.accessibility.AccessibleValue#setCurrentAccessibleValue(java.lang.Number)
   */
  public boolean setCurrentAccessibleValue(Number number)
  {
    // 1.5 does this
    return false;
  }

  /**
   * Returns the minimum acceptable accessible value used
   * by this object, or null if no minimum value exists.
   * This default implementation always returns null.
   *
   * @return the minimum acceptable accessible value, or null
   *         if there is no minimum.
   * @see javax.accessibility.AccessibleValue#getMinimumAccessibleValue()
   */
  public Number getMinimumAccessibleValue()
  {
    return null;
  }

  /**
   * Returns the maximum acceptable accessible value used
   * by this object, or null if no maximum value exists.
   * This default implementation always returns null.
   *
   * @return the maximum acceptable accessible value, or null
   *         if there is no maximum.
   * @see javax.accessibility.AccessibleValue#getMaximumAccessibleValue()
   */
  public Number getMaximumAccessibleValue()
  {
    return null;
  }
  
  /**
   * Returns the role of this accessible object.
   *
   * @return the instance of <code>AccessibleRole</code>,
   *         which describes this object.
   * @see javax.accessibility.AccessibleRole
   */
  public AccessibleRole getAccessibleRole() 
  {
    return AccessibleRole.CHECK_BOX;
  }
  
  /**
   * Returns the state set of this accessible object.
   *
   * @return a set of <code>AccessibleState</code>s
   *         which represent the current state of the
   *         accessible object.
   * @see javax.accessibility.AccessibleState
   * @see javax.accessibility.AccessibleStateSet
   */
  public AccessibleStateSet getAccessibleStateSet()
  {
    AccessibleStateSet set = super.getAccessibleStateSet();
    if (state)
      set.add(AccessibleState.CHECKED);
    return set;
  }

}

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>Checkbox</code> with no label,
  * an initial state of off, and that is not part of any checkbox group.
  */
public 
Checkbox()
{
  this("", false, null);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Checkbox</code> with the specified
  * label, an initial state of off, and that is not part of any checkbox
  * group.
  *
  * @param label The label for this checkbox.
  */
public
Checkbox(String label)
{
  this(label, false, null);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Checkbox</code> with the specified
  * label and initial state, and that is not part of any checkbox
  * group.
  *
  * @param label The label for this checkbox.
  * @param state The initial state of the checkbox, <code>true</code> for
  * on, <code>false</code> for off.
  */
public
Checkbox(String label, boolean state)
{
  this(label, state, null);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Checkbox</code> with the specified
  * label, initial state, and checkbox group.
  *
  * @param label The label for this checkbox.
  * @param group The checkbox group for this box, or <code>null</code>
  * if there is no checkbox group.
  * @param state The initial state of the checkbox, <code>true</code> for
  * on, <code>false</code> for off.
  */
public
Checkbox(String label, CheckboxGroup group, boolean state)
{
  this(label, state, group);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Checkbox</code> with the specified
  * label, initial state, and checkbox group.
  *
  * @param label The label for this checkbox.
  * @param state The initial state of the checkbox, <code>true</code> for
  * on, <code>false</code> for off.
  * @param group The checkbox group for this box, or <code>null</code>
  * if there is no checkbox group.
  */
public
Checkbox(String label, boolean state, CheckboxGroup group)
{
  this.label = label;
  this.state = state;
  this.group = group;

  if ( state && group != null )
    {
      group.setSelectedCheckbox(this);
    }
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * Returns the label for this checkbox.
  *
  * @return The label for this checkbox.
  */
public String
getLabel()
{
  return(label);
}

/*************************************************************************/

/**
  * Sets the label for this checkbox to the specified value.
  *
  * @param label The new checkbox label.
  */
public synchronized void
setLabel(String label)
{
  this.label = label;
  if (peer != null)
    {
      CheckboxPeer cp = (CheckboxPeer) peer;
      cp.setLabel(label);
    }
}

/*************************************************************************/

/**
  * Returns the state of this checkbox.
  *
  * @return The state of this checkbox, which will be <code>true</code> for
  * on and <code>false</code> for off.
  */
public boolean
getState()
{
  return(state);
}

/*************************************************************************/

/**
  * Sets the state of this checkbox to the specified value.
  *
  * @param state The new state of the checkbox, which will be <code>true</code>
  * for on or <code>false</code> for off.
  */
public synchronized void
setState(boolean state)
{
  this.state = state;
  if (peer != null)
    {
      CheckboxPeer cp = (CheckboxPeer) peer;
      cp.setState (state);
    }
}

/*************************************************************************/

/**
  * Returns an array of length one containing the checkbox label if this
  * checkbox is selected.  Otherwise <code>null</code> is returned.
  *
  * @return The selection state of this checkbox.
  */
public Object[]
getSelectedObjects()
{
  if (state == false)
    return(null);

  Object[] objs = new Object[1];
  objs[0] = label;

  return(objs);
}

/*************************************************************************/

/**
  * Returns the checkbox group this object is a member of, if any.
  *
  * @return This object's checkbox group, of <code>null</code> if it is
  * not a member of any group.
  */
public CheckboxGroup
getCheckboxGroup()
{
  return(group);
}

/*************************************************************************/

/**
  * Sets this object's checkbox group to the specified group.
  *
  * @param group The new checkbox group, or <code>null</code> to make this
  * object part of no checkbox group.
  */
public synchronized void
setCheckboxGroup(CheckboxGroup group)
{
  this.group = group;
  if (peer != null)
    {
      CheckboxPeer cp = (CheckboxPeer) peer;
      cp.setCheckboxGroup (group);
    }
}

/*************************************************************************/

/**
  * Creates this object's native peer.
  */
public void
addNotify()
{
  if (peer == null)
    peer = getToolkit ().createCheckbox (this);
  super.addNotify ();
}

  public ItemListener[] getItemListeners ()
  {
    return (ItemListener[])
      AWTEventMulticaster.getListeners (item_listeners, ItemListener.class);
  }

/**
  * Adds a new listeners to the list of registered listeners for this object.
  *
  * @param listener The new listener to add.
  */
public synchronized void
addItemListener(ItemListener listener)
{
  item_listeners = AWTEventMulticaster.add(item_listeners, listener);
}

/*************************************************************************/

/**
  * Removes a listener from the list of registered listeners for this object.
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
  * Processes this event by calling <code>processItemEvent()</code> if it
  * is any instance of <code>ItemEvent</code>.  Otherwise it is passed to
  * the superclass for processing.
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
  * Processes this event by dispatching it to any registered listeners.
  *
  * @param event The <code>ItemEvent</code> to process.
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
  */
protected String
paramString()
{
  // Note: We cannot add the checkbox group information here because this
  // would trigger infinite recursion when CheckboxGroup.toString() is
  // called and the box is in its selected state.
  return ("label=" + label + ",state=" + state + "," + super.paramString());
}

/**
 * Gets the AccessibleContext associated with this <code>Checkbox</code>.
 * The context is created, if necessary.
 *
 * @return the associated context
 */
public AccessibleContext getAccessibleContext()
{
  /* Create the context if this is the first request */
  if (accessibleContext == null)
  {
    AccessibleAWTCheckbox ac = new AccessibleAWTCheckbox();
    accessibleContext = ac;
    addItemListener(ac);
  }
  return accessibleContext;
}

  /**
   * Generate a unique name for this checkbox.
   *
   * @return A unique name for this checkbox.
   */
  String generateName()
  {
    return "checkbox" + getUniqueLong();
  }

  private static synchronized long getUniqueLong()
  {
    return next_checkbox_number++;
  }
}
