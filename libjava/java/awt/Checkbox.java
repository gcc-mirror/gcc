/* Checkbox.java -- An AWT checkbox widget
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

import java.awt.peer.CheckboxPeer;
import java.awt.peer.ComponentPeer;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.Serializable;

/**
 * This class implements a component which has an on/off state.  Two
 * or more Checkboxes can be grouped by a CheckboxGroup.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey <tromey@redhat.com>
 */
public class Checkbox extends Component implements ItemSelectable, Serializable
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
  */
private boolean state;

// The list of listeners for this object.
private transient ItemListener item_listeners;

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

/*************************************************************************/

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

/*************************************************************************/

/**
  * Returns a debugging string for this object.
  */
protected String
paramString()
{
  return ("label=" + label + ",state=" + state + ",group=" + group
	  + "," + super.paramString());
}

} // class Checkbox 
