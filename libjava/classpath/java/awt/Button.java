/* Button.java -- AWT button widget
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.peer.ButtonPeer;
import java.lang.reflect.Array;
import java.util.EventListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleValue;

/**
  * This class provides a button widget for the AWT. 
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@cygnus.com)
  */
public class Button extends Component
  implements java.io.Serializable, Accessible
{

/*
 * Static Variables
 */

// FIXME: Need readObject/writeObject for serialization

// Serialization version constant
private static final long serialVersionUID = -8774683716313001058L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The action command name for this button.
  * This is package-private to avoid an accessor method.
  */
String actionCommand;

/**
  * @serial The label for this button.
  * This is package-private to avoid an accessor method.
  */
String label;

// List of ActionListeners for this class.
private transient ActionListener action_listeners;

  /*
   * The number used to generate the name returned by getName.
   */
  private static transient long next_button_number;
  
  protected class AccessibleAWTButton extends AccessibleAWTComponent
    implements AccessibleAction, AccessibleValue
  {
    protected AccessibleAWTButton()
    {
      // Do nothing here.
    }

    /* (non-Javadoc)
     * @see javax.accessibility.AccessibleAction#getAccessibleActionCount()
     */
    public int getAccessibleActionCount()
    {
      // Only 1 action possible
      return 1;
    }

    /* (non-Javadoc)
     * @see javax.accessibility.AccessibleAction#getAccessibleActionDescription(int)
     */
    public String getAccessibleActionDescription(int i)
    {
      // JDK 1.4.2 returns the string "click" for action 0.  However, the API
      // docs don't say what the string to be returned is, beyond being a
      // description of the action.  So we return the same thing for
      // compatibility with 1.4.2.
      if (i == 0)
        return "click";
      return null;
    }

    /* (non-Javadoc)
     * @see javax.accessibility.AccessibleAction#doAccessibleAction(int)
     */
    public boolean doAccessibleAction(int i)
    {
      if (i != 0)
        return false;
      processActionEvent(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, actionCommand));
      return true;
    }
    
    public String getAccessibleName()
    {
      return label;
    }
    
    public AccessibleAction getAccessibleAction()
    {
      return this;
    }
    
    public AccessibleValue getAccessibleValue()
    {
      return this;
    }

    /* (non-Javadoc)
     * @see javax.accessibility.AccessibleValue#getCurrentAccessibleValue()
     */
    public Number getCurrentAccessibleValue()
    {
      // Docs say return 1 if selected, but buttons can't be selected, right?
      return new Integer(0);
    }

    /* (non-Javadoc)
     * @see javax.accessibility.AccessibleValue#setCurrentAccessibleValue(java.lang.Number)
     */
    public boolean setCurrentAccessibleValue(Number number)
    {
      // Since there's no selection with buttons, we're ignoring this.
      // TODO someone who knows shoulw check this.
      return false;
    }

    /* (non-Javadoc)
     * @see javax.accessibility.AccessibleValue#getMinimumAccessibleValue()
     */
    public Number getMinimumAccessibleValue()
    {
      return new Integer(0);
    }

    /* (non-Javadoc)
     * @see javax.accessibility.AccessibleValue#getMaximumAccessibleValue()
     */
    public Number getMaximumAccessibleValue()
    {
      return new Integer(0);
    }
    
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PUSH_BUTTON;
    }
  }

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>Button</code> with no label.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless()
  * returns true
  */
public
Button()
{
  this("");
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Button</code> with the specified
  * label.  The action command name is also initialized to this value.
  *
  * @param label The label to display on the button.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless()
  * returns true
  */
public
Button(String label)
{
  this.label = label;
  actionCommand = label;

  if (GraphicsEnvironment.isHeadless ())
    throw new HeadlessException ();
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * Returns the label for this button.
  *
  * @return The label for this button.
  */
public String
getLabel()
{
  return(label);
}

/*************************************************************************/

/**
  * Sets the label for this button to the specified value.
  *
  * @param label The new label for this button.
  */
public synchronized void
setLabel(String label)
{
  this.label = label;
  actionCommand = label;
  if (peer != null)
    {
      ButtonPeer bp = (ButtonPeer) peer;
      bp.setLabel (label);
    }
}

/*************************************************************************/

/**
  * Returns the action command name for this button.
  *
  * @return The action command name for this button.
  */
public String
getActionCommand()
{
  return(actionCommand);
}

/*************************************************************************/

/**
  * Sets the action command name for this button to the specified value.
  *
  * @param actionCommand The new action command name.
  */
public void
setActionCommand(String actionCommand)
{
  this.actionCommand = actionCommand == null ? label : actionCommand;
}

/*************************************************************************/

/**
  * Adds a new entry to the list of listeners that will receive
  * action events from this button.
  *
  * @param listener The listener to add.
  */
public synchronized void
addActionListener(ActionListener listener)
{
  action_listeners = AWTEventMulticaster.add(action_listeners, listener);
}

/*************************************************************************/

/**
  * Removes the specified listener from the list of listeners that will
  * receive action events from this button.
  * 
  * @param listener The listener to remove.
  */
public synchronized void
removeActionListener(ActionListener listener)
{
  action_listeners = AWTEventMulticaster.remove(action_listeners, listener);
}

  /**
   * Returns all added <code>ActionListener</code> objects.
   *
   * @return an array of listeners
   *
   * @since 1.4
   */
  public synchronized ActionListener[] getActionListeners()
  {
    return (ActionListener[])
      AWTEventMulticaster.getListeners(action_listeners,
                                       ActionListener.class);
  }

/**
 * Returns all registered EventListers of the given listenerType. 
 * listenerType must be a subclass of EventListener, or a 
 * ClassClassException is thrown.
 *
 * @param listenerType the listener type to return
 *
 * @return an array of listeners
 * 
 * @exception ClassCastException If listenerType doesn't specify a class or
 * interface that implements @see java.util.EventListener.
 *
 * @since 1.3 
 */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ActionListener.class)
      return getActionListeners();
    return (EventListener[]) Array.newInstance(listenerType, 0);
  }

/*************************************************************************/

/**
  * Notifies this button that it should create its native peer object.
  */
public void
addNotify()
{
  if (peer == null)
    peer = getToolkit ().createButton (this);
  super.addNotify();
}

/*************************************************************************/

/**
  * Processes an event for this button.  If the specified event is an
  * instance of <code>ActionEvent</code>, then the
  * <code>processActionEvent()</code> method is called to dispatch it
  * to any registered listeners.  Otherwise, the superclass method
  * will be invoked.  Note that this method will not be called at all
  * unless <code>ActionEvent</code>'s are enabled.  This will be done
  * implicitly if any listeners are added.
  *
  * @param event The event to process.
  */
protected void
processEvent(AWTEvent event)
{
  if (event instanceof ActionEvent)
    processActionEvent((ActionEvent)event);
  else
    super.processEvent(event);
}

/*************************************************************************/

/**
  * This method dispatches an action event for this button to any
  * registered listeners.
  *
  * @param event The event to process.
  */
protected void
processActionEvent(ActionEvent event)
{
  if (action_listeners != null)
    action_listeners.actionPerformed(event);
}

void
dispatchEventImpl(AWTEvent e)
{
  if (e.id <= ActionEvent.ACTION_LAST 
      && e.id >= ActionEvent.ACTION_FIRST
      && (action_listeners != null 
	  || (eventMask & AWTEvent.ACTION_EVENT_MASK) != 0))
    processEvent(e);
  else
    super.dispatchEventImpl(e);
}

/*************************************************************************/

/**
  * Returns a debugging string for this button.
  *
  * @return A debugging string for this button.
  */
protected String
paramString()
{
  return getName () + "," + getX () + "," + getY () + ","
    + getWidth () + "x" + getHeight () + ",label=" + getLabel ();
}

/**
 * Gets the AccessibleContext associated with this <code>Button</code>.
 * The context is created, if necessary.
 *
 * @return the associated context
 */
public AccessibleContext getAccessibleContext()
{
  /* Create the context if this is the first request */
  if (accessibleContext == null)
    accessibleContext = new AccessibleAWTButton();
  return accessibleContext;
}

  /**
   * Generate a unique name for this button.
   *
   * @return A unique name for this button.
   */
  String generateName ()
  {
    return "button" + getUniqueLong ();
  }

  private static synchronized long getUniqueLong ()
  {
    return next_button_number++;
  }

} // class Button 

