/* TextField.java -- A one line text entry field
   Copyright (C) 1999 Free Software Foundation, Inc.

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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.peer.TextFieldPeer;
import java.awt.peer.TextComponentPeer;
import java.awt.peer.ComponentPeer;

/**
  * This class implements a single line text entry field widget
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class TextField extends TextComponent implements java.io.Serializable
{

/*
 * Static Variables
 */

// Serialization constant
private static final long serialVersionUID = -2966288784432217853L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The number of columns in the text entry field.
  */
private int columns;

/**
  * @serial The character that is echoed when doing protected input
  */
private char echoChar;

// List of registered ActionListener's for this object.
private ActionListener action_listeners;

/*************************************************************************/

/*
 * Constructors
 */

/*
 * Initializes a new instance of <code>TextField</code> that is empty
 * and has one column.
 */
public
TextField()
{
  this("", 1);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>TextField</code> containing
  * the specified text.  The number of columns will be equal to the
  * length of the text string.
  *
  * @param text The text to display in the field.
  */
public
TextField(String text)
{
  this(text, text.length());
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>TextField</code> that is empty
  * and has the specified number of columns.
  *
  * @param columns The number of columns in the text field.
  */
public
TextField(int columns)
{
  this("", columns);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>TextField</code> with the
  * specified text and number of columns.
  *
  * @param text The text to display in the field.
  * @param columns The number of columns in the field.
  */
public
TextField(String text, int columns)
{
  super(text);
  this.columns = columns;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the number of columns in the field.
  *
  * @return The number of columns in the field.
  */
public int
getColumns()
{
  return(columns);
}

/*************************************************************************/

/**
  * Sets the number of columns in this field to the specified value.
  *
  * @param columns The new number of columns in the field.
  *
  * @exception IllegalArgumentException If columns is less than zero.
  */
public synchronized void
setColumns(int columns)
{
  if (columns < 0)
    throw new IllegalArgumentException("Value is less than zero: " +
                                       columns);

  this.columns = columns;
  // FIXME: How to we communicate this to our peer?
}

/*************************************************************************/

/**
  * Returns the character that is echoed to the screen when a text 
  * field is protected (such as when a password is being entered).
  *
  * @return The echo character for this text field.
  */
public char
getEchoChar()
{
  return(echoChar);
}

/*************************************************************************/

/**
  * Sets the character that is echoed when protected input such as
  * a password is displayed.
  *
  * @param echoChar The new echo character.
  */
public void
setEchoChar(char echoChar)
{
  this.echoChar = echoChar;

  TextFieldPeer tfp = (TextFieldPeer)getPeer();
  if (tfp != null)
    tfp.setEchoChar(echoChar);
}

/*************************************************************************/

/**
  * Sets the character that is echoed when protected input such as
  * a password is displayed.
  *
  * @param echoChar The new echo character.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>setEchoChar()</code>
  */
public void
setEchoCharacter(char echoChar)
{
  setEchoChar(echoChar);
}

/*************************************************************************/

/**
  * Tests whether or not this text field has an echo character set
  * so that characters the user type are not echoed to the screen.
  *
  * @return <code>true</code> if an echo character is set,
  * <code>false</code> otherwise.
  */
public boolean
echoCharIsSet()
{
  if (echoChar == '\u0000')
    return(false);
  else
    return(true);
}

/*************************************************************************/

/**
  * Returns the minimum size for this text field.
  *
  * @return The minimum size for this text field.
  */
public Dimension
getMinimumSize()
{
  return(getMinimumSize(getColumns()));
}

/*************************************************************************/

/**
  * Returns the minimum size of a text field with the specified number
  * of columns.
  *
  * @param columns The number of columns to get the minimum size for.
  */
public Dimension
getMinimumSize(int columns)
{
  TextFieldPeer tfp = (TextFieldPeer)getPeer();
  if (tfp == null)
    return(null); // FIXME: What do we do if there is no peer?

  return(tfp.getMinimumSize(columns));
}

/*************************************************************************/

/**
  * Returns the minimum size for this text field.
  *
  * @return The minimum size for this text field.
  *
  * @deprecated This method is depcreated in favor of
  * <code>getMinimumSize()</code>.
  */
public Dimension
minimumSize()
{
  return(getMinimumSize(getColumns()));
}

/*************************************************************************/

/**
  * Returns the minimum size of a text field with the specified number
  * of columns.
  *
  * @param columns The number of columns to get the minimum size for.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>getMinimumSize(int)</code>.
  */
public Dimension
minimumSize(int columns)
{
  return(getMinimumSize(columns));
}

/*************************************************************************/

/**
  * Returns the preferred size for this text field.
  *
  * @return The preferred size for this text field.
  */
public Dimension
getPreferredSize()
{
  return(getPreferredSize(getColumns()));
}

/*************************************************************************/

/**
  * Returns the preferred size of a text field with the specified number
  * of columns.
  *
  * @param columns The number of columns to get the preferred size for.
  */
public Dimension
getPreferredSize(int columns)
{
  TextFieldPeer tfp = (TextFieldPeer)getPeer();
  if (tfp == null)
    return(null); // FIXME: What do we do if there is no peer?

  return(tfp.getPreferredSize(columns));
}

/*************************************************************************/

/**
  * Returns the preferred size for this text field.
  *
  * @return The preferred size for this text field.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>getPreferredSize()</code>.
  */
public Dimension
preferredSize()
{
  return(getPreferredSize(getColumns()));
}

/*************************************************************************/

/**
  * Returns the preferred size of a text field with the specified number
  * of columns.
  *
  * @param columns The number of columns to get the preferred size for.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>getPreferredSize(int)</code>.
  */
public Dimension
preferredSize(int columns)
{
  return(getPreferredSize(columns));
}

/*************************************************************************/

/**
  * Notifies this object that it should create its native peer.
  */
public void
addNotify()
{
  if (getPeer() != null)
    return;

  setPeer((ComponentPeer)getToolkit().createTextField(this));
}

/*************************************************************************/

/**
  * Addes a new listener to the list of action listeners for this
  * object.
  *
  * @param listener The listener to add to the list.
  */
public synchronized void
addActionListener(ActionListener listener)
{
  action_listeners = AWTEventMulticaster.add(action_listeners, listener);

  enableEvents(AWTEvent.ACTION_EVENT_MASK);
}

/*************************************************************************/

/**
  * Removes the specified listener from the list of action listeners
  * for this object.
  *
  * @param listener The listener to remove from the list.
  */
public synchronized void
removeActionListener(ActionListener listener)
{
  action_listeners = AWTEventMulticaster.remove(action_listeners, listener);
}

/*************************************************************************/

/**
  * Processes the specified event.  If the event is an instance of
  * <code>ActionEvent</code> then <code>processActionEvent()</code> is
  * called to process it, otherwise the event is sent to the
  * superclass.
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
  * Processes an action event by calling any registered listeners.
  * Note to subclasses: This method is not called unless action events
  * are enabled on this object.  This will be true if any listeners
  * are registered, or if action events were specifically enabled
  * using <code>enableEvents()</code>.
  * 
  * @param event The event to process.
  */
protected void
processActionEvent(ActionEvent event)
{
  if (action_listeners != null)
    action_listeners.actionPerformed(event);
}

/*************************************************************************/

/**
  * Returns a debug string for this object.
  *
  * @return A debug string for this object.
  */
protected String
paramString()
{
  return(getClass().getName() + "(columns=" + getColumns() + ",echoChar=" +
         getEchoChar());
}

} // class TextField
