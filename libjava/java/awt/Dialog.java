/* Dialog.java -- An AWT dialog box
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

import java.awt.peer.DialogPeer;

/**
 * A dialog box widget class.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Tom Tromey <tromey@redhat.com>
 */
public class Dialog extends Window
{

/*
 * Static Variables
 */

// Serialization constant
private static final long serialVersionUID = 5920926903803293709L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial Indicates whether or not this dialog box is modal.
  */
private boolean modal;

/**
  * @serial Indicates whether or not this dialog box is resizable.
  */
private boolean resizable = true;

/**
  * @serial The title string for this dialog box, which can be
  * <code>null</code>.
  */
private String title;

/**
  * This field indicates whether the dialog is undecorated or not.
  */
private boolean undecorated = false;

/**
  * Indicates that we are blocked for modality in show
  */
private boolean blocked = false;

/**
  * Secondary EventQueue to handle AWT events while
  * we are blocked for modality in show
  */
private EventQueue eq2 = null;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>Dialog</code> with the specified
  * parent, that is resizable and not modal, and which has no title.
  *
  * @param parent The parent frame of this dialog box.
  *
  * @exception IllegalArgumentException If the owner's GraphicsConfiguration
  * is not from a screen device, or if owner is null. This exception is always
  * thrown when GraphicsEnvironment.isHeadless() returns true.
  */
public
Dialog(Frame parent)
{
  this(parent, "", false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Dialog</code> with the specified
  * parent and modality, that is resizable and which has no title.
  *
  * @param parent The parent frame of this dialog box.
  * @param modal <code>true</code> if this dialog box is modal,
  * <code>false</code> otherwise.
  *
  * @exception IllegalArgumentException If the owner's GraphicsConfiguration
  * is not from a screen device, or if owner is null. This exception is always
  * thrown when GraphicsEnvironment.isHeadless() returns true.
  */
public
Dialog(Frame parent, boolean modal)
{
  this(parent, "", modal);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Dialog</code> with the specified
  * parent, that is resizable and not modal, and which has the specified
  * title.
  *
  * @param parent The parent frame of this dialog box.
  * @param title The title string for this dialog box.
  *
  * @exception IllegalArgumentException If the owner's GraphicsConfiguration
  * is not from a screen device, or if owner is null. This exception is always
  * thrown when GraphicsEnvironment.isHeadless() returns true.
  */
public
Dialog(Frame parent, String title)
{
  this(parent, title, false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Dialog</code> with the specified,
  * parent, title, and modality, that is resizable.
  *
  * @param parent The parent frame of this dialog box.
  * @param title The title string for this dialog box.
  * @param modal <code>true</code> if this dialog box is modal,
  * <code>false</code> otherwise.
  *
  * @exception IllegalArgumentException If owner is null or
  * GraphicsEnvironment.isHeadless() returns true.
  */
public
Dialog(Frame parent, String title, boolean modal)
{
  this (parent, title, modal, parent.getGraphicsConfiguration ());
}

/**
 * Initializes a new instance of <code>Dialog</code> with the specified,
 * parent, title, modality and <code>GraphicsConfiguration</code>,
 * that is resizable.
 *
 * @param parent The parent frame of this dialog box.
 * @param title The title string for this dialog box.
 * @param modal <code>true</code> if this dialog box is modal,
 * <code>false</code> otherwise.
 * @param gc The <code>GraphicsConfiguration</code> object to use.
 *
 * @exception IllegalArgumentException If owner is null, the
 * GraphicsConfiguration is not a screen device or
 * GraphicsEnvironment.isHeadless() returns true.
 *
 * @since 1.4
 */
public
Dialog (Frame parent, String title, boolean modal, GraphicsConfiguration gc)
{
  super (parent, gc);

  // A null title is equivalent to an empty title  
  this.title = (title != null) ? title : "";
  this.modal = modal;
  visible = false;

  setLayout(new BorderLayout());
}

/**
 * Initializes a new instance of <code>Dialog</code> with the specified,
 * parent, that is resizable.
 *
 * @exception IllegalArgumentException If parent is null. This exception is
 * always thrown when GraphicsEnvironment.isHeadless() returns true.
 *
 * @since 1.2
 */
public
Dialog (Dialog owner)
{
  this (owner, "", false, owner.getGraphicsConfiguration ());
}

/**
 * Initializes a new instance of <code>Dialog</code> with the specified,
 * parent and title, that is resizable.
 *
 * @exception IllegalArgumentException If parent is null. This exception is
 * always thrown when GraphicsEnvironment.isHeadless() returns true.
 *
 * @since 1.2
 */
public
Dialog (Dialog owner, String title)
{
  this (owner, title, false, owner.getGraphicsConfiguration ());
}

/**
 * Initializes a new instance of <code>Dialog</code> with the specified,
 * parent, title and modality, that is resizable.
 *
 * @exception IllegalArgumentException If parent is null. This exception is
 * always thrown when GraphicsEnvironment.isHeadless() returns true.
 *
 * @since 1.2
 */
public
Dialog (Dialog owner, String title, boolean modal)
{
  this (owner, title, modal, owner.getGraphicsConfiguration ());
}

/**
 * Initializes a new instance of <code>Dialog</code> with the specified,
 * parent, title, modality and <code>GraphicsConfiguration</code>,
 * that is resizable.
 *
 * @exception IllegalArgumentException If parent is null, the
 * GraphicsConfiguration is not a screen device or
 * GraphicsEnvironment.isHeadless() returns true.
 *
 * @since 1.4
 */
public
Dialog (Dialog parent, String title, boolean modal, GraphicsConfiguration gc)
{
  super (parent, parent.getGraphicsConfiguration ());

  // A null title is equivalent to an empty title  
  this.title = (title != null) ? title : "";
  this.modal = modal;
  visible = false;

  setLayout (new BorderLayout ());
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * Returns the title of this dialog box.
  * 
  * @return The title of this dialog box.
  */
public String
getTitle()
{
  return(title);
}

/*************************************************************************/

/**
  * Sets the title of this dialog box to the specified string.
  *
  * @param title The new title.
  */
public synchronized void
setTitle(String title)
{
  // A null title is equivalent to an empty title  
  this.title = (title != null) ? title : "";

  if (peer != null)
    {
      DialogPeer d = (DialogPeer) peer;
      d.setTitle (title);
    }
}

/*************************************************************************/

/**
  * Tests whether or not this dialog box is modal.
  *
  * @return <code>true</code> if this dialog box is modal,
  * <code>false</code> otherwise.
  */
public boolean
isModal()
{
  return(modal);
}

/*************************************************************************/

/**
  * Changes the modality of this dialog box.  This can only be done before
  * the peer is created.
  *
  * @param modal <code>true</code> to make this dialog box modal,
  * <code>false</code> to make it non-modal.
  */
public void
setModal(boolean modal)
{
  this.modal = modal;
}

/*************************************************************************/

/**
  * Tests whether or not this dialog box is resizable.
  *
  * @return <code>true</code> if this dialog is resizable, <code>false</code>,
  * otherwise.
  */
public boolean
isResizable()
{
  return(resizable);
}

/*************************************************************************/

/**
  * Changes the resizability of this dialog box.
  *
  * @param resizable <code>true</code> to make this dialog resizable,
  * <code>false</code> to make it non-resizable.
  */
public synchronized void
setResizable(boolean resizable)
{
  this.resizable = resizable;
  if (peer != null)
    {
      DialogPeer d = (DialogPeer) peer;
      d.setResizable (resizable);
    }
}

/*************************************************************************/

/**
  * Creates this object's native peer.
  */
public synchronized void
addNotify()
{
  if (peer == null)
    peer = getToolkit ().createDialog (this);
  super.addNotify ();
}

/*************************************************************************/

/**
  * Makes this dialog visible and brings it to the front.
  * If the dialog is modal and is not already visible, this call will not
  *  return until the dialog is hidden by someone calling hide or dispose.
  * If this is the event dispatching thread we must ensure that another event
  *  thread runs while the one which invoked this method is blocked. 
  */
public synchronized void
show()
{
  super.show();
  
  if (isModal())
    {
      // If already shown (and blocked) just return
      if (blocked)
	return;

      /* If show is called in the dispatch thread for a modal dialog it will
         block so we must run another thread so the events keep being
	 dispatched.*/
      if (EventQueue.isDispatchThread ())
        {
	  EventQueue eq = Toolkit.getDefaultToolkit().getSystemEventQueue();
          eq2 = new EventQueue ();
	  eq.push (eq2);
	}
      
      try 
        {
	  blocked = true;
	  wait ();
	  blocked = false;
        } 
      catch (InterruptedException e)
        {
	  blocked = false;
        }
	
      if (eq2 != null)
        {
	  eq2.pop ();
	  eq2 = null;
	}
    }  
}

/*************************************************************************/

/**
  * Hides the Dialog and then
  * causes show() to return if it is currently blocked.
  */

public synchronized void 
hide ()
{
  if (blocked)
    {
      notifyAll ();
    }

  super.hide();
}

/*************************************************************************/

/**
  * Disposes the Dialog and then causes show() to return
  * if it is currently blocked.
  */

public synchronized void 
dispose ()
{
  if (blocked)
    {
      notifyAll ();
    }

  super.dispose();
}

/*************************************************************************/

/**
  * Returns a debugging string for this component.
  * 
  * @return A debugging string for this component.
  */
protected String
paramString()
{
  return ("title+" + title + ",modal=" + modal +
	  ",resizable=" + resizable + "," + super.paramString());
}

  /**
   * Returns whether this frame is undecorated or not.
   * 
   * @since 1.4
   */
  public boolean isUndecorated ()
  {
    return undecorated;
  }

  /**
   * Disables or enables decorations for this frame. This method can only be
   * called while the frame is not displayable.
   * 
   * @exception IllegalComponentStateException If this frame is displayable.
   * 
   * @since 1.4
   */
  public void setUndecorated (boolean undecorated)
  {
    if (isDisplayable ())
      throw new IllegalComponentStateException ();

    this.undecorated = undecorated;
  }
} // class Dialog

