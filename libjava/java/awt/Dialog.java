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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.awt;

import java.awt.peer.DialogPeer;
import java.awt.peer.WindowPeer;
import java.awt.peer.ContainerPeer;
import java.awt.peer.ComponentPeer;

/**
  * A dialog box widget class.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey <tromey@redhat.com>
  */
public class Dialog extends Window implements java.io.Serializable
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
private boolean resizable;

/**
  * @serial The title string for this dialog box, which can be
  * <code>null</code>.
  */
private String title;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>Dialog</code> with the specified
  * parent, that is not resizable and not modal, and which has no title.
  *
  * @param parent The parent frame of this dialog box.
  */
public
Dialog(Frame parent)
{
  this(parent, "", false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Dialog</code> with the specified
  * parent and modality, that is not resizable and which has no title.
  *
  * @param parent The parent frame of this dialog box.
  * @param modal <true> if this dialog box is modal, <code>false</code>
  * otherwise.
  */
public
Dialog(Frame parent, boolean modal)
{
  this(parent, "", modal);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Dialog</code> with the specified
  * parent, that is not resizable and not modal, and which has the specified
  * title.
  *
  * @param parent The parent frame of this dialog box.
  * @param title The title string for this dialog box.
  */
public
Dialog(Frame parent, String title)
{
  this(parent, title, false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Dialog</code> with the specified,
  * parent, title, and modality, that is not resizable.
  *
  * @param parent The parent frame of this dialog box.
  * @param title The title string for this dialog box.
  * @param modal <true> if this dialog box is modal, <code>false</code>
  * otherwise.
  */
public
Dialog(Frame parent, String title, boolean modal)
{
  super(parent);

  this.title = title;
  this.modal = modal;
  resizable = false;

  setLayout(new BorderLayout());
}

public
Dialog (Dialog owner)
{
  this (owner, "", false);
}

public
Dialog (Dialog owner, String title)
{
  this (owner, title, false);
}

public
Dialog (Dialog owner, String title, boolean modal)
{
  super (owner);
  this.modal = modal;
  this.title = title;
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
  this.title = title;
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
  */
public void
show()
{
  super.show();
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

} // class Dialog

