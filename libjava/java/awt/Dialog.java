/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.peer.DialogPeer;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date April 17, 2001
 */

public class Dialog extends Window
{
  public Dialog (Dialog owner)
  {
    this (owner, "", false);
  }

  public Dialog (Dialog owner, String title)
  {
    this (owner, title, false);
  }

  public Dialog (Dialog owner, String title, boolean modal)
  {
    super (owner);
    this.modal = modal;
    this.title = title;
    setLayout (new BorderLayout ());
  }

  public Dialog (Frame owner)
  {
    this (owner, "", false);
  }

  public Dialog (Frame owner, boolean modal)
  {
    this (owner, "", modal);
  }

  public Dialog (Frame owner, String title)
  {
    this (owner, title, false);
  }

  public Dialog (Frame owner, String title, boolean modal)
  {
    super (owner);
    this.modal = modal;
    this.title = title;
    setLayout (new BorderLayout ());
  }

  /** Create the peer if it does not already exist.  */
  public void addNotify ()
  {
    if (peer == null)
      peer = getToolkit ().createDialog (this);
    super.addNotify ();
  }

  public boolean isModal ()
  {
    return modal;
  }

  public void setModal (boolean modal)
  {
    this.modal = modal;
  }

  public String getTitle ()
  {
    return title;
  }

  public void setTitle (String title)
  {
    this.title = title;
    if (peer != null)
      {
	DialogPeer d = (DialogPeer) peer;
	d.setTitle (title);
      }
  }

  public void show ()
  {
    boolean vis = isVisible ();
    super.show ();
    if (modal && vis)
      {
	// Don't return until something happens.  We lock on the peer
	// instead of `this' so that we don't interfere with whatever
	// locks the caller might want to use.
	synchronized (peer)
	  {
	    try
	      {
		peer.wait ();
	      }
	    catch (InterruptedException _)
	      {
	      }
	  }
      }
  }

  public void hide ()
  {
    super.hide ();
    synchronized (peer)
      {
	peer.notify ();
      }
  }

  public void dispose ()
  {
    super.dispose ();
    synchronized (peer)
      {
	peer.notify ();
      }
  }

  public boolean isResizable ()
  {
    return resizable;
  }

  public void setResizable (boolean resizable)
  {
    this.resizable = resizable;
    if (peer != null)
      {
	DialogPeer d = (DialogPeer) peer;
	d.setResizable (resizable);
      }
  }

  protected String paramString ()
  {
    return ("Dialog["
	    + title + ","
	    + modal + ","
	    + resizable + "]");
  }

  // True if dialog is modal.
  private boolean modal;
  // True if dialog is resizable by the user.
  private boolean resizable = false;
  // Dialog title.
  private String title;
}
