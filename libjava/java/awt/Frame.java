/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.peer.FramePeer;

/* A very incomplete placeholder. */

public class Frame extends Window implements MenuContainer
{
  MenuBar menuBar = null;
  String title;

  public Frame ()
  { /* FIXME */ }

  public Frame (String title)
  {
    this();
    setTitle(title);
  }

  public String getTitle () { return title; }

  public void setTitle (String title)
  {
    this.title = title;
    if (peer != null)
      ((FramePeer)peer).setTitle(title);
  }

  public synchronized void dispose ()
  { /* FIXME */ }

  public synchronized void setMenuBar (MenuBar menuBar)
  { this.menuBar = menuBar; }

  public synchronized void addNotify ()
  {
    if (peer == null)
      {
	FramePeer fpeer = Toolkit.getDefaultToolkit().createFrame(this);
	// Compiler bug requires cast ??;  FIXME?
	peer = (java.awt.peer.ComponentPeer) fpeer;
	if (width + height > 0)
	  peer.setBounds(x, y, width, height);
      }
    super.addNotify();
  }
}
