/* Copyright (C) 1999  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;
//import java.awt.peer.ComponentPeer;

/* A very incomplete placeholder. */

public abstract class Component implements MenuContainer
{
  Container parent;
  java.awt.peer.ComponentPeer peer;
  int x, y, width, height;

  public Container getParent () { return parent; }

  /** @deprecated */
  public java.awt.peer.ComponentPeer getPeer () { return peer; }

  public void setVisible (boolean b)
  { /* FIXME */ }

  public void setSize (Dimension d)
  { setSize(d.width, d.height); }

  public void setSize (int width, int height)
  {
    this.width = width;  this.height = height;
    if (peer != null)
      peer.setBounds(x, y, width, height);
  }

  public void setLocation (int x, int y)
  {
    this.x = x;  this.y = y;
    if (peer != null)
      peer.setBounds(x, y, width, height);
  }

  public void setLocation (Point pt)
  { setLocation(pt.x, pt.y); }

  public void setBounds (int x, int y, int w, int h)
  {
    this.x = x;  this.y = y;
    this.width = w;  this.height = h;
    if (peer != null)
      peer.setBounds(x, y, w, h);
  }

  public void setBounds (Rectangle rect)
  { setBounds(rect.x, rect.y, rect.width, rect.height); }

  public Rectangle getBounds ()
  {
    return new Rectangle(x, y, width, height);
  }

  public Point getLocation ()
  {
    return new Point(x, y);
  }

  public Dimension getSize ()
  {
    return new Dimension(width, height);
  }

  public Dimension getMinimumSize ()
  {
    if (peer == null)
      return new Dimension(width, height);
    else
      return peer.getMinimumSize();
  }

  public Dimension getPreferredSize ()
  {
    if (peer == null)
      return new Dimension(width, height);
    else
      return peer.getPreferredSize();
  }

  public synchronized void addKeyListener (KeyListener listener)
  { /* FIXME */ }

  public boolean isFocusTraversable ()
  { /* FIXME */ return false; }

  public void addNotify () { }
}
