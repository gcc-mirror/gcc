/* Copyright (C) 1999  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date Fenruary, 1999.
 */

/* Status:  Believed complete and correct. */

public interface KeyListener extends java.util.EventListener
{
  public void keyPressed (KeyEvent w);
  public void keyReleased (KeyEvent w);
  public void keyTyped (KeyEvent w);
}
