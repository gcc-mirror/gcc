/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date February, 1999.
 */

/* Status:  Believed complete and correct. */

public abstract class WindowAdapter implements WindowListener
{
  public void windowActivated (WindowEvent w) { }
  public void windowClosed (WindowEvent w) { }
  public void windowClosing (WindowEvent w) { }
  public void windowDeactivated (WindowEvent w) { }
  public void windowDeiconified (WindowEvent w) { }
  public void windowIconified (WindowEvent w) { }
  public void windowOpened (WindowEvent w) { }
}
