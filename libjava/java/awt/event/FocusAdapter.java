/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */

/* Status: Believed complete and correct to JDK 1.2.  */

public abstract class FocusAdapter implements FocusListener
{
  public void focusGained (FocusEvent e)
  {
  }

  public void focusLost (FocusEvent e)
  {
  }
}
