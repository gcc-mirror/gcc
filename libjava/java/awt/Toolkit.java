/* Copyright (C) 1999  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.peer.*;

/* A very incomplete placeholder. */

public abstract class Toolkit
{
  static Toolkit defaultToolkit;

  public static synchronized Toolkit getDefaultToolkit()
  {
    if (defaultToolkit == null)
      init();
    return defaultToolkit;
  }

  protected abstract FramePeer createFrame(Frame target);

  private static native void init();
  // static { init(); }
}
