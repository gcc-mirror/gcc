/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.peer.*;
import java.net.URL;

/* A very incomplete placeholder. */

public abstract class Toolkit
{
  static Toolkit defaultToolkit;
  static EventQueue systemEventQueue = new EventQueue();

  public static synchronized Toolkit getDefaultToolkit()
  {
    if (defaultToolkit == null)
      init();
    return defaultToolkit;
  }

  protected abstract FramePeer createFrame(Frame target);
  public abstract Image getImage(URL url);

  protected abstract ButtonPeer createButton (Button target);
  protected abstract ContainerPeer createContainer (Container target);
  protected abstract LabelPeer createLabel (Label target);
  protected abstract ScrollbarPeer createScrollbar (Scrollbar target);

  public final EventQueue getSystemEventQueue()
  {
    return systemEventQueue;
  }

  private static void init() { }
  // private static native void init();
  // static { init(); }
}
