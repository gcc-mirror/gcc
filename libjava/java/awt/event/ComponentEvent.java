/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */

/* Status: Believed complete and correct to JDK 1.2.  */

public class ComponentEvent extends AWTEvent
{
  public static final int COMPONENT_FIRST = 100;
  public static final int COMPONENT_HIDDEN = 103;
  public static final int COMPONENT_LAST = 103;
  public static final int COMPONENT_MOVED = 100;
  public static final int COMPONENT_RESIZED = 101;
  public static final int COMPONENT_SHOWN = 102;

  public ComponentEvent (Component source, int id)
  {
    super(source, id);
  }

  public Component getComponent ()
  {
    return (Component) source;
  }

  public String paramString ()
  {
    return super.paramString ();
  }
}
