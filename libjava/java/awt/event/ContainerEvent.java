/* Copyright (C) 2000, 2001  Free Software Foundation

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

public class ContainerEvent extends ComponentEvent
{
  public static final int COMPONENT_ADDED = 300;
  public static final int COMPONENT_REMOVED = 301;
  public static final int CONTAINER_FIRST = 300;
  public static final int CONTAINER_LAST = 301;

  /** @specnote In JDK1.2 and 1.3, source is a Component. */
  public ContainerEvent (Component source, int id, Component child)
  {
    super (source, id);
    this.child = child;
  }

  public Component getChild ()
  {
    return child;
  }

  public Container getContainer ()
  {
    return (Container) source;
  }

  public String paramString ()
  {
    String r;
    switch (id)
      {
        case COMPONENT_ADDED:
	 r = "COMPONENT_ADDED";
	break;
	case COMPONENT_REMOVED:
	 r = "COMPONENT_REMOVED";
	break;
	default:
	  r = "unknown id";
	break;

      }
    r += ",child=" + child;
    return r;
  }

  private Component child;
}
