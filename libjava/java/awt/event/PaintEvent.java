/* Copyright (C) 2000  Free Software Foundation

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

public class PaintEvent extends ComponentEvent
{
  public static final int PAINT = 800;
  public static final int PAINT_FIRST = 800;
  public static final int PAINT_LAST = 801;
  public static final int UPDATE = 801;

  public PaintEvent (Component source, int id, Rectangle updateRect)
  {
    super (source, id);
    this.updateRect = updateRect;
  }

  public Rectangle getUpdateRect ()
  {
    return updateRect;
  }

  public String paramString ()
  {
    String r;
    switch (id)
      {
        case UPDATE:
	  r = "UPDATE";
	break;
	case PAINT:
	  r = "PAINT";
	break;
	default:
	  r = "unknown id";
	break;	
      }
    
    r += ",updateRect=" + updateRect;
    return r;
  }

  public void setUpdateRect (Rectangle updateRect)
  {
    this.updateRect = updateRect;
  }

  private Rectangle updateRect;
}
