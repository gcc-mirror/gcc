/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/* Status: Believed complete and correct to JDK 1.2.  */

public class WindowEvent extends ComponentEvent
{
  public static final int WINDOW_ACTIVATED = 205;
  public static final int WINDOW_CLOSED = 202;
  public static final int WINDOW_CLOSING = 201;
  public static final int WINDOW_DEACTIVATED = 206;
  public static final int WINDOW_DEICONIFIED = 204;
  public static final int WINDOW_FIRST = 200;
  public static final int WINDOW_ICONIFIED = 203;
  public static final int WINDOW_LAST = 206;
  public static final int WINDOW_OPENED = 200;

  public WindowEvent (Window source, int id)
  {
    super (source, id);
  }

  public Window getWindow ()
  {
    return (Window) source;
  }

  public String paramString ()
  {
    String r = "";
    switch (id)
      {
        case WINDOW_ACTIVATED:
	  r = "WINDOW_ACTIVATED";
	break;
	case WINDOW_CLOSED:
	  r = "WINDOW_CLOSED";
	break;
	case WINDOW_CLOSING:
	  r = "WINDOW_CLOSING";
	break;
	case WINDOW_DEACTIVATED:
	  r = "WINDOW_DEACTIVATED";
	break;
	case WINDOW_DEICONIFIED:
	  r = "WINDOW_DEICONIFIED";
	break;
	case WINDOW_ICONIFIED:
	  r = "WINDOW_ICONIFIED";
	break;
	case WINDOW_OPENED:
	  r = "WINDOW_OPENED";
	break;
      }
    return r;
  }
}
