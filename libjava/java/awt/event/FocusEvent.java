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

public class FocusEvent extends ComponentEvent
{
  public static final int FOCUS_FIRST = 1004;
  public static final int FOCUS_GAINED = 1004;
  public static final int FOCUS_LAST = 1005;
  public static final int FOCUS_LOST = 1005;

  public FocusEvent (Component source, int id)
  {
    super (source, id);
    this.temporary = false;
  }

  public FocusEvent (Component source, int id, boolean temporary)
  {
    super (source, id);
    this.temporary = temporary;
  }

  public boolean isTemporary ()
  {
    return temporary;
  }

  public String paramString ()
  {
    String r = "";
    switch (id)
      {
        case FOCUS_GAINED:
	  r += "FOCUS_GAINED";
	break;
	case FOCUS_LOST:
	  r += "FOCUS_LOST";
	break;
	default:
	  r += "unknown id";
	break;
      }
    r += (temporary ? "temporary" : "permanent");
    return r;
  }

  private boolean temporary;
}
