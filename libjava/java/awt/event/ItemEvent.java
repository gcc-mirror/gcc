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

public class ItemEvent extends AWTEvent
{
  public static final int DESELECTED = 2;
  public static final int ITEM_FIRST = 701;
  public static final int ITEM_LAST = 701;
  public static final int ITEM_STATE_CHANGED = 701;
  public static final int SELECTED = 1;

  public ItemEvent (ItemSelectable source, int id, Object item, int sc)
  {
    super (source, id);
    this.item = item;
    this.stateChange = sc;
  }

  public Object getItem ()
  {
    return item;
  }

  public ItemSelectable getItemSelectable ()
  {
    return (ItemSelectable) source;
  }

  public int getStateChange ()
  {
    return stateChange;
  }

  public String paramString ()
  {
    String r;
    switch (id)
      {
        case ITEM_STATE_CHANGED:
	  r = "ITEM_STATE_CHANGED";
	break;
	default:
	  r = "unknown id";
	break;
      }
    
    r += ",item=" + item + ",stateChange=";
    switch (stateChange)
      {
        case SELECTED:
	  r += "SELECTED";
	break;
	case DESELECTED:
	  r += "DESELECTED";
	break;
	default:
	  r += "unknown";
	break;
      }
      
    return r;
  }

  private Object item;
  private int stateChange;
}
