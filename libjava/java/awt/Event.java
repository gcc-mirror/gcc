/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct, except for the paramString()
 * method, which is stubbed.
 */

public class Event
{
  public static final int SHIFT_MASK = 1,
			  CTRL_MASK = 2,
			  META_MASK = 4,
			  ALT_MASK = 8;

  public static final int ACTION_EVENT = 1001,
			  BACK_SPACE = 8,
			  CAPS_LOCK = 1022,
			  DELETE = 127,
			  DOWN = 1005,
			  END = 1001,
			  ENTER = 10,
			  ESCAPE = 27,
			  F1 = 1008,
			  F10 = 1017,
			  F11 = 1018,
			  F12 = 1019,
			  F2 = 1009,
			  F3 = 1010,
			  F4 = 1011,
			  F5 = 1012,
			  F6 = 1013,
			  F7 = 1014,
			  F8 = 1015,
			  F9 = 1016,
			  GOT_FOCUS = 1004,
			  HOME = 1000,
			  INSERT = 1025,
			  KEY_ACTION = 403,
			  KEY_ACTION_RELEASE = 404,
			  KEY_PRESS = 401,
			  KEY_RELEASE = 402,
			  LEFT = 1006,
			  LIST_DESELECT = 702,
			  LIST_SELECT = 701,
			  LOAD_FILE = 1002,
			  LOST_FOCUS = 1005,
			  MOUSE_DOWN = 501,
			  MOUSE_DRAG = 506,
			  MOUSE_ENTER = 504,
			  MOUSE_EXIT = 505,
			  MOUSE_MOVE = 503,
			  MOUSE_UP = 502,
			  NUM_LOCK = 1023,
			  PAUSE = 1024,
			  PGDN = 1003,
			  PGUP = 1002,
			  PRINT_SCREEN = 1020,
			  RIGHT = 1007,
			  SAVE_FILE = 1003,
			  SCROLL_ABSOLUTE = 605,
			  SCROLL_BEGIN = 606,
			  SCROLL_END = 607,
			  SCROLL_LINE_DOWN = 602,
			  SCROLL_LINE_UP = 601,
			  SCROLL_LOCK = 1021,
			  SCROLL_PAGE_DOWN = 604,
			  SCROLL_PAGE_UP = 603,
			  TAB = 9,
			  UP = 1004,
			  WINDOW_DEICONIFY = 204,
			  WINDOW_DESTROY = 201,
			  WINDOW_EXPOSE = 202,
			  WINDOW_ICONIFY = 203,
			  WINDOW_MOVED = 205;

  public Event evt;
  public Object arg;
  public int clickCount;
  public int id;
  public int key; 
  public int modifiers;
  public Object target;
  public long when;
  public int x;
  public int y;

  public Event (Object target, int id, Object arg)
  {
    this.id = id;
    this.target = target;
    this.arg = arg;
  }
  
  public Event (Object target, long when, int id, int x, int y, int key, 
		int modifiers)
  {
    this.target = target;
    this.when = when;
    this.id = id;
    this.x = x;
    this.y = y;
    this.key = key;
    this.modifiers = modifiers;
  }

  public Event (Object target, long when, int id, int x, int y, int key, 
	        int modifiers, Object arg) 
  {
    this (target, when, id, x, y, key, modifiers);
    this.arg = arg;
  }

  public boolean controlDown ()
  {
    return ((modifiers & CTRL_MASK) == 0 ? false : true);
  }

  public boolean metaDown ()
  {
    return ((modifiers & META_MASK) == 0 ? false : true);
  }

  protected String paramString ()
  {
    return "Event.paramString() not implemented";
  }

  public boolean shiftDown() 
  {
    return ((modifiers & SHIFT_MASK) == 0 ? false : true);
  }

  public String toString()
  {
    String r = getClass() + "[id=" + id + ",x=" + x + ",y=" + y + "target=" 
               + ((target == null) ? "null" : target) + "]";
    return r;
  }

  public void translate (int x, int y)
  {
    this.x += x;
    this.y += y;
  }
}
