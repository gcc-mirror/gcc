/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.awt;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public class Event implements java.io.Serializable
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

  public Object arg;
  public int clickCount;
  boolean consumed;		// Required by serialization spec.
  public Event evt;
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
    return "id=" + id + ",x=" + x + ",y=" + y + "target=" + target;
  }

  public boolean shiftDown() 
  {
    return ((modifiers & SHIFT_MASK) == 0 ? false : true);
  }

  public String toString()
  {
    return getClass().getName() + "[" + paramString() + "]";
  }

  public void translate (int x, int y)
  {
    this.x += x;
    this.y += y;
  }
}
