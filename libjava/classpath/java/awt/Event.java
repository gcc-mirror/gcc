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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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
  static final long serialVersionUID = 5488922509400504703L;

  public static final int SHIFT_MASK = 1;
  public static final int CTRL_MASK = 2;
  public static final int META_MASK = 4;
  public static final int ALT_MASK = 8;

  public static final int ACTION_EVENT = 1001;
  public static final int BACK_SPACE = 8;
  public static final int CAPS_LOCK = 1022;
  public static final int DELETE = 127;
  public static final int DOWN = 1005;
  public static final int END = 1001;
  public static final int ENTER = 10;
  public static final int ESCAPE = 27;
  public static final int F1 = 1008;
  public static final int F10 = 1017;
  public static final int F11 = 1018;
  public static final int F12 = 1019;
  public static final int F2 = 1009;
  public static final int F3 = 1010;
  public static final int F4 = 1011;
  public static final int F5 = 1012;
  public static final int F6 = 1013;
  public static final int F7 = 1014;
  public static final int F8 = 1015;
  public static final int F9 = 1016;
  public static final int GOT_FOCUS = 1004;
  public static final int HOME = 1000;
  public static final int INSERT = 1025;
  public static final int KEY_ACTION = 403;
  public static final int KEY_ACTION_RELEASE = 404;
  public static final int KEY_PRESS = 401;
  public static final int KEY_RELEASE = 402;
  public static final int LEFT = 1006;
  public static final int LIST_DESELECT = 702;
  public static final int LIST_SELECT = 701;
  public static final int LOAD_FILE = 1002;
  public static final int LOST_FOCUS = 1005;
  public static final int MOUSE_DOWN = 501;
  public static final int MOUSE_DRAG = 506;
  public static final int MOUSE_ENTER = 504;
  public static final int MOUSE_EXIT = 505;
  public static final int MOUSE_MOVE = 503;
  public static final int MOUSE_UP = 502;
  public static final int NUM_LOCK = 1023;
  public static final int PAUSE = 1024;
  public static final int PGDN = 1003;
  public static final int PGUP = 1002;
  public static final int PRINT_SCREEN = 1020;
  public static final int RIGHT = 1007;
  public static final int SAVE_FILE = 1003;
  public static final int SCROLL_ABSOLUTE = 605;
  public static final int SCROLL_BEGIN = 606;
  public static final int SCROLL_END = 607;
  public static final int SCROLL_LINE_DOWN = 602;
  public static final int SCROLL_LINE_UP = 601;
  public static final int SCROLL_LOCK = 1021;
  public static final int SCROLL_PAGE_DOWN = 604;
  public static final int SCROLL_PAGE_UP = 603;
  public static final int TAB = 9;
  public static final int UP = 1004;
  public static final int WINDOW_DEICONIFY = 204;
  public static final int WINDOW_DESTROY = 201;
  public static final int WINDOW_EXPOSE = 202;
  public static final int WINDOW_ICONIFY = 203;
  public static final int WINDOW_MOVED = 205;

  public Object arg;
  public int clickCount;
  boolean consumed;             // Required by serialization spec.
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
    return "id=" + id + ",x=" + x + ",y=" + y
      + ",target=" + target + ",arg=" + arg;
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
