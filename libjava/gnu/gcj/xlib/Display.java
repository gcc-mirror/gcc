/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

import gnu.gcj.RawData;

/**
 * A connection to an X11 display.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */

public class Display
{
  static
  {
    staticInit();
  }

  public Display()
  {
    init();
  }
    
  private static native void staticInit();
  private native void init();
  protected native void finalize();

  RawData display = null;

  /* TODO?: Rather than storing such data here, we might consider
     using the context manager facilities provided by Xlib... */
  private Dictionary xids = new Hashtable();

  protected final void addXID(int xid, XID window)
  {
    xids.put(new Integer(xid), window);
  }
  
  protected final void removeXID(int xid)
  {
    xids.remove(new Integer(xid));
  }

  public final Window getDefaultRootWindow()
  {
    int rootXID = getDefaultRootWindowXID();
    return getWindow(rootXID);
  }

  public final XID getXID(int xid)
  {
    return (XID) xids.get(new Integer(xid));
  }

  public final Window getWindow(int xid)
  {
    Window window = (Window) getXID(xid);
    if (window == null)
      {
	window = new Window(this, xid);
	addXID(xid, window);
      }
    return window;
  }

  public final Screen getDefaultScreen()
  {
    /* Screens objects are not cached since they are lightweight.
       We just create a new object when requested. */
    return new Screen(this, getDefaultScreenNumber());
  }

  public final native int getDefaultScreenNumber();

  private final native int getDefaultRootWindowXID();
    
  private Dictionary atoms = new Hashtable();

  public final int getAtom(String name)
  {
    Integer atomInt = (Integer) atoms.get(name);
    if (atomInt == null)
      return internAtom(name);
    return atomInt.intValue();
  }

  // TODO?: cache reverse lookup too?
  public final native String getAtomName(int atom);

  private final native int internAtom(String name);

  public native void flush();
}
