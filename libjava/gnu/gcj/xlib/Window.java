/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;
import java.awt.Rectangle;

/**
 * An X11 window.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class Window extends Drawable
{
  // Must correspond with X.h definitions:
  public static final int COPY_FROM_PARENT = 0;
  public static final int INPUT_OUTPUT     = 1;
  public static final int INPUT_ONLY       = 2;

  public Window(Window parent, Rectangle bounds,
		WindowAttributes attributes)
  {
    this(parent, bounds, attributes, null);
  }
  
  public Window(Window parent, Rectangle bounds,
		WindowAttributes attributes, Visual visual)
  {
    this(parent, bounds, 0, attributes, COPY_FROM_PARENT, visual);
  }

  public Window(Window parent, Rectangle bounds, int borderWidth,
		WindowAttributes attributes, int windowIOClass,
		Visual visual)
  {
    this(parent.display,
	 parent.createChildXID(bounds, borderWidth, attributes, 
			       windowIOClass, visual));
    this.owned = true;
  }

  protected Window(Display display, int xid)
  {
    super(display, xid);
    display.addXID(xid, this);
  }

  protected void finalize()
  {
    display.removeXID(xid);
    if (owned)
      {
	destroy();
	owned = false;
      }
  }

  protected native void destroy();

  protected native int createChildXID(Rectangle bounds,
				      int borderWidth,
				      WindowAttributes attributes,
				      int windowIOClass, 
				      Visual visual);

  public native void setAttributes(WindowAttributes attributes);

  public native void map();
  public native void unmap();
  
  protected boolean owned = false;

  public native void setProperty(int nameAtom, int typeAtom, byte[] data);
  
  public void setProperty(int nameAtom, int typeAtom, String data)
  {
    int length = data.length();
    byte[] byteData = new byte[length];
    
    for (int i=0; i<length; i++)
      byteData[i] = (byte) data.charAt(i);

    setProperty(nameAtom, typeAtom, byteData);
  }

  public native void setWMProtocols(int[] atoms);
  public native int[] getWMProtocols();

  public void setProperty(String nameAtom, String typeAtom, String data)
  {
    int xaName = display.getAtom(nameAtom);
    int xaType = display.getAtom(typeAtom);
    
    setProperty(xaName, xaType, data);
  }

  public native void setBounds(int x, int y, int width, int height);
}
