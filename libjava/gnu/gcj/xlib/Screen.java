/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/**
 * A flyweight class that denotes an X11 screen.  Display and screen
 * number is the only data kept by this class.  The real screen
 * structure is stored in the display.  There may exist several
 * objects denoting the same screen.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public final class Screen
{
  static final int UNKNOWN = -1;

  Display display;
  int screenNumber = UNKNOWN;
  RawData structure;
  
  Screen(Display display, RawData screenStructure)
  {
    structure = screenStructure;
    this.display = display;
  }

  public Screen(Display display)
  {
    this(display, display.getDefaultScreenNumber());
  }

  public Screen(Display display, int screenNumber)
  {
    this.display = display;
    this.screenNumber = screenNumber;
    initStructure();
  }

  public final Display getDisplay()
  {
    return display;
  }

  public Window getRootWindow()
  {
    int rootXID = getRootWindowXID();
    return display.getWindow(rootXID);
  }

  public Visual getRootVisual()
  {
    RawData visualStructure = getRootVisualStructure();
    int depth = getRootDepth();
    return new Visual(visualStructure, this, depth);
  }

  private native RawData getRootVisualStructure();

  public native int getRootDepth();
  public native int getRootWindowXID();
  public native int getDefaultColormapXID();

  native void initStructure();

  public Colormap getDefaultColormap()
  {
    return new Colormap(this, getDefaultColormapXID());
  }
  
  public final int getScreenNumber()
  {
    if (screenNumber == UNKNOWN)
      screenNumber = findScreenNumber();
    return screenNumber;
  }

  public native int findScreenNumber();
}
