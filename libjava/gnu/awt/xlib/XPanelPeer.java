/* Copyright (C) 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.*;
import java.awt.peer.*;
import java.awt.image.*;
import gnu.gcj.xlib.WMSizeHints;
import gnu.gcj.xlib.WindowAttributes;
import gnu.gcj.xlib.Display;
import gnu.gcj.xlib.Visual;
import gnu.gcj.xlib.Screen;

public class XPanelPeer extends XCanvasPeer implements PanelPeer
{

  public XPanelPeer(Panel panel)
  {
    super(panel);
  }

  // no reason to override yet
  //void initWindowProperties();
  //gnu.gcj.xlib.Window getParentWindow();
  

  // Implementing ContainerPeer:
  
  // Default is no insets...
  static final Insets INSETS_0_PROTOTYPE = new Insets(0, 0, 0, 0);

  public Insets getInsets()
  {
    return (Insets) INSETS_0_PROTOTYPE.clone();
  }

  public Insets insets()
  {
    return getInsets();
  }

  public void beginValidate()
  {
    // NOP
  }
  
  public void endValidate()
  {
    // NOP
  }
}
