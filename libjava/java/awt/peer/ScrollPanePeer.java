/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.Adjustable;

public interface ScrollPanePeer extends ContainerPeer
{
  void childResized(int width, int height);
  int getHScrollbarHeight();
  int getVScrollbarWidth();
  void setScrollPosition(int x, int y);
  void setUnitIncrement(Adjustable adj, int increment);
  void setValue(Adjustable adj, int value);
}
