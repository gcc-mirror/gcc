/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.Image;
import java.awt.MenuBar;

public interface FramePeer extends WindowPeer
{
  void setIconImage(Image image);
  void setMenuBar(MenuBar mb);
  void setResizable(boolean resizable);
  void setTitle(String title);
}
