/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.Menu;

public interface MenuBarPeer extends MenuComponentPeer
{
  void addHelpMenu(Menu menu);
  void add(Menu menu);
  void remove(int index);
}
