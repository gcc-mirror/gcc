/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.MenuItem;

public interface MenuPeer extends MenuItemPeer
{
  void add(MenuItem item);
  void addSeparator();
  void remove(int index);
}
