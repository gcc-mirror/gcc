/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.Dimension;

public interface ListPeer extends ComponentPeer
{
  void add(String item, int index);
  void delItems(int start_index, int end_index);
  void deselect(int index);
  int[] getSelectedIndexes();
  void makeVisible(int index);
  void removeAll();
  void select(int index);
  void setMultipleMode(boolean multipleMode);
}
