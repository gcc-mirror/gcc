/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

public interface ChoicePeer extends ComponentPeer
{
  void add(String item, int index);
  void remove(int index);
  void select(int index);
}

