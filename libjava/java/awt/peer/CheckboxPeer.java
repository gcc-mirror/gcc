/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

public interface CheckboxPeer extends ComponentPeer
{
  void setCheckboxGroup(java.awt.CheckboxGroup group);
  void setLabel(String label);
  void setState(boolean state);
}
