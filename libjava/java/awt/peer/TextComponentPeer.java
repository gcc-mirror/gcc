/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

public interface TextComponentPeer extends ComponentPeer
{
  int getCaretPosition();
  int getSelectionEnd();
  int getSelectionStart();
  String getText();
  void select(int start, int end);
  void setCaretPosition(int pos);
  void setEditable(boolean editable);
  void setText(String text);
}
