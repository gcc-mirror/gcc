/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.Dimension;

public interface TextAreaPeer extends TextComponentPeer
{
  Dimension getMinimumSize(int rows, int columns);
  Dimension getPreferredSize(int rows, int columns);
  void insert(String text, int pos);
  void insertText(String text, int pos);
  Dimension minimumSize(int rows, int cols);
  Dimension preferredSize(int rows, int cols);
  void replaceRange(String text, int start, int end);
  void replaceText(String text, int start, int end);
}
