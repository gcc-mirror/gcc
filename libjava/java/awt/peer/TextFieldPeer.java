/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.peer;

import java.awt.Dimension;

public interface TextFieldPeer extends TextComponentPeer
{
  Dimension getMinimumSize(int columns);
  Dimension getPreferredSize(int columns);
  Dimension minimumSize(int columns);
  Dimension preferredSize(int columns);
  void setEchoChar(char echo);
  void setEchoCharacter(char echo);
}
