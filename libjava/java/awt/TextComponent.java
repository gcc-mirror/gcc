/* Copyright (C) 1999, 2001  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

import java.awt.event.*;
import java.awt.peer.TextComponentPeer;

/* A very incomplete placeholder. */

public class TextComponent extends Component
{
  protected TextListener textListener;

  char[] buffer;
  int length;
  int caretPosition;

  public synchronized void addTextListener (TextListener listener)
  {
    textListener = AWTEventMulticaster.add (textListener, listener);
  }

  public synchronized String getText ()
  { return new String(buffer, 0, length); }

  public synchronized void setText (String text)
  {
    length = text.length();
    if (buffer == null || buffer.length < length)
      buffer = new char[length];
    text.getChars(0, length, buffer, 0);
  }

  public int getCaretPosition () { return caretPosition; }

  public void setCaretPosition (int pos)
  {
    caretPosition = pos;
    if (peer != null)
      {
	TextComponentPeer t = (TextComponentPeer) peer;
	t.setCaretPosition (pos);
      }
  }
}
