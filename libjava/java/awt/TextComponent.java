/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;

/* A very incomplete placeholder. */

public class TextComponent extends Component
{
  char[] buffer;
  int length;
  int caretPosition;

  public synchronized String getText ()
  { return new String(buffer, 0, length); }

  public synchronized void setText (String text)
  {
    length = text.length();
    if (buffer == null || buffer.length < length)
      buffer = new char[length];
    text.getChars(0, length, buffer, 0);
  }

  public synchronized void addTextListener (TextListener listener)
  { /* FIXME */ }

  public int getCaretPosition () { return caretPosition; }

  public void setCaretPosition (int pos) { caretPosition = pos; }

}
