/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/* A very incomplete placeholder. */

public class KeyEvent extends InputEvent
{
  int keyCode;
  char keyChar;
  int modifiers;

  public KeyEvent (Component source, int id, long when,
		  int modifiers, int keyCode, char keyChar)
  {
    super(source, id);
    this.keyCode = keyCode;
    this.keyChar = keyChar;
    this.modifiers = modifiers;
  }

  public int getKeyCode () { return keyCode; }

  public char getKeyChar () { return keyChar; }

  public void setKeyCode (int keyCode) { this.keyCode = keyCode; }

  public void setKeyChar (char keyChar) { this.keyChar = keyChar; }

  public void setModifiers (int modifiers) { this.modifiers = modifiers; }
}
