/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.event.KeyEvent;

/* Status: Complete, except for hashCode(). Untested. */

public class MenuShortcut implements java.io.Serializable
{
  // Fields from the serialization spec. Decalare others "transient".
  int key;
  boolean usesShift;

  public MenuShortcut(int key)
  {
    this.key = key;
  }

  public MenuShortcut(int key, boolean useShiftModifier)
  {
    this.key = key;
    this.usesShift = useShiftModifier;
  }

  public int getKey()
  {
    return key;
  }

  public boolean usesShiftModifier()
  {
    return usesShift;
  }

  public boolean equals(MenuShortcut ms)
  {
    return (ms.key == key && ms.usesShift == usesShift);
  }

  public boolean equals(Object obj)
  {
    if (obj instanceof MenuShortcut)
      {
        MenuShortcut ms = (MenuShortcut) obj;
	return (ms.key == key && ms.usesShift == usesShift);
      }      
    return false;
  }

  public int hashCode()
  {
    // FIXME: find/implement the correct algorithm for this
    if (usesShift)
      return (2 * key);
    else
      return key;
  }

  public String toString()
  {
    return paramString(); // ?
  }
  
  protected String paramString()
  {
    return KeyEvent.getKeyText(key);
  }
}
