/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/* Status: Believed complete and correct to JDK 1.2.  */

public abstract class InputEvent extends ComponentEvent
{
  public static final int ALT_GRAPH_MASK = 32;
  public static final int ALT_MASK = 8;
  public static final int BUTTON1_MASK = 16;
  public static final int BUTTON2_MASK = 8;
  public static final int BUTTON3_MASK = 4;
  public static final int CTRL_MASK = 2;
  public static final int META_MASK = 4;
  public static final int SHIFT_MASK = 1;

  InputEvent (Component source, int id)  // Not public
  {
    super(source, id);
  }

  public boolean isShiftDown ()
  {
    return (modifiers & SHIFT_MASK) != 0;
  }

  public boolean isControlDown ()
  {
    return (modifiers & CTRL_MASK) != 0;
  }

  public boolean isMetaDown ()
  {
    return (modifiers & META_MASK) != 0;
  }

  public boolean isAltDown ()
  {
    return (modifiers & ALT_MASK) != 0;
  }

  public long getWhen ()
  {
    return when;
  }

  public int getModifiers ()
  {
    return modifiers;
  }

  public boolean isConsumed ()
  {
    return consumed;
  }

  public void consume ()
  {
    /* FIXME */
    consumed = true;
  }

  private long when;
  private int modifiers;
}
