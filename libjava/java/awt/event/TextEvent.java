/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/* Status: Believed complete and correct to JDK 1.2.  */

public class TextEvent extends AWTEvent
{
  public static final int TEXT_FIRST = 900;
  public static final int TEXT_LAST = 900;
  public static final int TEXT_VALUE_CHANGED = 900;

  public TextEvent (Object source, int id)
  {
    super (source, id);
  }

  public String paramString ()
  {
    return "TEXT_VALUE_CHANGED";
  }
}
