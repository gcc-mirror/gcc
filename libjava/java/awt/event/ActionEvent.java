/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/* Status: Believed complete and correct to JDK 1.2.  */

public class ActionEvent extends AWTEvent
{
  public static final int ACTION_FIRST = 1001;
  public static final int ACTION_LAST = 1001;
  public static final int ACTION_PERFORMED = 1001;
  public static final int ALT_MASK = 8;
  public static final int CTRL_MASK = 2;
  public static final int META_MASK = 4;
  public static final int SHIFT_MASK = 1;

  String actionCommand;
  int modifiers;

  public ActionEvent (Object source, int id, String command)
  {
    super(source, id);
    actionCommand = command;
  }

  public ActionEvent (Object source, int id, String command, int modifiers)
  {
    super(source, id);
    actionCommand = command;
    this.modifiers = modifiers;
  }

  public String getActionCommand ()
  {
    return actionCommand;
  }

  public int getModifiers ()
  {
    return modifiers;
  }

  public String paramString ()
  {
    return ("ActionEvent[" + actionCommand + "," + modifiers
	    + ";" + super.paramString () + "]");
  }
}
