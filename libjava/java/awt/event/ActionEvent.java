/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/* A very incomplete placeholder. */

public class ActionEvent extends AWTEvent
{
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

  public String getActionCommand () { return actionCommand; }

  public int getModifiers () { return modifiers; }
}
