/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;

/* A very incomplete placeholder. */

public class InputEvent extends ComponentEvent
{
  InputEvent (Object source, int id)  // Not public
  {
    super(source, id);
  }

  public void consume ()
  { /* FIXME */ }
}
