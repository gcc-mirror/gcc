/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.WindowListener;

/* A very incomplete placeholder. */

public class Window extends Container
{
  public void dispose ()
  { /* FIXME */ }

  public synchronized void addWindowListener (WindowListener listener)
  { /* FIXME */ }


  public void show ()
  {
    addNotify();
    // validate FIXME
    // validate setVisible FIXME
  }
}
