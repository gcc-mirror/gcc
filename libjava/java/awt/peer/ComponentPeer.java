/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.peer;
import java.awt.*;

/* A very incomplete placeholder. */

public interface ComponentPeer
{
  public abstract Toolkit getToolkit ();

  public Dimension getMinimumSize ();

  public Dimension getPreferredSize ();

  public void setBounds (int x, int y, int w, int h);
}
