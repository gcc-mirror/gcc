/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* Status:  Believed complete and correct. */

public interface LayoutManager
{
  public void addLayoutComponent (String name, Component comp);
  public void layoutContainer (Container parent);
  public Dimension minimumLayoutSize (Container parent);
  public Dimension preferredLayoutSize (Container parent);
  public void removeLayoutComponent (Component comp);
}
