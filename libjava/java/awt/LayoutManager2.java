/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* Status:  Believed complete and correct. */

public interface LayoutManager2 extends LayoutManager
{
  public void addLayoutComponent (Component comp, Object constraints);
  public float getLayoutAlignmentX (Container target);
  public float getLayoutAlignmentY (Container target);
  public void invalidateLayout (Container target);
  public Dimension maximumLayoutSize (Container target);
}
