/* Copyright (C) 1999  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* A very incomplete placeholder. */

public class BorderLayout implements LayoutManager2
{
  int hgap;
  int vgap;

  public BorderLayout (int hgap, int vgap)
  {
    this.hgap = hgap;
    this.vgap = vgap;
  }

  public void addLayoutComponent (String name, Component comp)
  { /* FIXME */ }
  public void layoutContainer (Container parent)
  { /* FIXME */ }
  public Dimension minimumLayoutSize (Container parent)
  { /* FIXME */ return null; }
  public Dimension preferredLayoutSize (Container parent)
  { /* FIXME */ return null; }
  public void removeLayoutComponent (Component comp)
  { /* FIXME */ }

  public void addLayoutComponent (Component comp, Object constraints)
  { /* FIXME */ }
  public float getLayoutAlignmentX (Container target)
  { /* FIXME */ return (float) 0.0; }
  public float getLayoutAlignmentY (Container target)
  { /* FIXME */ return (float) 0.0; }
  public void invalidateLayout (Container target)
  { /* FIXME */ }
  public Dimension maximumLayoutSize (Container target)
  { /* FIXME */ return null; }

}
