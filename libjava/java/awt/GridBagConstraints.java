// GridBagConstraints.java - Constraints for GridBag layout manager

/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.io.Serializable;

/** This specifies the constraints for a component managed by the
 * GridBagLayout layout manager.  */
public class GridBagConstraints implements Cloneable, Serializable
{
  /** Fill in both directions.  */
  public static final int BOTH = 1;
  /** Don't fill.  */
  public static final int NONE = 0;
  /** Fill horizontally.  */
  public static final int HORIZONTAL = 2;
  /** Fill vertically.  */
  public static final int VERTICAL = 3;

  /** Position in the center.  */
  public static final int CENTER = 10;
  /** Position to the east.  */
  public static final int EAST = 13;
  /** Position to the north.  */
  public static final int NORTH = 11;
  /** Position to the northeast.  */
  public static final int NORTHEAST = 12;
  /** Position to the northwest.  */
  public static final int NORTHWEST = 18;
  /** Position to the south.  */
  public static final int SOUTH = 15;
  /** Position to the southeast.  */
  public static final int SOUTHEAST = 14;
  /** Position to the southwest.  */
  public static final int SOUTHWEST = 16;
  /** Position to the west.  */
  public static final int WEST = 17;

  /** Occupy all remaining cells except last cell.  */
  public static final int RELATIVE = -1;
  /** Occupy all remaining cells.  */
  public static final int REMAINDER = 0;

  public int anchor;
  public int fill;
  public int gridheight;
  public int gridwidth;
  public int gridx;
  public int gridy;
  public Insets insets;
  public int ipadx;
  public int ipady;
  public double weightx;
  public double weighty;

  /** Create a copy of this object.  */
  public Object clone ()
  {
    GridBagConstraints g = (GridBagConstraints) super.clone ();
    g.insets = (Insets) insets.clone ();
    return g;
  }

  /** Create a new GridBagConstraints object with the default
   * parameters.  */
  public GridBagConstraints ()
  {
    this.anchor = CENTER;
    this.fill = NONE;
    this.gridx = RELATIVE;
    this.gridy = RELATIVE;
    this.gridwidth = 1;
    this.gridheight = 1;
    this.ipadx = 0;
    this.ipady = 0;
    this.insets = new Insets (0, 0, 0, 0);
    this.weightx = 0;
    this.weighty = 0;
  }

  /** Create a new GridBagConstraints object with the indicated
   * parameters.  */
  public GridBagConstraints (int gridx, int gridy,
			     int gridwidth, int gridheight,
			     double weightx, double weighty,
			     int anchor, int fill,
			     Insets insets, int ipadx, int ipady)
  {
    this.anchor = anchor;
    this.fill = fill;
    this.gridx = gridx;
    this.gridy = gridy;
    this.gridwidth = gridwidth;
    this.gridheight = gridheight;
    this.ipadx = ipadx;
    this.ipady = ipady;
    this.insets = insets;
    this.weightx = weightx;
    this.weighty = weighty;
  }
}
