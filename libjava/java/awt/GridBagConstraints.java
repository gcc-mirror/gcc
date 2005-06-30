/* GridBagConstraints.java -- Constraints for GridBag layout manager
   Copyright (C) 2000, 2001, 2002, 2004  Free Software Foundation

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.awt;

import java.io.Serializable;

/**
 * This specifies the constraints for a component managed by the
 * GridBagLayout layout manager.
 */
public class GridBagConstraints implements Cloneable, Serializable
{
  static final long serialVersionUID = -1000070633030801713L;

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

  /**
   * Position to where the first text line would end. Equals to NORTHEAST for
   * horizontal left-to-right orientations.
   */
  public static final int FIRST_LINE_END = 24;

  /**
   * Position to where the first text line would start. Equals to NORTHWEST for
   * horizontal left-to-right orientations.
   */
  public static final int FIRST_LINE_START = 23;

  /**
   * Position to where the last text line would end. Equals to SOUTHEAST for
   * horizontal left-to-right orientations.
   */
  public static final int LAST_LINE_END = 26;

  /**
   * Position to where the last text line would start. Equals to SOUTHWEST for
   * horizontal left-to-right orientations.
   */
  public static final int LAST_LINE_START = 25;

  /**
   * Position to where a text line would end. Equals to EAST for
   * left-to-right orientations.
   */
  public static final int LINE_END = 22;

  /**
   * Position to where a text line would start. Equals to WEST for
   * left-to-right orientations.
   */
  public static final int LINE_START = 21;

  /**
   * Position to where a page ends. Equals SOUTH for horizontal orientations.
   */
  public static final int PAGE_END = 20;

  /**
   * Position to where a page starts. Equals NORTH for horizontal orientations.
   */
  public static final int PAGE_START = 19;

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
    try
      {
	GridBagConstraints g = (GridBagConstraints) super.clone ();
	g.insets = (Insets) insets.clone ();
	return g;
      }
    catch (CloneNotSupportedException _)
      {
	// Can't happen.
	return null;
      }
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
