// GridLayout.java - Grid-based layout engine

/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

/** This class implements a grid-based layout scheme.  Components are
 * all given the same size and are laid out from left to right and top
 * to bottom.  A GridLayout is configured with a number of rows and a
 * number of columns.  If both are specified, then the number of
 * columns is ignored and is derived from the number of rows and the
 * total number of components.  If either is zero then that dimension
 * is computed based on the actual size of the container.  An
 * exception is thrown if an attempt is made to set both the number of
 * rows and the number of columns to 0.  This class also supports
 * horizontal and vertical gaps; these are used as spacing between
 * cells.
 *
 * @author Tom Tromey <tromey@redhat.com>
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class GridLayout implements LayoutManager, Serializable
{
  /** Add a new component to the layout.  This particular implementation
   * does nothing.
   * @param name The name of the component to add.
   * @param component The component to add.
   */
  public void addLayoutComponent (String name, Component comp)
  {
    // Nothing.
  }

  /** Return the number of columns in this layout.  */
  public int getColumns ()
  {
    return cols;
  }

  /** Return the horizontal gap.  */
  public int getHgap ()
  {
    return hgap;
  }

  /** Return the number of rows in this layout.  */
  public int getRows ()
  {
    return rows;
  }

  /** Return the vertical gap.  */
  public int getVgap ()
  {
    return vgap;
  }

  /** Create a new <code>GridLayout</code> with one row and any number
   * of columns.  Both gaps are set to 0.
   */
  public GridLayout ()
  {
    this (1, 0, 0, 0);
  }

  /** Create a new <code>GridLayout</code> with the specified number
   * of rows and columns.  Both gaps are set to 0.  Note that the row
   * and column settings cannot both be zero.  If both the row and
   * column values are non-zero, the rows value takes precedence.
   * @param rows Number of rows
   * @param cols Number of columns
   * @exception IllegalArgumentException If rows and columns are both
   *        0, or if either are negative
   */
  public GridLayout (int rows, int cols)
  {
    this (rows, cols, 0, 0);
  }

  /** Create a new GridLayout with the specified number of rows and
   * columns and the specified gaps.
   * Note that the row and column settings cannot both be
   * zero.  If both the row and column values are non-zero, the rows value
   * takes precedence.
   * @param rows Number of rows
   * @param cols Number of columns
   * @param hgap The horizontal gap
   * @param vgap The vertical gap
   * @exception IllegalArgumentException If rows and columns are both
   *        0, if either are negative, or if either gap is negative
   */
  public GridLayout (int rows, int cols, int hgap, int vgap)
  {
    if (rows < 0)
      throw new IllegalArgumentException ("number of rows cannot be negative");
    if (cols < 0)
      throw new IllegalArgumentException ("number of columns cannot be negative");
    if (rows == 0 && cols == 0)
      throw new IllegalArgumentException ("both rows and columns cannot be 0");
    if (hgap < 0)
      throw new IllegalArgumentException ("horizontal gap must be nonnegative");
    if (vgap < 0)
      throw new IllegalArgumentException ("vertical gap must be nonnegative");
    this.rows = rows;
    this.cols = cols;
    this.hgap = hgap;
    this.vgap = vgap;
  }

  /** Lay out the container's components based on current settings.
   * The free space in the container is divided evenly into the specified
   * number of rows and columns in this object.
   * @param parent The container to lay out
   */
  public void layoutContainer (Container parent)
  {
    int num = parent.ncomponents;
    // This is more efficient than calling getComponents().
    Component[] comps = parent.component;

    int real_rows = rows;
    int real_cols = cols;
    if (real_rows == 0)
      real_rows = (num + real_cols - 1) / real_cols;
    else
      real_cols = (num + real_rows - 1) / real_rows;

    // We might have less than a single row.  In this case we expand
    // to fill.
    if (num < real_cols)
      real_cols = num;

    Dimension d = parent.getSize ();
    Insets ins = parent.getInsets ();

    // Compute width and height of each cell in the grid.
    int tw = d.width - ins.left - ins.right;
    tw = (tw - (real_cols - 1) * hgap) / real_cols;
    int th = d.height - ins.top - ins.bottom;
    th = (th - (real_rows - 1) * vgap) / real_rows;

    // If the cells are too small, still try to do something.
    if (tw < 0)
      tw = 1;
    if (th < 0)
      th = 1;

    int x = ins.left;
    int y = ins.top;
    int i = 0;
    int recount = 0;

    while (i < num)
      {
	comps[i].setBounds (x, y, tw, th);

	++i;
	++recount;
	if (recount == real_cols)
	  {
	    recount = 0;
	    y += vgap + th;
	    x = ins.left;
	  }
	else
	  x += hgap + tw;
      }
  }

  /** Get the minimum layout size of the container.
   * @param cont The parent container
   */
  public Dimension minimumLayoutSize (Container cont)
  {
    return getSize (cont, true);
  }

  /** Get the preferred layout size of the container.
   * @param cont The parent container
   */
  public Dimension preferredLayoutSize (Container cont)
  {
    return getSize (cont, false);
  }

  /** Remove the indicated component from this layout manager.
   * This particular implementation does nothing.
   * @param comp The component to remove
   */
  public void removeLayoutComponent (Component comp)
  {
    // Nothing.
  }

  /** Set the number of columns.
   * @param newCols
   * @exception IllegalArgumentException If the number of columns is
   *     negative, or if the number of columns is zero and the number
   *     of rows is already 0.
   */
  public void setColumns (int newCols)
  {
    if (cols < 0)
      throw new IllegalArgumentException ("number of columns cannot be negative");
    if (newCols == 0 && rows == 0)
      throw new IllegalArgumentException ("number of rows is already 0");
    this.cols = newCols;
  }

  /** Set the horizontal gap
   * @param hgap The horizontal gap
   * @exception IllegalArgumentException If the hgap value is less than zero.
   */
  public void setHgap (int hgap)
  {
    if (hgap < 0)
      throw new IllegalArgumentException ("horizontal gap must be nonnegative");
    this.hgap = hgap;
  }

  /** Set the number of rows
   * @param newRows
   * @exception IllegalArgumentException If the number of rows is
   *     negative, or if the number of rows is zero and the number
   *     of columns is already 0.
   */
  public void setRows (int newRows)
  {
    if (rows < 0)
      throw new IllegalArgumentException ("number of rows cannot be negative");
    if (newRows == 0 && cols == 0)
      throw new IllegalArgumentException ("number of columns is already 0");
    this.rows = newRows;
  }

  /** Set the vertical gap.
   * @param vgap The vertical gap
   * @exception IllegalArgumentException If the vgap value is less than zero.
   */
  public void setVgap (int vgap)
  {
    if (vgap < 0)
      throw new IllegalArgumentException ("vertical gap must be nonnegative");
    this.vgap = vgap;
  }

  /** Return String description of this object.  */
  public String toString ()
  {
    return ("[" + getClass ().getName ()
	    + ",hgap=" + hgap + ",vgap=" + vgap
	    + ",rows=" + rows + ",cols=" + cols
	    + "]");
  }

  // This method is used to compute the various sizes.
  private Dimension getSize (Container parent, boolean is_min)
  {
    int w = 0, h = 0, num = parent.ncomponents;
    // This is more efficient than calling getComponents().
    Component[] comps = parent.component;

    for (int i = 0; i < num; ++i)
      {
	Dimension d;

	if (is_min)
	  d = comps[i].getMinimumSize ();
	else
	  d = comps[i].getPreferredSize ();

	w = Math.max (d.width, w);
	h = Math.max (d.height, h);
      }

    int real_rows = rows;
    int real_cols = cols;
    if (real_rows == 0)
      real_rows = (num + real_cols - 1) / real_cols;
    else
      real_cols = (num + real_rows - 1) / real_rows;

    Insets ins = parent.getInsets ();
    // We subtract out an extra gap here because the gaps are only
    // between cells.
    w = ins.left + ins.right + real_cols * (w + hgap) - hgap;
    h = ins.top + ins.bottom + real_rows * (h + vgap) - vgap;
    return new Dimension (w, h);
  }

  /**
   * @serial The number of columns in the grid.
   */
  private int cols;

  /**
   * @serial The number of rows in the grid.
   */
  private int rows;

  /**
   * @serial The horizontal gap between columns
   */
  private int hgap;

  /**
   * @serial The vertical gap between rows
   */
  private int vgap;
}
