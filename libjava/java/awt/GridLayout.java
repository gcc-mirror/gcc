// GridLayout.java - Grid-based layout engine

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.io.Serializable;

/** This class implements a grid-based layout scheme.  Components are
 * all given the same size and are laid out from left to right and top
 * to bottom.  A GridLayout is configured with a number of rows and a
 * number of columns.  If either is zero then that dimension is
 * computed based on the actual size of the container.  An exception
 * is thrown if an attempt is made to set both the number of rows and
 * the number of columns to 0.  This class also support horizontal and
 * vertical gaps; these are used as spacing between cells.
 */
public class GridLayout implements LayoutManager, Serializable
{
  /** Add a new component to the layout.  This particular implementation
   * does nothing.
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

  /** Create a new GridLayout with one row and any number of columns.
   * Both gaps are set to 0.
   */
  public GridLayout ()
  {
    this (1, 0, 0, 0);
  }

  /** Create a new GridLayout with the specified number of rows and
   * columns.  Both gaps are set to 0.
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
   * @param parent The parent container
   */
  public void layoutContainer (Container parent)
  {
    int num = parent.getComponentCount ();
    // This is more efficient than calling getComponents().
    Component[] comps = parent.component;

    int real_rows = rows;
    int real_cols = cols;
    if (real_rows == 0)
      real_rows = (num + real_cols - 1) / real_cols;
    else
      real_cols = (num + real_rows - 1) / real_rows;

    Dimension d = parent.getSize ();
    Insets ins = parent.getInsets ();

    int tw = d.width - ins.left - ins.right;
    int th = d.height - ins.top - ins.bottom;

    int w = (tw - (real_rows - 1) * hgap) / real_rows;
    int h = (th - (real_cols - 1) * vgap) / real_cols;

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
    int w = 0, h = 0, num = parent.getComponentCount ();
    // This is more efficient than calling getComponents().
    Component[] comps = parent.component;

    for (int i = 0; i < num; ++i)
      {
	// FIXME: can we just directly read the fields in Component?
	// Or will that not work with subclassing?
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

    // We subtract out an extra gap here because the gaps are only
    // between cells.
    return new Dimension (real_rows * (w + hgap) - hgap,
			  real_cols * (h + vgap) - vgap);
  }

  // The gaps.
  private int hgap;
  private int vgap;
  // Number of rows and columns.
  private int rows;
  private int cols;
}
