// FlowLayout.java - Grid-based layout engine

/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.io.Serializable;

/** This class implements a flow-based layout.  Components are laid
 * out in order from left to right.  When a component cannot be placed
 * without horizontal clipping, a new row is started.  This class
 * supports horizontal and vertical gaps.  These are used for spacing
 * between components.
 */
public class FlowLayout implements LayoutManager, Serializable
{
  /** Constant that specifies left alignment.  */
  public static final int LEFT = 0;
  /** Constant that specifies center alignment.  */
  public static final int CENTER = 1;
  /** Constant that specifies right alignment.  */
  public static final int RIGHT = 2;

  /** Constant that specifies alignment to leading edge of container's
   * orientation.  */
  public static final int LEADING = 3;
  /** Constant that specifies alignment to trailing edge of container's
   * orientation.  */
  public static final int TRAILING = 4;

  /** Add a new component to the layout.  This particular implementation
   * does nothing.
   */
  public void addLayoutComponent (String name, Component comp)
  {
    // Nothing.
  }

  /** Return the alignment.  */
  public int getAlignment ()
  {
    return align;
  }

  /** Return the horizontal gap.  */
  public int getHgap ()
  {
    return hgap;
  }

  /** Return the vertical gap.  */
  public int getVgap ()
  {
    return vgap;
  }

  /** Create a new FlowLayout with center alignment.
   * Both gaps are set to 5.
   */
  public FlowLayout ()
  {
    this (CENTER, 5, 5);
  }

  /** Create a new FlowLayout with the alignment.
   * columns.  Both gaps are set to 5.
   * @param align Alignment
   */
  public FlowLayout (int align)
  {
    this (align, 5, 5);
  }

  /** Create a new FlowLayout with the specified alignment and gaps.
   * @param align Alignment
   * @param hgap The horizontal gap
   * @param vgap The vertical gap
   * @exception IllegalArgumentException If either gap is negative
   */
  public FlowLayout (int align, int hgap, int vgap)
  {
    // Use methods to set fields so that we can have all the checking
    // in one place.
    setVgap (vgap);
    setHgap (hgap);
    setAlignment (align);
  }

  /** Lay out the container's components based on current settings.
   * @param parent The parent container
   */
  public void layoutContainer (Container parent)
  {
    int num = parent.getComponentCount ();
    // This is more efficient than calling getComponents().
    Component[] comps = parent.component;

    Dimension d = parent.getSize ();
    Insets ins = parent.getInsets ();

    ComponentOrientation orient = parent.getComponentOrientation ();
    boolean left_to_right = orient.isLeftToRight ();

    int y = ins.top + vgap;
    int i = 0;
    while (i < num)
      {
	// Find the components which go in the current row.
	int new_w = ins.left + hgap + ins.right;
	int new_h = 0;
	int j;
	boolean found_one = false;
	for (j = i; j < num && ! found_one; ++j)
	  {
	    // FIXME: this is very inefficient.
	    Dimension c = comps[i].getPreferredSize ();

	    // Skip invisible items.
	    if (! comps[i].visible)
	      continue;

	    int next_w = new_w + hgap + c.width;
	    if (next_w <= d.width || ! found_one)
	      {
		new_w = next_w;
		new_h = Math.max (new_h, c.height);
		found_one = true;
	      }
	    else
	      {
		// Must start a new row, and we already found an item
		break;
	      }
	  }

	// Set the location of each component for this row.
	int x;

	int myalign = align;
	if (align == LEADING)
	  myalign = left_to_right ? LEFT : RIGHT;
	else if (align == TRAILING)
	  myalign = left_to_right ? RIGHT : LEFT;

	if (myalign == LEFT)
	  x = ins.left + hgap;
	else if (myalign == CENTER)
	  x = (d.width - new_w) / 2;
	else
	  x = d.width - new_w;

	for (int k = i; i < j; ++k)
	  {
	    // FIXME: this is very inefficient.
	    Dimension c = comps[i].getPreferredSize ();
	    if (comps[i].visible)
	      {
		comps[i].setLocation (x, y);
		x += c.width + vgap;
	      }
	  }

	// Advance to next row.
	i = j;
	y += new_h + vgap;
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

  /** Set the alignment.
   * @param align The alignment
   */
  public void setAlignment (int align)
  {
    if (align != LEFT && align != RIGHT && align != CENTER
	&& align != LEADING && align != TRAILING)
      throw new IllegalArgumentException ("invalid align: " + align);
    this.align = align;
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
    return ("[" + getClass ().getName () + ",hgap=" + hgap + ",vgap=" + vgap
	    + ",align=" + align + "]");
  }

  // This method is used to compute the various sizes.
  private Dimension getSize (Container parent, boolean is_min)
  {
    int w, h, num = parent.getComponentCount ();
    // This is more efficient than calling getComponents().
    Component[] comps = parent.component;

    w = 0;
    h = 0;
    for (int i = 0; i < num; ++i)
      {
	if (! comps[i].visible)
	  continue;

	// FIXME: can we just directly read the fields in Component?
	// Or will that not work with subclassing?
	Dimension d;

	if (is_min)
	  d = comps[i].getMinimumSize ();
	else
	  d = comps[i].getPreferredSize ();

	w += d.width;
	h = Math.max (d.height, h);
      }

    Insets ins = parent.getInsets ();

    w += (num + 1) * hgap + ins.left + ins.right;
    h += 2 * vgap + ins.top + ins.bottom;

    return new Dimension (w, h);
  }

  // Alignment.
  private int align;
  // The gaps.
  private int hgap;
  private int vgap;
}
