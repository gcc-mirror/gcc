/* FlowLayout.java -- Grid-based layout engine
   Copyright (C) 1999, 2000, 2001, 2002, 2004  Free Software Foundation

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

/** This class implements a flow-based layout.  Components are laid
 * out in order from left to right.  When a component cannot be placed
 * without horizontal clipping, a new row is started.  This class
 * supports horizontal and vertical gaps.  These are used for spacing
 * between components.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
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

  // Serialization constant
  private static final long serialVersionUID = -7262534875583282631L;

  /**
   * Add a new component to the layout.  This particular implementation
   * does nothing.
   *
   * @param name the name
   * @param comp the component
   */
  public void addLayoutComponent (String name, Component comp)
  {
    // Nothing.
  }

  /**
   * Returns the current justification value for this object.
   *
   * @return The current justification value for this object.
   */
  public int getAlignment ()
  {
    return align;
  }

  /**
   * Returns the horizontal gap between components.
   *
   * @return The horizontal gap between components.
   */
  public int getHgap ()
  {
    return hgap;
  }

  /**
   * Returns the vertical gap between lines of components.
   *
   * @return The vertical gap between lines of components.
   */
  public int getVgap ()
  {
    return vgap;
  }

  /**
   * Initializes a new instance of <code>FlowLayout</code> with a center
   * justification and a default horizontal and vertical gap of 5.
   */
  public FlowLayout ()
  {
    this (CENTER, 5, 5);
  }

  /**
   * Initializes a new instance of <code>FlowLayout</code> with the specified
   * justification and a default horizontal and vertical gap of 5.
   *
   * @param align The justification setting, which should be one of the
   * contants in this class.
   */
  public FlowLayout (int align)
  {
    this (align, 5, 5);
  }

  /**
   * Initializes a new instance of <code>FlowLayout</code> with the specified
   * justification and gap values
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
    synchronized (parent.getTreeLock ())
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
            for (j = i; j < num; ++j)
              {
                // Skip invisible items.
                if (! comps[j].visible)
                  continue;

                Dimension c = comps[j].getPreferredSize ();

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

            if (myalign == RIGHT)
              x = ins.left + (d.width - new_w) + hgap;
            else if (myalign == CENTER)
              x = ins.left + (d.width - new_w) / 2 + hgap;
            else // LEFT and all other values of align.
              x = ins.left + hgap;

            for (int k = i; k < j; ++k)
              {
                if (comps[k].visible)
                  {
                    Dimension c = comps[k].getPreferredSize ();
                    comps[k].setBounds (x, y + (new_h - c.height) / 2,
                                        c.width, c.height);
                    x += c.width + hgap;
                  }
              }

            // Advance to next row.
            i = j;
            y += new_h + vgap;
          }
      }
  }

  /**
   * Returns the minimum layout size for the specified container using
   * this layout.
   * @param cont The parent container
   * @return The minimum layout size.
   */
  public Dimension minimumLayoutSize (Container cont)
  {
    return getSize (cont, true);
  }

  /**
   * Returns the preferred layout size for the specified container using
   * this layout.
   * @param cont The parent container
   * @return The preferred layout size.
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

  /**
   * Sets the justification value for this object to the specified value.
   *
   * @param align The new justification value for this object, which must
   * be one of the constants in this class.
   */
  public void setAlignment (int align)
  {
    // The JDK accepts invalid values and treats them as
    // LEFT during layout, so do we. The invalid value is even stored,
    // getAlignment() returns the same invalid value.
    this.align = align;
  }

  /**
   * Sets the horizontal gap between lines of components to the specified value.
   * No Exception is thrown if hgap < 0.
   *
   * @param hgap The new horizontal gap between components.
   */
  public void setHgap (int hgap)
  {
    this.hgap = hgap;
  }

  /**
   * Sets the vertical gap between lines of components to the specified value.
   * No Exception is thrown if vgap < 0.
   *
   * @param vgap The new vertical gap.
   */
  public void setVgap (int vgap)
  {
    this.vgap = vgap;
  }

  /** Return String description of this object.
   * @return A string representation of this object.
   */
  public String toString ()
  {
    return ("[" + getClass ().getName () + ",hgap=" + hgap + ",vgap=" + vgap
            + ",align=" + align + "]");
  }

  // This method is used to compute the various sizes.
  private Dimension getSize (Container parent, boolean is_min)
  {
    synchronized (parent.getTreeLock ())
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

        if (num == 0)
          w += 2 * hgap + ins.left + ins.right;
        else
          w += (num + 1) * hgap + ins.left + ins.right;
        h += 2 * vgap + ins.top + ins.bottom;

        return new Dimension (w, h);
      }
  }

  /**
   * @serial The justification alignment of the lines of components, which
   * will be one of the constants defined in this class.
   */
  private int align;

  /**
   * @serial The horizontal gap between components.
   */
  private int hgap;

  /**
   * @serial The vertical gap between lines of components.
   */
  private int vgap;
}
