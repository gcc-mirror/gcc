// CardLayout.java - Card-based layout engine

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.util.Enumeration;
import java.util.Hashtable;
import java.io.Serializable;

/** This class implements a card-based layout scheme.  Each included
 * component is treated as a card.  Only one card can be shown at a
 * time.  This class includes methods for changing which card is
 * shown.
 *
 * @version 0.0
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 2, 2000
 */
public class CardLayout implements LayoutManager2, Serializable
{
  /** Create a new CardLayout object with both gaps zero.  */
  public CardLayout ()
  {
    this (0, 0);
  }

  /** Create a new CardLayout object with the specified horizontal and
   * vertical gaps.
   * @param hgap The horizontal gap
   * @param vgap The vertical gap
   */
  public CardLayout (int hgap, int vgap)
  {
    this.hgap = hgap;
    this.vgap = vgap;
    this.map = new Hashtable ();
  }

  /** Add a new component to the layout.  The constraint must be a
   * string which is used to name the component.  This string can
   * later be used to refer to the particular component.
   * @param comp The component to add
   * @param constraints The name by which the component can later be called
   * @exception IllegalArgumentException If `constraints' is not a string
   */
  public void addLayoutComponent (Component comp, Object constraints)
  {
    if (! (constraints instanceof String))
      throw new IllegalArgumentException ("Object " + constraints
					  + " is not a string");
    map.put (constraints, comp);
  }

  /** Add a new component to the layout.  The name can be used later
   * to refer to the component.
   * @param name The name by which the component can later be called
   * @param comp The component to add
   * @deprecated
   */
  public void addLayoutComponent (String name, Component comp)
  {
    addLayoutComponent (comp, name);
  }

  /** Cause the first component in the container to be displayed.
   * @param parent The parent container
   */
  public void first (Container parent)
  {
    gotoComponent (parent, FIRST, null);
  }

  /** Return this layout manager's horizontal gap.  */
  public int getHgap ()
  {
    return hgap;
  }

  /** Return this layout manager's x alignment.  This method always
   * returns Component.CENTER_ALIGNMENT.
   * @param parent Container using this layout manager instance
   */
  public float getLayoutAlignmentX (Container parent)
  {
    return Component.CENTER_ALIGNMENT;
  }

  /** Returns this layout manager's y alignment.  This method always
   * returns Component.CENTER_ALIGNMENT.
   * @param parent Container using this layout manager instance
   */
  public float getLayoutAlignmentY (Container parent)
  {
    return Component.CENTER_ALIGNMENT;
  }

  /** Return this layout manager's vertical gap.  */
  public int getVgap ()
  {
    return vgap;
  }

  /** Invalidate this layout manager's state.  */
  public void invalidateLayout (Container target)
  {
    // Do nothing.
  }

  /** Cause the last component in the container to be displayed.
   * @param parent The parent container
   */
  public void last (Container parent)
  {
    gotoComponent (parent, LAST, null);
  }

  /** Lay out the container's components based on the current
   * settings.
   * @param parent The parent container
   */
  public void layoutContainer (Container parent)
  {
    int width = parent.width;
    int height = parent.height;

    Insets ins = parent.getInsets ();

    int num = parent.ncomponents;
    Component[] comps = parent.component;

    for (int i = 0; i < num; ++i)
      {
	if (comps[i].isVisible ())
	  {
	    // Only resize the one we care about.
	    comps[i].setBounds (hgap + ins.left, vgap + ins.top,
				width - 2 * hgap - ins.left - ins.right,
				height - 2 * vgap - ins.top - ins.bottom);
	    break;
	  }
      }
  }

  /** Get the maximum layout size of the container.
   * @param target The parent container
   */
  public Dimension maximumLayoutSize (Container target)
  {
    // The JCL says that this returns Integer.MAX_VALUE for both
    // dimensions.  But that just seems wrong to me.
    return getSize (target, MAX);
  }

  /** Get the minimum layout size of the container.
   * @param target The parent container
   */
  public Dimension minimumLayoutSize (Container target)
  {
    return getSize (target, MIN);
  }

  /** Cause the next component in the container to be displayed.
   * @param parent The parent container
   */
  public void next (Container parent)
  {
    gotoComponent (parent, NEXT, null);
  }

  /** Get the preferred layout size of the container.
   * @param target The parent container
   */
  public Dimension preferredLayoutSize (Container parent)
  {
    return getSize (parent, PREF);
  }

  /** Cause the previous component in the container to be displayed.
   * @param parent The parent container
   */
  public void previous (Container parent)
  {
    gotoComponent (parent, PREV, null);
  }

  /** Remove the indicated component from this layout manager.
   * @param comp The component to remove
   */
  public void removeLayoutComponent (Component comp)
  {
    Enumeration e = map.keys ();
    while (e.hasMoreElements ())
      {
	Object key = e.nextElement ();
	if (map.get (key) == comp)
	  {
	    map.remove (key);
	    break;
	  }
      }
  }

  /** Set this layout manager's horizontal gap.
   * @param hgap The new gap
   */
  public void setHgap (int hgap)
  {
    this.hgap = hgap;
  }

  /** Set this layout manager's vertical gap.
   * @param vgap The new gap
   */
  public void setVgap (int vgap)
  {
    this.vgap = vgap;
  }

  /** Cause the named component to be shown.  If the component name is
   * unknown, this method does nothing.
   * @param parent The parent container
   * @param name The name of the component to show
   */
  public void show (Container parent, String name)
  {
    Object target = map.get (name);
    if (target != null)
      gotoComponent (parent, NONE, (Component) target);
  }

  public String toString ()
  {
    return getClass ().getName () + "[" + hgap + "," + vgap + "]";
  }

  // This implements first(), last(), next(), and previous().
  private void gotoComponent (Container parent, int what,
			      Component target)
  {
    int num = parent.getComponentCount ();
    // This is more efficient than calling getComponents().
    Component[] comps = parent.component;
    int choice = -1;

    if (what == FIRST)
      choice = 0;
    else if (what == LAST)
      choice = num;
    else if (what >= 0)
      choice = what;

    for (int i = 0; i < num; ++i)
      {
	// If TARGET is set then we are looking for a specific
	// component.
	if (target != null)
	  {
	    if (target == comps[i])
	      choice = i;
	  }

	if (comps[i].isVisible ())
	  {
	    if (what == NEXT)
	      {
		choice = i + 1;
		if (choice == num)
		  choice = num - 1;
	      }
	    else if (what == PREV)
	      {
		choice = i - 1;
		if (choice < 0)
		  choice = 0;
	      }
	    else
	      {
		// Do nothing if we're already looking at the right
		// component.
		if (choice == i)
		  return;
	      }
	    comps[i].setVisible (false);

	    if (choice >= 0)
	      break;
	  }
      }

    comps[choice].setVisible (true);
  }

  // Compute the size according to WHAT.
  private Dimension getSize (Container parent, int what)
  {
    int w = 0, h = 0, num = parent.ncomponents;
    Component[] comps = parent.component;

    for (int i = 0; i < num; ++i)
      {
	Dimension d;

	if (what == MIN)
	  d = comps[i].getMinimumSize ();
	else if (what == MAX)
	  d = comps[i].getMaximumSize ();
	else
	  d = comps[i].getPreferredSize ();

	w = Math.max (d.width, w);
	h = Math.max (d.height, h);
      }

    Insets i = parent.getInsets ();
    w += 2 * hgap + i.right + i.left;
    h += 2 * vgap + i.bottom + i.top;

    // Handle overflow.
    if (w < 0)
      w = Integer.MAX_VALUE;
    if (h < 0)
      h = Integer.MAX_VALUE;

    return new Dimension (w, h);
  }

  // The gaps.
  private int hgap;
  private int vgap;

  // This hashtable maps a name to a component.
  private Hashtable map;

  // These constants are used by the private gotoComponent method.
  private int FIRST = 0;
  private int LAST = 1;
  private int NEXT = 2;
  private int PREV = 3;
  private int NONE = 4;

  // These constants are used by the private getSize method.
  private int MIN = 0;
  private int MAX = 1;
  private int PREF = 2;
}
