/* CardLayout.java -- Card-based layout engine
   Copyright (C) 1999, 2000, 2002, 2003, 2004  Free Software Foundation

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
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * This class implements a card-based layout scheme.  Each included
 * component is treated as a card.  Only one card can be shown at a
 * time.  This class includes methods for changing which card is
 * shown.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class CardLayout implements LayoutManager2, Serializable
{
  private static final long serialVersionUID = -4328196481005934313L;

  /**
   * Initializes a new instance of <code>CardLayout</code> with horizontal
   * and vertical gaps of 0.
   */
  public CardLayout ()
  {
    this (0, 0);
  }

  /**
   * Create a new <code>CardLayout</code> object with the specified
   * horizontal and vertical gaps.
   *
   * @param hgap The horizontal gap
   * @param vgap The vertical gap
   */
  public CardLayout (int hgap, int vgap)
  {
    this.hgap = hgap;
    this.vgap = vgap;
    this.tab = new Hashtable ();
  }

  /**
   * Add a new component to the layout.  The constraint must be a
   * string which is used to name the component.  This string can
   * later be used to refer to the particular component.
   *
   * @param comp The component to add
   * @param constraints The name by which the component can later be called
   * 
   * @exception IllegalArgumentException If `constraints' is not a
   * <code>String</code>
   */
  public void addLayoutComponent (Component comp, Object constraints)
  {
    if (! (constraints instanceof String))
      throw new IllegalArgumentException ("Object " + constraints
					  + " is not a string");
    addLayoutComponent ((String) constraints, comp);
  }

  /**
   * Add a new component to the layout.  The name can be used later
   * to refer to the component.
   * 
   * @param name The name by which the component can later be called
   * @param comp The component to add
   * 
   * @deprecated This method is deprecated in favor of
   * <code>addLayoutComponent(Component, Object)</code>.
   */
  public void addLayoutComponent (String name, Component comp)
  {
    tab.put (name, comp);
    // First component added is the default component.
    comp.setVisible(tab.size() == 1);
  }

  /**
   * Cause the first component in the container to be displayed.
   *
   * @param parent The parent container
   */
  public void first (Container parent)
  {
    gotoComponent (parent, FIRST);
  }

  /**
   * Return this layout manager's horizontal gap.
   *
   * @return the horizontal gap
   */
  public int getHgap ()
  {
    return hgap;
  }

  /**
   * Return this layout manager's x alignment.  This method always
   * returns Component.CENTER_ALIGNMENT.
   * 
   * @param parent Container using this layout manager instance
   *
   * @return the x-axis alignment
   */
  public float getLayoutAlignmentX (Container parent)
  {
    return Component.CENTER_ALIGNMENT;
  }

  /**
   * Returns this layout manager's y alignment.  This method always
   * returns Component.CENTER_ALIGNMENT.
   * 
   * @param parent Container using this layout manager instance
   *
   * @return the y-axis alignment
   */
  public float getLayoutAlignmentY (Container parent)
  {
    return Component.CENTER_ALIGNMENT;
  }

  /**
   * Return this layout manager's vertical gap.
   *
   * @return the vertical gap
   */
  public int getVgap ()
  {
    return vgap;
  }

  /**
   * Invalidate this layout manager's state.
   */
  public void invalidateLayout (Container target)
  {
    // Do nothing.
  }

  /**
   * Cause the last component in the container to be displayed.
   * 
   * @param parent The parent container
   */
  public void last (Container parent)
  {
    gotoComponent (parent, LAST);
  }

  /**
   * Lays out the container.  This is done by resizing the child components
   * to be the same size as the parent, less insets and gaps.
   *
   * @param parent The parent container.
   */ 
  public void layoutContainer (Container parent)
  {
    synchronized (parent.getTreeLock ())
      {
	int width = parent.width;
	int height = parent.height;

	Insets ins = parent.getInsets ();

	int num = parent.ncomponents;
	Component[] comps = parent.component;

	int x = ins.left + hgap;
	int y = ins.top + vgap;
	width = width - 2 * hgap - ins.left - ins.right;
	height = height - 2 * vgap - ins.top - ins.bottom;

	for (int i = 0; i < num; ++i)
	  comps[i].setBounds (x, y, width, height);
      }
  }

  /**
   * Get the maximum layout size of the container.
   * 
   * @param target The parent container
   *
   * @return the maximum layout size
   */
  public Dimension maximumLayoutSize (Container target)
  {
    // The JCL says that this returns Integer.MAX_VALUE for both
    // dimensions.  But that just seems wrong to me.
    return getSize (target, MAX);
  }

  /**
   * Get the minimum layout size of the container.
   * 
   * @param target The parent container
   *
   * @return the minimum layout size
   */
  public Dimension minimumLayoutSize (Container target)
  {
    return getSize (target, MIN);
  }

  /**
   * Cause the next component in the container to be displayed.  If
   * this current card is the  last one in the deck, the first
   * component is displayed.
   * 
   * @param parent The parent container
   */
  public void next (Container parent)
  {
    gotoComponent (parent, NEXT);
  }

  /**
   * Get the preferred layout size of the container.
   * 
   * @param parent The parent container
   *
   * @return the preferred layout size
   */
  public Dimension preferredLayoutSize (Container parent)
  {
    return getSize (parent, PREF);
  }

  /**
   * Cause the previous component in the container to be displayed.
   * If this current card is the first one in the deck, the last
   * component is displayed.
   * 
   * @param parent The parent container
   */
  public void previous (Container parent)
  {
    gotoComponent (parent, PREV);
  }

  /**
   * Remove the indicated component from this layout manager.
   * 
   * @param comp The component to remove
   */
  public void removeLayoutComponent (Component comp)
  {
    Enumeration e = tab.keys ();
    while (e.hasMoreElements ())
      {
	Object key = e.nextElement ();
	if (tab.get (key) == comp)
	  {
	    tab.remove (key);
	    Container parent = comp.getParent();
	    next(parent);
	    break;
	  }
      }
  }

  /**
   * Set this layout manager's horizontal gap.
   * 
   * @param hgap The new gap
   */
  public void setHgap (int hgap)
  {
    this.hgap = hgap;
  }

  /**
   * Set this layout manager's vertical gap.
   * 
   * @param vgap The new gap
   */
  public void setVgap (int vgap)
  {
    this.vgap = vgap;
  }

  /**
   * Cause the named component to be shown.  If the component name is
   * unknown, this method does nothing.
   * 
   * @param parent The parent container
   * @param name The name of the component to show
   */
  public void show (Container parent, String name)
  {
    Object target = tab.get (name);
    if (target != null)
      {
	int num = parent.ncomponents;
	// This is more efficient than calling getComponents().
	Component[] comps = parent.component;
	for (int i = 0; i < num; ++i)
	  {
	    if (comps[i].isVisible())
	      {
		if (target == comps[i])
		  return;
		comps[i].setVisible (false);
	      }
	  }
	((Component) target).setVisible (true);
      }
  }

  /**
   * Returns a string representation of this layout manager.
   *
   * @return A string representation of this object.
   */
  public String toString ()
  {
    return getClass ().getName () + "[" + hgap + "," + vgap + "]";
  }

  /**
   * This implements first(), last(), next(), and previous().
   * 
   * @param parent The parent container
   * @param what The type of goto: FIRST, LAST, NEXT or PREV
   */
  private void gotoComponent (Container parent, int what)
  {
    synchronized (parent.getTreeLock ())
      {
	int num = parent.ncomponents;
	// This is more efficient than calling getComponents().
	Component[] comps = parent.component;

	if (num == 1)
	  {
	    comps[0].setVisible(true);
	    return;
	  }

	int choice = -1;

	if (what == FIRST)
	  choice = 0;
	else if (what == LAST)
	  choice = num - 1;

	for (int i = 0; i < num; ++i)
	  {
	    if (comps[i].isVisible ())
	      {
		if (what == NEXT)
		  {
		    choice = i + 1;
		    if (choice == num)
		      choice = 0;
		  }
		else if (what == PREV)
		  {
		    choice = i - 1;
		    if (choice < 0)
		      choice = num - 1;
		  }
		else if (choice == i)
		  {
		    // Do nothing if we're already looking at the right
		    // component.
		    return;
		  }
		comps[i].setVisible (false);
 
		if (choice >= 0)
		  break;
	      }
	  }

	if (choice >= 0 && choice < num)
	  comps[choice].setVisible (true);
      }
  }

  // Compute the size according to WHAT.
  private Dimension getSize (Container parent, int what)
  {
    synchronized (parent.getTreeLock ())
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
  }

  /**
   * @serial Horizontal gap value.
   */
  private int hgap;

  /**
   * @serial Vertical gap value.
   */
  private int vgap;

  /**
   * @serial Table of named components.
   */
  private Hashtable tab;

  // These constants are used by the private gotoComponent method.
  private static final int FIRST = 0;
  private static final int LAST = 1;
  private static final int NEXT = 2;
  private static final int PREV = 3;

  // These constants are used by the private getSize method.
  private static final int MIN = 0;
  private static final int MAX = 1;
  private static final int PREF = 2;
}
