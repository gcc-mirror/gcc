/* LayoutManager2.java -- enhanced layout manager
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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

/**
 * Layout manager for laying out containers based on contraints. The
 * constraints control how the layout will proceed.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @see LayoutManager
 * @see Container
 * @since 1.0
 * @status updated to 1.4
 */
public interface LayoutManager2 extends LayoutManager
{
  /**
   * Adds the specified component to the layout, with the specified
   * constraint object.
   *
   * @param component the component to add
   * @param constraint the constraint to satisfy
   */
  void addLayoutComponent(Component component, Object contraint);

  /**
   * Determines the maximum size of the specified target container.
   *
   * @param target the container to lay out
   * @return the maximum size of the container
   * @see Component#getMaximumSize()
   */
  Dimension maximumLayoutSize(Container target);

  /**
   * Returns the preferred X axis alignment for the specified target
   * container.  This value will range from 0 to 1 where 0 is alignment
   * closest to the origin, 0.5 is centered, and 1 is aligned furthest
   * from the origin.
   *
   * @param target the target container
   * @return the x-axis alignment preference
   */
  float getLayoutAlignmentX(Container target);

  /**
   * Returns the preferred Y axis alignment for the specified target
   * container.  This value will range from 0 to 1 where 0 is alignment
   * closest to the origin, 0.5 is centered, and 1 is aligned furthest
   * from the origin.
   *
   * @param target the target container
   * @return the y-axis alignment preference
   */
  float getLayoutAlignmentY(Container target);

  /**
   * Forces the layout manager to purge any cached information about the
   * layout of the target container.  This will force it to be recalculated.
   *
   * @param target the target container
   */
  void invalidateLayout(Container target);
} // interface LayoutManager2
