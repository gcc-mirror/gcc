/* HierarchyBoundsAdapter.java -- convenience class for writing listeners
   Copyright (C) 2000, 2002 Free Software Foundation

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

package java.awt.event;

/**
 * This class implements <code>HierarchyBoundsListener</code> and implements
 * all methods with empty bodies.  This allows a listener interested in
 * implementing only a subset of the <code>HierarchyBoundsListener</code>
 * interface to extend this class and override only the desired methods.
 *
 * @author Bryce McKinlay
 * @see HierarchyBoundsListener
 * @see HierarchyEvent
 * @since 1.3
 * @status updated to 1.4
 */
public abstract class HierarchyBoundsAdapter implements HierarchyBoundsListener
{
  /**
   * Do nothing default constructor for subclasses.
   */
  public HierarchyBoundsAdapter()
  {
  }

  /**
   * Implements this method from the interface with an empty body.
   *
   * @param event the event, ignored in this implementation
   */
  public void ancestorMoved(HierarchyEvent event)
  {
  }

  /**
   * Implements this method from the interface with an empty body.
   *
   * @param event the event, ignored in this implementation
   */
  public void ancestorResized(HierarchyEvent event)
  {
  }
}
