/* CompatibilityFocusTraversalPolicy.java -- Provides compatibility to old
                                             focus API
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.FocusTraversalPolicy;
import java.util.HashMap;

/**
 * Provides compatibility to the older focus API in
 * {@link JComponent#setNextFocusableComponent(Component)}.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
class CompatibilityFocusTraversalPolicy
  extends FocusTraversalPolicy
{

  /**
   * The focus traversal policy that has been installed on the focus cycle
   * root before, and to which we fall back.
   */
  private FocusTraversalPolicy fallback;

  /**
   * Maps components to their next focused components.
   */
  private HashMap forward;

  /**
   * Maps components to their previous focused components.
   */
  private HashMap backward;

  /**
   * Creates a new CompatibilityFocusTraversalPolicy with the specified
   * policy as fallback.
   *
   * @param p the fallback policy
   */
  CompatibilityFocusTraversalPolicy(FocusTraversalPolicy p)
  {
    fallback = p;
    forward = new HashMap();
    backward = new HashMap();
  }

  public Component getComponentAfter(Container root, Component current)
  {
    Component next = (Component) forward.get(current);
    if (next == null && fallback != null)
      next = fallback.getComponentAfter(root, current);
    return next;
  }

  public Component getComponentBefore(Container root, Component current)
  {
    Component previous = (Component) backward.get(current);
    if (previous == null && fallback != null)
      previous = fallback.getComponentAfter(root, current);
    return previous;
  }

  public Component getFirstComponent(Container root)
  {
    Component first = null;
    if (fallback != null)
      first = fallback.getFirstComponent(root);
    return first;
  }

  public Component getLastComponent(Container root)
  {
    Component last = null;
    if (fallback != null)
      last = fallback.getLastComponent(root);
    return last;
  }

  public Component getDefaultComponent(Container root)
  {
    Component def = null;
    if (fallback != null)
      def = fallback.getDefaultComponent(root);
    return def;
  }

  /**
   * Sets a next focused component for a specified component. This is called
   * by {@link JComponent#setNextFocusableComponent(Component)}.
   *
   * @param current the current component
   * @param next the next focused component
   */
  void setNextFocusableComponent(Component current, Component next)
  {
    forward.put(current, next);
    backward.put(next, current);
  }

  /**
   * Sets a next focused component for a specified component. This is called
   * by {@link JComponent#setNextFocusableComponent(Component)}.
   *
   * @param current the current component
   * @param next the next focused component
   */
  void addNextFocusableComponent(Component current, Component next)
  {
    forward.put(current, next);
    backward.put(next, current);
  }

  /**
   * Removes a focused component mapping. This is called
   * by {@link JComponent#setNextFocusableComponent(Component)}.
   *
   * @param current the current component
   * @param next the next focused component
   */
  void removeNextFocusableComponent(Component current, Component next)
  {
    forward.remove(current);
    backward.remove(next);
  }
}
