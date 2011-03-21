/* SortingFocusTraversalPolicy.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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
import java.util.Comparator;
import java.util.Iterator;
import java.util.TreeSet;

/**
 * @author Graydon Hoare
 * @author Michael Koch
 *
 * @since 1.4
 */
public class SortingFocusTraversalPolicy
  extends InternalFrameFocusTraversalPolicy
{
  /**
   * The comparator used to sort elements in the focus traversal cycle
   * managed by this class.
   */
  Comparator comparator;

  /**
   * <p>Whether or not to perform an "implicit DownCycle" when selecting
   * successor components within a focus cycle.</p>
   *
   * <p>When this is true, requesting the "next" component following a
   * component which is a focus cycle root (and, necessarily, a container)
   * will enter the focus cycle root of that container, and return its
   * default focus.</p>
   *
   * <p>When this property is false, requesting the "next" component will
   * simply advance within the containing focus cycle, subject to the
   * {@link #comparator} order and the {@link #accept} judgment.</p>
   *
   * @see #getImplicitDownCycleTraversal()
   */
  boolean implicitDownCycleTraversal = true;

  /**
   * Creates a new <code>SortingFocusTraversalPolicy</code> with no
   * comparator set.
   */
  protected SortingFocusTraversalPolicy()
  {
    // Do nothing here.
  }

  /**
   * Creates a new <code>SortingFocusTraversalPolicy</code> with the given
   * comparator set.
   *
   * @param comparator the comparator to set
   */
  public SortingFocusTraversalPolicy(Comparator<? super Component> comparator)
  {
    this.comparator = comparator;
  }

  /**
   * Decide whether a component is an acceptable focus owner.
   *
   * @param comp The component which is a candidate for focus ownership.
   *
   * @return true if the component is focusable, displayable, visible, and
   * enabled; otherwise false
   */
  protected boolean accept(Component comp)
  {
    return (comp.isVisible()
            && comp.isDisplayable()
            && comp.isEnabled()
            && comp.isFocusable());
  }

  /**
   * Get the current value of the {@link #comparator} property.
   *
   * @return the current value of the property
   *
   * @see #setComparator
   */
  protected Comparator<? super Component> getComparator()
  {
    return comparator;
  }

  /**
   * Set the current value of the {@link #comparator} property.
   *
   * @param comparator the new value of the property
   *
   * @see #getComparator
   */
  protected void setComparator(Comparator<? super Component> comparator)
  {
    this.comparator = comparator;
  }

  private TreeSet getSortedCycle(Container root, TreeSet set)
  {
    if (set == null)
      set = (getComparator() == null
             ? new TreeSet()
             : new TreeSet(getComparator()));

    if (root != null)
      {
        Component[] comps = root.getComponents();
        for (int i = 0; i < comps.length; ++i)
          {
            Component c = comps[i];
            if (accept(c))
              set.add(c);
            if (c instanceof Container)
              getSortedCycle((Container) c, set);
          }
      }
    return set;
  }

  /**
   * Return the component which follows the specified component in this
   * focus cycle, relative to the order imposed by {@link
   * #comparator}. Candidate components are only considered if they are
   * accepted by the {@link #accept} method.
   *
   * If {@link #getImplicitDownCycleTraversal} is <code>true</code> and the
   * <code>comp</code> is a focus cycle root, an "implicit DownCycle"
   * occurs and the method returns the
   * <code>getDefaultComponent(comp)</code>.
   *
   * @param root the focus cycle root to search for a successor within
   * @param comp the component to search for the successor of
   *
   * @return the component following the specified component under
   * the specified root, or null if no such component is found
   *
   * @throws IllegalArgumentException if either argument is null, or
   * if the root is not a focus cycle root of the component
   */
  public Component getComponentAfter(Container root,
                                     Component comp)
  {
    if (comp == null || root == null || !comp.isFocusCycleRoot(root))
      throw new IllegalArgumentException();

    if (getImplicitDownCycleTraversal()
        && comp instanceof Container
        && ((Container)comp).isFocusCycleRoot())
      {
        return getDefaultComponent((Container) comp);
      }

    TreeSet set = getSortedCycle(root, null);
    Iterator i = set.iterator();
    while (i.hasNext())
      {
        Component c = (Component) i.next();
        if (c != null && c.equals(comp))
          {
            if (i.hasNext())
              return (Component) i.next();
            break;
          }
      }
    return null;
  }


  /**
   * Return the component which precedes the specified component in this
   * focus cycle, relative to the order imposed by {@link
   * #comparator}. Candidate components are only considered if they are
   * accepted by the {@link #accept} method.
   *
   * @param root the focus cycle root to search for a predecessor within
   * @param comp the component to search for the predecessor of
   *
   * @return the component preceding the specified component under the
   * specified root, or null if no such component is found
   *
   * @throws IllegalArgumentException if either argument is null, or
   * if the root is not a focus cycle root of the component
   */
  public Component getComponentBefore(Container root,
                                      Component comp)
  {
    if (comp == null || root == null || !comp.isFocusCycleRoot(root))
      throw new IllegalArgumentException();
    TreeSet set = getSortedCycle(root, null);
    Iterator i = set.iterator();
    Component prev = null;
    while (i.hasNext())
      {
        Component c = (Component) i.next();
        if (c != null && c.equals(comp))
          break;
        prev = c;
      }
    return prev;
  }

  /**
   * Return the default component of <code>root</code>, which is by default
   * the same as the first component, returned by {@link
   * #getFirstComponent}.
   *
   * @param root the focus cycle root to return the default component of
   *
   * @return the default focus component for <code>root</code>
   *
   * @throws IllegalArgumentException if root is null
   */
  public Component getDefaultComponent(Container root)
  {
    return getFirstComponent(root);
  }

  /**
   * Return the first focusable component of the focus cycle root
   * <code>comp</code> under the ordering imposed by the {@link
   * #comparator} property. Candidate components are only considered if
   * they are accepted by the {@link #accept} method.
   *
   * @param root the focus cycle root to search for the first component of
   *
   * @return the first component under <code>root</code>, or null if
   * no components are found.
   *
   * @throws IllegalArgumentException if root is null
   */
  public Component getFirstComponent(Container root)
  {
    if (root == null)
      throw new IllegalArgumentException();
    TreeSet set = getSortedCycle(root, null);
    Iterator i = set.iterator();
    if (i.hasNext())
      return (Component) i.next();
    return null;
  }

  /**
   * Return the last focusable component of the focus cycle root
   * <code>comp</code> under the ordering imposed by the {@link
   * #comparator} property. Candidate components are only considered if
   * they are accepted by the {@link #accept} method.
   *
   * @param root the focus cycle root to search for the last component of
   *
   * @return the last component under <code>root</code>, or null if
   * no components are found.
   *
   * @throws IllegalArgumentException if root is null
   */
  public Component getLastComponent(Container root)
  {
    if (root == null)
      throw new IllegalArgumentException();
    TreeSet set = getSortedCycle(root, null);
    Iterator i = set.iterator();
    Component last = null;
    while (i.hasNext())
      last = (Component) i.next();
    return last;
  }

  /**
   * Return the current value of the {@link #implicitDownCycleTraversal}
   * property.
   *
   * @return the current value of the property
   *
   * @see #setImplicitDownCycleTraversal
   */
  public boolean getImplicitDownCycleTraversal()
  {
    return implicitDownCycleTraversal;
  }

  /**
   * Set the current value of the {@link #implicitDownCycleTraversal}
   * property.
   *
   * @param down the new value of the property
   *
   * @see #getImplicitDownCycleTraversal
   */
  public void setImplicitDownCycleTraversal(boolean down)
  {
    implicitDownCycleTraversal = down;
  }
}
