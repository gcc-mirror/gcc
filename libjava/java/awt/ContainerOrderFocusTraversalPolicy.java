/* ContainerOrderFocusTraversalPolicy.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

/**
 * @author Michael Koch
 * @since 1.4
 */
public class ContainerOrderFocusTraversalPolicy extends FocusTraversalPolicy
  implements Serializable
{
  /**
   * Compatible to JDK 1.4+
   */
  static final long serialVersionUID = 486933713763926351L;

  private boolean implicitDownCycleTraversal = true;

  /**
   * Creates the <code>ContainerOrderFocusTraversalPolicy</code> object.
   */
  public ContainerOrderFocusTraversalPolicy()
  {
    // Nothing to do here
  }

  /**
   * Returns the Component that should receive the focus after current.
   * root must be a focus cycle root of current.
   *
   * @exception IllegalArgumentException If root is not a focus cycle
   * root of current, or if either root or current is null.
   */
  public Component getComponentAfter(Container root, Component current)
  {
    if (root == null
        || current == null)
      throw new IllegalArgumentException ();
    
    return null;
  }

  /**
   * Returns the Component that should receive the focus before current.
   * root must be a focus cycle root of current.
   *
   * @exception IllegalArgumentException If root is not a focus cycle
   * root of current, or if either root or current is null.
   */
  public Component getComponentBefore(Container root, Component current)
  {
    if (root == null
        || current == null)
      throw new IllegalArgumentException ();

    return null;
  }

  /**
   * Returns the first Component of root that should receive the focus.
   *
   * @exception IllegalArgumentException If root is null.
   */
  public Component getFirstComponent(Container root)
  {
    if (root == null)
      throw new IllegalArgumentException ();

    if (!root.isVisible ()
        || !root.isDisplayable ())
      return null;

    if (accept (root))
      return root;

    Component[] componentArray = root.getComponents ();
    
    for (int i = 0; i < componentArray.length; i++)
      {
        Component component = componentArray [i];
	
        if (component instanceof Container)
          {
            Component result = getLastComponent ((Container) component);

            if (result != null)
              return result;
          }
        else
          {
            if (accept (component))
              return component;
          }
      }

    return null;
  }

  /**
   * Returns the last Component of root that should receive the focus.
   *
   * @exception IllegalArgumentException If root is null.
   */
  public Component getLastComponent(Container root)
  {
    if (root == null)
      throw new IllegalArgumentException ();

    if (!root.isVisible ()
        || !root.isDisplayable ())
      return null;

    if (accept (root))
      return root;

    Component[] componentArray = root.getComponents ();
    
    for (int i = componentArray.length - 1; i >= 0; i++)
      {
        Component component = componentArray [i];
	
        if (component instanceof Container)
          {
            Component result = getLastComponent ((Container) component);

            if (result != null)
              return result;
          }
        else
          {
            if (accept (component))
              return component;
          }
      }

    return null;
  }

  /**
   * Returns the default Component of root that should receive the focus.
   *
   * @exception IllegalArgumentException If root is null.
   */
  public Component getDefaultComponent(Container root)
  {
    return getFirstComponent (root);
  }

  public void setImplicitDownCycleTraversal(boolean value)
  {
    implicitDownCycleTraversal = value;
  }

  public boolean getImplicitDownCycleTraversal()
  {
    return implicitDownCycleTraversal;
  }

  protected boolean accept(Component current)
  {
    return (current.visible
            && current.isDisplayable()
            && current.enabled
            && current.focusable);
  }
} // class ContainerOrderFocusTraversalPolicy
