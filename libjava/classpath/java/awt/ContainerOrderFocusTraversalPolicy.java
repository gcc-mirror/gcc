/* ContainerOrderFocusTraversalPolicy.java -- 
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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

/**
 * ContainerOrderFocusTraversalPolicy defines a focus traversal order
 * based on the order in which Components were packed in a Container.
 * This policy performs a pre-order traversal of the Component
 * hierarchy starting from a given focus cycle root.  Portions of the
 * hierarchy that are not visible and displayable are skipped.
 *
 * By default, this policy transfers focus down-cycle implicitly.
 * That is, if a forward traversal is requested on a focus cycle root
 * and the focus cycle root has focusable children, the focus will
 * automatically be transfered down to the lower focus cycle.
 *
 * The default implementation of accept accepts only Components that
 * are visible, displayable, enabled and focusable.  Derived classes
 * can override these acceptance criteria by overriding accept.
 *
 * @author Michael Koch
 * @author Thomas Fitzsimmons (fitzsim@redhat.com)
 * @since 1.4
 */
public class ContainerOrderFocusTraversalPolicy extends FocusTraversalPolicy
  implements Serializable
{
  /**
   * Compatible to JDK 1.4+
   */
  static final long serialVersionUID = 486933713763926351L;

  /**
   * True if implicit down cycling is enabled.
   */
  private boolean implicitDownCycleTraversal = true;

  /**
   * Creates the <code>ContainerOrderFocusTraversalPolicy</code> object.
   */
  public ContainerOrderFocusTraversalPolicy ()
  {
    // Nothing to do here
  }

  /**
   * Returns the Component that should receive the focus after current.
   * root must be a focus cycle root of current.
   *
   * @param root a focus cycle root of current
   * @param current a (possibly indirect) child of root, or root itself
   *
   * @return the next Component in the focus traversal order for root,
   * or null if no acceptable Component exists.
   *
   * @exception IllegalArgumentException If root is not a focus cycle
   * root of current, or if either root or current is null.
   */
  public Component getComponentAfter (Container root, Component current)
  {
    if (root == null)
      throw new IllegalArgumentException ("focus cycle root is null");
    if (current == null)
      throw new IllegalArgumentException ("current component is null");

    if (!root.isFocusCycleRoot ())
      throw new IllegalArgumentException ("root is not a focus cycle root");

    Container ancestor = current.getFocusCycleRootAncestor ();
    Container prevAncestor = ancestor;
    while (ancestor != root)
      {
	ancestor = current.getFocusCycleRootAncestor ();
	if (ancestor == prevAncestor)
	  {
            // We've reached the top focus cycle root ancestor.  Check
            // if it is root.
            if (ancestor == null)
              ancestor = root;
            else if (ancestor != root)
	      throw new IllegalArgumentException ("the given container is not"
						  + " a focus cycle root of the"
						  + " current component");
            else
              break;
	  }
	prevAncestor = ancestor;
      }

    // FIXME: is this the right thing to do here? It moves the context
    // for traversal up one focus traversal cycle.  We'll need a test
    // for this.
    if ((Component) root == current)
      root = current.getFocusCycleRootAncestor ();

    // Check if we've reached the top of the component hierarchy.  If
    // so then we want to loop around to the first component in the
    // focus traversal cycle.
    if (current instanceof Window)
      return getFirstComponent ((Container) current);

    Container parent = current.getParent ();
    synchronized (parent.getTreeLock ())
      {
        Component[] components = parent.getComponents ();
        int componentIndex = 0;
        int numComponents = parent.getComponentCount ();

        // Find component's index.
        for (int i = 0; i < numComponents; i++)
          {
            if (components[i].equals(current))
              componentIndex = i;
          }

        // Search forward for the next acceptable component.  
        // Search through all components at least one time
        // i.e. start at componentIndex + 1 --> nComponents -1 --> 0  ---> componentIndex
        int i = componentIndex + 1;
        int end = numComponents - 1;
        Component next = getNextAvailableComponent(components, i, end);
        if (next != null)
          return next;
        
        // Now check remainder of components from 0 to componentIndex
        i = 0;
        end = componentIndex;
        next = getNextAvailableComponent(components, i, end);
        if (next != null)
          return next; 
        
        // No focusable components after current in its Container.  So go
        // to the next Component after current's Container (parent).
        Component result = getComponentAfter (root, parent);
        return result;
      }
  }
  
  /**
   * Gets the next available component in the array between the given range.
   * 
   * @param components - the array of components.
   * @param start - where to start
   * @param end - where to end
   * @return next component if found
   */
  private Component getNextAvailableComponent(Component[] components, int start, int end)
  {
    while (start <= end)
      {
        Component c = components[start];

        if (c.visible && c.isDisplayable() && c.enabled && c.focusable)
          return c;

        if (c instanceof Container)
          {
            Component result = getFirstComponent((Container) c);

            if (result != null && implicitDownCycleTraversal && result.visible
                && result.isDisplayable() && result.enabled && result.focusable)
              return result;
          }
        start++;
      }

    return null;
  }

  /**
   * Gets the previous available component in the array between the given range.
   * 
   * @param components - the array of components.
   * @param start - where to start
   * @param end - where to end
   * @return previous component if found
   */
  Component getPrevAvailableComponent(Component[] components, int start, int end)
  {
    while (start >= end) 
      {
        Component c = components[start];
        if (c.visible && c.isDisplayable() && c.enabled && c.focusable)
          return c;

        if (c instanceof Container)
          {
            Component result = getLastComponent((Container) c);

            if (result != null
                && (result.visible && result.isDisplayable() && result.enabled && result.focusable))
              return result;
          }
        start--;
      }
    return null;
  }

  /**
   * Returns the Component that should receive the focus before
   * <code>current</code>. <code>root</code> must be a focus cycle root of
   * current.
   * 
   * @param root a focus cycle root of current
   * @param current a (possibly indirect) child of root, or root itself
   * @return the previous Component in the focus traversal order for root, or
   *         null if no acceptable Component exists.
   * @exception IllegalArgumentException If root is not a focus cycle root of
   *              current, or if either root or current is null.
   */
  public Component getComponentBefore (Container root, Component current)
  {
    if (root == null)
      throw new IllegalArgumentException ("focus cycle root is null");
    if (current == null)
      throw new IllegalArgumentException ("current component is null");

    if (!root.isFocusCycleRoot ())
      throw new IllegalArgumentException ("root is not a focus cycle root");

    Container ancestor = current.getFocusCycleRootAncestor ();
    Container prevAncestor = ancestor;
    while (ancestor != root)
      {
	ancestor = current.getFocusCycleRootAncestor ();
	if (ancestor == prevAncestor)
	  {
	    // We've reached the top focus cycle root ancestor.  Check
	    // if it is root.
            if (ancestor == null)
              ancestor = root;
            else if (ancestor != root)
	      throw new IllegalArgumentException ("the given container is not"
						  + " a focus cycle root of the"
						  + " current component");
	    else
	      break;
	  }
	prevAncestor = ancestor;
      }

    // FIXME: is this the right thing to do here? It moves the context
    // for traversal up one focus traversal cycle.  We'll need a test
    // for this.
    if ((Component) root == current)
      root = current.getFocusCycleRootAncestor ();

    // Check if we've reached the top of the component hierarchy.  If
    // so then we want to loop around to the last component in the
    // focus traversal cycle.
    if (current instanceof Window)
      return getLastComponent ((Container) current);

    Container parent = current.getParent ();

    synchronized (parent.getTreeLock ())
      {
        Component[] components = parent.getComponents ();
        int componentIndex = 0;
        int numComponents = parent.getComponentCount ();

        // Find component's index.
        for (int i = 0; i < numComponents; i++)
          {
            if (components[i] == current)
              componentIndex = i;
          }

        // Search through all components at least one time
        // i.e. start at componentIndex - 1 --> 0 --> numComponents -1  ---> componentIndex
        int i = componentIndex - 1;
        int end = 0;
        Component prev = getPrevAvailableComponent(components, i, end);
        if (prev != null)
          return prev;
        
        // Now check remainder of components
        i = numComponents -1;
        end = componentIndex;
        prev = getPrevAvailableComponent(components, i, end);
        if (prev != null)
          return prev; 

        // No focusable components before current in its Container.  So go
        // to the previous Component before current's Container (parent).
        Component result = getComponentBefore (root, parent);

        return result;
      }
  }

  /**
   * Returns the first Component of root that should receive the focus.
   *
   * @param root a focus cycle root
   *
   * @return the first Component in the focus traversal order for
   * root, or null if no acceptable Component exists.
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

    if (accept(root))
      return root;

    int ncomponents = root.getComponentCount();
    for (int i = 0; i < ncomponents; i++)
      {
        Component component = root.getComponent(i);
        if (component instanceof Container
            && !((Container) component).isFocusCycleRoot())
          {
            Component first = null;
            Container cont = (Container) component;
            if (cont.isFocusTraversalPolicyProvider())
              {
                FocusTraversalPolicy childPol = cont.getFocusTraversalPolicy();
                first = childPol.getFirstComponent(cont);
              }
            else
              first = getFirstComponent(cont);
            if (first != null)
              return first;
          }
        else if (accept(component))
	  return component;
      }

    return null;
  }

  /**
   * Returns the last Component of root that should receive the focus.
   *
   * @param root a focus cycle root
   *
   * @return the last Component in the focus traversal order for
   * root, or null if no acceptable Component exists.
   *
   * @exception IllegalArgumentException If root is null.
   */
  public Component getLastComponent (Container root)
  {
    if (root == null)
      throw new IllegalArgumentException ();

    if (!root.isVisible ()
        || !root.isDisplayable ())
      return null;

    if (root.visible && root.isDisplayable() && root.enabled
        && root.focusable)
      return root;

    Component[] componentArray = root.getComponents ();
    
    for (int i = componentArray.length - 1; i >= 0; i--)
      {
        Component component = componentArray [i];
	
	if (component.visible && component.isDisplayable() && component.enabled
            && component.focusable)
	  return component;

        if (component instanceof Container)
          {
            Component result = getLastComponent ((Container) component);

            if (result != null &&
                result.visible && result.isDisplayable() && result.enabled
                && result.focusable)
              return result;
          }
      }

    return null;
  }

  /**
   * Returns the default Component of root that should receive the focus.
   *
   * @param root a focus cycle root
   *
   * @return the default Component in the focus traversal order for
   * root, or null if no acceptable Component exists.
   *
   * @exception IllegalArgumentException If root is null.
   */
  public Component getDefaultComponent (Container root)
  {
    return getFirstComponent (root);
  }

  /**
   * Set whether or not implicit down cycling is enabled.  If it is,
   * then initiating a forward focus traversal operation onto a focus
   * cycle root, the focus will be implicitly transferred into the
   * root container's focus cycle.
   *
   * @param value the setting for implicit down cycling
   */
  public void setImplicitDownCycleTraversal (boolean value)
  {
    implicitDownCycleTraversal = value;
  }

  /**
   * Check whether or not implicit down cycling is enabled.  If it is,
   * then initiating a forward focus traversal operation onto a focus
   * cycle root, the focus will be implicitly transferred into the
   * root container's focus cycle.
   *
   * @return true if the focus will be transferred down-cycle
   * implicitly
   */
  public boolean getImplicitDownCycleTraversal ()
  {
    return implicitDownCycleTraversal;
  }

  /**
   * Check whether the given Component is an acceptable target for the
   * keyboard input focus.
   *
   * @param current the Component to check
   *
   * @return true if current is acceptable, false otherwise
   */
  protected boolean accept (Component current)
  {
    return (current.visible
            && current.isDisplayable ()
            && current.enabled
            && current.focusable);
  }
}
