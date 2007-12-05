/* Container.java -- parent container class in AWT
   Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation

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

import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;
import java.awt.peer.LightweightPeer;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Collections;
import java.util.EventListener;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.accessibility.Accessible;

/**
 * A generic window toolkit object that acts as a container for other objects.
 * Components are tracked in a list, and new elements are at the end of the
 * list or bottom of the stacking order.
 *
 * @author original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 *
 * @since 1.0
 *
 * @status still missing 1.4 support, some generics from 1.5
 */
public class Container extends Component
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 4613797578919906343L;

  /* Serialized fields from the serialization spec. */
  int ncomponents;
  Component[] component;
  LayoutManager layoutMgr;

  /**
   * @since 1.4
   */
  boolean focusCycleRoot;

  /**
   * Indicates if this container provides a focus traversal policy.
   *
   * @since 1.5
   */
  private boolean focusTraversalPolicyProvider;

  int containerSerializedDataVersion;

  /* Anything else is non-serializable, and should be declared "transient". */
  transient ContainerListener containerListener;

  /** The focus traversal policy that determines how focus is
      transferred between this Container and its children. */
  private FocusTraversalPolicy focusTraversalPolicy;

  /**
   * The focus traversal keys, if not inherited from the parent or default
   * keyboard manager. These sets will contain only AWTKeyStrokes that
   * represent press and release events to use as focus control.
   *
   * @see #getFocusTraversalKeys(int)
   * @see #setFocusTraversalKeys(int, Set)
   * @since 1.4
   */
  transient Set[] focusTraversalKeys;

  /**
   * Default constructor for subclasses.
   */
  public Container()
  {
    // Nothing to do here.
  }

  /**
   * Returns the number of components in this container.
   *
   * @return The number of components in this container.
   */
  public int getComponentCount()
  {
    return countComponents ();
  }

  /**
   * Returns the number of components in this container.
   *
   * @return The number of components in this container.
   *
   * @deprecated use {@link #getComponentCount()} instead
   */
  public int countComponents()
  {
    return ncomponents;
  }

  /**
   * Returns the component at the specified index.
   *
   * @param n The index of the component to retrieve.
   *
   * @return The requested component.
   *
   * @throws ArrayIndexOutOfBoundsException If the specified index is invalid
   */
  public Component getComponent(int n)
  {
    synchronized (getTreeLock ())
      {
        if (n < 0 || n >= ncomponents)
          throw new ArrayIndexOutOfBoundsException("no such component");

        return component[n];
      }
  }

  /**
   * Returns an array of the components in this container.
   *
   * @return The components in this container.
   */
  public Component[] getComponents()
  {
    synchronized (getTreeLock ())
      {
        Component[] result = new Component[ncomponents];

        if (ncomponents > 0)
          System.arraycopy(component, 0, result, 0, ncomponents);

        return result;
      }
  }

  /**
   * Returns the insets for this container, which is the space used for
   * borders, the margin, etc.
   *
   * @return The insets for this container.
   */
  public Insets getInsets()
  {
    return insets ();
  }

  /**
   * Returns the insets for this container, which is the space used for
   * borders, the margin, etc.
   *
   * @return The insets for this container.
   * @deprecated use {@link #getInsets()} instead
   */
  public Insets insets()
  {
    Insets i;
    if (peer == null || peer instanceof LightweightPeer)
      i = new Insets (0, 0, 0, 0);
    else
      i = ((ContainerPeer) peer).getInsets ();
    return i;
  }

  /**
   * Adds the specified component to this container at the end of the
   * component list.
   *
   * @param comp The component to add to the container.
   *
   * @return The same component that was added.
   */
  public Component add(Component comp)
  {
    addImpl(comp, null, -1);
    return comp;
  }

  /**
   * Adds the specified component to the container at the end of the
   * component list.  This method should not be used. Instead, use
   * <code>add(Component, Object)</code>.
   *
   * @param name The name of the component to be added.
   * @param comp The component to be added.
   *
   * @return The same component that was added.
   *
   * @see #add(Component,Object)
   */
  public Component add(String name, Component comp)
  {
    addImpl(comp, name, -1);
    return comp;
  }

  /**
   * Adds the specified component to this container at the specified index
   * in the component list.
   *
   * @param comp The component to be added.
   * @param index The index in the component list to insert this child
   * at, or -1 to add at the end of the list.
   *
   * @return The same component that was added.
   *
   * @throws ArrayIndexOutOfBoundsException If the specified index is invalid.
   */
  public Component add(Component comp, int index)
  {
    addImpl(comp, null, index);
    return comp;
  }

  /**
   * Adds the specified component to this container at the end of the
   * component list.  The layout manager will use the specified constraints
   * when laying out this component.
   *
   * @param comp The component to be added to this container.
   * @param constraints The layout constraints for this component.
   */
  public void add(Component comp, Object constraints)
  {
    addImpl(comp, constraints, -1);
  }

  /**
   * Adds the specified component to this container at the specified index
   * in the component list.  The layout manager will use the specified
   * constraints when layout out this component.
   *
   * @param comp The component to be added.
   * @param constraints The layout constraints for this component.
   * @param index The index in the component list to insert this child
   * at, or -1 to add at the end of the list.
   *
   * @throws ArrayIndexOutOfBoundsException If the specified index is invalid.
   */
  public void add(Component comp, Object constraints, int index)
  {
    addImpl(comp, constraints, index);
  }

  /**
   * This method is called by all the <code>add()</code> methods to perform
   * the actual adding of the component.  Subclasses who wish to perform
   * their own processing when a component is added should override this
   * method.  Any subclass doing this must call the superclass version of
   * this method in order to ensure proper functioning of the container.
   *
   * @param comp The component to be added.
   * @param constraints The layout constraints for this component, or
   * <code>null</code> if there are no constraints.
   * @param index The index in the component list to insert this child
   * at, or -1 to add at the end of the list.
   *
   * @throws ArrayIndexOutOfBoundsException If the specified index is invalid.
   */
  protected void addImpl(Component comp, Object constraints, int index)
  {
    synchronized (getTreeLock ())
      {
        if (index > ncomponents
            || (index < 0 && index != -1)
            || comp instanceof Window
            || (comp instanceof Container
                && ((Container) comp).isAncestorOf(this)))
          throw new IllegalArgumentException();

        // Reparent component, and make sure component is instantiated if
        // we are.
        if (comp.parent != null)
          comp.parent.remove(comp);

        if (component == null)
          component = new Component[4]; // FIXME, better initial size?
   
        // This isn't the most efficient implementation.  We could do less
        // copying when growing the array.  It probably doesn't matter.
        if (ncomponents >= component.length)
          {
            int nl = component.length * 2;
            Component[] c = new Component[nl];
            System.arraycopy(component, 0, c, 0, ncomponents);
            component = c;
          }
  
        if (index == -1)
          component[ncomponents++] = comp;
        else
          {
            System.arraycopy(component, index, component, index + 1,
                             ncomponents - index);
            component[index] = comp;
            ++ncomponents;
          }

        // Give the new component a parent.
        comp.parent = this;

        // Update the counter for Hierarchy(Bounds)Listeners.
        int childHierarchyListeners = comp.numHierarchyListeners;
        if (childHierarchyListeners > 0)
          updateHierarchyListenerCount(AWTEvent.HIERARCHY_EVENT_MASK,
                                       childHierarchyListeners);
        int childHierarchyBoundsListeners = comp.numHierarchyBoundsListeners;
        if (childHierarchyBoundsListeners > 0)
          updateHierarchyListenerCount(AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK,
                                       childHierarchyListeners);

        // Invalidate the layout of this container.
        if (valid)
          invalidate();

        // Create the peer _after_ the component has been added, so that
        // the peer gets to know about the component hierarchy.
        if (peer != null)
          {
            // Notify the component that it has a new parent.
            comp.addNotify();
          }

        // Notify the layout manager.
        if (layoutMgr != null)
          {
	    // If we have a LayoutManager2 the constraints are "real",
	    // otherwise they are the "name" of the Component to add.
            if (layoutMgr instanceof LayoutManager2)
              {
                LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
                lm2.addLayoutComponent(comp, constraints);
              }
            else if (constraints instanceof String)
              layoutMgr.addLayoutComponent((String) constraints, comp);
            else
              layoutMgr.addLayoutComponent("", comp);
          }

        // We previously only sent an event when this container is showing.
        // Also, the event was posted to the event queue. A Mauve test shows
        // that this event is not delivered using the event queue and it is
        // also sent when the container is not showing.
        if (containerListener != null
            || (eventMask & AWTEvent.CONTAINER_EVENT_MASK) != 0)
          {
            ContainerEvent ce = new ContainerEvent(this,
                                                ContainerEvent.COMPONENT_ADDED,
                                                comp);
            dispatchEvent(ce);
          }

        // Notify hierarchy listeners.
        comp.fireHierarchyEvent(HierarchyEvent.HIERARCHY_CHANGED, comp,
                                this, HierarchyEvent.PARENT_CHANGED);
      }
  }

  /**
   * Removes the component at the specified index from this container.
   *
   * @param index The index of the component to remove.
   */
  public void remove(int index)
  {
    synchronized (getTreeLock ())
      {
        if (index < 0 || index >= ncomponents)
          throw new ArrayIndexOutOfBoundsException();

        Component r = component[index];
        if (peer != null)
          r.removeNotify();

        if (layoutMgr != null)
          layoutMgr.removeLayoutComponent(r);

        // Update the counter for Hierarchy(Bounds)Listeners.
        int childHierarchyListeners = r.numHierarchyListeners;
        if (childHierarchyListeners > 0)
          updateHierarchyListenerCount(AWTEvent.HIERARCHY_EVENT_MASK,
                                       -childHierarchyListeners);
        int childHierarchyBoundsListeners = r.numHierarchyBoundsListeners;
        if (childHierarchyBoundsListeners > 0)
          updateHierarchyListenerCount(AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK,
                                       -childHierarchyListeners);

        r.parent = null;

        System.arraycopy(component, index + 1, component, index,
                         ncomponents - index - 1);
        component[--ncomponents] = null;

        if (valid)
          invalidate();

        if (containerListener != null
            || (eventMask & AWTEvent.CONTAINER_EVENT_MASK) != 0)
          {
            // Post event to notify of removing the component.
            ContainerEvent ce = new ContainerEvent(this,
                                              ContainerEvent.COMPONENT_REMOVED,
                                              r);
            dispatchEvent(ce);
          }

        // Notify hierarchy listeners.
        r.fireHierarchyEvent(HierarchyEvent.HIERARCHY_CHANGED, r,
                             this, HierarchyEvent.PARENT_CHANGED);
      }
  }

  /**
   * Removes the specified component from this container.
   *
   * @param comp The component to remove from this container.
   */
  public void remove(Component comp)
  {
    synchronized (getTreeLock ())
      {
        for (int i = 0; i < ncomponents; ++i)
          {
            if (component[i] == comp)
              {
                remove(i);
                break;
              }
          }
      }
  }

  /**
   * Removes all components from this container.
   */
  public void removeAll()
  {
    synchronized (getTreeLock ())
      {
        // In order to allow the same bad tricks to be used as in RI
        // this code has to stay exactly that way: In a real-life app
        // a Container subclass implemented its own vector for
        // subcomponents, supplied additional addXYZ() methods
        // and overrode remove(int) and removeAll (the latter calling
        // super.removeAll() ).
        // By doing it this way, user code cannot prevent the correct
        // removal of components.
        while (ncomponents > 0)
          {
            ncomponents--;
            Component r = component[ncomponents];
            component[ncomponents] = null;

            if (peer != null)
              r.removeNotify();

            if (layoutMgr != null)
              layoutMgr.removeLayoutComponent(r);

            r.parent = null;

            // Send ContainerEvent if necessary.
            if (containerListener != null
                || (eventMask & AWTEvent.CONTAINER_EVENT_MASK) != 0)
              {
                // Post event to notify of removing the component.
                ContainerEvent ce
                  = new ContainerEvent(this,
                                       ContainerEvent.COMPONENT_REMOVED,
                                       r);
                dispatchEvent(ce);
              }

            // Update the counter for Hierarchy(Bounds)Listeners.
            int childHierarchyListeners = r.numHierarchyListeners;
            if (childHierarchyListeners > 0)
              updateHierarchyListenerCount(AWTEvent.HIERARCHY_EVENT_MASK,
                                           -childHierarchyListeners);
            int childHierarchyBoundsListeners = r.numHierarchyBoundsListeners;
            if (childHierarchyBoundsListeners > 0)
              updateHierarchyListenerCount(AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK,
                                           -childHierarchyListeners);


            // Send HierarchyEvent if necessary.
            fireHierarchyEvent(HierarchyEvent.HIERARCHY_CHANGED, r, this,
                               HierarchyEvent.PARENT_CHANGED);

          }

        if (valid)
          invalidate();
      }
  }

  /**
   * Returns the current layout manager for this container.
   *
   * @return The layout manager for this container.
   */
  public LayoutManager getLayout()
  {
    return layoutMgr;
  }

  /**
   * Sets the layout manager for this container to the specified layout
   * manager.
   *
   * @param mgr The new layout manager for this container.
   */
  public void setLayout(LayoutManager mgr)
  {
    layoutMgr = mgr;
    if (valid)
      invalidate();
  }

  /**
   * Layout the components in this container.
   */
  public void doLayout()
  {
    layout ();
  }

  /**
   * Layout the components in this container.
   *
   * @deprecated use {@link #doLayout()} instead
   */
  public void layout()
  {
    if (layoutMgr != null)
      layoutMgr.layoutContainer (this);
  }

  /**
   * Invalidates this container to indicate that it (and all parent
   * containers) need to be laid out.
   */
  public void invalidate()
  {
    super.invalidate();
    if (layoutMgr != null && layoutMgr instanceof LayoutManager2)
      {
        LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
        lm2.invalidateLayout(this);
      }
  }

  /**
   * Re-lays out the components in this container.
   */
  public void validate()
  {
    ComponentPeer p = peer;
    if (! valid && p != null)
      {
        ContainerPeer cPeer = null;
        if (p instanceof ContainerPeer)
          cPeer = (ContainerPeer) peer;
        synchronized (getTreeLock ())
          {
            if (cPeer != null)
              cPeer.beginValidate();
            validateTree();
            valid = true;
            if (cPeer != null)
              cPeer.endValidate();
          }
      }
  }

  /**
   * Recursively invalidates the container tree.
   */
  private final void invalidateTree()
  {
    synchronized (getTreeLock())
      {
        for (int i = 0; i < ncomponents; i++)
          {
            Component comp = component[i];
            if (comp instanceof Container)
              ((Container) comp).invalidateTree();
            else if (comp.valid)
              comp.invalidate();
          }
        if (valid)
          invalidate();
      }
  }

  /**
   * Recursively validates the container tree, recomputing any invalid
   * layouts.
   */
  protected void validateTree()
  {
    if (!valid)
      {
        ContainerPeer cPeer = null;
        if (peer instanceof ContainerPeer)
          {
            cPeer = (ContainerPeer) peer;
            cPeer.beginLayout();
          }

        doLayout ();
        for (int i = 0; i < ncomponents; ++i)
          {
            Component comp = component[i];

            if (comp instanceof Container && ! (comp instanceof Window)
                && ! comp.valid)
              {
                ((Container) comp).validateTree();
              }
            else
              {
                comp.validate();
              }
          }

        if (cPeer != null)
          {
            cPeer = (ContainerPeer) peer;
            cPeer.endLayout();
          }
      }

    /* children will call invalidate() when they are layed out. It
       is therefore important that valid is not set to true
       until after the children have been layed out. */
    valid = true;

  }

  public void setFont(Font f)
  {
    Font oldFont = getFont();
    super.setFont(f);
    Font newFont = getFont();
    if (newFont != oldFont && (oldFont == null || ! oldFont.equals(newFont)))
      {
        invalidateTree();
      }
  }

  /**
   * Returns the preferred size of this container.
   *
   * @return The preferred size of this container.
   */
  public Dimension getPreferredSize()
  {
    return preferredSize ();
  }

  /**
   * Returns the preferred size of this container.
   *
   * @return The preferred size of this container.
   *
   * @deprecated use {@link #getPreferredSize()} instead
   */
  public Dimension preferredSize()
  {
    Dimension size = prefSize;
    // Try to return cached value if possible.
    if (size == null || !(prefSizeSet || valid))
      {
        // Need to lock here.
        synchronized (getTreeLock())
          {
            LayoutManager l = layoutMgr;
            if (l != null)
              prefSize = l.preferredLayoutSize(this);
            else
              prefSize = super.preferredSizeImpl();
            size = prefSize;
          }
      }
    if (size != null)
      return new Dimension(size);
    else
      return size;
  }

  /**
   * Returns the minimum size of this container.
   *
   * @return The minimum size of this container.
   */
  public Dimension getMinimumSize()
  {
    return minimumSize ();
  }

  /**
   * Returns the minimum size of this container.
   *
   * @return The minimum size of this container.
   *
   * @deprecated use {@link #getMinimumSize()} instead
   */
  public Dimension minimumSize()
  {
    Dimension size = minSize;
    // Try to return cached value if possible.
    if (size == null || !(minSizeSet || valid))
      {
        // Need to lock here.
        synchronized (getTreeLock())
          {
            LayoutManager l = layoutMgr;
            if (l != null)
              minSize = l.minimumLayoutSize(this);
            else
              minSize = super.minimumSizeImpl();
            size = minSize;
          }
      }
    if (size != null)
      return new Dimension(size);
    else
      return size;
  }

  /**
   * Returns the maximum size of this container.
   *
   * @return The maximum size of this container.
   */
  public Dimension getMaximumSize()
  {
    Dimension size = maxSize;
    // Try to return cached value if possible.
    if (size == null || !(maxSizeSet || valid))
      {
        // Need to lock here.
        synchronized (getTreeLock())
          {
            LayoutManager l = layoutMgr;
            if (l instanceof LayoutManager2)
              maxSize = ((LayoutManager2) l).maximumLayoutSize(this);
            else {
              maxSize = super.maximumSizeImpl();
            }
            size = maxSize;
          }
      }
    if (size != null)
      return new Dimension(size);
    else
      return size;
  }

  /**
   * Returns the preferred alignment along the X axis.  This is a value
   * between 0 and 1 where 0 represents alignment flush left and
   * 1 means alignment flush right, and 0.5 means centered.
   *
   * @return The preferred alignment along the X axis.
   */
  public float getAlignmentX()
  {
    LayoutManager layout = getLayout();
    float alignmentX = 0.0F;
    if (layout != null && layout instanceof LayoutManager2)
      {
        synchronized (getTreeLock())
          {
            LayoutManager2 lm2 = (LayoutManager2) layout;
            alignmentX = lm2.getLayoutAlignmentX(this);
          }
      }
    else
      alignmentX = super.getAlignmentX();
    return alignmentX;
  }

  /**
   * Returns the preferred alignment along the Y axis.  This is a value
   * between 0 and 1 where 0 represents alignment flush top and
   * 1 means alignment flush bottom, and 0.5 means centered.
   *
   * @return The preferred alignment along the Y axis.
   */
  public float getAlignmentY()
  {
    LayoutManager layout = getLayout();
    float alignmentY = 0.0F;
    if (layout != null && layout instanceof LayoutManager2)
      {
        synchronized (getTreeLock())
          {
            LayoutManager2 lm2 = (LayoutManager2) layout;
            alignmentY = lm2.getLayoutAlignmentY(this);
          }
      }
    else
      alignmentY = super.getAlignmentY();
    return alignmentY;
  }

  /**
   * Paints this container.  The implementation of this method in this
   * class forwards to any lightweight components in this container.  If
   * this method is subclassed, this method should still be invoked as
   * a superclass method so that lightweight components are properly
   * drawn.
   *
   * @param g - The graphics context for this paint job.
   */
  public void paint(Graphics g)
  {
    if (isShowing())
      {
        visitChildren(g, GfxPaintVisitor.INSTANCE, true);
      }
  }

  /**
   * Updates this container.  The implementation of this method in this
   * class forwards to any lightweight components in this container.  If
   * this method is subclassed, this method should still be invoked as
   * a superclass method so that lightweight components are properly
   * drawn.
   *
   * @param g The graphics context for this update.
   *
   * @specnote The specification suggests that this method forwards the
   *           update() call to all its lightweight children. Tests show
   *           that this is not done either in the JDK. The exact behaviour
   *           seems to be that the background is cleared in heavyweight
   *           Containers, and all other containers
   *           directly call paint(), causing the (lightweight) children to
   *           be painted.
   */
  public void update(Graphics g)
  {
    // It seems that the JDK clears the background of containers like Panel
    // and Window (within this method) but not of 'plain' Containers or
    // JComponents. This could
    // lead to the assumption that it only clears heavyweight containers.
    // However that is not quite true. In a test with a custom Container
    // that overrides isLightweight() to return false, the background is
    // also not cleared. So we do a check on !(peer instanceof LightweightPeer)
    // instead.
    if (isShowing())
      {
        ComponentPeer p = peer;
        if (! (p instanceof LightweightPeer))
          {
            g.clearRect(0, 0, getWidth(), getHeight());
          }
        paint(g);
      }
  }

  /**
   * Prints this container.  The implementation of this method in this
   * class forwards to any lightweight components in this container.  If
   * this method is subclassed, this method should still be invoked as
   * a superclass method so that lightweight components are properly
   * drawn.
   *
   * @param g The graphics context for this print job.
   */
  public void print(Graphics g)
  {
    super.print(g);
    visitChildren(g, GfxPrintVisitor.INSTANCE, true);
  }

  /**
   * Paints all of the components in this container.
   *
   * @param g The graphics context for this paint job.
   */
  public void paintComponents(Graphics g)
  {
    if (isShowing())
      visitChildren(g, GfxPaintAllVisitor.INSTANCE, false);
  }

  /**
   * Prints all of the components in this container.
   *
   * @param g The graphics context for this print job.
   */
  public void printComponents(Graphics g)
  {
    super.paint(g);
    visitChildren(g, GfxPrintAllVisitor.INSTANCE, true);
  }

  /**
   * Adds the specified container listener to this object's list of
   * container listeners.
   *
   * @param listener The listener to add.
   */
  public synchronized void addContainerListener(ContainerListener listener)
  {
    if (listener != null)
      {
        containerListener = AWTEventMulticaster.add(containerListener,
                                                    listener);
        newEventsOnly = true;
      }
  }

  /**
   * Removes the specified container listener from this object's list of
   * container listeners.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeContainerListener(ContainerListener listener)
  {
    containerListener = AWTEventMulticaster.remove(containerListener, listener);
  }

  /**
   * @since 1.4
   */
  public synchronized ContainerListener[] getContainerListeners()
  {
    return (ContainerListener[])
      AWTEventMulticaster.getListeners(containerListener,
                                       ContainerListener.class);
  }

  /**
   * Returns all registered {@link EventListener}s of the given 
   * <code>listenerType</code>.
   *
   * @param listenerType the class of listeners to filter (<code>null</code> 
   *                     not permitted).
   *                     
   * @return An array of registered listeners.
   * 
   * @throws ClassCastException if <code>listenerType</code> does not implement
   *                            the {@link EventListener} interface.
   * @throws NullPointerException if <code>listenerType</code> is 
   *                              <code>null</code>.
   *                            
   * @see #getContainerListeners()
   * 
   * @since 1.3
   */
  public <T extends EventListener> T[] getListeners(Class<T> listenerType)
  {
    if (listenerType == ContainerListener.class)
      return (T[]) getContainerListeners();
    return super.getListeners(listenerType);
  }

  /**
   * Processes the specified event.  This method calls
   * <code>processContainerEvent()</code> if this method is a
   * <code>ContainerEvent</code>, otherwise it calls the superclass
   * method.
   *
   * @param e The event to be processed.
   */
  protected void processEvent(AWTEvent e)
  {
    if (e instanceof ContainerEvent)
      processContainerEvent((ContainerEvent) e);
    else
      super.processEvent(e);
  }

  /**
   * Called when a container event occurs if container events are enabled.
   * This method calls any registered listeners.
   *
   * @param e The event that occurred.
   */
  protected void processContainerEvent(ContainerEvent e)
  {
    if (containerListener == null)
      return;
    switch (e.id)
      {
      case ContainerEvent.COMPONENT_ADDED:
        containerListener.componentAdded(e);
        break;

      case ContainerEvent.COMPONENT_REMOVED:
        containerListener.componentRemoved(e);
        break;
      }
  }

  /**
   * AWT 1.0 event processor.
   *
   * @param e The event that occurred.
   *
   * @deprecated use {@link #dispatchEvent(AWTEvent)} instead
   */
  public void deliverEvent(Event e)
  {
    if (!handleEvent (e))
      {
        synchronized (getTreeLock ())
          {
            Component parent = getParent ();

            if (parent != null)
              parent.deliverEvent (e);
          }
      }
  }

  /**
   * Returns the component located at the specified point.  This is done
   * by checking whether or not a child component claims to contain this
   * point.  The first child component that does is returned.  If no
   * child component claims the point, the container itself is returned,
   * unless the point does not exist within this container, in which
   * case <code>null</code> is returned.
   * 
   * When components overlap, the first component is returned. The component
   * that is closest to (x, y), containing that location, is returned. 
   * Heavyweight components take precedence of lightweight components.
   * 
   * This function does not ignore invisible components. If there is an invisible
   * component at (x,y), it will be returned.
   *
   * @param x The X coordinate of the point.
   * @param y The Y coordinate of the point.
   *
   * @return The component containing the specified point, or
   * <code>null</code> if there is no such point.
   */
  public Component getComponentAt(int x, int y)
  {
    return locate (x, y);
  }

  /**
   * Returns the mouse pointer position relative to this Container's
   * top-left corner.  If allowChildren is false, the mouse pointer
   * must be directly over this container.  If allowChildren is true,
   * the mouse pointer may be over this container or any of its
   * descendents.
   *
   * @param allowChildren true to allow descendents, false if pointer
   * must be directly over Container.
   *
   * @return relative mouse pointer position
   *
   * @throws HeadlessException if in a headless environment
   */
  public Point getMousePosition(boolean allowChildren) throws HeadlessException
  {
    return super.getMousePositionHelper(allowChildren);
  }

  boolean mouseOverComponent(Component component, boolean allowChildren)
  {
    if (allowChildren)
      return isAncestorOf(component);
    else
      return component == this;
  }

  /**
   * Returns the component located at the specified point.  This is done
   * by checking whether or not a child component claims to contain this
   * point.  The first child component that does is returned.  If no
   * child component claims the point, the container itself is returned,
   * unless the point does not exist within this container, in which
   * case <code>null</code> is returned.
   * 
   * When components overlap, the first component is returned. The component
   * that is closest to (x, y), containing that location, is returned. 
   * Heavyweight components take precedence of lightweight components.
   * 
   * This function does not ignore invisible components. If there is an invisible
   * component at (x,y), it will be returned.
   * 
   * @param x The x position of the point to return the component at.
   * @param y The y position of the point to return the component at.
   *
   * @return The component containing the specified point, or <code>null</code>
   * if there is no such point.
   *
   * @deprecated use {@link #getComponentAt(int, int)} instead
   */
  public Component locate(int x, int y)
  {
    synchronized (getTreeLock ())
      {
        if (!contains (x, y))
          return null;
        
        // First find the component closest to (x,y) that is a heavyweight.
        for (int i = 0; i < ncomponents; ++i)
          {
            Component comp = component[i];
            int x2 = x - comp.x;
            int y2 = y - comp.y;
            if (comp.contains (x2, y2) && !comp.isLightweight())
              return comp;
          }
        
        // if a heavyweight component is not found, look for a lightweight
        // closest to (x,y).
        for (int i = 0; i < ncomponents; ++i)
          {
            Component comp = component[i];
            int x2 = x - comp.x;
            int y2 = y - comp.y;
            if (comp.contains (x2, y2) && comp.isLightweight())
              return comp;
          }
        
        return this;
      }
  }

  /**
   * Returns the component located at the specified point.  This is done
   * by checking whether or not a child component claims to contain this
   * point.  The first child component that does is returned.  If no
   * child component claims the point, the container itself is returned,
   * unless the point does not exist within this container, in which
   * case <code>null</code> is returned.
   *
   * The top-most child component is returned in the case where components overlap.
   * This is determined by finding the component closest to (x,y) and contains 
   * that location. Heavyweight components take precedence of lightweight components.
   * 
   * This function does not ignore invisible components. If there is an invisible
   * component at (x,y), it will be returned.
   * 
   * @param p The point to return the component at.
   * @return The component containing the specified point, or <code>null</code>
   * if there is no such point.
   */
  public Component getComponentAt(Point p)
  {
    return getComponentAt (p.x, p.y);
  }

  /**
   * Locates the visible child component that contains the specified position. 
   * The top-most child component is returned in the case where there is overlap
   * in the components. If the containing child component is a Container,
   * this method will continue searching for the deepest nested child 
   * component. Components which are not visible are ignored during the search.
   * 
   * findComponentAt differs from getComponentAt, because it recursively 
   * searches a Container's children.
   * 
   * @param x - x coordinate
   * @param y - y coordinate
   * @return null if the component does not contain the position. 
   * If there is no child component at the requested point and the point is 
   * within the bounds of the container the container itself is returned.
   */
  public Component findComponentAt(int x, int y)
  {
    synchronized (getTreeLock ())
      {
        if (! contains(x, y))
          return null;

        for (int i = 0; i < ncomponents; ++i)
          {
            // Ignore invisible children...
            if (!component[i].isVisible())
              continue;

            int x2 = x - component[i].x;
            int y2 = y - component[i].y;
            // We don't do the contains() check right away because
            // findComponentAt would redundantly do it first thing.
            if (component[i] instanceof Container)
              {
                Container k = (Container) component[i];
                Component r = k.findComponentAt(x2, y2);
                if (r != null)
                  return r;
              }
            else if (component[i].contains(x2, y2))
              return component[i];
          }

        return this;
      }
  }
  
  /**
   * Locates the visible child component that contains the specified position. 
   * The top-most child component is returned in the case where there is overlap
   * in the components. If the containing child component is a Container,
   * this method will continue searching for the deepest nested child 
   * component. Components which are not visible are ignored during the search.
   * 
   * findComponentAt differs from getComponentAt, because it recursively 
   * searches a Container's children.
   * 
   * @param p - the component's location
   * @return null if the component does not contain the position. 
   * If there is no child component at the requested point and the point is 
   * within the bounds of the container the container itself is returned.
   */
  public Component findComponentAt(Point p)
  {
    return findComponentAt(p.x, p.y);
  }

  /**
   * Called when this container is added to another container to inform it
   * to create its peer.  Peers for any child components will also be
   * created.
   */
  public void addNotify()
  {
    synchronized (getTreeLock())
      {
        super.addNotify();
        addNotifyContainerChildren();
      }
  }

  /**
   * Called when this container is removed from its parent container to
   * inform it to destroy its peer.  This causes the peers of all child
   * component to be destroyed as well.
   */
  public void removeNotify()
  {
    synchronized (getTreeLock ())
      {
        int ncomps = ncomponents;
        Component[] comps = component;
        for (int i = ncomps - 1; i >= 0; --i)
          {
            Component comp = comps[i];
            if (comp != null)
              comp.removeNotify();
          }
        super.removeNotify();
      }
  }

  /**
   * Tests whether or not the specified component is contained within
   * this components subtree.
   *
   * @param comp The component to test.
   *
   * @return <code>true</code> if this container is an ancestor of the
   * specified component, <code>false</code> otherwise.
   */
  public boolean isAncestorOf(Component comp)
  {
    synchronized (getTreeLock ())
      {
        while (true)
          {
            if (comp == null)
              return false;
            if (comp == this)
              return true;
            comp = comp.getParent();
          }
      }
  }

  /**
   * Returns a string representing the state of this container for
   * debugging purposes.
   *
   * @return A string representing the state of this container.
   */
  protected String paramString()
  {
    if (layoutMgr == null)
      return super.paramString();

    StringBuffer sb = new StringBuffer();
    sb.append(super.paramString());
    sb.append(",layout=");
    sb.append(layoutMgr.getClass().getName());
    return sb.toString();
  }

  /**
   * Writes a listing of this container to the specified stream starting
   * at the specified indentation point.
   *
   * @param out The <code>PrintStream</code> to write to.
   * @param indent The indentation point.
   */
  public void list(PrintStream out, int indent)
  {
    synchronized (getTreeLock ())
      {
        super.list(out, indent);
        for (int i = 0; i < ncomponents; ++i)
          component[i].list(out, indent + 2);
      }
  }

  /**
   * Writes a listing of this container to the specified stream starting
   * at the specified indentation point.
   *
   * @param out The <code>PrintWriter</code> to write to.
   * @param indent The indentation point.
   */
  public void list(PrintWriter out, int indent)
  {
    synchronized (getTreeLock ())
      {
        super.list(out, indent);
        for (int i = 0; i < ncomponents; ++i)
          component[i].list(out, indent + 2);
      }
  }

  /**
   * Sets the focus traversal keys for a given traversal operation for this
   * Container.
   *
   * @exception IllegalArgumentException If id is not one of
   * KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS,
   * KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS,
   * KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS,
   * or KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS,
   * or if keystrokes contains null, or if any Object in keystrokes is not an
   * AWTKeyStroke, or if any keystroke represents a KEY_TYPED event, or if any
   * keystroke already maps to another focus traversal operation for this
   * Container.
   *
   * @since 1.4
   */
  public void setFocusTraversalKeys(int id,
				    Set<? extends AWTKeyStroke> keystrokes)
  {
    if (id != KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException ();

    if (keystrokes == null)
      {
        Container parent = getParent ();

        while (parent != null)
          {
            if (parent.areFocusTraversalKeysSet (id))
              {
                keystrokes = parent.getFocusTraversalKeys (id);
                break;
              }
            parent = parent.getParent ();
          }

        if (keystrokes == null)
          keystrokes = KeyboardFocusManager.getCurrentKeyboardFocusManager ().
            getDefaultFocusTraversalKeys (id);
      }

    Set sa;
    Set sb;
    Set sc;
    String name;
    switch (id)
      {
      case KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS:
        sa = getFocusTraversalKeys
          (KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS);
        sb = getFocusTraversalKeys
          (KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS);
        sc = getFocusTraversalKeys
          (KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS);
        name = "forwardFocusTraversalKeys";
        break;
      case KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS:
        sa = getFocusTraversalKeys
          (KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS);
        sb = getFocusTraversalKeys
          (KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS);
        sc = getFocusTraversalKeys
          (KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS);
        name = "backwardFocusTraversalKeys";
        break;
      case KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS:
        sa = getFocusTraversalKeys
          (KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS);
        sb = getFocusTraversalKeys
          (KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS);
        sc = getFocusTraversalKeys
          (KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS);
        name = "upCycleFocusTraversalKeys";
        break;
      case KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS:
        sa = getFocusTraversalKeys
          (KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS);
        sb = getFocusTraversalKeys
          (KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS);
        sc = getFocusTraversalKeys
          (KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS);
        name = "downCycleFocusTraversalKeys";
        break;
      default:
        throw new IllegalArgumentException ();
      }

    int i = keystrokes.size ();
    Iterator iter = keystrokes.iterator ();

    while (--i >= 0)
      {
        Object o = iter.next ();
        if (!(o instanceof AWTKeyStroke)
            || sa.contains (o) || sb.contains (o) || sc.contains (o)
            || ((AWTKeyStroke) o).keyCode == KeyEvent.VK_UNDEFINED)
          throw new IllegalArgumentException ();
      }

    if (focusTraversalKeys == null)
      focusTraversalKeys = new Set[4];

    keystrokes =
      Collections.unmodifiableSet(new HashSet<AWTKeyStroke>(keystrokes));
    firePropertyChange (name, focusTraversalKeys[id], keystrokes);

    focusTraversalKeys[id] = keystrokes;
  }
  
  /**
   * Returns the Set of focus traversal keys for a given traversal operation for
   * this Container.
   *
   * @exception IllegalArgumentException If id is not one of
   * KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS,
   * KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS,
   * KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS,
   * or KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS.
   *
   * @since 1.4
   */
  public Set<AWTKeyStroke> getFocusTraversalKeys (int id)
  {
    if (id != KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException ();

    Set s = null;

    if (focusTraversalKeys != null)
      s = focusTraversalKeys[id];

    if (s == null && parent != null)
      s = parent.getFocusTraversalKeys (id);

    return s == null ? (KeyboardFocusManager.getCurrentKeyboardFocusManager()
                        .getDefaultFocusTraversalKeys(id)) : s;
  }

  /**
   * Returns whether the Set of focus traversal keys for the given focus
   * traversal operation has been explicitly defined for this Container.
   * If this method returns false, this Container is inheriting the Set from
   * an ancestor, or from the current KeyboardFocusManager.
   *
   * @exception IllegalArgumentException If id is not one of
   * KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS,
   * KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS,
   * KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS,
   * or KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS.
   *
   * @since 1.4
   */
  public boolean areFocusTraversalKeysSet (int id)
  {
    if (id != KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException ();

    return focusTraversalKeys != null && focusTraversalKeys[id] != null;
  }

  /**
   * Check whether the given Container is the focus cycle root of this
   * Container's focus traversal cycle.  If this Container is a focus
   * cycle root itself, then it will be in two different focus cycles
   * -- it's own, and that of its ancestor focus cycle root's.  In
   * that case, if <code>c</code> is either of those containers, this
   * method will return true.
   *
   * @param c the candidate Container
   *
   * @return true if c is the focus cycle root of the focus traversal
   * cycle to which this Container belongs, false otherwise
   *
   * @since 1.4
   */
  public boolean isFocusCycleRoot (Container c)
  {
    if (this == c
        && isFocusCycleRoot ())
      return true;

    Container ancestor = getFocusCycleRootAncestor ();

    if (c == ancestor)
      return true;

    return false;
  }

  /**
   * If this Container is a focus cycle root, set the focus traversal
   * policy that determines the focus traversal order for its
   * children.  If non-null, this policy will be inherited by all
   * inferior focus cycle roots.  If <code>policy</code> is null, this
   * Container will inherit its policy from the closest ancestor focus
   * cycle root that's had its policy set.
   *
   * @param policy the new focus traversal policy for this Container or null
   *
   * @since 1.4
   */
  public void setFocusTraversalPolicy (FocusTraversalPolicy policy)
  {
    focusTraversalPolicy = policy;
  }

  /**
   * Return the focus traversal policy that determines the focus
   * traversal order for this Container's children.  This method
   * returns null if this Container is not a focus cycle root.  If the
   * focus traversal policy has not been set explicitly, then this
   * method will return an ancestor focus cycle root's policy instead.
   *
   * @return this Container's focus traversal policy or null
   *
   * @since 1.4
   */
  public FocusTraversalPolicy getFocusTraversalPolicy ()
  {
    if (!isFocusCycleRoot ())
      return null;

    if (focusTraversalPolicy == null)
      {
        Container ancestor = getFocusCycleRootAncestor ();

	if (ancestor != this && ancestor !=  null)
	  return ancestor.getFocusTraversalPolicy ();
	else
	  {
	    KeyboardFocusManager manager = KeyboardFocusManager.getCurrentKeyboardFocusManager ();

	    return manager.getDefaultFocusTraversalPolicy ();
	  }
      }
    else
      return focusTraversalPolicy;
  }

  /**
   * Check whether this Container's focus traversal policy has been
   * explicitly set.  If it has not, then this Container will inherit
   * its focus traversal policy from one of its ancestor focus cycle
   * roots.
   *
   * @return true if focus traversal policy is set, false otherwise
  */
  public boolean isFocusTraversalPolicySet ()
  {
    return focusTraversalPolicy == null;
  }

  /**
   * Set whether or not this Container is the root of a focus
   * traversal cycle.  This Container's focus traversal policy
   * determines the order of focus traversal.  Some policies prevent
   * the focus from being transferred between two traversal cycles
   * until an up or down traversal operation is performed.  In that
   * case, normal traversal (not up or down) is limited to this
   * Container and all of this Container's descendents that are not
   * descendents of inferior focus cycle roots.  In the default case
   * however, ContainerOrderFocusTraversalPolicy is in effect, and it
   * supports implicit down-cycle traversal operations.
   *
   * @param focusCycleRoot true if this is a focus cycle root, false otherwise
   *
   * @since 1.4
   */
  public void setFocusCycleRoot (boolean focusCycleRoot)
  {
    this.focusCycleRoot = focusCycleRoot;
  }

  /**
   * Set to <code>true</code> if this container provides a focus traversal
   * policy, <code>false</code> when the root container's focus
   * traversal policy should be used.
   *
   * @return <code>true</code> if this container provides a focus traversal
   *        policy, <code>false</code> when the root container's focus
   *        traversal policy should be used
   *
   * @see #setFocusTraversalPolicyProvider(boolean)
   *
   * @since 1.5
   */
  public final boolean isFocusTraversalPolicyProvider()
  {
    return focusTraversalPolicyProvider;
  }

  /**
   * Set to <code>true</code> if this container provides a focus traversal
   * policy, <code>false</code> when the root container's focus
   * traversal policy should be used.
   *
   * @param b <code>true</code> if this container provides a focus traversal
   *        policy, <code>false</code> when the root container's focus
   *        traversal policy should be used
   * 
   * @see #isFocusTraversalPolicyProvider()
   *
   * @since 1.5
   */
  public final void setFocusTraversalPolicyProvider(boolean b)
  {
    focusTraversalPolicyProvider = b;
  }

  /**
   * Check whether this Container is a focus cycle root.
   *
   * @return true if this is a focus cycle root, false otherwise
   *
   * @since 1.4
   */
  public boolean isFocusCycleRoot ()
  {
    return focusCycleRoot;
  }

  /**
   * Transfer focus down one focus traversal cycle.  If this Container
   * is a focus cycle root, then its default component becomes the
   * focus owner, and this Container becomes the current focus cycle
   * root.  No traversal will occur if this Container is not a focus
   * cycle root.
   *
   * @since 1.4
   */
  public void transferFocusDownCycle ()
  {
    if (isFocusCycleRoot())
      {
        KeyboardFocusManager fm =
          KeyboardFocusManager.getCurrentKeyboardFocusManager();
        fm.setGlobalCurrentFocusCycleRoot(this);
        FocusTraversalPolicy policy = getFocusTraversalPolicy();
        Component defaultComponent = policy.getDefaultComponent(this);
        if (defaultComponent != null)
          defaultComponent.requestFocus();
      }
  }

  /**
   * Sets the ComponentOrientation property of this container and all components
   * contained within it.
   *
   * @exception NullPointerException If orientation is null
   *
   * @since 1.4
   */
  public void applyComponentOrientation (ComponentOrientation orientation)
  {
    if (orientation == null)
      throw new NullPointerException();

    setComponentOrientation(orientation);
    for (int i = 0; i < ncomponents; i++)
      {
        if (component[i] instanceof Container)
             ((Container) component[i]).applyComponentOrientation(orientation); 
          else
             component[i].setComponentOrientation(orientation);
      }
  }

  public void addPropertyChangeListener (PropertyChangeListener listener)
  {
    // TODO: Why is this overridden?
    super.addPropertyChangeListener(listener);
  }

  public void addPropertyChangeListener (String propertyName,
                                         PropertyChangeListener listener)
  {
    // TODO: Why is this overridden?
    super.addPropertyChangeListener(propertyName, listener);
  }


  /**
   * Sets the Z ordering for the component <code>comp</code> to
   * <code>index</code>. Components with lower Z order paint above components
   * with higher Z order.
   *
   * @param comp the component for which to change the Z ordering
   * @param index the index to set
   *
   * @throws NullPointerException if <code>comp == null</code>
   * @throws IllegalArgumentException if comp is an ancestor of this container
   * @throws IllegalArgumentException if <code>index</code> is not in
   *         <code>[0, getComponentCount()]</code> for moving between
   *         containers or <code>[0, getComponentCount() - 1]</code> for moving
   *         inside this container
   * @throws IllegalArgumentException if <code>comp == this</code>
   * @throws IllegalArgumentException if <code>comp</code> is a
   *         <code>Window</code>
   *
   * @see #getComponentZOrder(Component)
   *
   * @since 1.5
   */
  public final void setComponentZOrder(Component comp, int index)
  {
    if (comp == null)
      throw new NullPointerException("comp must not be null");
    if (comp instanceof Container && ((Container) comp).isAncestorOf(this))
      throw new IllegalArgumentException("comp must not be an ancestor of "
                                         + "this");
    if (comp instanceof Window)
      throw new IllegalArgumentException("comp must not be a Window");

    if (comp == this)
      throw new IllegalArgumentException("cannot add component to itself");

    synchronized (getTreeLock())
      {
        // FIXME: Implement reparenting.
        if ( comp.getParent() != this)
          throw new AssertionError("Reparenting is not implemented yet");
        else
          {
            // Find current component index.
            int currentIndex = getComponentZOrder(comp);
            if (currentIndex < index)
              {
                System.arraycopy(component, currentIndex + 1, component,
                                 currentIndex, index - currentIndex);
              }
            else
              {
                System.arraycopy(component, index, component, index + 1,
                                 currentIndex - index);
              }
            component[index] = comp;
          }
      }
  }

  /**
   * Returns the Z ordering index of <code>comp</code>. If <code>comp</code>
   * is not a child component of this Container, this returns <code>-1</code>.
   *
   * @param comp the component for which to query the Z ordering
   *
   * @return the Z ordering index of <code>comp</code> or <code>-1</code> if
   *         <code>comp</code> is not a child of this Container
   *
   * @see #setComponentZOrder(Component, int)
   *
   * @since 1.5
   */
  public final int getComponentZOrder(Component comp)
  {
    synchronized (getTreeLock())
      {
        int index = -1;
        if (component != null)
          {
            for (int i = 0; i < ncomponents; i++)
              {
                if (component[i] == comp)
                  {
                    index = i;
                    break;
                  }
              }
          }
        return index;
      }
  }

  // Hidden helper methods.

  /**
   * Perform a graphics operation on the children of this container.
   * For each applicable child, the visitChild() method will be called
   * to perform the graphics operation.
   *
   * @param gfx The graphics object that will be used to derive new
   * graphics objects for the children.
   *
   * @param visitor Object encapsulating the graphics operation that
   * should be performed.
   *
   * @param lightweightOnly If true, only lightweight components will
   * be visited.
   */
  private void visitChildren(Graphics gfx, GfxVisitor visitor,
                             boolean lightweightOnly)
  {
    synchronized (getTreeLock())
      {
        for (int i = ncomponents - 1; i >= 0; --i)
          {
            Component comp = component[i];
            boolean applicable = comp.isVisible()
                                 && (comp.isLightweight() || ! lightweightOnly);
            
            if (applicable)
              visitChild(gfx, visitor, comp);
          }
      }
  }

  /**
   * Perform a graphics operation on a child. A translated and clipped
   * graphics object will be created, and the visit() method of the
   * visitor will be called to perform the operation.
   *
   * @param gfx The graphics object that will be used to derive new
   * graphics objects for the child.
   *
   * @param visitor Object encapsulating the graphics operation that
   * should be performed.
   *
   * @param comp The child component that should be visited.
   */
  private void visitChild(Graphics gfx, GfxVisitor visitor,
                          Component comp)
  {
    Rectangle bounds = comp.getBounds();
    
    if(!gfx.hitClip(bounds.x,bounds.y, bounds.width, bounds.height))
      return;
    Graphics g2 = gfx.create(bounds.x, bounds.y, bounds.width,
                             bounds.height);
    try
      {
        g2.setFont(comp.getFont());
        visitor.visit(comp, g2);
      }
    finally
      {
        g2.dispose();
      }
  }

  /**
   * Overridden to dispatch events to lightweight descendents.
   *
   * @param e the event to dispatch.
   */
  void dispatchEventImpl(AWTEvent e)
  {
    LightweightDispatcher dispatcher = LightweightDispatcher.getInstance(); 
    if (! isLightweight() && dispatcher.dispatchEvent(e))
      {
        // Some lightweight descendent got this event dispatched. Consume
        // it and let the peer handle it.
        e.consume();
        ComponentPeer p = peer;
        if (p != null)
          p.handleEvent(e);
      }
    else
      {
        super.dispatchEventImpl(e);
      }
  }

  /**
   * This is called by the lightweight dispatcher to avoid recursivly
   * calling into the lightweight dispatcher.
   *
   * @param e the event to dispatch
   *
   * @see LightweightDispatcher#redispatch(MouseEvent, Component, int)
   */
  void dispatchNoLightweight(AWTEvent e)
  {
    super.dispatchEventImpl(e);
  }

  /**
   * Tests if this container has an interest in the given event id.
   *
   * @param eventId The event id to check.
   *
   * @return <code>true</code> if a listener for the event id exists or
   *         if the eventMask is set for the event id.
   *
   * @see java.awt.Component#eventTypeEnabled(int)
   */
  boolean eventTypeEnabled(int eventId)
  {
    if(eventId <= ContainerEvent.CONTAINER_LAST 
       && eventId >= ContainerEvent.CONTAINER_FIRST)
      return containerListener != null
        || (eventMask & AWTEvent.CONTAINER_EVENT_MASK) != 0;
      else 
        return super.eventTypeEnabled(eventId);
  }

  // This is used to implement Component.transferFocus.
  Component findNextFocusComponent(Component child)
  {
    synchronized (getTreeLock ())
      {
        int start, end;
        if (child != null)
          {
            for (start = 0; start < ncomponents; ++start)
              {
                if (component[start] == child)
                  break;
              }
            end = start;
            // This special case lets us be sure to terminate.
            if (end == 0)
              end = ncomponents;
            ++start;
          }
        else
          {
            start = 0;
            end = ncomponents;
          }

        for (int j = start; j != end; ++j)
          {
            if (j >= ncomponents)
              {
                // The JCL says that we should wrap here.  However, that
                // seems wrong.  To me it seems that focus order should be
                // global within in given window.  So instead if we reach
                // the end we try to look in our parent, if we have one.
                if (parent != null)
                  return parent.findNextFocusComponent(this);
                j -= ncomponents;
              }
            if (component[j] instanceof Container)
              {
                Component c = component[j];
                c = c.findNextFocusComponent(null);
                if (c != null)
                  return c;
              }
            else if (component[j].isFocusTraversable())
              return component[j];
          }

        return null;
      }
  }

  /**
   * Fires hierarchy events to the children of this container and this
   * container itself. This overrides {@link Component#fireHierarchyEvent}
   * in order to forward this event to all children.
   */
  void fireHierarchyEvent(int id, Component changed, Container parent,
                          long flags)
  {
    // Only propagate event if there is actually a listener waiting for it.
    if ((id == HierarchyEvent.HIERARCHY_CHANGED && numHierarchyListeners > 0)
        || ((id == HierarchyEvent.ANCESTOR_MOVED
             || id == HierarchyEvent.ANCESTOR_RESIZED)
            && numHierarchyBoundsListeners > 0))
      {
        for (int i = 0; i < ncomponents; i++)
          component[i].fireHierarchyEvent(id, changed, parent, flags);
        super.fireHierarchyEvent(id, changed, parent, flags);
      }
  }

  /**
   * Adjusts the number of hierarchy listeners of this container and all of
   * its parents. This is called by the add/remove listener methods and
   * structure changing methods in Container.
   *
   * @param type the type, either {@link AWTEvent#HIERARCHY_BOUNDS_EVENT_MASK}
   *        or {@link AWTEvent#HIERARCHY_EVENT_MASK}
   * @param delta the number of listeners added or removed
   */
  void updateHierarchyListenerCount(long type, int delta)
  {
    if (type == AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK)
      numHierarchyBoundsListeners += delta;
    else if (type == AWTEvent.HIERARCHY_EVENT_MASK)
      numHierarchyListeners += delta;
    else
      assert false : "Should not reach here";

    if (parent != null)
      parent.updateHierarchyListenerCount(type, delta);
  }

  /**
   * Notifies interested listeners about resizing or moving the container.
   * This performs the super behaviour (sending component events) and
   * additionally notifies any hierarchy bounds listeners on child components.
   *
   * @param resized true if the component has been resized, false otherwise
   * @param moved true if the component has been moved, false otherwise
   */
  void notifyReshape(boolean resized, boolean moved)
  {
    // Notify component listeners.
    super.notifyReshape(resized, moved);

    if (ncomponents > 0)
      {
        // Notify hierarchy bounds listeners.
        if (resized)
          {
            for (int i = 0; i < getComponentCount(); i++)
              {
                Component child = getComponent(i);
                child.fireHierarchyEvent(HierarchyEvent.ANCESTOR_RESIZED,
                                         this, parent, 0);
              }
          }
        if (moved)
          {
            for (int i = 0; i < getComponentCount(); i++)
              {
                Component child = getComponent(i);
                child.fireHierarchyEvent(HierarchyEvent.ANCESTOR_MOVED,
                                         this, parent, 0);
              }
          }
      }
  }

  private void addNotifyContainerChildren()
  {
    synchronized (getTreeLock ())
      {
        for (int i = ncomponents;  --i >= 0; )
          {
            component[i].addNotify();
          }
      }
  }

  /**
   * Deserialize this Container:
   * <ol>
   * <li>Read from the stream the default serializable fields.</li>
   * <li>Read a list of serializable ContainerListeners as optional
   * data.  If the list is null, no listeners will be registered.</li>
   * <li>Read this Container's FocusTraversalPolicy as optional data.
   * If this is null, then this Container will use a
   * DefaultFocusTraversalPolicy.</li>
   * </ol>
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if deserialization fails
   * @throws IOException if the stream fails
   */
  private void readObject (ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    s.defaultReadObject ();
    String key = (String) s.readObject ();
    while (key != null)
      {
        Object object = s.readObject ();
        if ("containerL".equals (key))
          addContainerListener((ContainerListener) object);
        // FIXME: under what key is the focus traversal policy stored?
        else if ("focusTraversalPolicy".equals (key))
          setFocusTraversalPolicy ((FocusTraversalPolicy) object);

        key = (String) s.readObject();
      }
  }

  /**
   * Serialize this Container:
   * <ol>
   * <li>Write to the stream the default serializable fields.</li>
   * <li>Write the list of serializable ContainerListeners as optional
   * data.</li>
   * <li>Write this Container's FocusTraversalPolicy as optional data.</li>
   * </ol>
   *
   * @param s the stream to write to
   * @throws IOException if the stream fails
   */
  private void writeObject (ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject ();
    AWTEventMulticaster.save (s, "containerL", containerListener);
    if (focusTraversalPolicy instanceof Serializable)
      s.writeObject (focusTraversalPolicy);
    else
      s.writeObject (null);
  }

  // Nested classes.

  /* The following classes are used in concert with the
     visitChildren() method to implement all the graphics operations
     that requires traversal of the containment hierarchy. */

  abstract static class GfxVisitor
  {
    public abstract void visit(Component c, Graphics gfx);
  }

  static class GfxPaintVisitor extends GfxVisitor
  {
    public static final GfxVisitor INSTANCE = new GfxPaintVisitor();
    
    public void visit(Component c, Graphics gfx)
    {
      c.paint(gfx);
    }
  }

  static class GfxPrintVisitor extends GfxVisitor
  {
    public static final GfxVisitor INSTANCE = new GfxPrintVisitor();
    
    public void visit(Component c, Graphics gfx)
    {
      c.print(gfx);
    }
  }

  static class GfxPaintAllVisitor extends GfxVisitor
  {
    public static final GfxVisitor INSTANCE = new GfxPaintAllVisitor();

    public void visit(Component c, Graphics gfx)
    {
      c.paintAll(gfx);
    }
  }

  static class GfxPrintAllVisitor extends GfxVisitor
  {
    public static final GfxVisitor INSTANCE = new GfxPrintAllVisitor();

    public void visit(Component c, Graphics gfx)
    {
      c.printAll(gfx);
    }
  }

  /**
   * This class provides accessibility support for subclasses of container.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   *
   * @since 1.3
   */
  protected class AccessibleAWTContainer extends AccessibleAWTComponent
  {
    /**
     * Compatible with JDK 1.4+.
     */
    private static final long serialVersionUID = 5081320404842566097L;

    /**
     * The handler to fire PropertyChange when children are added or removed.
     *
     * @serial the handler for property changes
     */
    protected ContainerListener accessibleContainerHandler
      = new AccessibleContainerHandler();

    /**
     * The default constructor.
     */
    protected AccessibleAWTContainer()
    {
      Container.this.addContainerListener(accessibleContainerHandler);
    }

    /**
     * Return the number of accessible children of the containing accessible
     * object (at most the total number of its children).
     *
     * @return the number of accessible children
     */
    public int getAccessibleChildrenCount()
    {
      synchronized (getTreeLock ())
        {
          int count = 0;
          int i = component == null ? 0 : component.length;
          while (--i >= 0)
            if (component[i] instanceof Accessible)
              count++;
          return count;
        }
    }

    /**
     * Return the nth accessible child of the containing accessible object.
     *
     * @param i the child to grab, zero-based
     * @return the accessible child, or null
     */
    public Accessible getAccessibleChild(int i)
    {
      synchronized (getTreeLock ())
        {
          if (component == null)
            return null;
          int index = -1;
          while (i >= 0 && ++index < component.length)
            if (component[index] instanceof Accessible)
              i--;
          if (i < 0)
            return (Accessible) component[index];
          return null;
        }
    }

    /**
     * Return the accessible child located at point (in the parent's
     * coordinates), if one exists.
     *
     * @param p the point to look at
     *
     * @return an accessible object at that point, or null
     *
     * @throws NullPointerException if p is null
     */
    public Accessible getAccessibleAt(Point p)
    {
      Component c = getComponentAt(p.x, p.y);
      return c != Container.this && c instanceof Accessible ? (Accessible) c
        : null;
    }

    /**
     * This class fires a <code>PropertyChange</code> listener, if registered,
     * when children are added or removed from the enclosing accessible object.
     *
     * @author Eric Blake (ebb9@email.byu.edu)
     *
     * @since 1.3
     */
    protected class AccessibleContainerHandler implements ContainerListener
    {
      /**
       * Default constructor.
       */
      protected AccessibleContainerHandler()
      {
        // Nothing to do here.
      }

      /**
       * Fired when a component is added; forwards to the PropertyChange
       * listener.
       *
       * @param e the container event for adding
       */
      public void componentAdded(ContainerEvent e)
      {
        AccessibleAWTContainer.this.firePropertyChange
          (ACCESSIBLE_CHILD_PROPERTY, null, e.getChild());
      }

      /**
       * Fired when a component is removed; forwards to the PropertyChange
       * listener.
       *
       * @param e the container event for removing
       */
      public void componentRemoved(ContainerEvent e)
      {
        AccessibleAWTContainer.this.firePropertyChange
          (ACCESSIBLE_CHILD_PROPERTY, e.getChild(), null);
      }
    } // class AccessibleContainerHandler
  } // class AccessibleAWTContainer
} // class Container
