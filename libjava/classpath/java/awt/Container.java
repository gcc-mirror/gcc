/* Container.java -- parent container class in AWT
   Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005  Free Software Foundation

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
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.ContainerPeer;
import java.awt.peer.LightweightPeer;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
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
import javax.swing.SwingUtilities;

/**
 * A generic window toolkit object that acts as a container for other objects.
 * Components are tracked in a list, and new elements are at the end of the
 * list or bottom of the stacking order.
 *
 * @author original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 *
 * @since 1.0
 *
 * @status still missing 1.4 support
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

  LightweightDispatcher dispatcher;

  Dimension maxSize;

  /**
   * @since 1.4
   */
  boolean focusCycleRoot;

  int containerSerializedDataVersion;

  /* Anything else is non-serializable, and should be declared "transient". */
  transient ContainerListener containerListener;
  transient PropertyChangeSupport changeSupport; 

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
   * Swaps the components at position i and j, in the container.
   */

  protected void swapComponents (int i, int j)
  {   
    synchronized (getTreeLock ())
      {
        if (i < 0 
            || i >= component.length
            || j < 0 
            || j >= component.length)
          throw new ArrayIndexOutOfBoundsException ();
        Component tmp = component[i];
        component[i] = component[j];
        component[j] = tmp;
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
    if (peer == null)
      return new Insets (0, 0, 0, 0);

    return ((ContainerPeer) peer).getInsets ();
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
        comp.parent = this;
        if (peer != null)
          {
            if (comp.isLightweight ())
	      {
		enableEvents (comp.eventMask);
		if (!isLightweight ())
		  enableEvents (AWTEvent.PAINT_EVENT_MASK);
	      }
          }

        invalidate();

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

        // Notify the layout manager.
        if (layoutMgr != null)
          {
            if (layoutMgr instanceof LayoutManager2)
              {
                LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
                lm2.addLayoutComponent(comp, constraints);
              }
            else if (constraints instanceof String)
              layoutMgr.addLayoutComponent((String) constraints, comp);
            else
              layoutMgr.addLayoutComponent(null, comp);
          }

        if (isShowing ())
          {
            // Post event to notify of adding the component.
            ContainerEvent ce = new ContainerEvent(this,
                                                   ContainerEvent.COMPONENT_ADDED,
                                                   comp);
            getToolkit().getSystemEventQueue().postEvent(ce);
          }
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
        Component r = component[index];

        r.removeNotify();

        System.arraycopy(component, index + 1, component, index,
                         ncomponents - index - 1);
        component[--ncomponents] = null;

        invalidate();

        if (layoutMgr != null)
          layoutMgr.removeLayoutComponent(r);

        r.parent = null;

        if (isShowing ())
          {
            // Post event to notify of removing the component.
            ContainerEvent ce = new ContainerEvent(this,
                                                   ContainerEvent.COMPONENT_REMOVED,
                                                   r);
            getToolkit().getSystemEventQueue().postEvent(ce);
          }
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
        while (ncomponents > 0)
          remove(0);
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
  }

  /**
   * Re-lays out the components in this container.
   */
  public void validate()
  {
    synchronized (getTreeLock ())
      {
        if (! isValid() && peer != null)
          {
            validateTree();
          }
      }
  }

  /**
   * Recursively invalidates the container tree.
   */
  void invalidateTree()
  {
    for (int i = 0; i < ncomponents; i++)
      {
        Component comp = component[i];
        comp.invalidate();
        if (comp instanceof Container)
          ((Container) comp).invalidateTree();
      }
  }

  /**
   * Recursively validates the container tree, recomputing any invalid
   * layouts.
   */
  protected void validateTree()
  {
    if (valid)
      return;

    ContainerPeer cPeer = null;
    if (peer != null && ! (peer instanceof LightweightPeer))
      {
        cPeer = (ContainerPeer) peer;
        cPeer.beginValidate();
      }

    for (int i = 0; i < ncomponents; ++i)
      {
        Component comp = component[i];

        if (comp.getPeer () == null)
          comp.addNotify();
      }

    doLayout ();
    for (int i = 0; i < ncomponents; ++i)
      {
        Component comp = component[i];

        if (! comp.isValid())
          {
            if (comp instanceof Container)
              {
                ((Container) comp).validateTree();
              }
            else
              {
                component[i].validate();
              }
          }
      }

    /* children will call invalidate() when they are layed out. It
       is therefore important that valid is not set to true
       until after the children have been layed out. */
    valid = true;

    if (cPeer != null)
      cPeer.endValidate();
  }

  public void setFont(Font f)
  {
    super.setFont(f);
    // FIXME: Although it might make more sense to invalidate only
    // those children whose font == null, Sun invalidates all children.
    // So we'll do the same.
    invalidateTree();
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
    if (layoutMgr != null)
      return layoutMgr.preferredLayoutSize (this);
    else
      return super.preferredSize ();
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
    if (layoutMgr != null)
      return layoutMgr.minimumLayoutSize (this);
    else
      return super.minimumSize ();
  }

  /**
   * Returns the maximum size of this container.
   *
   * @return The maximum size of this container.
   */
  public Dimension getMaximumSize()
  {
    if (layoutMgr != null && layoutMgr instanceof LayoutManager2)
      {
        LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
        return lm2.maximumLayoutSize(this);
      }
    else
      return super.getMaximumSize();
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
    if (layoutMgr instanceof LayoutManager2)
      {
        LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
        return lm2.getLayoutAlignmentX(this);
      }
    else
      return super.getAlignmentX();
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
    if (layoutMgr instanceof LayoutManager2)
      {
        LayoutManager2 lm2 = (LayoutManager2) layoutMgr;
        return lm2.getLayoutAlignmentY(this);
      }
    else
      return super.getAlignmentY();
  }

  /**
   * Paints this container.  The implementation of this method in this
   * class forwards to any lightweight components in this container.  If
   * this method is subclassed, this method should still be invoked as
   * a superclass method so that lightweight components are properly
   * drawn.
   *
   * @param g The graphics context for this paint job.
   */
  public void paint(Graphics g)
  {
    if (!isShowing())
      return;
    // Paint self first.
    super.paint(g);
    // Visit heavyweights as well, in case they were
    // erased when we cleared the background for this container.
    visitChildren(g, GfxPaintVisitor.INSTANCE, false);
  }

  /**
   * Updates this container.  The implementation of this method in this
   * class forwards to any lightweight components in this container.  If
   * this method is subclassed, this method should still be invoked as
   * a superclass method so that lightweight components are properly
   * drawn.
   *
   * @param g The graphics context for this update.
   */
  public void update(Graphics g)
  {
    super.update(g);
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
    super.paint(g);
    visitChildren(g, GfxPaintAllVisitor.INSTANCE, true);
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
    containerListener = AWTEventMulticaster.add(containerListener, listener);
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
   * Returns an array of all the objects currently registered as FooListeners
   * upon this Container. FooListeners are registered using the addFooListener
   * method.
   *
   * @exception ClassCastException If listenerType doesn't specify a class or
   * interface that implements @see java.util.EventListener.
   *
   * @since 1.3
   */
  public EventListener[] getListeners(Class listenerType)
  {
    if (listenerType == ContainerListener.class)
      return getContainerListeners();
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
   * Returns the component located at the specified point.  This is done
   * by checking whether or not a child component claims to contain this
   * point.  The first child component that does is returned.  If no
   * child component claims the point, the container itself is returned,
   * unless the point does not exist within this container, in which
   * case <code>null</code> is returned.
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
        for (int i = 0; i < ncomponents; ++i)
          {
            // Ignore invisible children...
            if (!component[i].isVisible ())
              continue;

            int x2 = x - component[i].x;
            int y2 = y - component[i].y;
            if (component[i].contains (x2, y2))
              return component[i];
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
   * @param p The point to return the component at.
   * @return The component containing the specified point, or <code>null</code>
   * if there is no such point.
   */
  public Component getComponentAt(Point p)
  {
    return getComponentAt (p.x, p.y);
  }

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
    super.addNotify();
    addNotifyContainerChildren();
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
        for (int i = 0; i < ncomponents; ++i)
          component[i].removeNotify();
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
  public void setFocusTraversalKeys(int id, Set keystrokes)
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
      focusTraversalKeys = new Set[3];

    keystrokes = Collections.unmodifiableSet (new HashSet (keystrokes));
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
  public Set getFocusTraversalKeys (int id)
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

	if (ancestor != this)
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
    KeyboardFocusManager manager = KeyboardFocusManager.getCurrentKeyboardFocusManager ();

    manager.downFocusCycle (this);
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
      throw new NullPointerException ();
  }
  
  public void addPropertyChangeListener (PropertyChangeListener listener)
  {
    if (listener == null)
      return;

    if (changeSupport == null)
      changeSupport = new PropertyChangeSupport (this);

    changeSupport.addPropertyChangeListener (listener);
  }
  
  public void addPropertyChangeListener (String name,
                                         PropertyChangeListener listener)
  {
    if (listener == null)
      return;
    
    if (changeSupport == null)
      changeSupport = new PropertyChangeSupport (this);

    changeSupport.addPropertyChangeListener (name, listener);
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
    synchronized (getTreeLock ())
      {
        for (int i = ncomponents - 1; i >= 0; --i)
          {
            Component comp = component[i];
            // If we're visiting heavyweights as well,
            // don't recurse into Containers here. This avoids
            // painting the same nested child multiple times.
            boolean applicable = comp.isVisible()
              && (comp.isLightweight()
                  || !lightweightOnly && ! (comp instanceof Container));

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
    Rectangle oldClip = gfx.getClipBounds();
    if (oldClip == null)
      oldClip = bounds;

    Rectangle clip = oldClip.intersection(bounds);

    if (clip.isEmpty()) return;

    boolean clipped = false;
    boolean translated = false;
    try
      {
        gfx.setClip(clip.x, clip.y, clip.width, clip.height);
        clipped = true;
        gfx.translate(bounds.x, bounds.y);
        translated = true;
        visitor.visit(comp, gfx);
      }
    finally
      {
        if (translated)
          gfx.translate (-bounds.x, -bounds.y);
        if (clipped)
          gfx.setClip (oldClip.x, oldClip.y, oldClip.width, oldClip.height);
      }
  }

  void dispatchEventImpl(AWTEvent e)
  {
    // Give lightweight dispatcher a chance to handle it.
    if (eventTypeEnabled (e.id)
        && dispatcher != null 
        && dispatcher.handleEvent (e))
      return;
    
    if ((e.id <= ContainerEvent.CONTAINER_LAST
             && e.id >= ContainerEvent.CONTAINER_FIRST)
        && (containerListener != null
            || (eventMask & AWTEvent.CONTAINER_EVENT_MASK) != 0))
      processEvent(e);
    else
      super.dispatchEventImpl(e);
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

  private void addNotifyContainerChildren()
  {
    synchronized (getTreeLock ())
      {
        for (int i = ncomponents;  --i >= 0; )
          {
            component[i].addNotify();
            if (component[i].isLightweight ())
	      {

                // If we're not lightweight, and we just got a lightweight
                // child, we need a lightweight dispatcher to feed it events.
                if (! this.isLightweight()) 
                  {
                    if (dispatcher == null)
                      dispatcher = new LightweightDispatcher (this);
                  }	
	  

		enableEvents(component[i].eventMask);
		if (peer != null && !isLightweight ())
		  enableEvents (AWTEvent.PAINT_EVENT_MASK);
	      }
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

/**
 * There is a helper class implied from stack traces called
 * LightweightDispatcher, but since it is not part of the public API,
 * rather than mimic it exactly we write something which does "roughly
 * the same thing".
 */

class LightweightDispatcher implements Serializable
{
  private static final long serialVersionUID = 5184291520170872969L;
  private Container nativeContainer;
  private Cursor nativeCursor;
  private long eventMask;
  
  private transient Component mouseEventTarget;
  private transient Component pressedComponent;
  private transient Component lastComponentEntered;
  private transient Component tempComponent;
  private transient int pressCount;
  
  LightweightDispatcher(Container c)
  {
    nativeContainer = c;
  }

  void acquireComponentForMouseEvent(MouseEvent me)
  {
    int x = me.getX ();
    int y = me.getY ();

    // Find the candidate which should receive this event.
    Component parent = nativeContainer;
    Component candidate = null;
    Point p = me.getPoint();
    while (candidate == null && parent != null)
      {
        candidate =
          SwingUtilities.getDeepestComponentAt(parent, p.x, p.y);
        if (candidate == null || (candidate.eventMask & me.getID()) == 0)
        {
          candidate = null;
          p = SwingUtilities.convertPoint(parent, p.x, p.y, parent.parent);
          parent = parent.parent;
        }
      }

    // If the only candidate we found was the native container itself,
    // don't dispatch any event at all.  We only care about the lightweight
    // children here.
    if (candidate == nativeContainer)
      candidate = null;

    // If our candidate is new, inform the old target we're leaving.
    if (lastComponentEntered != null
        && lastComponentEntered.isShowing()
        && lastComponentEntered != candidate)
      {
        // Old candidate could have been removed from 
        // the nativeContainer so we check first.
        if (SwingUtilities.isDescendingFrom(lastComponentEntered, nativeContainer))
        {
          Point tp = 
            SwingUtilities.convertPoint(nativeContainer, 
                                        x, y, lastComponentEntered);
          MouseEvent exited = new MouseEvent (lastComponentEntered, 
                                              MouseEvent.MOUSE_EXITED,
                                              me.getWhen (), 
                                              me.getModifiersEx (), 
                                              tp.x, tp.y,
                                              me.getClickCount (),
                                              me.isPopupTrigger (),
                                              me.getButton ());
          tempComponent = lastComponentEntered;
          lastComponentEntered = null;
          tempComponent.dispatchEvent(exited);
        }
        lastComponentEntered = null;
      }
    // If we have a candidate, maybe enter it.
    if (candidate != null)
      {
        mouseEventTarget = candidate;
        if (candidate.isLightweight() 
            && candidate.isShowing()
            && candidate != nativeContainer
            && candidate != lastComponentEntered)
	  {			
            lastComponentEntered = mouseEventTarget;
            Point cp = SwingUtilities.convertPoint(nativeContainer, 
                                                   x, y, lastComponentEntered);
            MouseEvent entered = new MouseEvent (lastComponentEntered, 
                                                 MouseEvent.MOUSE_ENTERED,
                                                 me.getWhen (), 
                                                 me.getModifiersEx (), 
                                                 cp.x, cp.y,
                                                 me.getClickCount (),
                                                 me.isPopupTrigger (),
                                                 me.getButton ());
            lastComponentEntered.dispatchEvent (entered);
          }
      }

    if (me.getID() == MouseEvent.MOUSE_RELEASED
        || me.getID() == MouseEvent.MOUSE_PRESSED && pressCount > 0
        || me.getID() == MouseEvent.MOUSE_DRAGGED)
      // If any of the following events occur while a button is held down,
      // they should be dispatched to the same component to which the
      // original MOUSE_PRESSED event was dispatched:
      //   - MOUSE_RELEASED
      //   - MOUSE_PRESSED: another button pressed while the first is held down
      //   - MOUSE_DRAGGED
      if (SwingUtilities.isDescendingFrom(pressedComponent, nativeContainer))
        mouseEventTarget = pressedComponent;
    else if (me.getID() == MouseEvent.MOUSE_CLICKED)
      {
        // Don't dispatch CLICKED events whose target is not the same as the
        // target for the original PRESSED event.
        if (candidate != pressedComponent)
          mouseEventTarget = null;
        else if (pressCount == 0)
          pressedComponent = null;
      }
  }

  boolean handleEvent(AWTEvent e)
  {
    if (e instanceof MouseEvent)
      {
        MouseEvent me = (MouseEvent) e;

        acquireComponentForMouseEvent(me);
	
        // Avoid dispatching ENTERED and EXITED events twice.
        if (mouseEventTarget != null
            && mouseEventTarget.isShowing()
            && e.getID() != MouseEvent.MOUSE_ENTERED
            && e.getID() != MouseEvent.MOUSE_EXITED)
          {
            MouseEvent newEvt = 
              SwingUtilities.convertMouseEvent(nativeContainer, me, 
                                               mouseEventTarget);
            mouseEventTarget.dispatchEvent(newEvt);

            switch (e.getID())
              {
                case MouseEvent.MOUSE_PRESSED:
                  if (pressCount++ == 0)
                    pressedComponent = mouseEventTarget;
                  break;

                case MouseEvent.MOUSE_RELEASED:
                  // Clear our memory of the original PRESSED event, only if
                  // we're not expecting a CLICKED event after this. If
                  // there is a CLICKED event after this, it will do clean up.
                  if (--pressCount == 0
                      && mouseEventTarget != pressedComponent)
                    pressedComponent = null;
                  break;
              }
              if (newEvt.isConsumed())
                e.consume();
          }
      }
    
    return e.isConsumed();
  }

} // class LightweightDispatcher
