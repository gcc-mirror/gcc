/* RepaintManager.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.image.VolatileImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;

/**
 * <p>The repaint manager holds a set of dirty regions, invalid components,
 * and a double buffer surface.  The dirty regions and invalid components
 * are used to coalesce multiple revalidate() and repaint() calls in the
 * component tree into larger groups to be refreshed "all at once"; the
 * double buffer surface is used by root components to paint
 * themselves.</p>
 *
 * <p>In general, painting is very confusing in swing. see <a
 * href="http://java.sun.com/products/jfc/tsc/articles/painting/index.html">this
 * document</a> for more details.</p>
 *
 * @author Graydon Hoare (graydon@redhat.com)
 */
public class RepaintManager
{
  /**
   * The current repaint managers, indexed by their ThreadGroups.
   */
  static HashMap currentRepaintManagers;
  
  /**
   * <p>A helper class which is placed into the system event queue at
   * various times in order to facilitate repainting and layout. There is
   * typically only one of these objects active at any time. When the
   * {@link RepaintManager} is told to queue a repaint, it checks to see if
   * a {@link RepaintWorker} is "live" in the system event queue, and if
   * not it inserts one using {@link SwingUtilities#invokeLater}.</p>
   *
   * <p>When the {@link RepaintWorker} comes to the head of the system
   * event queue, its {@link RepaintWorker#run} method is executed by the
   * swing paint thread, which revalidates all invalid components and
   * repaints any damage in the swing scene.</p>
   */
  protected class RepaintWorker
    implements Runnable
  {

    boolean live;

    public RepaintWorker()
    {
      live = false;
    }

    public synchronized void setLive(boolean b) 
    {
      live = b;
    }

    public synchronized boolean isLive()
    {
      return live;
    }

    public void run()
    {
      ThreadGroup threadGroup = Thread.currentThread().getThreadGroup();
      RepaintManager rm =
        (RepaintManager) currentRepaintManagers.get(threadGroup);
      setLive(false);
      rm.validateInvalidComponents();
      rm.paintDirtyRegions();
    }

  }

  /**
   * Compares two components using their depths in the component hierarchy.
   * A component with a lesser depth (higher level components) are sorted
   * before components with a deeper depth (low level components). This is used
   * to order paint requests, so that the higher level components are painted
   * before the low level components get painted.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class ComponentComparator implements Comparator
  {

    /**
     * Compares two components.
     *
     * @param o1 the first component
     * @param o2 the second component
     *
     * @return a negative integer, if <code>o1</code> is higher in the
     *         hierarchy than <code>o2</code>, zero, if both are at the same
     *         level and a positive integer, if <code>o1</code> is deeper in
     *         the hierarchy than <code>o2</code> 
     */
    public int compare(Object o1, Object o2)
    {
      if (o1 instanceof JComponent && o2 instanceof JComponent)
        {
          JComponent c1 = (JComponent) o1;
          JComponent c2 = (JComponent) o2;
          return getDepth(c1) - getDepth(c2);
        }
      else
        throw new ClassCastException("This comparator can only be used with "
                                     + "JComponents");
    }

    /**
     * Computes the depth for a given JComponent.
     *
     * @param c the component to compute the depth for
     *
     * @return the depth of the component
     */
    private int getDepth(JComponent c)
    {
      Component comp = c;
      int depth = 0;
      while (comp != null)
        {
          comp = comp.getParent();
          depth++;
        }
      return depth;
    }
  }

  /** 
   * A table storing the dirty regions of components.  The keys of this
   * table are components, the values are rectangles. Each component maps
   * to exactly one rectangle.  When more regions are marked as dirty on a
   * component, they are union'ed with the existing rectangle.
   *
   * @see #addDirtyRegion
   * @see #getDirtyRegion
   * @see #isCompletelyDirty
   * @see #markCompletelyClean
   * @see #markCompletelyDirty
   */
  HashMap dirtyComponents;

  HashMap workDirtyComponents;

  /**
   * Stores the order in which the components get repainted.
   */
  ArrayList repaintOrder;
  ArrayList workRepaintOrder;

  /**
   * The comparator used for ordered inserting into the repaintOrder list. 
   */
  Comparator comparator;

  /**
   * A single, shared instance of the helper class. Any methods which mark
   * components as invalid or dirty eventually activate this instance. It
   * is added to the event queue if it is not already active, otherwise
   * reused.
   *
   * @see #addDirtyRegion
   * @see #addInvalidComponent
   */
  RepaintWorker repaintWorker;

  /** 
   * The set of components which need revalidation, in the "layout" sense.
   * There is no additional information about "what kind of layout" they
   * need (as there is with dirty regions), so it is just a vector rather
   * than a table.
   *
   * @see #addInvalidComponent
   * @see #removeInvalidComponent
   * @see #validateInvalidComponents
   */
  ArrayList invalidComponents;
  ArrayList workInvalidComponents;

  /** 
   * Whether or not double buffering is enabled on this repaint
   * manager. This is merely a hint to clients; the RepaintManager will
   * always return an offscreen buffer when one is requested.
   * 
   * @see #isDoubleBufferingEnabled
   * @see #setDoubleBufferingEnabled
   */
  boolean doubleBufferingEnabled;

  /** 
   * The current offscreen buffer. This is reused for all requests for
   * offscreen drawing buffers. It grows as necessary, up to {@link
   * #doubleBufferMaximumSize}, but there is only one shared instance.
   *
   * @see #getOffscreenBuffer
   * @see #doubleBufferMaximumSize
   */
  Image doubleBuffer;

  /**
   * The maximum width and height to allocate as a double buffer. Requests
   * beyond this size are ignored.
   *
   * @see #paintDirtyRegions
   * @see #getDoubleBufferMaximumSize
   * @see #setDoubleBufferMaximumSize
   */
  Dimension doubleBufferMaximumSize;


  /**
   * Create a new RepaintManager object.
   */
  public RepaintManager()
  {
    dirtyComponents = new HashMap();
    workDirtyComponents = new HashMap();
    repaintOrder = new ArrayList();
    workRepaintOrder = new ArrayList();
    invalidComponents = new ArrayList();
    workInvalidComponents = new ArrayList();
    repaintWorker = new RepaintWorker();
    doubleBufferMaximumSize = new Dimension(2000,2000);
    doubleBufferingEnabled = true;
  }

  /**
   * Returns the <code>RepaintManager</code> for the current thread's
   * thread group. The default implementation ignores the
   * <code>component</code> parameter and returns the same repaint manager
   * for all components.
   *
   * @param component a component to look up the manager of
   *
   * @return the current repaint manager for the calling thread's thread group
   *         and the specified component
   *
   * @see #setCurrentManager
   */
  public static RepaintManager currentManager(Component component)
  {
    if (currentRepaintManagers == null)
      currentRepaintManagers = new HashMap();
    ThreadGroup threadGroup = Thread.currentThread().getThreadGroup();
    RepaintManager currentManager =
      (RepaintManager) currentRepaintManagers.get(threadGroup);
    if (currentManager == null)
      {
        currentManager = new RepaintManager();
        currentRepaintManagers.put(threadGroup, currentManager);
      }
    return currentManager;
  }

  /**
   * Returns the <code>RepaintManager</code> for the current thread's
   * thread group. The default implementation ignores the
   * <code>component</code> parameter and returns the same repaint manager
   * for all components.
   *
   * This method is only here for backwards compatibility with older versions
   * of Swing and simply forwards to {@link #currentManager(Component)}.
   *
   * @param component a component to look up the manager of
   *
   * @return the current repaint manager for the calling thread's thread group
   *         and the specified component
   *
   * @see #setCurrentManager
   */
  public static RepaintManager currentManager(JComponent component)
  {
    return currentManager((Component)component);
  }

  /**
   * Sets the repaint manager for the calling thread's thread group.
   *
   * @param manager the repaint manager to set for the current thread's thread
   *        group
   *
   * @see #currentManager(Component)
   */
  public static void setCurrentManager(RepaintManager manager)
  {
    if (currentRepaintManagers == null)
      currentRepaintManagers = new HashMap();

    ThreadGroup threadGroup = Thread.currentThread().getThreadGroup();
    currentRepaintManagers.put(threadGroup, manager);
  }

  /**
   * Add a component to the {@link #invalidComponents} vector. If the
   * {@link #repaintWorker} class is not active, insert it in the system
   * event queue.
   *
   * @param component The component to add
   *
   * @see #removeInvalidComponent
   */
  public synchronized void addInvalidComponent(JComponent component)
  {
    Component ancestor = component.getParent();

    while (ancestor != null
           && (! (ancestor instanceof JComponent)
               || ! ((JComponent) ancestor).isValidateRoot() ))
      ancestor = ancestor.getParent();

    if (ancestor != null
        && ancestor instanceof JComponent
        && ((JComponent) ancestor).isValidateRoot())
      component = (JComponent) ancestor;

    if (invalidComponents.contains(component))
      return;

    invalidComponents.add(component);
    
    if (! repaintWorker.isLive())
      {
        repaintWorker.setLive(true);
        SwingUtilities.invokeLater(repaintWorker);
      }
  }

  /**
   * Remove a component from the {@link #invalidComponents} vector.
   *
   * @param component The component to remove
   *
   * @see #addInvalidComponent
   */
  public synchronized void removeInvalidComponent(JComponent component)
  {
    invalidComponents.remove(component);
  }

  /**
   * Add a region to the set of dirty regions for a specified component.
   * This involves union'ing the new region with any existing dirty region
   * associated with the component. If the {@link #repaintWorker} class
   * is not active, insert it in the system event queue.
   *
   * @param component The component to add a dirty region for
   * @param x The left x coordinate of the new dirty region
   * @param y The top y coordinate of the new dirty region
   * @param w The width of the new dirty region
   * @param h The height of the new dirty region
   *
   * @see #addDirtyRegion
   * @see #getDirtyRegion
   * @see #isCompletelyDirty
   * @see #markCompletelyClean
   * @see #markCompletelyDirty
   */
  public synchronized void addDirtyRegion(JComponent component, int x, int y,
                                          int w, int h)
  {
    if (w == 0 || h == 0 || !component.isShowing())
      return;
    Rectangle r = new Rectangle(x, y, w, h);
    if (dirtyComponents.containsKey(component))
      r = r.union((Rectangle)dirtyComponents.get(component));
    else
      insertInRepaintOrder(component);
    dirtyComponents.put(component, r);
    if (! repaintWorker.isLive())
      {
        repaintWorker.setLive(true);
        SwingUtilities.invokeLater(repaintWorker);
      }
  }

  /**
   * Inserts a component into the repaintOrder list in an ordered fashion,
   * using a binary search.
   *
   * @param c the component to be inserted
   */
  private void insertInRepaintOrder(JComponent c)
  {
    if (comparator == null)
      comparator = new ComponentComparator();
    int insertIndex = Collections.binarySearch(repaintOrder, c, comparator);
    if (insertIndex < 0)
      insertIndex = -(insertIndex + 1);
    repaintOrder.add(insertIndex, c);
  }

  /**
   * Get the dirty region associated with a component, or <code>null</code>
   * if the component has no dirty region.
   *
   * @param component The component to get the dirty region of
   *
   * @return The dirty region of the component
   *
   * @see #dirtyComponents
   * @see #addDirtyRegion
   * @see #isCompletelyDirty
   * @see #markCompletelyClean
   * @see #markCompletelyDirty
   */
  public Rectangle getDirtyRegion(JComponent component)
  {
    Rectangle dirty = (Rectangle) dirtyComponents.get(component);
    if (dirty == null)
      dirty = new Rectangle();
    return dirty;
  }
  
  /**
   * Mark a component as dirty over its entire bounds.
   *
   * @param component The component to mark as dirty
   *
   * @see #dirtyComponents
   * @see #addDirtyRegion
   * @see #getDirtyRegion
   * @see #isCompletelyDirty
   * @see #markCompletelyClean
   */
  public void markCompletelyDirty(JComponent component)
  {
    Rectangle r = component.getBounds();
    addDirtyRegion(component, r.x, r.y, r.width, r.height);
    component.isCompletelyDirty = true;
  }

  /**
   * Remove all dirty regions for a specified component
   *
   * @param component The component to mark as clean
   *
   * @see #dirtyComponents
   * @see #addDirtyRegion
   * @see #getDirtyRegion
   * @see #isCompletelyDirty
   * @see #markCompletelyDirty
   */
  public void markCompletelyClean(JComponent component)
  {
    synchronized (this)
      {
        dirtyComponents.remove(component);
      }
    component.isCompletelyDirty = false;
  }

  /**
   * Return <code>true</code> if the specified component is completely
   * contained within its dirty region, otherwise <code>false</code>
   *
   * @param component The component to check for complete dirtyness
   *
   * @return Whether the component is completely dirty
   *
   * @see #dirtyComponents
   * @see #addDirtyRegion
   * @see #getDirtyRegion
   * @see #isCompletelyDirty
   * @see #markCompletelyClean
   */
  public boolean isCompletelyDirty(JComponent component)
  {
    if (! dirtyComponents.containsKey(component))
      return false;
    return component.isCompletelyDirty;
  }

  /**
   * Validate all components which have been marked invalid in the {@link
   * #invalidComponents} vector.
   */
  public void validateInvalidComponents()
  {
    // In order to keep the blocking of application threads minimal, we switch
    // the invalidComponents field with the workInvalidComponents field and
    // work with the workInvalidComponents field.
    synchronized(this)
    {
      ArrayList swap = invalidComponents;
      invalidComponents = workInvalidComponents;
      workInvalidComponents = swap;
    }
    for (Iterator i = workInvalidComponents.iterator(); i.hasNext(); )
      {
        JComponent comp = (JComponent) i.next();
        if (! (comp.isVisible() && comp.isShowing()))
          continue;
        comp.validate();
      }
    workInvalidComponents.clear();
  }

  /**
   * Repaint all regions of all components which have been marked dirty in
   * the {@link #dirtyComponents} table.
   */
  public synchronized void paintDirtyRegions()
  {
    // In order to keep the blocking of application threads minimal, we switch
    // the dirtyComponents field with the workdirtyComponents field and the
    // repaintOrder field with the workRepaintOrder field and work with the
    // work* fields.
    synchronized(this)
    {
      ArrayList swap = workRepaintOrder;
      workRepaintOrder = repaintOrder;
      repaintOrder = swap;
      HashMap swap2 = workDirtyComponents;
      workDirtyComponents = dirtyComponents;
      dirtyComponents = swap2;
    }
    for (Iterator i = workRepaintOrder.iterator(); i.hasNext();)
      {
        JComponent comp = (JComponent) i.next();
        // If a component is marked completely clean in the meantime, then skip
        // it.
        Rectangle damaged = (Rectangle) workDirtyComponents.get(comp);
        if (damaged == null || damaged.isEmpty())
          continue;
        comp.paintImmediately(damaged);
      }
    workRepaintOrder.clear();
    workDirtyComponents.clear();
  }

  /**
   * Get an offscreen buffer for painting a component's image. This image
   * may be smaller than the proposed dimensions, depending on the value of
   * the {@link #doubleBufferMaximumSize} property.
   *
   * @param component The component to return an offscreen buffer for
   * @param proposedWidth The proposed width of the offscreen buffer
   * @param proposedHeight The proposed height of the offscreen buffer
   *
   * @return A shared offscreen buffer for painting
   *
   * @see #doubleBuffer
   */
  public Image getOffscreenBuffer(Component component, int proposedWidth,
                                  int proposedHeight)
  {
    if (doubleBuffer == null 
        || (((doubleBuffer.getWidth(null) < proposedWidth) 
             || (doubleBuffer.getHeight(null) < proposedHeight))
            && (proposedWidth < doubleBufferMaximumSize.width)
            && (proposedHeight < doubleBufferMaximumSize.height)))
      {
        doubleBuffer = component.createImage(proposedWidth, proposedHeight);
      }
    return doubleBuffer;
  }

  /**
   * Creates and returns a volatile offscreen buffer for the specified
   * component that can be used as a double buffer. The returned image
   * is a {@link VolatileImage}. Its size will be <code>(proposedWidth,
   * proposedHeight)</code> except when the maximum double buffer size
   * has been set in this RepaintManager.
   *
   * @param comp the Component for which to create a volatile buffer
   * @param proposedWidth the proposed width of the buffer
   * @param proposedHeight the proposed height of the buffer
   *
   * @since 1.4
   *
   * @see VolatileImage
   */
  public Image getVolatileOffscreenBuffer(Component comp, int proposedWidth,
                                          int proposedHeight)
  {
    int maxWidth = doubleBufferMaximumSize.width;
    int maxHeight = doubleBufferMaximumSize.height;
    return comp.createVolatileImage(Math.min(maxWidth, proposedWidth),
                                    Math.min(maxHeight, proposedHeight));
  }
  

  /**
   * Get the value of the {@link #doubleBufferMaximumSize} property.
   *
   * @return The current value of the property
   *
   * @see #setDoubleBufferMaximumSize
   */
  public Dimension getDoubleBufferMaximumSize()
  {
    return doubleBufferMaximumSize;
  }

  /**
   * Set the value of the {@link #doubleBufferMaximumSize} property.
   *
   * @param size The new value of the property
   *
   * @see #getDoubleBufferMaximumSize
   */
  public void setDoubleBufferMaximumSize(Dimension size)
  {
    doubleBufferMaximumSize = size;
  }

  /**
   * Set the value of the {@link #doubleBufferingEnabled} property.
   *
   * @param buffer The new value of the property
   *
   * @see #isDoubleBufferingEnabled
   */
  public void setDoubleBufferingEnabled(boolean buffer)
  {
    doubleBufferingEnabled = buffer;
  }

  /**
   * Get the value of the {@link #doubleBufferingEnabled} property.
   *
   * @return The current value of the property
   *
   * @see #setDoubleBufferingEnabled
   */
  public boolean isDoubleBufferingEnabled()
  {
    return doubleBufferingEnabled;
  }
  
  public String toString()
  {
    return "RepaintManager";
  }
}
