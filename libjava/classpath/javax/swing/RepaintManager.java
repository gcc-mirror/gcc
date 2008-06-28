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

import gnu.classpath.SystemProperties;
import gnu.java.awt.LowPriorityEvent;

import java.applet.Applet;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.InvocationEvent;
import java.awt.image.VolatileImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.WeakHashMap;

/**
 * <p>The repaint manager holds a set of dirty regions, invalid components,
 * and a double buffer surface.  The dirty regions and invalid components
 * are used to coalesce multiple revalidate() and repaint() calls in the
 * component tree into larger groups to be refreshed "all at once"; the
 * double buffer surface is used by root components to paint
 * themselves.</p>
 *
 * <p>See <a
 * href="http://java.sun.com/products/jfc/tsc/articles/painting/index.html">this
 * document</a> for more details.</p>
 * document</a> for more details.</p>
 *
 * @author Roman Kennke (kennke@aicas.com)
 * @author Graydon Hoare (graydon@redhat.com)
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class RepaintManager
{
  /**
   * An InvocationEvent subclass that implements LowPriorityEvent. This is used
   * to defer the execution of RepaintManager requests as long as possible on
   * the event queue. This way we make sure that all available input is
   * processed before getting active with the RepaintManager. This allows
   * for better optimization (more validate and repaint requests can be
   * coalesced) and thus has a positive effect on performance for GUI
   * applications under heavy load.
   */
  private static class RepaintWorkerEvent
    extends InvocationEvent
    implements LowPriorityEvent
  {

    /**
     * Creates a new RepaintManager event.
     *
     * @param source the source
     * @param runnable the runnable to execute
     */
    public RepaintWorkerEvent(Object source, Runnable runnable,
                              Object notifier, boolean catchEx)
    {
      super(source, runnable, notifier, catchEx);
    }

    /**
     * An application that I met implements its own event dispatching and
     * calls dispatch() via reflection, and only checks declared methods,
     * that is, it expects this method to be in the event's class, not
     * in a superclass. So I put this in here... sigh.
     */
    public void dispatch()
    {
      super.dispatch();
    }
  }
  
  /**
   * The current repaint managers, indexed by their ThreadGroups.
   */
  static WeakHashMap currentRepaintManagers;

  /**
   * A rectangle object to be reused in damaged regions calculation.
   */
  private static Rectangle rectCache = new Rectangle();

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
  private class RepaintWorker
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
      try
        {
          ThreadGroup threadGroup = Thread.currentThread().getThreadGroup();
          RepaintManager rm =
            (RepaintManager) currentRepaintManagers.get(threadGroup);
          rm.validateInvalidComponents();
          rm.paintDirtyRegions();
        }
      finally
        {
          setLive(false);
        }
    }

  }

  /** 
   * A table storing the dirty regions of components.  The keys of this
   * table are components, the values are rectangles. Each component maps
   * to exactly one rectangle.  When more regions are marked as dirty on a
   * component, they are union'ed with the existing rectangle.
   *
   * This is package private to avoid a synthetic accessor method in inner
   * class.
   *
   * @see #addDirtyRegion
   * @see #getDirtyRegion
   * @see #isCompletelyDirty
   * @see #markCompletelyClean
   * @see #markCompletelyDirty
   */
  private HashMap dirtyComponents;

  /**
   * The dirtyComponents which is used in paintDiryRegions to avoid unnecessary
   * locking.
   */
  private HashMap dirtyComponentsWork;

  /**
   * A single, shared instance of the helper class. Any methods which mark
   * components as invalid or dirty eventually activate this instance. It
   * is added to the event queue if it is not already active, otherwise
   * reused.
   *
   * @see #addDirtyRegion
   * @see #addInvalidComponent
   */
  private RepaintWorker repaintWorker;

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
  private ArrayList invalidComponents;

  /** 
   * Whether or not double buffering is enabled on this repaint
   * manager. This is merely a hint to clients; the RepaintManager will
   * always return an offscreen buffer when one is requested.
   * 
   * @see #isDoubleBufferingEnabled
   * @see #setDoubleBufferingEnabled
   */
  private boolean doubleBufferingEnabled;

  /**
   * The offscreen buffers. This map holds one offscreen buffer per
   * Window/Applet and releases them as soon as the Window/Applet gets garbage
   * collected.
   */
  private WeakHashMap offscreenBuffers;

  /**
   * The maximum width and height to allocate as a double buffer. Requests
   * beyond this size are ignored.
   *
   * @see #paintDirtyRegions
   * @see #getDoubleBufferMaximumSize
   * @see #setDoubleBufferMaximumSize
   */
  private Dimension doubleBufferMaximumSize;


  /**
   * Create a new RepaintManager object.
   */
  public RepaintManager()
  {
    dirtyComponents = new HashMap();
    dirtyComponentsWork = new HashMap();
    invalidComponents = new ArrayList();
    repaintWorker = new RepaintWorker();
    doubleBufferMaximumSize = new Dimension(2000,2000);
    doubleBufferingEnabled =
      SystemProperties.getProperty("gnu.swing.doublebuffering", "true")
                      .equals("true");
    offscreenBuffers = new WeakHashMap();
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
      currentRepaintManagers = new WeakHashMap();
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
      currentRepaintManagers = new WeakHashMap();

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
  public void addInvalidComponent(JComponent component)
  {
    Component validateRoot = null;
    Component c = component;
    while (c != null)
      {
        // Special cases we don't bother validating are when the invalidated
        // component (or any of it's ancestors) is inside a CellRendererPane
        // or if it doesn't have a peer yet (== not displayable).
        if (c instanceof CellRendererPane || ! c.isDisplayable())
          return;
        if (c instanceof JComponent && ((JComponent) c).isValidateRoot())
          {
            validateRoot = c;
            break;
          }

        c = c.getParent();
      }

    // If we didn't find a validate root, then we don't validate.
    if (validateRoot == null)
      return;

    // Make sure the validate root and all of it's ancestors are visible.
    c = validateRoot;
    while (c != null)
      {
        if (! c.isVisible() || ! c.isDisplayable())
          return;
        c = c.getParent();
      }

    if (invalidComponents.contains(validateRoot))
      return;

    //synchronized (invalidComponents)
    //  {
        invalidComponents.add(validateRoot);
    //  }

    if (! repaintWorker.isLive())
      {
        repaintWorker.setLive(true);
        invokeLater(repaintWorker);
      }
  }

  /**
   * Remove a component from the {@link #invalidComponents} vector.
   *
   * @param component The component to remove
   *
   * @see #addInvalidComponent
   */
  public void removeInvalidComponent(JComponent component)
  {
    synchronized (invalidComponents)
      {
        invalidComponents.remove(component);
      }
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
  public void addDirtyRegion(JComponent component, int x, int y,
                             int w, int h)
  {
    if (w <= 0 || h <= 0 || !component.isShowing())
      return;
    component.computeVisibleRect(rectCache);
    SwingUtilities.computeIntersection(x, y, w, h, rectCache);

    if (! rectCache.isEmpty())
      {
        synchronized (dirtyComponents)
          {
            Rectangle dirtyRect = (Rectangle)dirtyComponents.get(component);
            if (dirtyRect != null)
              {
                SwingUtilities.computeUnion(rectCache.x, rectCache.y,
                                            rectCache.width, rectCache.height,
                                            dirtyRect);
              }
            else
              {
                dirtyComponents.put(component, rectCache.getBounds());
              }
          }

        if (! repaintWorker.isLive())
          {
            repaintWorker.setLive(true);
            invokeLater(repaintWorker);
          }
      }
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
    addDirtyRegion(component, 0, 0, Integer.MAX_VALUE, Integer.MAX_VALUE);
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
    synchronized (dirtyComponents)
      {
        dirtyComponents.remove(component);
      }
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
    boolean dirty = false;
    Rectangle r = getDirtyRegion(component);
    if(r.width == Integer.MAX_VALUE && r.height == Integer.MAX_VALUE)
      dirty = true;
    return dirty;
  }

  /**
   * Validate all components which have been marked invalid in the {@link
   * #invalidComponents} vector.
   */
  public void validateInvalidComponents()
  {
    // We don't use an iterator here because that would fail when there are
    // components invalidated during the validation of others, which happens
    // quite frequently. Instead we synchronize the access a little more.
    while (invalidComponents.size() > 0)
      {
        Component comp;
        synchronized (invalidComponents)
          {
            comp = (Component) invalidComponents.remove(0);
          }
        // Validate the validate component.
        if (! (comp.isVisible() && comp.isShowing()))
          continue;
        comp.validate();
      }
  }

  /**
   * Repaint all regions of all components which have been marked dirty in the
   * {@link #dirtyComponents} table.
   */
  public void paintDirtyRegions()
  {
    // Short circuit if there is nothing to paint.
    if (dirtyComponents.size() == 0)
      return;

    // Swap dirtyRegions with dirtyRegionsWork to avoid locking.
    synchronized (dirtyComponents)
      {
        HashMap swap = dirtyComponents;
        dirtyComponents = dirtyComponentsWork;
        dirtyComponentsWork = swap;
      }

    // Compile a set of repaint roots.
    HashSet repaintRoots = new HashSet();
    Set components = dirtyComponentsWork.keySet();
    for (Iterator i = components.iterator(); i.hasNext();)
      {
        JComponent dirty = (JComponent) i.next();
        compileRepaintRoots(dirtyComponentsWork, dirty, repaintRoots);
      }

    for (Iterator i = repaintRoots.iterator(); i.hasNext();)
      {
        JComponent comp = (JComponent) i.next();
        Rectangle damaged = (Rectangle) dirtyComponentsWork.remove(comp);
        if (damaged == null || damaged.isEmpty())
          continue;
        comp.paintImmediately(damaged);
      }
    dirtyComponentsWork.clear();
  }

  /**
   * Compiles a list of components that really get repainted. This is called
   * once for each component in the dirtyRegions HashMap, each time with
   * another <code>dirty</code> parameter. This searches up the component
   * hierarchy of <code>dirty</code> to find the highest parent that is also
   * marked dirty and merges the dirty regions.
   *
   * @param dirtyRegions the dirty regions 
   * @param dirty the component for which to find the repaint root
   * @param roots the list to which new repaint roots get appended
   */
  private void compileRepaintRoots(HashMap dirtyRegions, JComponent dirty,
                                   HashSet roots)
  {
    Component current = dirty;
    Component root = dirty;

    // This will contain the dirty region in the root coordinate system,
    // possibly clipped by ancestor's bounds.
    Rectangle originalDirtyRect = (Rectangle) dirtyRegions.get(dirty); 
    rectCache.setBounds(originalDirtyRect);

    // The bounds of the current component.
    int x = dirty.getX();
    int y = dirty.getY();
    int w = dirty.getWidth();
    int h = dirty.getHeight();

    // Do nothing if dirty region is clipped away by the component's bounds.
    rectCache = SwingUtilities.computeIntersection(0, 0, w, h, rectCache);
    if (rectCache.isEmpty())
      return;

    // The cumulated offsets. 
    int dx = 0;
    int dy = 0;
    // The actual offset for the found root.
    int rootDx = 0;
    int rootDy = 0;

    // Search the highest component that is also marked dirty.
    Component parent;
    while (true)
      {
        parent = current.getParent();
        if (parent == null || !(parent instanceof JComponent))
          break;

        current = parent;
        // Update the offset.
        dx += x;
        dy += y;
        rectCache.x += x;
        rectCache.y += y;
        
        x = current.getX();
        y = current.getY();
        w = current.getWidth();
        h = current.getHeight();
        rectCache = SwingUtilities.computeIntersection(0, 0, w, h, rectCache);

        // Don't paint if the dirty regions is clipped away by any of
        // its ancestors.
        if (rectCache.isEmpty())
          return;

        // We can skip to the next up when this parent is not dirty.
        if (dirtyRegions.containsKey(parent))
          {
            root = current;
            rootDx = dx;
            rootDy = dy;
          }
      }

    // Merge the rectangles of the root and the requested component if
    // the are different.
    if (root != dirty)
      {
        rectCache.x += rootDx - dx;
        rectCache.y += rootDy - dy;
        Rectangle dirtyRect = (Rectangle) dirtyRegions.get(root);
        SwingUtilities.computeUnion(rectCache.x, rectCache.y, rectCache.width,
                                    rectCache.height, dirtyRect);
      }

    // Adds the root to the roots set.
    if (! roots.contains(root))
      roots.add(root);
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
   */
  public Image getOffscreenBuffer(Component component, int proposedWidth,
                                  int proposedHeight)
  {
    Component root = SwingUtilities.getWindowAncestor(component);
    Image buffer = (Image) offscreenBuffers.get(root);
    if (buffer == null 
        || buffer.getWidth(null) < proposedWidth 
        || buffer.getHeight(null) < proposedHeight)
      {
        int width = Math.max(proposedWidth, root.getWidth());
        width = Math.min(doubleBufferMaximumSize.width, width);
        int height = Math.max(proposedHeight, root.getHeight());
        height = Math.min(doubleBufferMaximumSize.height, height);
        buffer = component.createImage(width, height);
        offscreenBuffers.put(root, buffer);
      }
    return buffer;
  }

  /**
   * Blits the back buffer of the specified root component to the screen.
   * This is package private because it must get called by JComponent.
   *
   * @param comp the component to be painted
   * @param x the area to paint on screen, in comp coordinates
   * @param y the area to paint on screen, in comp coordinates
   * @param w the area to paint on screen, in comp coordinates
   * @param h the area to paint on screen, in comp coordinates
   */
  void commitBuffer(Component comp, int x, int y, int w, int h)
  {
    Component root = comp;
    while (root != null
	   && ! (root instanceof Window || root instanceof Applet))
      {
	x += root.getX();
	y += root.getY();
	root = root.getParent();
      }

    if (root != null)
      {
        Graphics g = root.getGraphics();
        Image buffer = (Image) offscreenBuffers.get(root);
        if (buffer != null)
          {
            // Make sure we have a sane clip at this point.
            g.clipRect(x, y, w, h);
            g.drawImage(buffer, 0, 0, root);
            g.dispose();
          }
      }
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
    Component root = SwingUtilities.getWindowAncestor(comp);
    Image buffer = (Image) offscreenBuffers.get(root);
    if (buffer == null 
        || buffer.getWidth(null) < proposedWidth 
        || buffer.getHeight(null) < proposedHeight
        || !(buffer instanceof VolatileImage))
      {
        int width = Math.max(proposedWidth, root.getWidth());
        width = Math.min(doubleBufferMaximumSize.width, width);
        int height = Math.max(proposedHeight, root.getHeight());
        height = Math.min(doubleBufferMaximumSize.height, height);
        buffer = root.createVolatileImage(width, height);
        if (buffer != null)
          offscreenBuffers.put(root, buffer);
      }
    return buffer;
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

  /**
   * Sends an RepaintManagerEvent to the event queue with the specified
   * runnable. This is similar to SwingUtilities.invokeLater(), only that the
   * event is a low priority event in order to defer the execution a little
   * more.
   */
  private void invokeLater(Runnable runnable)
  {
    Toolkit tk = Toolkit.getDefaultToolkit();
    EventQueue evQueue = tk.getSystemEventQueue();
    InvocationEvent ev = new RepaintWorkerEvent(evQueue, runnable, null, false);
    evQueue.postEvent(ev);
  }
}
