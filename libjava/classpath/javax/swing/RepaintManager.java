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
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.image.VolatileImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
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
 * <p>In general, painting is very confusing in swing. see <a
 * href="http://java.sun.com/products/jfc/tsc/articles/painting/index.html">this
 * document</a> for more details.</p>
 *
 * @author Roman Kennke (kennke@aicas.com)
 * @author Graydon Hoare (graydon@redhat.com)
 */
public class RepaintManager
{
  /**
   * The current repaint managers, indexed by their ThreadGroups.
   */
  private static WeakHashMap currentRepaintManagers;

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
     * @return a negative integer, if <code>o1</code> is bigger in than
     *         <code>o2</code>, zero, if both are at the same size and a
     *         positive integer, if <code>o1</code> is smaller than
     *         <code>o2</code> 
     */
    public int compare(Object o1, Object o2)
    {
      if (o1 instanceof JComponent && o2 instanceof JComponent)
        {
          JComponent c1 = (JComponent) o1;
          Rectangle d1 = (Rectangle) dirtyComponents.get(c1);
          JComponent c2 = (JComponent) o2;
          Rectangle d2 = (Rectangle) dirtyComponents.get(c2);
          return d2.width * d2.height - d1.width * d1.height;
        }
      throw new ClassCastException("This comparator can only be used with "
                                   + "JComponents");
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
  HashMap dirtyComponents;

  /**
   * The comparator used for ordered inserting into the repaintOrder list. 
   */
  private transient Comparator comparator;

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
   * Indicates if the RepaintManager is currently repainting an area.
   */
  private boolean repaintUnderway;

  /**
   * This holds buffer commit requests when the RepaintManager is working.
   * This maps Component objects (the top level components) to Rectangle
   * objects (the area of the corresponding buffer that must be blitted on
   * the component).
   */
  private HashMap commitRequests;

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
    invalidComponents = new ArrayList();
    repaintWorker = new RepaintWorker();
    doubleBufferMaximumSize = new Dimension(2000,2000);
    doubleBufferingEnabled = true;
    offscreenBuffers = new WeakHashMap();
    repaintUnderway = false;
    commitRequests = new HashMap();
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
    Component ancestor = component;

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

    synchronized (invalidComponents)
      {
        invalidComponents.add(component);
      }

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
        if (dirtyComponents.containsKey(component))
          {
            SwingUtilities.computeUnion(rectCache.x, rectCache.y,
                                        rectCache.width, rectCache.height,
                                   (Rectangle) dirtyComponents.get(component));
          }
        else
          {
            synchronized (dirtyComponents)
              {
                dirtyComponents.put(component, rectCache.getBounds());
              }
          }

        if (! repaintWorker.isLive())
          {
            repaintWorker.setLive(true);
            SwingUtilities.invokeLater(repaintWorker);
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
    synchronized (dirtyComponents)
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
   * Repaint all regions of all components which have been marked dirty in
   * the {@link #dirtyComponents} table.
   */
  public void paintDirtyRegions()
  {
    // Short cicuit if there is nothing to paint.
    if (dirtyComponents.size() == 0)
      return;

    synchronized (dirtyComponents)
      {
        // We sort the components by their size here. This way we have a good
        // chance that painting the bigger components also paints the smaller
        // components and we don't need to paint them twice.
        ArrayList repaintOrder = new ArrayList(dirtyComponents.size());
        repaintOrder.addAll(dirtyComponents.keySet());
        if (comparator == null)
          comparator = new ComponentComparator();
        Collections.sort(repaintOrder, comparator);
        repaintUnderway = true;
        for (Iterator i = repaintOrder.iterator(); i.hasNext();)
          {
            JComponent comp = (JComponent) i.next();
            // If a component is marked completely clean in the meantime, then skip
            // it.
            Rectangle damaged = (Rectangle) dirtyComponents.get(comp);
            if (damaged == null || damaged.isEmpty())
              continue;
            comp.paintImmediately(damaged);
            dirtyComponents.remove(comp);
          }
        repaintUnderway = false;
        commitRemainingBuffers();
      }
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
    Component root = SwingUtilities.getRoot(component);
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
   * Blits the back buffer of the specified root component to the screen. If
   * the RepaintManager is currently working on a paint request, the commit
   * requests are queued up and committed at once when the paint request is
   * done (by {@link #commitRemainingBuffers}). This is package private because
   * it must get called by JComponent.
   *
   * @param root the component, either a Window or an Applet instance
   * @param area the area to paint on screen
   */
  void commitBuffer(Component root, Rectangle area)
  {
    // We synchronize on dirtyComponents here because that is what
    // paintDirtyRegions also synchronizes on while painting.
    synchronized (dirtyComponents)
      {
        // If the RepaintManager is not currently painting, then directly
        // blit the requested buffer on the screen.
        if (! repaintUnderway)
          {
            Graphics g = root.getGraphics();
            Image buffer = (Image) offscreenBuffers.get(root);
            Rectangle clip = g.getClipBounds();
            if (clip != null)
              area = SwingUtilities.computeIntersection(clip.x, clip.y,
                                                        clip.width, clip.height,
                                                        area);
            int dx1 = area.x;
            int dy1 = area.y;
            int dx2 = area.x + area.width;
            int dy2 = area.y + area.height;
            // Make sure we have a sane clip at this point.
            g.clipRect(area.x, area.y, area.width, area.height);

            // Make sure the coordinates are inside the buffer, everything else
            // might lead to problems.
            // TODO: This code should not really be necessary, however, in fact
            // we have two issues here:
            // 1. We shouldn't get repaint requests in areas outside the buffer
            //    region in the first place. This still happens for example
            //    when a component is inside a JViewport, and the component has
            //    a size that would reach beyond the window size.
            // 2. Graphics.drawImage() should not behave strange when trying
            //    to draw regions outside the image.
            int bufferWidth = buffer.getWidth(root);
            int bufferHeight = buffer.getHeight(root);
            dx1 = Math.min(bufferWidth, dx1);
            dy1 = Math.min(bufferHeight, dy1);
            dx2 = Math.min(bufferWidth, dx2);
            dy2 = Math.min(bufferHeight, dy2);
            g.drawImage(buffer, dx1, dy1, dx2, dy2,
                                dx1, dy1, dx2, dy2, root);
            g.dispose();
          }
        // Otherwise queue this request up, until all the RepaintManager work
        // is done.
        else
          {
            if (commitRequests.containsKey(root))
              SwingUtilities.computeUnion(area.x, area.y, area.width,
                                          area.height,
                                         (Rectangle) commitRequests.get(root));
            else
              commitRequests.put(root, area);
          }
      }
  }

  /**
   * Commits the queued up back buffers to screen all at once.
   */
  private void commitRemainingBuffers()
  {
    // We synchronize on dirtyComponents here because that is what
    // paintDirtyRegions also synchronizes on while painting.
    synchronized (dirtyComponents)
      {
        Set entrySet = commitRequests.entrySet();
        Iterator i = entrySet.iterator();
        while (i.hasNext())
          {
            Map.Entry entry = (Map.Entry) i.next();
            Component root = (Component) entry.getKey();
            Rectangle area = (Rectangle) entry.getValue();
            commitBuffer(root, area);
            i.remove();
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
