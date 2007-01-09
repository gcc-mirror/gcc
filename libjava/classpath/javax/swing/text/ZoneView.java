/* ZoneView.java -- An effective BoxView subclass
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


package javax.swing.text;

import java.awt.Shape;
import java.util.ArrayList;
import java.util.LinkedList;

import javax.swing.event.DocumentEvent;

/**
 * A View implementation that delays loading of sub views until they are
 * needed for display or internal transformations. This can be used for
 * editors that need to handle large documents more effectivly than the
 * standard {@link BoxView}.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.3
 */
public class ZoneView
  extends BoxView
{

  /**
   * The default zone view implementation. The specs suggest that this is
   * a subclass of AsyncBoxView, so do we.
   */
  static class Zone
    extends AsyncBoxView
  {
    /**
     * The start position for this zone.
     */
    private Position p0;

    /**
     * The end position for this zone.
     */
    private Position p1;

    /**
     * Creates a new Zone for the specified element, start and end positions.
     *
     * @param el the element
     * @param pos0 the start position
     * @param pos1 the end position
     * @param axis the major axis
     */
    Zone(Element el, Position pos0, Position pos1, int axis)
    {
      super(el, axis);
      p0 = pos0;
      p1 = pos1;
    }

    /**
     * Returns the start offset of the zone.
     *
     * @return the start offset of the zone
     */
    public int getStartOffset()
    {
      return p0.getOffset();
    }

    /**
     * Returns the end offset of the zone.
     *
     * @return the end offset of the zone
     */
    public int getEndOffset()
    {
      return p1.getOffset();
    }
  }

  /**
   * The maximumZoneSize.
   */
  private int maximumZoneSize;

  /**
   * The maximum number of loaded zones.
   */
  private int maxZonesLoaded;

  /**
   * A queue of loaded zones. When the number of loaded zones exceeds the
   * maximum number of zones, the oldest zone(s) get unloaded.
   */
  private LinkedList loadedZones;

  /**
   * Creates a new ZoneView for the specified element and axis.
   *
   * @param element the element for which to create a ZoneView
   * @param axis the major layout axis for the box
   */
  public ZoneView(Element element, int axis)
  {
    super(element, axis);
    maximumZoneSize = 8192;
    maxZonesLoaded = 3;
    loadedZones = new LinkedList();
  }

  /**
   * Sets the maximum zone size. Note that zones might still become larger
   * then the size specified when a singe child view is larger for itself,
   * because zones are formed on child view boundaries.
   *
   * @param size the maximum zone size to set
   *
   * @see #getMaximumZoneSize()
   */
  public void setMaximumZoneSize(int size)
  {
    maximumZoneSize = size;
  }

  /**
   * Returns the maximum zone size. Note that zones might still become larger
   * then the size specified when a singe child view is larger for itself,
   * because zones are formed on child view boundaries.
   *
   * @return the maximum zone size
   *
   * @see #setMaximumZoneSize(int)
   */
  public int getMaximumZoneSize()
  {
    return maximumZoneSize;
  }

  /**
   * Sets the maximum number of zones that are allowed to be loaded at the
   * same time. If the new number of allowed zones is smaller then the
   * previous settings, this unloads all zones the aren't allowed to be
   * loaded anymore.
   *
   * @param num the number of zones allowed to be loaded at the same time
   *
   * @throws IllegalArgumentException if <code>num &lt;= 0</code>
   *
   * @see #getMaxZonesLoaded()
   */
  public void setMaxZonesLoaded(int num)
  {
    if (num < 1)
      throw new IllegalArgumentException("Illegal number of zones");
    maxZonesLoaded = num;
    unloadOldestZones();
  }

  /**
   * Returns the number of zones that are allowed to be loaded.
   *
   * @return the number of zones that are allowed to be loaded
   *
   * @see #setMaxZonesLoaded(int)
   */
  public int getMaxZonesLoaded()
  {
    return maxZonesLoaded;
  }

  /**
   * Gets called after a zone has been loaded. This unloads the oldest zone(s)
   * when the maximum number of zones is reached.
   *
   * @param zone the zone that has been loaded
   */
  protected void zoneWasLoaded(View zone)
  {
    loadedZones.addLast(zone);
    unloadOldestZones();
  }

  /**
   * This unloads the specified zone. This is implemented to simply remove
   * all child views from that zone.
   *
   * @param zone the zone to be unloaded
   */
  protected void unloadZone(View zone)
  {
    zone.removeAll();
  }

  /**
   * Returns <code>true</code> when the specified zone is loaded,
   * <code>false</code> otherwise. The default implementation checks if
   * the zone view has child elements.
   *
   * @param zone the zone view to check
   *
   * @return <code>true</code> when the specified zone is loaded,
   *         <code>false</code> otherwise
   */
  protected boolean isZoneLoaded(View zone)
  {
    return zone.getViewCount() > 0;
  }

  /**
   * Creates a zone for the specified range. Subclasses can override this
   * to provide a custom implementation for the zones.
   *
   * @param p0 the start of the range
   * @param p1 the end of the range
   *
   * @return the zone
   */
  protected View createZone(int p0, int p1)
  {
    Document doc = getDocument();
    Position pos0 = null;
    Position pos1 = null;
    try
      {
        pos0 = doc.createPosition(p0);
        pos1 = doc.createPosition(p1);
      }
    catch (BadLocationException ex)
      {
        assert false : "Must not happen";
      }
    Zone zone = new Zone(getElement(), pos0, pos1, getAxis());
    return zone;
  }

  // --------------------------------------------------------------------------
  // CompositeView methods.
  // --------------------------------------------------------------------------

  /**
   * Overridden to not load all the child views. This methods creates
   * initial zones without actually loading them.
   *
   * @param vf not used
   */
  protected void loadChildren(ViewFactory vf)
  {
    int p0 = getStartOffset();
    int p1 = getEndOffset();
    append(createZone(p0, p1));
    checkZoneAt(p0);
  }

  /**
   * Returns the index of the child view at the document position
   * <code>pos</code>.
   *
   * This overrides the CompositeView implementation because the ZoneView does
   * not provide a one to one mapping from Elements to Views.
   *
   * @param pos the document position
   *
   * @return the index of the child view at the document position
   *         <code>pos</code>
   */
  protected int getViewIndexAtPosition(int pos)
  {
    int index = -1;
    boolean found = false;
    if (pos >= getStartOffset() && pos <= getEndOffset())
      {
        int upper = getViewCount() - 1;
        int lower = 0;
        index = (upper - lower) / 2 + lower;
        int bias = 0;
        do
          {
            View child = getView(index);
            int childStart = child.getStartOffset();
            int childEnd = child.getEndOffset();
            if (pos >= childStart && pos < childEnd)
              found = true;
            else if (pos < childStart)
              {
                upper = index;
                bias = -1;
              }
            else if (pos >= childEnd)
              {
                lower = index;
                bias = 1;
              }
            if (! found)
              {
                int newIndex = (upper - lower) / 2 + lower;
                if (newIndex == index)
                  index = newIndex + bias;
                else
                  index = newIndex;
              }
          } while (upper != lower && ! found);
      }
    // If no child view actually covers the specified offset, reset index to
    // -1.
    if (! found)
      index = -1;
    return index;
  }

  // --------------------------------------------------------------------------
  // View methods.
  // --------------------------------------------------------------------------

  public void insertUpdate(DocumentEvent e, Shape a, ViewFactory vf)
  {
    // TODO: Implement this.
  }

  public void removeUpdate(DocumentEvent e, Shape a, ViewFactory vf)
  {
    // TODO: Implement this.
  }

  protected boolean updateChildren(DocumentEvent.ElementChange ec,
                                   DocumentEvent e, ViewFactory vf)
  {
    // TODO: Implement this.
    return false;
  }

  // --------------------------------------------------------------------------
  // Internal helper methods.
  // --------------------------------------------------------------------------

  /**
   * A helper method to unload the oldest zones when there are more loaded
   * zones then allowed.
   */
  private void unloadOldestZones()
  {
    int maxZones = getMaxZonesLoaded();
    while (loadedZones.size() > maxZones)
      {
        View zone = (View) loadedZones.removeFirst();
        unloadZone(zone);
      }
  }

  /**
   * Checks if the zone view at position <code>pos</code> should be split
   * (its size is greater than maximumZoneSize) and tries to split it.
   *
   * @param pos the document position to check
   */
  private void checkZoneAt(int pos)
  {
    int viewIndex = getViewIndexAtPosition(pos); //, Position.Bias.Forward);
    View view = getView(viewIndex);
    int p0 = view.getStartOffset();
    int p1 = view.getEndOffset();
    if (p1 - p0 > maximumZoneSize)
      splitZone(viewIndex, p0, p1);
  }

  /**
   * Tries to break the view at the specified index and inside the specified
   * range into pieces that are acceptable with respect to the maximum zone
   * size.
   *
   * @param index the index of the view to split
   * @param p0 the start offset
   * @param p1 the end offset
   */
  private void splitZone(int index, int p0, int p1)
  {
    ArrayList newZones = new ArrayList();
    int p = p0;
    do
      {
        p0 = p;
        p = Math.min(getPreferredZoneEnd(p0), p1);
        newZones.add(createZone(p0, p));
      } while (p < p1);
    View[] newViews = new View[newZones.size()];
    newViews = (View[]) newZones.toArray(newViews);
    replace(index, 1, newViews);
  }

  /**
   * Calculates the positions at which a zone split is performed. This
   * tries to create zones sized close to half the maximum zone size.
   *
   * @param start the start offset
   *
   * @return the preferred end offset
   */
  private int getPreferredZoneEnd(int start)
  {
    Element el = getElement();
    int index = el.getElementIndex(start + (maximumZoneSize / 2));
    Element child = el.getElement(index);
    int p0 = child.getStartOffset();
    int p1 = child.getEndOffset();
    int end = p1;
    if (p0 - start > maximumZoneSize && p0 > start)
      end = p0;
    return end;
  }
}
