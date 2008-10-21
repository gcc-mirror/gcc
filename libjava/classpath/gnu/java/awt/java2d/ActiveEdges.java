/* ActiveEdges.java -- A collection of active edges for scanline conversion
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


package gnu.java.awt.java2d;

import gnu.java.lang.CPStringBuilder;

/**
 * A collection of active edges for scanline conversion.
 */
final class ActiveEdges
{

  /**
   * The active edges. This can contain null values at arbirary locations.
   * The method #sort() packs this together.
   */
  private PolyEdge[] activeEdges;

  /**
   * The actual number of active edges. The array can be bigger than this
   * number.
   */
  private int numActiveEdges;

  /**
   * Creates a new ActiveEdges object.
   */
  ActiveEdges()
  {
    activeEdges = new PolyEdge[8];
    numActiveEdges = 0;
  }

  /**
   * Clears out all active edges. This is cheap as it simply resets the
   * counter to 0. It does not release all references to PolyEdge instances.
   */
  void clear()
  {
    numActiveEdges = 0;
  }

  /**
   * Adds the specified edge to the list of active edges. This does not yet
   * sort the edges and therefore does destroy any order of the list.
   *
   * @param edge the edge to add
   */
  void add(PolyEdge edge)
  {
    // Grow array when necessary.
    int oldSize = activeEdges.length;
    if (numActiveEdges >= oldSize)
      {
        int newSize = oldSize + oldSize / 4 + 1;
        PolyEdge[] newEdges = new PolyEdge[newSize];
        System.arraycopy(activeEdges, 0, newEdges, 0, oldSize);
        activeEdges = newEdges;
      }
    activeEdges[numActiveEdges] = edge;
    numActiveEdges++;
  }

  /**
   * Intersects all active edges, sorts them according to their intersection
   * points and packs the array to remove unneeded edges. This does also
   * remove any edges that do not intersect the scanline (i.e. they end above
   * of the scanline).
   *
   * @param y the scanline height
   */
  void intersectSortAndPack(int n, int y)
  {
    // Intersect and pack in one go.
    int last = 0;
    PolyEdge tmp;
    for (int i = 0; i < numActiveEdges; i++)
      {
        PolyEdge edge = activeEdges[i];
        // Clear out edge that ends above the scanline.
        if (edge != null && edge.y1 >= y)
          {
            assert edge.y1 >= y && edge.y0 <= y : "edge must cross scanline";
            edge.intersect(n, y);
            activeEdges[last] = edge;
            last++;

            // Bubble up the added edge.
            for (int j = last - 1; j > 0; j--)
              {
                if (activeEdges[j].xIntersection
                    < activeEdges[j - 1].xIntersection)
                  {
                    tmp = activeEdges[j];
                    activeEdges[j] = activeEdges[j - 1];
                    activeEdges[j - 1] = tmp;
                  }
                else
                  {
                    // The beginning of the list is already sorted.
                    break;
                  }
              }
          }
      }
    numActiveEdges = last;

  }

  /**
   * Returns the number of active edges. This is only reliable after a
   * call to {@link #intersectSortAndPack(int, int)}.
   *
   * @return the number of active edges
   */
  int getNumActiveEdges()
  {
    return numActiveEdges;
  }

  /**
   * Returns the active edge at the position <code>i</code>.
   *
   * @param i the index
   *
   * @return the active edge at the specified index
   */
  PolyEdge getActiveEdge(int i)
  {
    return activeEdges[i];
  }

  /**
   * Removes all edges that end above the specified height.
   *
   * @param y the cut-off height
   */
  void remove(int y)
  {
    for (int i = 0; i < numActiveEdges; i++)
      {
        PolyEdge edge = activeEdges[i];
        if (edge != null && edge.y1 < y)
          {
            activeEdges[i] = null;
          }
      }
  }

  public String toString()
  {
    CPStringBuilder s = new CPStringBuilder();
    s.append("[ActiveEdges] ");
    for (int i = 0; i < numActiveEdges; i++)
      {
        s.append(activeEdges[i]);
        s.append(',');
      }
    return s.toString();
  }
}
