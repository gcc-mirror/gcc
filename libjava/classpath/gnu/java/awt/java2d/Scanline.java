/* Scanline.java -- A scanline for the scanline converter
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

/**
 * Represents a scanline in the {@link ScanlineConverter}. This is basically
 * a sorted list of {@link PolyEdge}s that is made for maximum reuse.
 */
class Scanline
{

  /**
   * The actual edges array. The fields can be null.
   */
  private PolyEdge edges;

  /**
   * Clears this scanline. This only resets the number of edges to 0. The
   * actual PolyEdge objects are preserved for possible later reuse.
   */
  void clear()
  {
    edges = null;
  }

  /**
   * Create a new Scanline.
   */
  Scanline()
  {
    // Nothing to do.
  }

  /**
   * Inserts an edge into this scanline. This is performed in a sorted fashion,
   * and so that it reuses as much existing resources as possible.
   */
  void addEdge(PolyEdge edge)
  {

    // Allocate PolyEdge when necessary or reuse an old one.
    edge.scanlineNext = edges;
    edges = edge;
  }

  /**
   * Returns the edges queue.
   *
   * @return the edges queue
   */
  PolyEdge getEdges()
  {
    return edges;
  }
}
