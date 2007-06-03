/* AxisHints.java -- Hints specific to an axis
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


package gnu.java.awt.font.autofit;

class AxisHints
{

  Segment[] segments;
  int majorDir;
  int numSegments;
  int numEdges;
  Edge[] edges;

  AxisHints()
  {
    segments = new Segment[4];
    edges = new Edge[4];
  }

  Segment newSegment()
  {
    if (numSegments >= segments.length)
      {
        // Grow array.
        int newMax = segments.length;
        newMax += (newMax >> 2) + 4; // From FreeType.
        Segment[] newSegs = new Segment[newMax];
        System.arraycopy(segments, 0, newSegs, 0, numSegments);
        segments = newSegs;
      }
    Segment seg = new Segment();
    segments[numSegments] = seg;
    numSegments++;
    return seg;
  }

  public Edge newEdge(int pos)
  {
    if (numEdges >= edges.length)
      {
        // Grow array.
        int newMax = edges.length;
        newMax += (newMax >> 2) + 4; // From FreeType.
        Edge[] newEdges = new Edge[newMax];
        System.arraycopy(edges, 0, newEdges, 0, numEdges);
        edges = newEdges;
      }
    int edgeIndex = numEdges;
    Edge edge = edges[edgeIndex] = new Edge();
    while (edgeIndex > 0 && edges[edgeIndex - 1].fpos > pos)
      {
        edges[edgeIndex] = edges[edgeIndex - 1];
        edgeIndex--;
      }
    edges[edgeIndex] = edge;
    numEdges++;
    edge.fpos = pos;

    return edge;

  }

  int getEdgeIndex(Edge edge2)
  {
    int idx = -1;
    for (int i = 0; i < numEdges; i++)
      {
        if (edges[i] == edge2)
          {
            idx = i;
            break;
          }
      }
    return idx;
  }
}
