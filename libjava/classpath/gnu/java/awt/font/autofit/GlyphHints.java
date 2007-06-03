/* GlyphHints.java -- Data and methods for actual hinting
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

import gnu.java.awt.font.FontDelegate;
import gnu.java.awt.font.opentype.truetype.Fixed;
import gnu.java.awt.font.opentype.truetype.Point;
import gnu.java.awt.font.opentype.truetype.Zone;

/**
 * The data and methods used for the actual hinting process.
 */
class GlyphHints
  implements Constants
{

  int xScale;
  int xDelta;
  int yScale;
  int yDelta;

  AxisHints[] axis;

  Point[] points;
  int numPoints;
  int maxPoints;

  Point[] contours;
  int numContours;
  int maxContours;

  ScriptMetrics metrics;

  int flags;

  GlyphHints()
  {
    axis = new AxisHints[Constants.DIMENSION_MAX];
    axis[Constants.DIMENSION_VERT] = new AxisHints();
    axis[Constants.DIMENSION_HORZ] = new AxisHints();

    xScale = Fixed.ONE;
    yScale = Fixed.ONE;
  }

  void rescale(ScriptMetrics m)
  {
    metrics = m;
    // TODO: Copy scalerFlags.
  }
  
  void reload(Zone outline)
  {
    numPoints = 0;
    numContours = 0;
    axis[0].numSegments = 0;
    axis[0].numEdges = 0;
    axis[1].numSegments = 0;
    axis[1].numEdges = 0;

    // Create/reallocate the contours array.
    int newMax = outline.getNumContours();
    if (newMax > maxContours || contours == null)
      {
        newMax = (newMax + 3) & ~3; // Taken from afhints.c .
        Point[] newContours = new Point[newMax];
        if (contours != null)
          {
            System.arraycopy(contours, 0, newContours, 0, maxContours);
          }
        contours = newContours;
        maxContours = newMax;
      }

    // Create/reallocate the points array.
    newMax = outline.getSize() + 2;
    if (newMax > maxPoints || points == null)
      {
        newMax = (newMax + 2 + 7) & ~7; // Taken from afhints.c .
        Point[] newPoints = new Point[newMax];
        if (points != null)
          {
            System.arraycopy(points, 0, newPoints, 0, maxPoints);
          }
        points = newPoints;
        maxPoints = newMax;
      }

    numPoints = outline.getSize() - 4; // 4 phantom points.
    numContours = outline.getNumContours();

    // Set major direction. We don't handle Type 1 fonts yet.
    axis[DIMENSION_HORZ].majorDir = DIR_UP;
    axis[DIMENSION_VERT].majorDir = DIR_LEFT;

    // TODO: Freetype seems to scale and translate the glyph at that point.
    // I suppose that this is not really needed.
    // The scales are scaling from font units to 1/64 device pixels.
    xScale = Fixed.valueOf16(outline.scaleX * 64);
    yScale = Fixed.valueOf16(outline.scaleY * 64);

    // FIXME: What is that xDelta and yDelta used for?
    System.arraycopy(outline.getPoints(), 0, points, 0, numPoints);

    // Setup prev and next and contours array.
    // TODO: Probably cache this.
    contours = new Point[numContours];
    Point currentContour = points[0];
    for (int i = 0, cIndex = 0; i < numPoints; i++)
      {
        // Start new contour when the last point has been a contour end.
        if (outline.isContourEnd(i))
          {
            // Connect the contour end point to the start point.
            points[i].setNext(currentContour);
            currentContour.setPrev(points[i]);
            contours[cIndex] = currentContour;
            cIndex++;
            currentContour = i < numPoints - 1 ? points[i + 1] : null;
          }
        else
          {
            // Connect the current and the previous point.
            points[i].setNext(points[i + 1]);
            points[i + 1].setPrev(points[i]);
          }
      }
    // Compute directions of in and out vectors of all points as well
    // as the weak point flag.
    for (int i = 0; i < numPoints; i++)
      {
        // Compute in and out dir.
        Point p = points[i];
        Point prev = p.getPrev();
        int inX = p.getOrigX() - prev.getOrigX();
        int inY = p.getOrigY() - prev.getOrigY();
        p.setInDir(Utils.computeDirection(inX, inY));
        Point next = p.getNext();
        int outX = next.getOrigX() - p.getOrigX();
        int outY = next.getOrigY() - p.getOrigY();
        p.setOutDir(Utils.computeDirection(outX, outY));

        if (p.isControlPoint())
          {
            setWeakPoint(p);
          }
        else if (p.getOutDir() == p.getInDir())
          {
            if (p.getOutDir() != DIR_NONE)
              setWeakPoint(p);
            else
              {
                int angleIn = Utils.atan(inY, inX);
                int angleOut = Utils.atan(outY, outX);
                int delta = Utils.angleDiff(angleIn, angleOut);
                if (delta < 2 && delta > -2)
                  setWeakPoint(p);
              }
          }
        else if (p.getInDir() == - p.getOutDir())
          {
            setWeakPoint(p);
          }
      }
    computeInflectionPoints();
  }

  private void setWeakPoint(Point p)
  {
    p.setFlags((byte) (p.getFlags() | Point.FLAG_WEAK_INTERPOLATION));
  }

  /**
   * Computes the inflection points for a glyph.
   */
  private void computeInflectionPoints()
  {
    // Do each contour separately.
    contours : for (int c = 0; c < contours.length; c++)
      {
        Point point = contours[c];
        Point first = point;
        Point start = point;
        Point end = point;
        do
          {
            end = end.getNext();
            if (end == first)
              continue contours;
          } while (end.getOrigX() == first.getOrigX()
                   && end.getOrigY() == first.getOrigY());

        // Extend segment start whenever possible.
        Point before = start;
        int angleIn;
        int angleSeg = Utils.atan(end.getOrigX() - start.getOrigX(),
                                  end.getOrigY() - start.getOrigY());
        do
          {
            do
              {
                start = before;
                before = before.getPrev();
                if (before == first)
                  continue contours;
              } while (before.getOrigX() == start.getOrigX()
                       && before.getOrigY() == start.getOrigY());
            angleIn = Utils.atan(start.getOrigX() - before.getOrigX(),
                                 start.getOrigY() - before.getOrigY());
          } while (angleIn == angleSeg);

        first = start;
        int diffIn = Utils.angleDiff(angleIn, angleSeg);
        // Now, process all segments in the contour.
        Point after;
        boolean finished = false;
        int angleOut, diffOut;
        do
          {
            // First, extend the current segment's end whenever possible.
            after = end;
            do
              {
                do
                  {
                    end = after;
                    after = after.getNext();
                    if (after == first)
                      finished = true;
                  } while (end.getOrigX() == after.getOrigX()
                           && end.getOrigY() == after.getOrigY());
                angleOut = Utils.atan(after.getOrigX() - end.getOrigX(),
                                      after.getOrigY() - end.getOrigY());
              } while (angleOut == angleSeg);
            diffOut = Utils.angleDiff(angleSeg, angleOut);
            if ((diffIn ^ diffOut) < 0)
              {
                // diffIn and diffOut have different signs, we have
                // inflection points here.
                do
                  {
                    start.addFlags(Point.FLAG_INFLECTION);
                    start = start.getNext();
                  } while (start != end);
                start.addFlags(Point.FLAG_INFLECTION);
              }
            start = end;
            end = after;
            angleSeg = angleOut;
            diffIn = diffOut;
          } while (! finished);
      }
  }

  boolean doHorizontal()
  {
    return (flags & FontDelegate.FLAG_NO_HINT_HORIZONTAL) == 0;
  }

  boolean doVertical()
  {
    return (flags & FontDelegate.FLAG_NO_HINT_VERTICAL) == 0;
  }

  void alignWeakPoints(int dim)
  {
    short touchFlag;
    Point point;
    // PASS 1 : Move segments to edge positions.
    if (dim == DIMENSION_HORZ)
      {
        touchFlag = Point.FLAG_DONE_X;
        for (int p = 0; p < numPoints; p++)
          {
            point = points[p];
            point.setU(point.getX());
            point.setV(point.getScaledX());
          }
      }
    else
      {
        touchFlag = Point.FLAG_DONE_Y;
        for (int p = 0; p < numPoints; p++)
          {
            point = points[p];
            point.setU(point.getY());
            point.setV(point.getScaledY());
          }
      }
    point = points[0];
    for (int c = 0; c < numContours; c++)
      {
        point = contours[c];
        int idx = getPointIndex(point);
        Point endPoint = point.getPrev();
        int endIdx = getPointIndex(endPoint);
        int firstIdx = idx;
        while (idx <=  endIdx
            && (point.getFlags() & touchFlag) == 0)
          {
            idx++;
            point = points[idx];
          }
        if (idx <= endIdx)
          {
            int firstTouched = idx;
            int curTouched = idx;
            idx++;
            point = points[idx];
            while (idx <= endIdx)
              {
                if ((point.getFlags() & touchFlag) != 0)
                  {
                    // We found two successive touch points. We interpolate
                    // all contour points between them.
                    iupInterp(curTouched + 1, idx - 1, curTouched, idx);
                    curTouched = idx;
                  }
                idx++;
                point = points[idx];
              }
            if (curTouched == firstTouched)
              {
                // This is a special case: Only one point was touched in the
                // contour. We thus simply shift the whole contour.
                iupShift(firstIdx, endIdx, curTouched);
              }
            else
              {
                // Now interpolate after the last touched point to the end
                // of the contour.
                iupInterp(curTouched + 1, endIdx, curTouched, firstTouched);
                // If the first contour point isn't touched, interpolate
                // from the contour start to the first touched point.
                if (firstTouched > 0)
                  {
                    iupInterp(firstIdx, firstTouched - 1, curTouched,
                              firstTouched);
                  }
              }
          }
      }
    // Now store the values back.
    if (dim == DIMENSION_HORZ)
      {
        for (int p = 0; p < numPoints; p++)
          {
            point = points[p];
            point.setX(point.getU());
          }
      }
    else
      {
        for (int p = 0; p < numPoints; p++)
          {
            point = points[p];
            point.setY(point.getU());
          }
      }
  }

  private void iupShift(int p1, int p2, int ref)
  {
    int delta = points[ref].getU() - points[ref].getV();
    for (int p = p1; p < ref; p++)
      {
        points[p].setU(points[p].getV() + delta);
      }
    for (int p = ref + 1; p <= p2; p++)
      {
        points[p].setU(points[p].getV() + delta);
      }
  }

  private void iupInterp(int p1, int p2, int ref1, int ref2)
  {
    int v1 = points[ref1].getV();
    int v2 = points[ref2].getV();
    int d1 = points[ref1].getU() - v1;
    int d2 = points[ref2].getU() - v2;
    if (p1 > p2)
      return;
    if (v1 == v2)
      {
        for (int p = p1; p <= p2; p++)
          {
            int u = points[p].getV();
            if (u <= v1)
              u += d1;
            else
              u += d2;
            points[p].setU(u);
          }
      }
    else if (v1 < v2)
      {
        for (int p = p1; p <= p2; p++)
          {
            int u = points[p].getV();
            if (u <= v1)
              u += d1;
            else if (u >= v2)
              u += d2;
            else
              {
                u = points[ref1].getU() + Utils.mulDiv(u - v1,
                                                       points[ref2].getU()
                                                       - points[ref1].getU(),
                                                       v2 - v1);
              }
            points[p].setU(u);
          }
      }
    else
      {
        for (int p = p1; p <= p2; p++)
          {
            int u = points[p].getV();
            if (u <= v2)
              u += d2;
            else if (u >= v1)
              u += d1;
            else
              {
                u = points[ref1].getU() + Utils.mulDiv(u - v1,
                                                       points[ref2].getU()
                                                       - points[ref1].getU(),
                                                       v2 - v1);
              }
            points[p].setU(u);
          }
      }
  }

  void alignStrongPoints(int dim)
  {
    AxisHints ax = axis[dim];
    Edge[] edges = ax.edges;
    int numEdges = ax.numEdges;
    short touchFlag;
    if (dim == DIMENSION_HORZ)
      touchFlag = Point.FLAG_DONE_X;
    else
      touchFlag = Point.FLAG_DONE_Y;

    if (numEdges > 0)
      {
        for (int p = 0; p < numPoints; p++)
          {
            Point point = points[p];
            if ((point.getFlags() & touchFlag) != 0)
              continue;
            // If this point is a candidate for weak interpolation, we
            // interpolate it after all strong points have been processed.
            if ((point.getFlags() & Point.FLAG_WEAK_INTERPOLATION) != 0
                && (point.getFlags() & Point.FLAG_INFLECTION) == 0)
              continue;

            int u, ou, fu, delta;
            if (dim == DIMENSION_VERT)
              {
                u = point.getOrigY();
                ou = point.getScaledY();
              }
            else
              {
                u = point.getOrigX();
                ou = point.getScaledX();
              }
            fu = u;
            // Is the point before the first edge?
            Edge edge = edges[0];
            // Inversed vertical dimension.
            delta = edge.fpos - u;
            if (delta >= 0)
              {
                u = edge.pos - (edge.opos - ou);
                storePoint(point, u, dim, touchFlag);
              }
            else
              {
                // Is the point after the last edge?
                edge = edges[numEdges - 1];
                delta = u - edge.fpos;
                if (delta >= 0)
                  {
                    u = edge.pos + (ou - edge.opos);
                    storePoint(point, u, dim, touchFlag);
                  }
                else
                  {
                    // Find enclosing edges.
                    int min = 0;
                    int max = numEdges;
                    int mid, fpos;
                    boolean found = false;
                    while (min < max)
                      {
                        mid = (max + min) / 2;
                        edge = edges[mid];
                        fpos = edge.fpos;
                        if (u < fpos)
                          max = mid;
                        else if (u > fpos)
                          min = mid + 1;
                        else
                          {
                            // Directly on the edge.
                            u = edge.pos;
                            storePoint(point, u, dim, touchFlag);
                            found = true;
                            break;
                          }
                      }
                    if (! found)
                      {
                        Edge before = edges[min - 1];
                        Edge after = edges[min];
                        if (before.scale == 0)
                          {
                            before.scale = Fixed.div16(after.pos - before.pos,
                                                     after.fpos - before.fpos);
                          }
                        u = before.pos + Fixed.mul16(fu - before.fpos,
                                                     before.scale);
                      }
                    storePoint(point, u, dim, touchFlag);
                  }
              }
          }
      }
  }

  private void storePoint(Point p, int u, int dim, short touchFlag)
  {
    if (dim == DIMENSION_HORZ)
      p.setX(u);
    else
      p.setY(u);
    p.addFlags(touchFlag);
  }

  void alignEdgePoints(int dim)
  {
    AxisHints ax = axis[dim];
    Edge[] edges = ax.edges;
    int numEdges = ax.numEdges;
    for (int e = 0; e < numEdges; e++)
      {
        Edge edge = edges[e];
        Segment seg = edge.first;
        do
          {
            Point point = seg.first;
            while (true)
              {
                if (dim == DIMENSION_HORZ)
                  {
                    point.setX(edge.pos);
                    point.addFlags(Point.FLAG_DONE_X);
                  }
                else
                  {
                    point.setY(edge.pos);
                    point.addFlags(Point.FLAG_DONE_Y);
                  }
                if (point == seg.last)
                  break;
                point = point.getNext();
              }
            seg = seg.edgeNext;
          } while (seg != edge.first);
      }
  }

  private int getPointIndex(Point p)
  {
    int idx = -1;
    for (int i = 0; i < numPoints; i++)
      {
        if (p == points[i])
          {
            idx = i;
            break;
          }
      }
    return idx;
  }

  public boolean doAlignEdgePoints()
  {
    return (flags & FontDelegate.FLAG_NO_HINT_EDGE_POINTS) == 0;
  }

  public boolean doAlignStrongPoints()
  {
    return (flags & FontDelegate.FLAG_NO_HINT_STRONG_POINTS) == 0;
  }

  public boolean doAlignWeakPoints()
  {
    return (flags & FontDelegate.FLAG_NO_HINT_WEAK_POINTS) == 0;
  }
}
