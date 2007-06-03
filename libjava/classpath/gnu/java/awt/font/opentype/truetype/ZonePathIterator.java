/* ZonePathIterator.java -- A PathIterator over glyph zones.
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

package gnu.java.awt.font.opentype.truetype;

import java.awt.geom.PathIterator;


/**
 * A PathIterator that enumerates the non-phantom points in a zone.
 *
 * <p><b>Lack of thread safety:</b> Instances of this class are
 * <i>not</i> safe to access from multiple concurrent threads.
 *
 * @see Zone
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
final class ZonePathIterator
  implements PathIterator
{
  /**
   * If <code>state</code> has this value, <code>currentSegment</code>
   * will emit a <code>SEG_LINETO</code> or <code>SEG_QUADTO</code> segment
   * to the current point. For a discussion of subtleties of on-curve
   * and off-curve points, please refer to the documentation for
   * {@link #getSegment}.
   */
  private static final int EMIT_SEGMENT = 0;


  /**
   * If <code>state</code> has this value, <code>currentSegment</code>
   * will emit a <code>SEG_CLOSE</code> in order to close the sub-path
   * for the current contour.
   */
  private static final int EMIT_CLOSE = 1;

  
  /**
   * If <code>state</code> has this value, <code>currentSegment</code>
   * will emit a <code>SEG_MOVETO</code> segment to the first point in
   * the current contour. If the first point is off-curve, a suitable
   * on-curve point is calculated.
   *
   * @see #getStartSegment
   */
  private static final int EMIT_MOVETO = 2;


  /**
   * The state of the iterator, which is one of
   * <code>EMIT_SEGMENT</code>, <code>EMIT_CLOSE</code>, or
   * <code>EMIT_MOVETO</code>.
   */
  private int state;



  /**
   * The zone whose segments are enumerated by this iterator.
   */
  private Zone zone;


  /**
   * The total number of points in the zone, not including the four
   * phantom points at its end.
   */
  private int numPoints;


  /**
   * The number of the current point.
   */
  private int curPoint;


  /**
   * The number of the first point in the current contour.
   */
  private int contourStart;

  
  private int type;

  /**
   * Constructs a ZonePathIterator for the specified zone.
   *
   * @param zone the zone whose segments will be enumerated
   * by this iterator.
   */
  ZonePathIterator(Zone zone, int t)
  {
    this.zone = zone;
    type = t;
    numPoints = zone.getSize() - /* four phantom points */ 4;

    // The first segment that needs to be emitted is a SEG_MOVETO.
    state = EMIT_MOVETO;
  }


  /**
   * Returns the winding rule. TrueType glyphs always use the non-zero
   * winding rule, so this method will always return {@link
   * PathIterator#WIND_NON_ZERO}.
   */
  public int getWindingRule()
  {
    return PathIterator.WIND_NON_ZERO;
  }



  public boolean isDone()
  {
    return (state != EMIT_CLOSE) && (curPoint >= numPoints);
  }

    
  public void next()
  {
    boolean onCurve;

    /* If the current point is the end of a segment, and no SEG_CLOSE
     * has been emitted yet, this will be the next segment.
     */
    if (zone.isContourEnd(curPoint) && (state != EMIT_CLOSE))
    {
      state = EMIT_CLOSE;
      return;
    }

    /* If the previously emitted segment was a SEG_CLOSE, we are now
     * at the beginning of a new contour.
     */
    if (state == EMIT_CLOSE)
    {
      contourStart = ++curPoint;
      state = EMIT_MOVETO;
      return;
    }

    onCurve = zone.isOnCurve(curPoint);

    /* If the last segment was a moveto, and the current point
     * (which is the first point in the contour) is off-curve,
     * we need to emit a quadto segment for the first point.
     */
    if ((state == EMIT_MOVETO) && !onCurve)
    {
      state = EMIT_SEGMENT;
      return;
    }


    curPoint++;

    /* If the last point has been off-curve, and the now current
     * point is on-curve, the last segment was a quadto that
     * had the now current point at its end. In this case, we can
     * skip a segment.
     */
    if (!onCurve && zone.isOnCurve(curPoint))
    {
      /* But if the skipped point is the end of a contour, we must not
       * skip the SEG_CLOSE. An example where this matters is the 'o'
       * glyph in the Helvetica font face that comes with MacOS X
       * 10.1.5.
       */
      if (zone.isContourEnd(curPoint))
      {
        state = EMIT_CLOSE;
        return;
      }

      curPoint++;
    }

    state = EMIT_SEGMENT;
  }


  /**
   * Determines the successor of the current point in the current
   * contour. The successor of the last point in a contour is the
   * start of that contour.
   *
   * @return the number of the point that follows the current point in
   * the same contour.
   */
  private int getSuccessor(int p)
  {
    if (zone.isContourEnd(p))
      return contourStart;
    else
      return p + 1;
  }
  


  /**
   * Retrieves the current path segment using single-precision
   * coordinate values.
   */
  public int currentSegment(float[] coords)
  {
    switch (state)
    {
    case EMIT_CLOSE:
      return PathIterator.SEG_CLOSE;
    
    case EMIT_MOVETO:
      return getStartSegment(curPoint, coords);

    default:
      return getSegment(curPoint, coords);
    }
  }


  /**
   * A helper array that is used by {@link
   * #currentSegment(double[])}.
   */
  float[] floats;


  /**
   * Retrieves the current path segment using double-precision
   * coordinate values.
   */
  public int currentSegment(double[] coords)
  {
    if (floats == null)
      floats = new float[6];
    int result;
    
    result = currentSegment(floats);
    for (int i = 0; i < 6; i++)
      coords[i] = floats[i];
    return result;
  }


  /**
   * Returns the segment for the specified point.
   *
   * <p><img src="doc-files/ZonePathIterator-1.png" width="426"
   * height="194" alt="An example curve" /></p>
   *
   * <p>If <code>cur</code> is an on-curve point, the returned segment
   * is a straight line to <code>cur</code>. In the illustration, this
   * would be the case for <code>cur = 4</code>.</p>
   *
   * <p>If <code>cur</code> is an off-curve point, and
   * <code>cur</code>&#x2019;s successor <code>succ</code> is also
   * off-curve, the returned segment is a quadratic B&eacute;zier
   * spline whose control point is <code>cur</code>, and whose end
   * point is located at the middle of the line connecting
   * <code>cur</code> and <code>succ</code>. In the illustration,
   * this would be the case for <code>cur = 5</code>.</p>
   *
   * <p>If <code>cur</code> is an off-curve point, and
   * <code>cur</code>&#x2019;s successor <code>succ</code> is
   * on-curve, the returned segment is a quadratic B&eacute;zier
   * spline whose control point is <code>cur</code>, and whose end
   * point is <code>succ</code>. In the illustration, this would
   * be the case for <code>cur = 6</code>.</p>
   *
   * @return either <code>PathIterator.SEG_LINETO</code> or
   * <code>PathIterator.SEG_QUADTO</code>.
   */
  private int getSegment(int cur, float[] coords)
  {
    int curX, curY;
    int succ, succX, succY;

    curX = zone.getX(cur, type);
    curY = zone.getY(cur, type);
    coords[0] = Fixed.floatValue(curX);
    coords[1] = Fixed.floatValue(curY);

    if (zone.isOnCurve(cur))
      return PathIterator.SEG_LINETO;

    succ = getSuccessor(cur);
    succX = zone.getX(succ, type);
    succY = zone.getY(succ, type);

    if (zone.isOnCurve(succ))
    {
      coords[2] = Fixed.floatValue(succX);
      coords[3] = Fixed.floatValue(succY);
    }
    else
    {
      coords[2] = Fixed.floatValue((curX + succX) / 2);
      coords[3] = Fixed.floatValue((curY + succY) / 2);
    }
    return PathIterator.SEG_QUADTO;
  }


  /**
   * Returns the start segment for the contour which starts
   * at the specified point.
   *
   * <p>If the contour starts with an on-curve point, the returned
   * segment is a <code>SEG_MOVETO</code> to that point.</p>
   *
   * <p>If the contour starts with an off-curve point, and the contour
   * ends with an on-curve point, the returned segment is a
   * <code>SEG_MOVETO</code> to the end point.</p>
   *
   * <p>If the contour starts with an off-curve point, and the contour
   * also ends with an off-curve point, the returned segment is a
   * <code>SEG_MOVETO</code> to the location at the middle between the
   * start and end points of the contour.</p>
   *
   * @return <code>PathIterator.SEG_MOVETO</code>.
   */
  private int getStartSegment(int contourStart, float[] coords)
  {
    int x, y;

    if (zone.isOnCurve(contourStart))
    {
      x = zone.getX(contourStart, type);
      y = zone.getY(contourStart, type);    
    }
    else
    {
      /* Find the last point of the current contour. */
      int contourEnd = contourStart;
      while (!zone.isContourEnd(contourEnd))
        ++contourEnd;

      if (zone.isOnCurve(contourEnd))
      {
        /* An example is the 'o' glyph of the Helvetica which comes
         * with Apple MacOS X 10.1.5.
         */
        x = zone.getX(contourEnd, type);
        y = zone.getY(contourEnd, type);
      }
      else
      {
        x = (zone.getX(contourStart, type) + zone.getX(contourEnd, type)) / 2;
        y = (zone.getY(contourStart, type) + zone.getY(contourEnd, type)) / 2;
      }
    }

    coords[0] = Fixed.floatValue(x);
    coords[1] = Fixed.floatValue(y);
    return PathIterator.SEG_MOVETO;
  }
}
