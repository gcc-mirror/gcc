/* Copyright (C) 2001  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

import java.awt.geom.*;
import java.io.Serializable;
import java.util.Arrays;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date May 10, 2001
 */

/** The Polygon class represents a closed region whose boundary is
    made of line segments.  The Polygon is defined by its vertices.  */
public class Polygon implements Shape, Serializable
{
  /** The bounds of the polygon.  This is null until the bounds have
   *  been computed for the first time; then it is correctly
   *  maintained whenever it is modified.  */
  protected Rectangle bounds;

  /** The number of points in the polygon.  */
  public int npoints;

  /** The x coordinates of the points.  */
  public int[] xpoints;

  /** The y coordinates of the points.  */
  public int[] ypoints;

  /** Create a new, empty Polygon.  */
  public Polygon ()
  {
    this.xpoints = new int[0];
    this.ypoints = new int[0];
    this.npoints = 0;
  }

  /** Create a new Polygon from the given vertices.
   * @param xpoints The x coordinates
   * @param ypoints The y coordinates
   * @param npoints The number of points
   */
  public Polygon (int[] xpoints, int[] ypoints, int npoints)
  {
    // We make explicit copies instead of relying on clone so that we
    // ensure the new arrays are the same size.
    this.xpoints = new int[npoints];
    this.ypoints = new int[npoints];
    System.arraycopy (xpoints, 0, this.xpoints, 0, npoints);
    System.arraycopy (ypoints, 0, this.ypoints, 0, npoints);
  }

  /** Append the specified point to this Polygon.
   * @param x The x coordinate
   * @param y The y coordinate
   */
  public void addPoint (int x, int y)
  {
    int[] newx = new int[npoints + 1];
    System.arraycopy (xpoints, 0, newx, 0, npoints);
    int[] newy = new int[npoints + 1];
    System.arraycopy (ypoints, 0, newy, 0, npoints);
    newx[npoints] = x;
    newy[npoints] = y;
    ++npoints;
    xpoints = newx;
    ypoints = newy;

    // It is simpler to just recompute.
    if (bounds != null)
      computeBoundingBox ();
  }

  /** Return true if the indicated point is inside this Polygon.
   * This uses an even-odd rule to determine insideness.
   * @param x The x coordinate
   * @param y The y coordinate
   * @returns true if the point is contained by this Polygon.
   */
  public boolean contains (double x, double y)
  {
    // What we do is look at each line segment.  If the line segment
    // crosses the "scan line" at y at a point x' < x, then we
    // increment our counter.  At the end, an even number means the
    // point is outside the polygon.  Instead of a number, though, we
    // use a boolean.
    boolean inside = false;
    for (int i = 0; i < npoints; ++i)
      {
	// Handle the wrap case.
	int x2 = (i == npoints) ? xpoints[0] : xpoints[i + 1];
	int y2 = (i == npoints) ? ypoints[0] : ypoints[i + 1];

	if (ypoints[i] == y2)
	  {
	    // We ignore horizontal lines.  This might give weird
	    // results in some situations -- ?
	    continue;
	  }

	double t = (y - ypoints[i]) / (double) (y2 - ypoints[i]);
	double x3 = xpoints[i] + t * (x2 - xpoints[i]);
	if (x3 < x)
	  inside = ! inside;
      }

    return inside;
  }

  /** Return true if the indicated rectangle is entirely inside this
   * Polygon.
   * This uses an even-odd rule to determine insideness.
   * @param x The x coordinate
   * @param y The y coordinate
   * @param w The width
   * @param h The height
   * @returns true if the rectangle is contained by this Polygon.
   */
  public boolean contains (double x, double y, double w, double h)
  {
    return intersectOrContains (x, y, w, h, false);
  }

  /** Return true if the indicated point is inside this Polygon.
   * This uses an even-odd rule to determine insideness.
   * @param x The x coordinate
   * @param y The y coordinate
   * @returns true if the point is contained by this Polygon.
   */
  public boolean contains (int x, int y)
  {
    return contains ((double) x, (double) y);
  }

  /** Return true if the indicated point is inside this Polygon.
   * This uses an even-odd rule to determine insideness.
   * @param p The point
   * @returns true if the point is contained by this Polygon.
   */
  public boolean contains (Point p)
  {
    return contains (p.x, p.y);
  }

  /** Return true if the indicated point is inside this Polygon.
   * This uses an even-odd rule to determine insideness.
   * @param p The point
   * @returns true if the point is contained by this Polygon.
   */
  public boolean contains (Point2D p)
  {
    return contains (p.getX (), p.getY ());
  }

  /** Return true if the indicated rectangle is entirely inside this
   * Polygon.  This uses an even-odd rule to determine insideness.
   * @param r The rectangle
   * @returns true if the rectangle is contained by this Polygon.
   */
  public boolean contains (Rectangle2D r)
  {
    return contains (r.getX (), r.getY (), r.getWidth (), r.getHeight ());
  }

  /** Returns the bounds of this Polygon.
   * @deprecated Use getBounds() instead.
   */
  public Rectangle getBoundingBox ()
  {
    if (bounds == null)
      computeBoundingBox ();
    return bounds;
  }

  /** Returns the bounds of this Polygon.  */
  public Rectangle getBounds ()
  {
    if (bounds == null)
      computeBoundingBox ();
    return bounds;
  }

  /** Returns the bounds of this Polygon.  */
  public Rectangle2D getBounds2D ()
  {
    if (bounds == null)
      computeBoundingBox ();
    return bounds;		// Why not?
  }

  /** Return an iterator for the boundary of this Polygon.
   * @param at A transform to apply to the coordinates.
   * @returns A path iterator for the Polygon's boundary.
   */
  public PathIterator getPathIterator (AffineTransform at)
  {
    return new Iterator (at);
  }

  /** Return an iterator for the boundary of this Polygon.
   * @param at A transform to apply to the coordinates.
   * @param flatness The flatness of the result; it is ignored by
   *                 this class.
   * @returns A path iterator for the Polygon's boundary.
   */
  public PathIterator getPathIterator (AffineTransform at, double flatness)
  {
    // We ignore the flatness.
    return new Iterator (at);
  }

  /** @deprecated use contains(int,int).  */
  public boolean inside (int x, int y)
  {
    return contains (x, y);
  }

  /** Return true if this Polygon's interior intersects the given
   * rectangle's interior.
   * @param x The x coordinate
   * @param y The y coordinate
   * @param w The width
   * @param h The height
   */
  public boolean intersects (double x, double y, double w, double h)
  {
    return intersectOrContains (x, y, w, h, true);
  }

  /** Return true if this Polygon's interior intersects the given
   * rectangle's interior.
   * @param r The rectangle
   */
  public boolean intersects (Rectangle2D r)
  {
    return intersects (r.getX (), r.getY (), r.getWidth (), r.getHeight ());
  }

  // This tests for intersection with or containment of a rectangle,
  // depending on the INTERSECT argument.
  private boolean intersectOrContains (double x, double y, double w, double h,
				       boolean intersect)
  {
    // First compute the rectangle of possible intersection points.
    Rectangle r = getBounds ();
    int minx = Math.max (r.x, (int) x);
    int maxx = Math.min (r.x + r.width, (int) (x + w));
    int miny = Math.max (r.y, (int) y);
    int maxy = Math.min (r.y + r.height, (int) (y + h));

    if (miny > maxy)
      return false;

    double[] crosses = new double[npoints + 1];

    for (; miny < maxy; ++miny)
      {
	// First compute every place where the polygon might intersect
	// the scan line at Y.
	int ins = 0;
	for (int i = 0; i < npoints; ++i)
	  {
	    // Handle the wrap case.
	    int x2 = (i == npoints) ? xpoints[0] : xpoints[i + 1];
	    int y2 = (i == npoints) ? ypoints[0] : ypoints[i + 1];

	    if (ypoints[i] == y2)
	      {
		// We ignore horizontal lines.  This might give weird
		// results in some situations -- ?
		continue;
	      }

	    double t = (((double) miny - ypoints[i])
			/ (double) (y2 - ypoints[i]));
	    double x3 = xpoints[i] + t * (x2 - xpoints[i]);
	    crosses[ins++] = x3;
	  }

	// Now we can sort into increasing order and look to see if
	// any point in the rectangle is in the polygon.  We examine
	// every other pair due to our even-odd rule.
	Arrays.sort (crosses, 0, ins);
	int i = intersect ? 0 : 1;
	for (; i < ins - 1; i += 2)
	  {
	    // Pathological case.
	    if (crosses[i] == crosses[i + 1])
	      continue;

	    // Found a point on the inside.
	    if ((crosses[i] >= x && crosses[i] < x + w)
		|| (crosses[i + 1] >= x && crosses[i + 1] < x + w))
	      {
		// If we're checking containment then we just lost.
		// But if we're checking intersection then we just
		// won.
		return intersect;
	      }
	  }
      }

    return false;
  }

  /** Translates all the vertices of the polygon via a given vector.
   * @param deltaX The X offset
   * @param deltaY The Y offset
   */
  public void translate (int deltaX, int deltaY)
  {
    for (int i = 0; i < npoints; ++i)
      {
	xpoints[i] += deltaX;
	ypoints[i] += deltaY;
      }

    if (bounds != null)
      {
	bounds.x += deltaX;
	bounds.y += deltaY;
      }
  }

  // This computes the bounding box if required.
  private void computeBoundingBox ()
  {
    if (npoints == 0)
      {
	// This is wrong if the user adds a new point, but we
	// account for that in addPoint().
	bounds = new Rectangle (0, 0, 0, 0);
      }
    else
      {
	int maxx = xpoints[0];
	int minx = xpoints[0];
	int maxy = ypoints[0];
	int miny = ypoints[0];

	for (int i = 1; i < npoints; ++i)
	  {
	    maxx = Math.max (maxx, xpoints[i]);
	    minx = Math.min (minx, xpoints[i]);
	    maxy = Math.max (maxy, ypoints[i]);
	    miny = Math.min (miny, ypoints[i]);
	  }

	bounds = new Rectangle (minx, miny, maxx - minx, maxy - miny);
      }
  }

  private class Iterator implements PathIterator
  {
    public AffineTransform xform;
    public int where;

    public Iterator (AffineTransform xform)
    {
      this.xform = xform;
      where = 0;
    }

    public int currentSegment (double[] coords)
    {
      int r;

      if (where < npoints)
	{
	  coords[0] = xpoints[where];
	  coords[1] = ypoints[where];
	  r = (where == 0) ? SEG_MOVETO : SEG_LINETO;
	  xform.transform (coords, 0, coords, 0, 1);
	  ++where;
	}
      else
	r = SEG_CLOSE;

      return r;
    }

    public int currentSegment (float[] coords)
    {
      int r;

      if (where < npoints)
	{
	  coords[0] = xpoints[where];
	  coords[1] = ypoints[where];
	  r = (where == 0) ? SEG_MOVETO : SEG_LINETO;
	  xform.transform (coords, 0, coords, 0, 1);
	}
      else
	r = SEG_CLOSE;

      return r;
    }

    public int getWindingRule ()
    {
      return WIND_EVEN_ODD;
    }

    public boolean isDone ()
    {
      return where == npoints + 1;
    }

    public void next ()
    {
      ++where;
    }
  }
}
