/* Area.java -- represents a shape built by constructive area geometry
   Copyright (C) 2002, 2004 Free Software Foundation

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package java.awt.geom;

import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Vector;


/**
 * The Area class represents any area for the purpose of
 * Constructive Area Geometry (CAG) manipulations. CAG manipulations
 * work as an area-wise form of boolean logic, where the basic operations are:
 * <P><li>Add (in boolean algebra: A <B>or</B> B)<BR>
 * <li>Subtract (in boolean algebra: A <B>and</B> (<B>not</B> B) )<BR>
 * <li>Intersect (in boolean algebra: A <B>and</B> B)<BR>
 * <li>Exclusive Or <BR>
 * <img src="doc-files/Area-1.png" width="342" height="302"
 * alt="Illustration of CAG operations" /><BR>
 * Above is an illustration of the CAG operations on two ring shapes.<P>
 *
 * The contains and intersects() methods are also more accurate than the
 * specification of #Shape requires.<P>
 *
 * Please note that constructing an Area can be slow
 * (Self-intersection resolving is proportional to the square of
 * the number of segments).<P>
 * @see #add(Area)
 * @see #subtract(Area)
 * @see #intersect(Area)
 * @see #exclusiveOr(Area)
 *
 * @author Sven de Marothy (sven@physto.se)
 *
 * @since 1.2
 * @status Works, but could be faster and more reliable.
 */
public class Area implements Shape, Cloneable
{
  /**
   * General numerical precision
   */
  private static final double EPSILON = 1E-11;

  /**
   * recursive subdivision epsilon - (see getRecursionDepth)
   */
  private static final double RS_EPSILON = 1E-13;

  /**
   * Snap distance - points within this distance are considered equal
   */
  private static final double PE_EPSILON = 1E-11;

  /**
   * Segment vectors containing solid areas and holes
   */
  private Vector solids;

  /**
   * Segment vectors containing solid areas and holes
   */
  private Vector holes;

  /**
   * Vector (temporary) storing curve-curve intersections
   */
  private Vector cc_intersections;

  /**
   * Winding rule WIND_NON_ZERO used, after construction,
   * this is irrelevant.
   */
  private int windingRule;

  /**
   * Constructs an empty Area
   */
  public Area()
  {
    solids = new Vector();
    holes = new Vector();
  }

  /**
   * Constructs an Area from any given Shape. <P>
   *
   * If the Shape is self-intersecting, the created Area will consist
   * of non-self-intersecting subpaths, and any inner paths which
   * are found redundant in accordance with the Shape's winding rule
   * will not be included.
   * 
   * @param s  the shape (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>s</code> is <code>null</code>.
   */
  public Area(Shape s)
  {
    this();

    Vector p = makeSegment(s);

    // empty path
    if (p == null)
      return;

    // delete empty paths
    for (int i = 0; i < p.size(); i++)
      if (((Segment) p.elementAt(i)).getSignedArea() == 0.0)
	p.remove(i--);

    /*
     * Resolve self intersecting paths into non-intersecting
     * solids and holes.
     * Algorithm is as follows:
     * 1: Create nodes at all self intersections
     * 2: Put all segments into a list
     * 3: Grab a segment, follow it, change direction at each node,
     *    removing segments from the list in the process
     * 4: Repeat (3) until no segments remain in the list
     * 5: Remove redundant paths and sort into solids and holes
     */
    Vector paths = new Vector();
    Segment v;

    for (int i = 0; i < p.size(); i++)
      {
	Segment path = (Segment) p.elementAt(i);
	createNodesSelf(path);
      }

    if (p.size() > 1)
      {
	for (int i = 0; i < p.size() - 1; i++)
	  for (int j = i + 1; j < p.size(); j++)
	    {
	      Segment path1 = (Segment) p.elementAt(i);
	      Segment path2 = (Segment) p.elementAt(j);
	      createNodes(path1, path2);
	    }
      }

    // we have intersecting points.
    Vector segments = new Vector();

    for (int i = 0; i < p.size(); i++)
      {
	Segment path = v = (Segment) p.elementAt(i);
	do
	  {
	    segments.add(v);
	    v = v.next;
	  }
	while (v != path);
      }

    paths = weilerAtherton(segments);
    deleteRedundantPaths(paths);
  }

  /**
   * Performs an add (union) operation on this area with another Area.<BR>
   * @param area - the area to be unioned with this one
   */
  public void add(Area area)
  {
    if (equals(area))
      return;
    if (area.isEmpty())
      return;

    Area B = (Area) area.clone();

    Vector pathA = new Vector();
    Vector pathB = new Vector();
    pathA.addAll(solids);
    pathA.addAll(holes);
    pathB.addAll(B.solids);
    pathB.addAll(B.holes);

    int nNodes = 0;

    for (int i = 0; i < pathA.size(); i++)
      {
	Segment a = (Segment) pathA.elementAt(i);
	for (int j = 0; j < pathB.size(); j++)
	  {
	    Segment b = (Segment) pathB.elementAt(j);
	    nNodes += createNodes(a, b);
	  }
      }

    Vector paths = new Vector();
    Segment v;

    // we have intersecting points.
    Vector segments = new Vector();

    // In a union operation, we keep all
    // segments of A oustide B and all B outside A
    for (int i = 0; i < pathA.size(); i++)
      {
	v = (Segment) pathA.elementAt(i);
	Segment path = v;
	do
	  {
	    if (v.isSegmentOutside(area))
	      segments.add(v);
	    v = v.next;
	  }
	while (v != path);
      }

    for (int i = 0; i < pathB.size(); i++)
      {
	v = (Segment) pathB.elementAt(i);
	Segment path = v;
	do
	  {
	    if (v.isSegmentOutside(this))
	      segments.add(v);
	    v = v.next;
	  }
	while (v != path);
      }

    paths = weilerAtherton(segments);
    deleteRedundantPaths(paths);
  }

  /**
   * Performs a subtraction operation on this Area.<BR>
   * @param area the area to be subtracted from this area.
   * @throws NullPointerException if <code>area</code> is <code>null</code>.
   */
  public void subtract(Area area)
  {
    if (isEmpty() || area.isEmpty())
      return;

    if (equals(area))
      {
	reset();
	return;
      }

    Vector pathA = new Vector();
    Area B = (Area) area.clone();
    pathA.addAll(solids);
    pathA.addAll(holes);

    // reverse the directions of B paths.
    setDirection(B.holes, true);
    setDirection(B.solids, false);

    Vector pathB = new Vector();
    pathB.addAll(B.solids);
    pathB.addAll(B.holes);

    int nNodes = 0;

    // create nodes
    for (int i = 0; i < pathA.size(); i++)
      {
	Segment a = (Segment) pathA.elementAt(i);
	for (int j = 0; j < pathB.size(); j++)
	  {
	    Segment b = (Segment) pathB.elementAt(j);
	    nNodes += createNodes(a, b);
	  }
      }

    Vector paths = new Vector();

    // we have intersecting points.
    Vector segments = new Vector();

    // In a subtraction operation, we keep all
    // segments of A oustide B and all B within A
    // We outsideness-test only one segment in each path
    // and the segments before and after any node
    for (int i = 0; i < pathA.size(); i++)
      {
	Segment v = (Segment) pathA.elementAt(i);
	Segment path = v;
	if (v.isSegmentOutside(area) && v.node == null)
	  segments.add(v);
	boolean node = false;
	do
	  {
	    if ((v.node != null || node))
	      {
		node = (v.node != null);
		if (v.isSegmentOutside(area))
		  segments.add(v);
	      }
	    v = v.next;
	  }
	while (v != path);
      }

    for (int i = 0; i < pathB.size(); i++)
      {
	Segment v = (Segment) pathB.elementAt(i);
	Segment path = v;
	if (! v.isSegmentOutside(this) && v.node == null)
	  segments.add(v);
	v = v.next;
	boolean node = false;
	do
	  {
	    if ((v.node != null || node))
	      {
		node = (v.node != null);
		if (! v.isSegmentOutside(this))
		  segments.add(v);
	      }
	    v = v.next;
	  }
	while (v != path);
      }

    paths = weilerAtherton(segments);
    deleteRedundantPaths(paths);
  }

  /**
   * Performs an intersection operation on this Area.<BR>
   * @param area - the area to be intersected with this area.
   * @throws NullPointerException if <code>area</code> is <code>null</code>.
   */
  public void intersect(Area area)
  {
    if (isEmpty() || area.isEmpty())
      {
	reset();
	return;
      }
    if (equals(area))
      return;

    Vector pathA = new Vector();
    Area B = (Area) area.clone();
    pathA.addAll(solids);
    pathA.addAll(holes);

    Vector pathB = new Vector();
    pathB.addAll(B.solids);
    pathB.addAll(B.holes);

    int nNodes = 0;

    // create nodes
    for (int i = 0; i < pathA.size(); i++)
      {
	Segment a = (Segment) pathA.elementAt(i);
	for (int j = 0; j < pathB.size(); j++)
	  {
	    Segment b = (Segment) pathB.elementAt(j);
	    nNodes += createNodes(a, b);
	  }
      }

    Vector paths = new Vector();

    // we have intersecting points.
    Vector segments = new Vector();

    // In an intersection operation, we keep all
    // segments of A within B and all B within A
    // (The rest must be redundant)
    // We outsideness-test only one segment in each path
    // and the segments before and after any node
    for (int i = 0; i < pathA.size(); i++)
      {
	Segment v = (Segment) pathA.elementAt(i);
	Segment path = v;
	if (! v.isSegmentOutside(area) && v.node == null)
	  segments.add(v);
	boolean node = false;
	do
	  {
	    if ((v.node != null || node))
	      {
		node = (v.node != null);
		if (! v.isSegmentOutside(area))
		  segments.add(v);
	      }
	    v = v.next;
	  }
	while (v != path);
      }

    for (int i = 0; i < pathB.size(); i++)
      {
	Segment v = (Segment) pathB.elementAt(i);
	Segment path = v;
	if (! v.isSegmentOutside(this) && v.node == null)
	  segments.add(v);
	v = v.next;
	boolean node = false;
	do
	  {
	    if ((v.node != null || node))
	      {
		node = (v.node != null);
		if (! v.isSegmentOutside(this))
		  segments.add(v);
	      }
	    v = v.next;
	  }
	while (v != path);
      }

    paths = weilerAtherton(segments);
    deleteRedundantPaths(paths);
  }

  /**
   * Performs an exclusive-or operation on this Area.<BR>
   * @param area - the area to be XORed with this area.
   * @throws NullPointerException if <code>area</code> is <code>null</code>.
   */
  public void exclusiveOr(Area area)
  {
    if (area.isEmpty())
      return;

    if (isEmpty())
      {
	Area B = (Area) area.clone();
	solids = B.solids;
	holes = B.holes;
	return;
      }
    if (equals(area))
      {
	reset();
	return;
      }

    Vector pathA = new Vector();

    Area B = (Area) area.clone();
    Vector pathB = new Vector();
    pathA.addAll(solids);
    pathA.addAll(holes);

    // reverse the directions of B paths.
    setDirection(B.holes, true);
    setDirection(B.solids, false);
    pathB.addAll(B.solids);
    pathB.addAll(B.holes);

    int nNodes = 0;

    for (int i = 0; i < pathA.size(); i++)
      {
	Segment a = (Segment) pathA.elementAt(i);
	for (int j = 0; j < pathB.size(); j++)
	  {
	    Segment b = (Segment) pathB.elementAt(j);
	    nNodes += createNodes(a, b);
	  }
      }

    Vector paths = new Vector();
    Segment v;

    // we have intersecting points.
    Vector segments = new Vector();

    // In an XOR operation, we operate on all segments
    for (int i = 0; i < pathA.size(); i++)
      {
	v = (Segment) pathA.elementAt(i);
	Segment path = v;
	do
	  {
	    segments.add(v);
	    v = v.next;
	  }
	while (v != path);
      }

    for (int i = 0; i < pathB.size(); i++)
      {
	v = (Segment) pathB.elementAt(i);
	Segment path = v;
	do
	  {
	    segments.add(v);
	    v = v.next;
	  }
	while (v != path);
      }

    paths = weilerAtherton(segments);
    deleteRedundantPaths(paths);
  }

  /**
   * Clears the Area object, creating an empty area.
   */
  public void reset()
  {
    solids = new Vector();
    holes = new Vector();
  }

  /**
   * Returns whether this area encloses any area.
   * @return true if the object encloses any area.
   */
  public boolean isEmpty()
  {
    if (solids.size() == 0)
      return true;

    double totalArea = 0;
    for (int i = 0; i < solids.size(); i++)
      totalArea += Math.abs(((Segment) solids.elementAt(i)).getSignedArea());
    for (int i = 0; i < holes.size(); i++)
      totalArea -= Math.abs(((Segment) holes.elementAt(i)).getSignedArea());
    if (totalArea <= EPSILON)
      return true;

    return false;
  }

  /**
   * Determines whether the Area consists entirely of line segments
   * @return true if the Area lines-only, false otherwise
   */
  public boolean isPolygonal()
  {
    for (int i = 0; i < holes.size(); i++)
      if (! ((Segment) holes.elementAt(i)).isPolygonal())
	return false;
    for (int i = 0; i < solids.size(); i++)
      if (! ((Segment) solids.elementAt(i)).isPolygonal())
	return false;
    return true;
  }

  /**
   * Determines if the Area is rectangular.<P>
   *
   * This is strictly qualified. An area is considered rectangular if:<BR>
   * <li>It consists of a single polygonal path.<BR>
   * <li>It is oriented parallel/perpendicular to the xy axis<BR>
   * <li>It must be exactly rectangular, i.e. small errors induced by
   * transformations may cause a false result, although the area is
   * visibly rectangular.<P>
   * @return true if the above criteria are met, false otherwise
   */
  public boolean isRectangular()
  {
    if (isEmpty())
      return true;

    if (holes.size() != 0 || solids.size() != 1)
      return false;

    Segment path = (Segment) solids.elementAt(0);
    if (! path.isPolygonal())
      return false;

    int nCorners = 0;
    Segment s = path;
    do
      {
	Segment s2 = s.next;
	double d1 = (s.P2.getX() - s.P1.getX())*(s2.P2.getX() - s2.P1.getX())/
	    ((s.P1.distance(s.P2)) * (s2.P1.distance(s2.P2)));
	double d2 = (s.P2.getY() - s.P1.getY())*(s2.P2.getY() - s2.P1.getY())/ 
	    ((s.P1.distance(s.P2)) * (s2.P1.distance(s2.P2)));
	double dotproduct = d1 + d2;

	// For some reason, only rectangles on the XY axis count.
	if (d1 != 0 && d2 != 0)
	  return false;

	if (Math.abs(dotproduct) == 0) // 90 degree angle
	  nCorners++;
	else if ((Math.abs(1.0 - dotproduct) > 0)) // 0 degree angle?
	  return false; // if not, return false

	s = s.next;
      }
    while (s != path);

    return nCorners == 4;
  }

  /**
   * Returns whether the Area consists of more than one simple
   * (non self-intersecting) subpath.
   *
   * @return true if the Area consists of none or one simple subpath,
   * false otherwise.
   */
  public boolean isSingular()
  {
    return (holes.size() == 0 && solids.size() <= 1);
  }

  /**
   * Returns the bounding box of the Area.<P> Unlike the CubicCurve2D and
   * QuadraticCurve2D classes, this method will return the tightest possible
   * bounding box, evaluating the extreme points of each curved segment.<P>
   * @return the bounding box
   */
  public Rectangle2D getBounds2D()
  {
    if (solids.size() == 0)
      return new Rectangle2D.Double(0.0, 0.0, 0.0, 0.0);

    double xmin;
    double xmax;
    double ymin;
    double ymax;
    xmin = xmax = ((Segment) solids.elementAt(0)).P1.getX();
    ymin = ymax = ((Segment) solids.elementAt(0)).P1.getY();

    for (int path = 0; path < solids.size(); path++)
      {
	Rectangle2D r = ((Segment) solids.elementAt(path)).getPathBounds();
	xmin = Math.min(r.getMinX(), xmin);
	ymin = Math.min(r.getMinY(), ymin);
	xmax = Math.max(r.getMaxX(), xmax);
	ymax = Math.max(r.getMaxY(), ymax);
      }

    return (new Rectangle2D.Double(xmin, ymin, (xmax - xmin), (ymax - ymin)));
  }

  /**
   * Returns the bounds of this object in Rectangle format.
   * Please note that this may lead to loss of precision.
   * 
   * @return The bounds.
   * @see #getBounds2D()
   */
  public Rectangle getBounds()
  {
    return getBounds2D().getBounds();
  }

  /**
   * Create a new area of the same run-time type with the same contents as
   * this one.
   *
   * @return the clone
   */
  public Object clone()
  {
    try
      {
	Area clone = new Area();
	for (int i = 0; i < solids.size(); i++)
	  clone.solids.add(((Segment) solids.elementAt(i)).cloneSegmentList());
	for (int i = 0; i < holes.size(); i++)
	  clone.holes.add(((Segment) holes.elementAt(i)).cloneSegmentList());
	return clone;
      }
    catch (CloneNotSupportedException e)
      {
	throw (Error) new InternalError().initCause(e); // Impossible
      }
  }

  /**
   * Compares two Areas.
   * 
   * @param area  the area to compare against this area (<code>null</code>
   *              permitted).
   * @return <code>true</code> if the areas are equal, and <code>false</code>
   *         otherwise.
   */
  public boolean equals(Area area)
  {
    if (area == null)
      return false;

    if (! getBounds2D().equals(area.getBounds2D()))
      return false;

    if (solids.size() != area.solids.size()
        || holes.size() != area.holes.size())
      return false;

    Vector pathA = new Vector();
    pathA.addAll(solids);
    pathA.addAll(holes);
    Vector pathB = new Vector();
    pathB.addAll(area.solids);
    pathB.addAll(area.holes);

    int nPaths = pathA.size();
    boolean[][] match = new boolean[2][nPaths];

    for (int i = 0; i < nPaths; i++)
      {
	for (int j = 0; j < nPaths; j++)
	  {
	    Segment p1 = (Segment) pathA.elementAt(i);
	    Segment p2 = (Segment) pathB.elementAt(j);
	    if (! match[0][i] && ! match[1][j])
	      if (p1.pathEquals(p2))
		match[0][i] = match[1][j] = true;
	  }
      }

    boolean result = true;
    for (int i = 0; i < nPaths; i++)
      result = result && match[0][i] && match[1][i];
    return result;
  }

  /**
   * Transforms this area by the AffineTransform at.
   * 
   * @param at  the transform.
   */
  public void transform(AffineTransform at)
  {
    for (int i = 0; i < solids.size(); i++)
      ((Segment) solids.elementAt(i)).transformSegmentList(at);
    for (int i = 0; i < holes.size(); i++)
      ((Segment) holes.elementAt(i)).transformSegmentList(at);

    // Note that the orientation is not invariant under inversion
    if ((at.getType() & AffineTransform.TYPE_FLIP) != 0)
      {
	setDirection(holes, false);
	setDirection(solids, true);
      }
  }

  /**
   * Returns a new Area equal to this one, transformed
   * by the AffineTransform at.
   * @param at  the transform.
   * @return the transformed area
   * @throws NullPointerException if <code>at</code> is <code>null</code>.
   */
  public Area createTransformedArea(AffineTransform at)
  {
    Area a = (Area) clone();
    a.transform(at);
    return a;
  }

  /**
   * Determines if the point (x,y) is contained within this Area.
   *
   * @param x the x-coordinate of the point.
   * @param y the y-coordinate of the point.
   * @return true if the point is contained, false otherwise.
   */
  public boolean contains(double x, double y)
  {
    int n = 0;
    for (int i = 0; i < solids.size(); i++)
      if (((Segment) solids.elementAt(i)).contains(x, y))
	n++;

    for (int i = 0; i < holes.size(); i++)
      if (((Segment) holes.elementAt(i)).contains(x, y))
	n--;

    return (n != 0);
  }

  /**
   * Determines if the Point2D p is contained within this Area.
   *
   * @param p the point.
   * @return <code>true</code> if the point is contained, <code>false</code> 
   *         otherwise.
   * @throws NullPointerException if <code>p</code> is <code>null</code>.
   */
  public boolean contains(Point2D p)
  {
    return contains(p.getX(), p.getY());
  }

  /**
   * Determines if the rectangle specified by (x,y) as the upper-left
   * and with width w and height h is completely contained within this Area,
   * returns false otherwise.<P>
   *
   * This method should always produce the correct results, unlike for other
   * classes in geom.
   * 
   * @param x the x-coordinate of the rectangle.
   * @param y the y-coordinate of the rectangle.
   * @param w the width of the the rectangle.
   * @param h the height of the rectangle.
   * @return <code>true</code> if the rectangle is considered contained
   */
  public boolean contains(double x, double y, double w, double h)
  {
    LineSegment[] l = new LineSegment[4];
    l[0] = new LineSegment(x, y, x + w, y);
    l[1] = new LineSegment(x, y + h, x + w, y + h);
    l[2] = new LineSegment(x, y, x, y + h);
    l[3] = new LineSegment(x + w, y, x + w, y + h);

    // Since every segment in the area must a contour
    // between inside/outside segments, ANY intersection
    // will mean the rectangle is not entirely contained.
    for (int i = 0; i < 4; i++)
      {
	for (int path = 0; path < solids.size(); path++)
	  {
	    Segment v;
	    Segment start;
	    start = v = (Segment) solids.elementAt(path);
	    do
	      {
		if (l[i].hasIntersections(v))
		  return false;
		v = v.next;
	      }
	    while (v != start);
	  }
	for (int path = 0; path < holes.size(); path++)
	  {
	    Segment v;
	    Segment start;
	    start = v = (Segment) holes.elementAt(path);
	    do
	      {
		if (l[i].hasIntersections(v))
		  return false;
		v = v.next;
	      }
	    while (v != start);
	  }
      }

    // Is any point inside?
    if (! contains(x, y))
      return false;

    // Final hoop: Is the rectangle non-intersecting and inside, 
    // but encloses a hole?
    Rectangle2D r = new Rectangle2D.Double(x, y, w, h);
    for (int path = 0; path < holes.size(); path++)
      if (! ((Segment) holes.elementAt(path)).isSegmentOutside(r))
        return false;

    return true;
  }

  /**
   * Determines if the Rectangle2D specified by r is completely contained
   * within this Area, returns false otherwise.<P>
   *
   * This method should always produce the correct results, unlike for other
   * classes in geom.
   * 
   * @param r the rectangle.
   * @return <code>true</code> if the rectangle is considered contained
   * 
   * @throws NullPointerException if <code>r</code> is <code>null</code>.
   */
  public boolean contains(Rectangle2D r)
  {
    return contains(r.getX(), r.getY(), r.getWidth(), r.getHeight());
  }

  /**
   * Determines if the rectangle specified by (x,y) as the upper-left
   * and with width w and height h intersects any part of this Area.
   * 
   * @param x  the x-coordinate for the rectangle.
   * @param y  the y-coordinate for the rectangle.
   * @param w  the width of the rectangle.
   * @param h  the height of the rectangle.
   * @return <code>true</code> if the rectangle intersects the area, 
   *         <code>false</code> otherwise.
   */
  public boolean intersects(double x, double y, double w, double h)
  {
    if (solids.size() == 0)
      return false;

    LineSegment[] l = new LineSegment[4];
    l[0] = new LineSegment(x, y, x + w, y);
    l[1] = new LineSegment(x, y + h, x + w, y + h);
    l[2] = new LineSegment(x, y, x, y + h);
    l[3] = new LineSegment(x + w, y, x + w, y + h);

    // Return true on any intersection
    for (int i = 0; i < 4; i++)
      {
	for (int path = 0; path < solids.size(); path++)
	  {
	    Segment v;
	    Segment start;
	    start = v = (Segment) solids.elementAt(path);
	    do
	      {
		if (l[i].hasIntersections(v))
		  return true;
		v = v.next;
	      }
	    while (v != start);
	  }
	for (int path = 0; path < holes.size(); path++)
	  {
	    Segment v;
	    Segment start;
	    start = v = (Segment) holes.elementAt(path);
	    do
	      {
		if (l[i].hasIntersections(v))
		  return true;
		v = v.next;
	      }
	    while (v != start);
	  }
      }

    // Non-intersecting, Is any point inside?
    if (contains(x + w * 0.5, y + h * 0.5))
      return true;

    // What if the rectangle encloses the whole shape?
    Point2D p = ((Segment) solids.elementAt(0)).getMidPoint();
    if ((new Rectangle2D.Double(x, y, w, h)).contains(p))
      return true;
    return false;
  }

  /**
   * Determines if the Rectangle2D specified by r intersects any
   * part of this Area.
   * @param r  the rectangle to test intersection with (<code>null</code>
   *           not permitted).
   * @return <code>true</code> if the rectangle intersects the area, 
   *         <code>false</code> otherwise.
   * @throws NullPointerException if <code>r</code> is <code>null</code>.
   */
  public boolean intersects(Rectangle2D r)
  {
    return intersects(r.getX(), r.getY(), r.getWidth(), r.getHeight());
  }

  /**
   * Returns a PathIterator object defining the contour of this Area,
   * transformed by at.
   * 
   * @param at  the transform.
   * @return A path iterator.
   */
  public PathIterator getPathIterator(AffineTransform at)
  {
    return (new AreaIterator(at));
  }

  /**
   * Returns a flattened PathIterator object defining the contour of this
   * Area, transformed by at and with a defined flatness.
   * 
   * @param at  the transform.
   * @param flatness the flatness.
   * @return A path iterator.
   */
  public PathIterator getPathIterator(AffineTransform at, double flatness)
  {
    return new FlatteningPathIterator(getPathIterator(at), flatness);
  }

  //---------------------------------------------------------------------
  // Non-public methods and classes 

  /**
   * Private pathiterator object.
   */
  private class AreaIterator implements PathIterator
  {
    private Vector segments;
    private int index;
    private AffineTransform at;

    // Simple compound type for segments
    class IteratorSegment
    {
      int type;
      double[] coords;

      IteratorSegment()
      {
	coords = new double[6];
      }
    }

    /**
     * The contructor here does most of the work,
     * creates a vector of IteratorSegments, which can
     * readily be returned
     */
    public AreaIterator(AffineTransform at)
    {
      this.at = at;
      index = 0;
      segments = new Vector();
      Vector allpaths = new Vector();
      allpaths.addAll(solids);
      allpaths.addAll(holes);

      for (int i = 0; i < allpaths.size(); i++)
        {
	  Segment v = (Segment) allpaths.elementAt(i);
	  Segment start = v;

	  IteratorSegment is = new IteratorSegment();
	  is.type = SEG_MOVETO;
	  is.coords[0] = start.P1.getX();
	  is.coords[1] = start.P1.getY();
	  segments.add(is);

	  do
	    {
	      is = new IteratorSegment();
	      is.type = v.pathIteratorFormat(is.coords);
	      segments.add(is);
	      v = v.next;
	    }
	  while (v != start);

	  is = new IteratorSegment();
	  is.type = SEG_CLOSE;
	  segments.add(is);
        }
    }

    public int currentSegment(double[] coords)
    {
      IteratorSegment s = (IteratorSegment) segments.elementAt(index);
      if (at != null)
	at.transform(s.coords, 0, coords, 0, 3);
      else
	for (int i = 0; i < 6; i++)
	  coords[i] = s.coords[i];
      return (s.type);
    }

    public int currentSegment(float[] coords)
    {
      IteratorSegment s = (IteratorSegment) segments.elementAt(index);
      double[] d = new double[6];
      if (at != null)
        {
	  at.transform(s.coords, 0, d, 0, 3);
	  for (int i = 0; i < 6; i++)
	    coords[i] = (float) d[i];
        }
      else
	for (int i = 0; i < 6; i++)
	  coords[i] = (float) s.coords[i];
      return (s.type);
    }

    // Note that the winding rule should not matter here,
    // EVEN_ODD is chosen because it renders faster.
    public int getWindingRule()
    {
      return (PathIterator.WIND_EVEN_ODD);
    }

    public boolean isDone()
    {
      return (index >= segments.size());
    }

    public void next()
    {
      index++;
    }
  }

  /**
   * Performs the fundamental task of the Weiler-Atherton algorithm,
   * traverse a list of segments, for each segment:
   * Follow it, removing segments from the list and switching paths
   * at each node. Do so until the starting segment is reached.
   *
   * Returns a Vector of the resulting paths.
   */
  private Vector weilerAtherton(Vector segments)
  {
    Vector paths = new Vector();
    while (segments.size() > 0)
      {
	// Iterate over the path
	Segment start = (Segment) segments.elementAt(0);
	Segment s = start;
	do
	  {
	    segments.remove(s);
	    if (s.node != null)
	      { // switch over
		s.next = s.node;
		s.node = null;
	      }
	    s = s.next; // continue
	  }
	while (s != start);

	paths.add(start);
      }
    return paths;
  }

  /**
   * A small wrapper class to store intersection points
   */
  private class Intersection
  {
    Point2D p; // the 2D point of intersection
    double ta; // the parametric value on a
    double tb; // the parametric value on b
    Segment seg; // segment placeholder for node setting

    public Intersection(Point2D p, double ta, double tb)
    {
      this.p = p;
      this.ta = ta;
      this.tb = tb;
    }
  }

  /**
   * Returns the recursion depth necessary to approximate the
   * curve by line segments within the error RS_EPSILON.
   *
   * This is done with Wang's formula:
   * L0 = max{0<=i<=N-2}(|xi - 2xi+1 + xi+2|,|yi - 2yi+1 + yi+2|)
   * r0 = log4(sqrt(2)*N*(N-1)*L0/8e)
   * Where e is the maximum distance error (RS_EPSILON)
   */
  private int getRecursionDepth(CubicSegment curve)
  {
    double x0 = curve.P1.getX();
    double y0 = curve.P1.getY();

    double x1 = curve.cp1.getX();
    double y1 = curve.cp1.getY();

    double x2 = curve.cp2.getX();
    double y2 = curve.cp2.getY();

    double x3 = curve.P2.getX();
    double y3 = curve.P2.getY();

    double L0 = Math.max(Math.max(Math.abs(x0 - 2 * x1 + x2),
                                  Math.abs(x1 - 2 * x2 + x3)),
                         Math.max(Math.abs(y0 - 2 * y1 + y2),
                                  Math.abs(y1 - 2 * y2 + y3)));

    double f = Math.sqrt(2) * 6.0 * L0 / (8.0 * RS_EPSILON);

    int r0 = (int) Math.ceil(Math.log(f) / Math.log(4.0));
    return (r0);
  }

  /**
   * Performs recursive subdivision:
   * @param c1 - curve 1
   * @param c2 - curve 2
   * @param depth1 - recursion depth of curve 1
   * @param depth2 - recursion depth of curve 2
   * @param t1 - global parametric value of the first curve's starting point
   * @param t2 - global parametric value of the second curve's starting point
   * @param w1 - global parametric length of curve 1
   * @param c1 - global parametric length of curve 2
   *
   * The final four parameters are for keeping track of the parametric
   * value of the curve. For a full curve t = 0, w = 1, w is halved with
   * each subdivision.
   */
  private void recursiveSubdivide(CubicCurve2D c1, CubicCurve2D c2,
                                  int depth1, int depth2, double t1,
                                  double t2, double w1, double w2)
  {
    boolean flat1 = depth1 <= 0;
    boolean flat2 = depth2 <= 0;

    if (flat1 && flat2)
      {
	double xlk = c1.getP2().getX() - c1.getP1().getX();
	double ylk = c1.getP2().getY() - c1.getP1().getY();

	double xnm = c2.getP2().getX() - c2.getP1().getX();
	double ynm = c2.getP2().getY() - c2.getP1().getY();

	double xmk = c2.getP1().getX() - c1.getP1().getX();
	double ymk = c2.getP1().getY() - c1.getP1().getY();
	double det = xnm * ylk - ynm * xlk;

	if (det + 1.0 == 1.0)
	  return;

	double detinv = 1.0 / det;
	double s = (xnm * ymk - ynm * xmk) * detinv;
	double t = (xlk * ymk - ylk * xmk) * detinv;
	if ((s < 0.0) || (s > 1.0) || (t < 0.0) || (t > 1.0))
	  return;

	double[] temp = new double[2];
	temp[0] = t1 + s * w1;
	temp[1] = t2 + t * w1;
	cc_intersections.add(temp);
	return;
      }

    CubicCurve2D.Double c11 = new CubicCurve2D.Double();
    CubicCurve2D.Double c12 = new CubicCurve2D.Double();
    CubicCurve2D.Double c21 = new CubicCurve2D.Double();
    CubicCurve2D.Double c22 = new CubicCurve2D.Double();

    if (! flat1 && ! flat2)
      {
	depth1--;
	depth2--;
	w1 = w1 * 0.5;
	w2 = w2 * 0.5;
	c1.subdivide(c11, c12);
	c2.subdivide(c21, c22);
	if (c11.getBounds2D().intersects(c21.getBounds2D()))
	  recursiveSubdivide(c11, c21, depth1, depth2, t1, t2, w1, w2);
	if (c11.getBounds2D().intersects(c22.getBounds2D()))
	  recursiveSubdivide(c11, c22, depth1, depth2, t1, t2 + w2, w1, w2);
	if (c12.getBounds2D().intersects(c21.getBounds2D()))
	  recursiveSubdivide(c12, c21, depth1, depth2, t1 + w1, t2, w1, w2);
	if (c12.getBounds2D().intersects(c22.getBounds2D()))
	  recursiveSubdivide(c12, c22, depth1, depth2, t1 + w1, t2 + w2, w1, w2);
	return;
      }

    if (! flat1)
      {
	depth1--;
	c1.subdivide(c11, c12);
	w1 = w1 * 0.5;
	if (c11.getBounds2D().intersects(c2.getBounds2D()))
	  recursiveSubdivide(c11, c2, depth1, depth2, t1, t2, w1, w2);
	if (c12.getBounds2D().intersects(c2.getBounds2D()))
	  recursiveSubdivide(c12, c2, depth1, depth2, t1 + w1, t2, w1, w2);
	return;
      }

    depth2--;
    c2.subdivide(c21, c22);
    w2 = w2 * 0.5;
    if (c1.getBounds2D().intersects(c21.getBounds2D()))
      recursiveSubdivide(c1, c21, depth1, depth2, t1, t2, w1, w2);
    if (c1.getBounds2D().intersects(c22.getBounds2D()))
      recursiveSubdivide(c1, c22, depth1, depth2, t1, t2 + w2, w1, w2);
  }

  /**
   * Returns a set of interesections between two Cubic segments
   * Or null if no intersections were found.
   *
   * The method used to find the intersection is recursive midpoint
   * subdivision. Outline description:
   *
   * 1) Check if the bounding boxes of the curves intersect,
   * 2) If so, divide the curves in the middle and test the bounding
   * boxes again,
   * 3) Repeat until a maximum recursion depth has been reached, where
   * the intersecting curves can be approximated by line segments.
   *
   * This is a reasonably accurate method, although the recursion depth
   * is typically around 20, the bounding-box tests allow for significant
   * pruning of the subdivision tree.
   */
  private Intersection[] cubicCubicIntersect(CubicSegment curve1,
                                             CubicSegment curve2)
  {
    Rectangle2D r1 = curve1.getBounds();
    Rectangle2D r2 = curve2.getBounds();

    if (! r1.intersects(r2))
      return null;

    cc_intersections = new Vector();
    recursiveSubdivide(curve1.getCubicCurve2D(), curve2.getCubicCurve2D(),
                       getRecursionDepth(curve1), getRecursionDepth(curve2),
                       0.0, 0.0, 1.0, 1.0);

    if (cc_intersections.size() == 0)
      return null;

    Intersection[] results = new Intersection[cc_intersections.size()];
    for (int i = 0; i < cc_intersections.size(); i++)
      {
	double[] temp = (double[]) cc_intersections.elementAt(i);
	results[i] = new Intersection(curve1.evaluatePoint(temp[0]), temp[0],
	                              temp[1]);
      }
    cc_intersections = null;
    return (results);
  }

  /**
   * Returns the intersections between a line and a quadratic bezier
   * Or null if no intersections are found1
   * This is done through combining the line's equation with the
   * parametric form of the Bezier and solving the resulting quadratic.
   */
  private Intersection[] lineQuadIntersect(LineSegment l, QuadSegment c)
  {
    double[] y = new double[3];
    double[] x = new double[3];
    double[] r = new double[3];
    int nRoots;
    double x0 = c.P1.getX();
    double y0 = c.P1.getY();
    double x1 = c.cp.getX();
    double y1 = c.cp.getY();
    double x2 = c.P2.getX();
    double y2 = c.P2.getY();

    double lx0 = l.P1.getX();
    double ly0 = l.P1.getY();
    double lx1 = l.P2.getX();
    double ly1 = l.P2.getY();
    double dx = lx1 - lx0;
    double dy = ly1 - ly0;

    // form r(t) = y(t) - x(t) for the bezier
    y[0] = y0;
    y[1] = 2 * (y1 - y0);
    y[2] = (y2 - 2 * y1 + y0);

    x[0] = x0;
    x[1] = 2 * (x1 - x0);
    x[2] = (x2 - 2 * x1 + x0);

    // a point, not a line
    if (dy == 0 && dx == 0)
      return null;

    // line on y axis
    if (dx == 0 || (dy / dx) > 1.0)
      {
	double k = dx / dy;
	x[0] -= lx0;
	y[0] -= ly0;
	y[0] *= k;
	y[1] *= k;
	y[2] *= k;
      }
    else
      {
	double k = dy / dx;
	x[0] -= lx0;
	y[0] -= ly0;
	x[0] *= k;
	x[1] *= k;
	x[2] *= k;
      }

    for (int i = 0; i < 3; i++)
      r[i] = y[i] - x[i];

    if ((nRoots = QuadCurve2D.solveQuadratic(r)) > 0)
      {
	Intersection[] temp = new Intersection[nRoots];
	int intersections = 0;
	for (int i = 0; i < nRoots; i++)
	  {
	    double t = r[i];
	    if (t >= 0.0 && t <= 1.0)
	      {
		Point2D p = c.evaluatePoint(t);

		// if the line is on an axis, snap the point to that axis.
		if (dx == 0)
		  p.setLocation(lx0, p.getY());
		if (dy == 0)
		  p.setLocation(p.getX(), ly0);

		if (p.getX() <= Math.max(lx0, lx1)
		    && p.getX() >= Math.min(lx0, lx1)
		    && p.getY() <= Math.max(ly0, ly1)
		    && p.getY() >= Math.min(ly0, ly1))
		  {
		    double lineparameter = p.distance(l.P1) / l.P2.distance(l.P1);
		    temp[i] = new Intersection(p, lineparameter, t);
		    intersections++;
		  }
	      }
	    else
	      temp[i] = null;
	  }
	if (intersections == 0)
	  return null;

	Intersection[] rValues = new Intersection[intersections];

	for (int i = 0; i < nRoots; i++)
	  if (temp[i] != null)
	    rValues[--intersections] = temp[i];
	return (rValues);
      }
    return null;
  }

  /**
   * Returns the intersections between a line and a cubic segment
   * This is done through combining the line's equation with the
   * parametric form of the Bezier and solving the resulting quadratic.
   */
  private Intersection[] lineCubicIntersect(LineSegment l, CubicSegment c)
  {
    double[] y = new double[4];
    double[] x = new double[4];
    double[] r = new double[4];
    int nRoots;
    double x0 = c.P1.getX();
    double y0 = c.P1.getY();
    double x1 = c.cp1.getX();
    double y1 = c.cp1.getY();
    double x2 = c.cp2.getX();
    double y2 = c.cp2.getY();
    double x3 = c.P2.getX();
    double y3 = c.P2.getY();

    double lx0 = l.P1.getX();
    double ly0 = l.P1.getY();
    double lx1 = l.P2.getX();
    double ly1 = l.P2.getY();
    double dx = lx1 - lx0;
    double dy = ly1 - ly0;

    // form r(t) = y(t) - x(t) for the bezier
    y[0] = y0;
    y[1] = 3 * (y1 - y0);
    y[2] = 3 * (y2 + y0 - 2 * y1);
    y[3] = y3 - 3 * y2 + 3 * y1 - y0;

    x[0] = x0;
    x[1] = 3 * (x1 - x0);
    x[2] = 3 * (x2 + x0 - 2 * x1);
    x[3] = x3 - 3 * x2 + 3 * x1 - x0;

    // a point, not a line
    if (dy == 0 && dx == 0)
      return null;

    // line on y axis
    if (dx == 0 || (dy / dx) > 1.0)
      {
	double k = dx / dy;
	x[0] -= lx0;
	y[0] -= ly0;
	y[0] *= k;
	y[1] *= k;
	y[2] *= k;
	y[3] *= k;
      }
    else
      {
	double k = dy / dx;
	x[0] -= lx0;
	y[0] -= ly0;
	x[0] *= k;
	x[1] *= k;
	x[2] *= k;
	x[3] *= k;
      }
    for (int i = 0; i < 4; i++)
      r[i] = y[i] - x[i];

    if ((nRoots = CubicCurve2D.solveCubic(r)) > 0)
      {
	Intersection[] temp = new Intersection[nRoots];
	int intersections = 0;
	for (int i = 0; i < nRoots; i++)
	  {
	    double t = r[i];
	    if (t >= 0.0 && t <= 1.0)
	      {
		// if the line is on an axis, snap the point to that axis.
		Point2D p = c.evaluatePoint(t);
		if (dx == 0)
		  p.setLocation(lx0, p.getY());
		if (dy == 0)
		  p.setLocation(p.getX(), ly0);

		if (p.getX() <= Math.max(lx0, lx1)
		    && p.getX() >= Math.min(lx0, lx1)
		    && p.getY() <= Math.max(ly0, ly1)
		    && p.getY() >= Math.min(ly0, ly1))
		  {
		    double lineparameter = p.distance(l.P1) / l.P2.distance(l.P1);
		    temp[i] = new Intersection(p, lineparameter, t);
		    intersections++;
		  }
	      }
	    else
	      temp[i] = null;
	  }

	if (intersections == 0)
	  return null;

	Intersection[] rValues = new Intersection[intersections];
	for (int i = 0; i < nRoots; i++)
	  if (temp[i] != null)
	    rValues[--intersections] = temp[i];
	return (rValues);
      }
    return null;
  }

  /**
   * Returns the intersection between two lines, or null if there is no
   * intersection.
   */
  private Intersection linesIntersect(LineSegment a, LineSegment b)
  {
    Point2D P1 = a.P1;
    Point2D P2 = a.P2;
    Point2D P3 = b.P1;
    Point2D P4 = b.P2;

    if (! Line2D.linesIntersect(P1.getX(), P1.getY(), P2.getX(), P2.getY(),
                                P3.getX(), P3.getY(), P4.getX(), P4.getY()))
      return null;

    double x1 = P1.getX();
    double y1 = P1.getY();
    double rx = P2.getX() - x1;
    double ry = P2.getY() - y1;

    double x2 = P3.getX();
    double y2 = P3.getY();
    double sx = P4.getX() - x2;
    double sy = P4.getY() - y2;

    double determinant = sx * ry - sy * rx;
    double nom = (sx * (y2 - y1) + sy * (x1 - x2));

    // Parallel lines don't intersect. At least we pretend they don't.
    if (Math.abs(determinant) < EPSILON)
      return null;

    nom = nom / determinant;

    if (nom == 0.0)
      return null;
    if (nom == 1.0)
      return null;

    Point2D p = new Point2D.Double(x1 + nom * rx, y1 + nom * ry);

    return new Intersection(p, p.distance(P1) / P1.distance(P2),
                            p.distance(P3) / P3.distance(P4));
  }

  /**
   * Determines if two points are equal, within an error margin
   * 'snap distance'
   */
  private boolean pointEquals(Point2D a, Point2D b)
  {
    return (a.equals(b) || a.distance(b) < PE_EPSILON);
  }

  /**
   * Helper method
   * Turns a shape into a Vector of Segments
   */
  private Vector makeSegment(Shape s)
  {
    Vector paths = new Vector();
    PathIterator pi = s.getPathIterator(null);
    double[] coords = new double[6];
    Segment subpath = null;
    Segment current = null;
    double cx;
    double cy;
    double subpathx;
    double subpathy;
    cx = cy = subpathx = subpathy = 0.0;

    this.windingRule = pi.getWindingRule();

    while (! pi.isDone())
      {
	Segment v;
	switch (pi.currentSegment(coords))
	  {
	  case PathIterator.SEG_MOVETO:
	    if (subpath != null)
	      { // close existing open path
		current.next = new LineSegment(cx, cy, subpathx, subpathy);
		current = current.next;
		current.next = subpath;
	      }
	    subpath = null;
	    subpathx = cx = coords[0];
	    subpathy = cy = coords[1];
	    break;

	  // replace 'close' with a line-to.
	  case PathIterator.SEG_CLOSE:
	    if (subpath != null && (subpathx != cx || subpathy != cy))
	      {
		current.next = new LineSegment(cx, cy, subpathx, subpathy);
		current = current.next;
		current.next = subpath;
		cx = subpathx;
		cy = subpathy;
		subpath = null;
	      }
	    else if (subpath != null)
	      {
		current.next = subpath;
		subpath = null;
	      }
	    break;
	  case PathIterator.SEG_LINETO:
	    if (cx != coords[0] || cy != coords[1])
	      {
		v = new LineSegment(cx, cy, coords[0], coords[1]);
		if (subpath == null)
		  {
		    subpath = current = v;
		    paths.add(subpath);
		  }
		else
		  {
		    current.next = v;
		    current = current.next;
		  }
		cx = coords[0];
		cy = coords[1];
	      }
	    break;
	  case PathIterator.SEG_QUADTO:
	    v = new QuadSegment(cx, cy, coords[0], coords[1], coords[2],
	                        coords[3]);
	    if (subpath == null)
	      {
		subpath = current = v;
		paths.add(subpath);
	      }
	    else
	      {
		current.next = v;
		current = current.next;
	      }
	    cx = coords[2];
	    cy = coords[3];
	    break;
	  case PathIterator.SEG_CUBICTO:
	    v = new CubicSegment(cx, cy, coords[0], coords[1], coords[2],
	                         coords[3], coords[4], coords[5]);
	    if (subpath == null)
	      {
		subpath = current = v;
		paths.add(subpath);
	      }
	    else
	      {
		current.next = v;
		current = current.next;
	      }

	    // check if the cubic is self-intersecting
	    double[] lpts = ((CubicSegment) v).getLoop();
	    if (lpts != null)
	      {
		// if it is, break off the loop into its own path.
		v.subdivideInsert(lpts[0]);
		v.next.subdivideInsert((lpts[1] - lpts[0]) / (1.0 - lpts[0]));

		CubicSegment loop = (CubicSegment) v.next;
		v.next = loop.next;
		loop.next = loop;

		v.P2 = v.next.P1 = loop.P2 = loop.P1; // snap points
		paths.add(loop);
		current = v.next;
	      }

	    cx = coords[4];
	    cy = coords[5];
	    break;
	  }
	pi.next();
      }

    if (subpath != null)
      { // close any open path
	if (subpathx != cx || subpathy != cy)
	  {
	    current.next = new LineSegment(cx, cy, subpathx, subpathy);
	    current = current.next;
	    current.next = subpath;
	  }
	else
	  current.next = subpath;
      }

    if (paths.size() == 0)
      return (null);

    return (paths);
  }

  /**
   * Find the intersections of two separate closed paths,
   * A and B, split the segments at the intersection points,
   * and create nodes pointing from one to the other
   */
  private int createNodes(Segment A, Segment B)
  {
    int nNodes = 0;

    Segment a = A;
    Segment b = B;

    do
      {
	do
	  {
	    nNodes += a.splitIntersections(b);
	    b = b.next;
	  }
	while (b != B);

	a = a.next; // move to the next segment
      }
    while (a != A); // until one wrap.

    return (nNodes);
  }

  /**
   * Find the intersections of a path with itself.
   * Splits the segments at the intersection points,
   * and create nodes pointing from one to the other.
   */
  private int createNodesSelf(Segment A)
  {
    int nNodes = 0;
    Segment a = A;

    if (A.next == A)
      return 0;

    do
      {
	Segment b = a.next;
	do
	  {
	    if (b != a) // necessary
	      nNodes += a.splitIntersections(b);
	    b = b.next;
	  }
	while (b != A);
	a = a.next; // move to the next segment
      }
    while (a != A); // until one wrap.

    return (nNodes);
  }

  /**
   * Deletes paths which are redundant from a list, (i.e. solid areas within
   * solid areas) Clears any nodes. Sorts the remaining paths into solids
   * and holes, sets their orientation and sets the solids and holes lists.
   */
  private void deleteRedundantPaths(Vector paths)
  {
    int npaths = paths.size();

    int[][] contains = new int[npaths][npaths];
    int[][] windingNumbers = new int[npaths][2];
    int neg;
    Rectangle2D[] bb = new Rectangle2D[npaths]; // path bounding boxes

    neg = ((windingRule == PathIterator.WIND_NON_ZERO) ? -1 : 1);

    for (int i = 0; i < npaths; i++)
      bb[i] = ((Segment) paths.elementAt(i)).getPathBounds();

    // Find which path contains which, assign winding numbers
    for (int i = 0; i < npaths; i++)
      {
	Segment pathA = (Segment) paths.elementAt(i);
	pathA.nullNodes(); // remove any now-redundant nodes, in case.
	int windingA = pathA.hasClockwiseOrientation() ? 1 : neg;

	for (int j = 0; j < npaths; j++)
	  if (i != j)
	    {
	      Segment pathB = (Segment) paths.elementAt(j);

	      // A contains B
	      if (bb[i].intersects(bb[j]))
	        {
		  Segment s = pathB.next;
		  while (s.P1.getY() == s.P2.getY() && s != pathB)
		    s = s.next;
		  Point2D p = s.getMidPoint();
		  if (pathA.contains(p.getX(), p.getY()))
		    contains[i][j] = windingA;
	        }
	      else
		// A does not contain B
		contains[i][j] = 0;
	    }
	  else
	    contains[i][j] = windingA; // i == j
      }

    for (int i = 0; i < npaths; i++)
      {
	windingNumbers[i][0] = 0;
	for (int j = 0; j < npaths; j++)
	  windingNumbers[i][0] += contains[j][i];
	windingNumbers[i][1] = contains[i][i];
      }

    Vector solids = new Vector();
    Vector holes = new Vector();

    if (windingRule == PathIterator.WIND_NON_ZERO)
      {
	for (int i = 0; i < npaths; i++)
	  {
	    if (windingNumbers[i][0] == 0)
	      holes.add(paths.elementAt(i));
	    else if (windingNumbers[i][0] - windingNumbers[i][1] == 0
	             && Math.abs(windingNumbers[i][0]) == 1)
	      solids.add(paths.elementAt(i));
	  }
      }
    else
      {
	windingRule = PathIterator.WIND_NON_ZERO;
	for (int i = 0; i < npaths; i++)
	  {
	    if ((windingNumbers[i][0] & 1) == 0)
	      holes.add(paths.elementAt(i));
	    else if ((windingNumbers[i][0] & 1) == 1)
	      solids.add(paths.elementAt(i));
	  }
      }

    setDirection(holes, false);
    setDirection(solids, true);
    this.holes = holes;
    this.solids = solids;
  }

  /**
   * Sets the winding direction of a Vector of paths
   * @param clockwise gives the direction,
   * true = clockwise, false = counter-clockwise
   */
  private void setDirection(Vector paths, boolean clockwise)
  {
    Segment v;
    for (int i = 0; i < paths.size(); i++)
      {
	v = (Segment) paths.elementAt(i);
	if (clockwise != v.hasClockwiseOrientation())
	  v.reverseAll();
      }
  }

  /**
   * Class representing a linked-list of vertices forming a closed polygon,
   * convex or concave, without holes.
   */
  private abstract class Segment implements Cloneable
  {
    // segment type, PathIterator segment types are used.
    Point2D P1;
    Point2D P2;
    Segment next;
    Segment node;

    Segment()
    {
      P1 = P2 = null;
      node = next = null;
    }

    /**
     * Reverses the direction of a single segment
     */
    abstract void reverseCoords();

    /**
     * Returns the segment's midpoint
     */
    abstract Point2D getMidPoint();

    /**
     * Returns the bounding box of this segment
     */
    abstract Rectangle2D getBounds();

    /**
     * Transforms a single segment
     */
    abstract void transform(AffineTransform at);

    /**
     * Returns the PathIterator type of a segment
     */
    abstract int getType();

    /**
     */
    abstract int splitIntersections(Segment b);

    /**
     * Returns the PathIterator coords of a segment
     */
    abstract int pathIteratorFormat(double[] coords);

    /**
     * Returns the number of intersections on the positive X axis,
     * with the origin at (x,y), used for contains()-testing
     *
     * (Although that could be done by the line-intersect methods,
     * a dedicated method is better to guarantee consitent handling
     * of endpoint-special-cases)
     */
    abstract int rayCrossing(double x, double y);

    /**
     * Subdivides the segment at parametric value t, inserting
     * the new segment into the linked list after this,
     * such that this becomes [0,t] and this.next becomes [t,1]
     */
    abstract void subdivideInsert(double t);

    /**
     * Returns twice the area of a curve, relative the P1-P2 line
     * Used for area calculations.
     */
    abstract double curveArea();

    /**
     * Compare two segments.
     */
    abstract boolean equals(Segment b);

    /**
     * Determines if this path of segments contains the point (x,y)
     */
    boolean contains(double x, double y)
    {
      Segment v = this;
      int crossings = 0;
      do
        {
	  int n = v.rayCrossing(x, y);
	  crossings += n;
	  v = v.next;
        }
      while (v != this);
      return ((crossings & 1) == 1);
    }

    /**
     * Nulls all nodes of the path. Clean up any 'hairs'.
     */
    void nullNodes()
    {
      Segment v = this;
      do
        {
	  v.node = null;
	  v = v.next;
        }
      while (v != this);
    }

    /**
     * Transforms each segment in the closed path
     */
    void transformSegmentList(AffineTransform at)
    {
      Segment v = this;
      do
        {
	  v.transform(at);
	  v = v.next;
        }
      while (v != this);
    }

    /**
     * Determines the winding direction of the path
     * By the sign of the area.
     */
    boolean hasClockwiseOrientation()
    {
      return (getSignedArea() > 0.0);
    }

    /**
     * Returns the bounds of this path
     */
    public Rectangle2D getPathBounds()
    {
      double xmin;
      double xmax;
      double ymin;
      double ymax;
      xmin = xmax = P1.getX();
      ymin = ymax = P1.getY();

      Segment v = this;
      do
        {
	  Rectangle2D r = v.getBounds();
	  xmin = Math.min(r.getMinX(), xmin);
	  ymin = Math.min(r.getMinY(), ymin);
	  xmax = Math.max(r.getMaxX(), xmax);
	  ymax = Math.max(r.getMaxY(), ymax);
	  v = v.next;
        }
      while (v != this);

      return (new Rectangle2D.Double(xmin, ymin, (xmax - xmin), (ymax - ymin)));
    }

    /**
     * Calculates twice the signed area of the path;
     */
    double getSignedArea()
    {
      Segment s;
      double area = 0.0;

      s = this;
      do
        {
	  area += s.curveArea();

	  area += s.P1.getX() * s.next.P1.getY()
	  - s.P1.getY() * s.next.P1.getX();
	  s = s.next;
        }
      while (s != this);

      return area;
    }

    /**
     * Reverses the orientation of the whole polygon
     */
    void reverseAll()
    {
      reverseCoords();
      Segment v = next;
      Segment former = this;
      while (v != this)
        {
	  v.reverseCoords();
	  Segment vnext = v.next;
	  v.next = former;
	  former = v;
	  v = vnext;
        }
      next = former;
    }

    /**
     * Inserts a Segment after this one
     */
    void insert(Segment v)
    {
      Segment n = next;
      next = v;
      v.next = n;
    }

    /**
     * Returns if this segment path is polygonal
     */
    boolean isPolygonal()
    {
      Segment v = this;
      do
        {
	  if (! (v instanceof LineSegment))
	    return false;
	  v = v.next;
        }
      while (v != this);
      return true;
    }

    /**
     * Clones this path
     */
    Segment cloneSegmentList() throws CloneNotSupportedException
    {
      Vector list = new Vector();
      Segment v = next;

      while (v != this)
        {
	  list.add(v);
	  v = v.next;
        }

      Segment clone = (Segment) this.clone();
      v = clone;
      for (int i = 0; i < list.size(); i++)
        {
	  clone.next = (Segment) ((Segment) list.elementAt(i)).clone();
	  clone = clone.next;
        }
      clone.next = v;
      return v;
    }

    /**
     * Creates a node between this segment and segment b
     * at the given intersection
     * @return the number of nodes created (0 or 1)
     */
    int createNode(Segment b, Intersection i)
    {
      Point2D p = i.p;
      if ((pointEquals(P1, p) || pointEquals(P2, p))
          && (pointEquals(b.P1, p) || pointEquals(b.P2, p)))
	return 0;

      subdivideInsert(i.ta);
      b.subdivideInsert(i.tb);

      // snap points
      b.P2 = b.next.P1 = P2 = next.P1 = i.p;

      node = b.next;
      b.node = next;
      return 1;
    }

    /**
     * Creates multiple nodes from a list of intersections,
     * This must be done in the order of ascending parameters,
     * and the parameters must be recalculated in accordance
     * with each split.
     * @return the number of nodes created
     */
    protected int createNodes(Segment b, Intersection[] x)
    {
      Vector v = new Vector();
      for (int i = 0; i < x.length; i++)
        {
	  Point2D p = x[i].p;
	  if (! ((pointEquals(P1, p) || pointEquals(P2, p))
	      && (pointEquals(b.P1, p) || pointEquals(b.P2, p))))
	    v.add(x[i]);
        }

      int nNodes = v.size();
      Intersection[] A = new Intersection[nNodes];
      Intersection[] B = new Intersection[nNodes];
      for (int i = 0; i < nNodes; i++)
	A[i] = B[i] = (Intersection) v.elementAt(i);

      // Create two lists sorted by the parameter
      // Bubble sort, OK I suppose, since the number of intersections
      // cannot be larger than 9 (cubic-cubic worst case) anyway
      for (int i = 0; i < nNodes - 1; i++)
        {
	  for (int j = i + 1; j < nNodes; j++)
	    {
	      if (A[i].ta > A[j].ta)
	        {
		  Intersection swap = A[i];
		  A[i] = A[j];
		  A[j] = swap;
	        }
	      if (B[i].tb > B[j].tb)
	        {
		  Intersection swap = B[i];
		  B[i] = B[j];
		  B[j] = swap;
	        }
	    }
        }
      // subdivide a
      Segment s = this;
      for (int i = 0; i < nNodes; i++)
        {
	  s.subdivideInsert(A[i].ta);

	  // renormalize the parameters
	  for (int j = i + 1; j < nNodes; j++)
	    A[j].ta = (A[j].ta - A[i].ta) / (1.0 - A[i].ta);

	  A[i].seg = s;
	  s = s.next;
        }

      // subdivide b, set nodes
      s = b;
      for (int i = 0; i < nNodes; i++)
        {
	  s.subdivideInsert(B[i].tb);

	  for (int j = i + 1; j < nNodes; j++)
	    B[j].tb = (B[j].tb - B[i].tb) / (1.0 - B[i].tb);

	  // set nodes
	  B[i].seg.node = s.next; // node a -> b 
	  s.node = B[i].seg.next; // node b -> a 

	  // snap points
	  B[i].seg.P2 = B[i].seg.next.P1 = s.P2 = s.next.P1 = B[i].p;
	  s = s.next;
        }
      return nNodes;
    }

    /**
     * Determines if two paths are equal.
     * Colinear line segments are ignored in the comparison.
     */
    boolean pathEquals(Segment B)
    {
      if (! getPathBounds().equals(B.getPathBounds()))
	return false;

      Segment startA = getTopLeft();
      Segment startB = B.getTopLeft();
      Segment a = startA;
      Segment b = startB;
      do
        {
	  if (! a.equals(b))
	    return false;

	  if (a instanceof LineSegment)
	    a = ((LineSegment) a).lastCoLinear();
	  if (b instanceof LineSegment)
	    b = ((LineSegment) b).lastCoLinear();

	  a = a.next;
	  b = b.next;
        }
      while (a != startA && b != startB);
      return true;
    }

    /**
     * Return the segment with the top-leftmost first point
     */
    Segment getTopLeft()
    {
      Segment v = this;
      Segment tl = this;
      do
        {
	  if (v.P1.getY() < tl.P1.getY())
	    tl = v;
	  else if (v.P1.getY() == tl.P1.getY())
	    {
	      if (v.P1.getX() < tl.P1.getX())
		tl = v;
	    }
	  v = v.next;
        }
      while (v != this);
      return tl;
    }

    /**
     * Returns if the path has a segment outside a shape
     */
    boolean isSegmentOutside(Shape shape)
    {
      return ! shape.contains(getMidPoint());
    }
  } // class Segment

  private class LineSegment extends Segment
  {
    public LineSegment(double x1, double y1, double x2, double y2)
    {
      super();
      P1 = new Point2D.Double(x1, y1);
      P2 = new Point2D.Double(x2, y2);
    }

    public LineSegment(Point2D p1, Point2D p2)
    {
      super();
      P1 = (Point2D) p1.clone();
      P2 = (Point2D) p2.clone();
    }

    /**
     * Clones this segment
     */
    public Object clone()
    {
      return new LineSegment(P1, P2);
    }

    /**
     * Transforms the segment
     */
    void transform(AffineTransform at)
    {
      P1 = at.transform(P1, null);
      P2 = at.transform(P2, null);
    }

    /**
     * Swap start and end points
     */
    void reverseCoords()
    {
      Point2D p = P1;
      P1 = P2;
      P2 = p;
    }

    /**
     * Returns the segment's midpoint
     */
    Point2D getMidPoint()
    {
      return (new Point2D.Double(0.5 * (P1.getX() + P2.getX()),
                                 0.5 * (P1.getY() + P2.getY())));
    }

    /**
     * Returns twice the area of a curve, relative the P1-P2 line
     * Obviously, a line does not enclose any area besides the line
     */
    double curveArea()
    {
      return 0;
    }

    /**
     * Returns the PathIterator type of a segment
     */
    int getType()
    {
      return (PathIterator.SEG_LINETO);
    }

    /**
     * Subdivides the segment at parametric value t, inserting
     * the new segment into the linked list after this,
     * such that this becomes [0,t] and this.next becomes [t,1]
     */
    void subdivideInsert(double t)
    {
      Point2D p = new Point2D.Double((P2.getX() - P1.getX()) * t + P1.getX(),
                                     (P2.getY() - P1.getY()) * t + P1.getY());
      insert(new LineSegment(p, P2));
      P2 = p;
      next.node = node;
      node = null;
    }

    /**
     * Determines if two line segments are strictly colinear
     */
    boolean isCoLinear(LineSegment b)
    {
      double x1 = P1.getX();
      double y1 = P1.getY();
      double x2 = P2.getX();
      double y2 = P2.getY();
      double x3 = b.P1.getX();
      double y3 = b.P1.getY();
      double x4 = b.P2.getX();
      double y4 = b.P2.getY();

      if ((y1 - y3) * (x4 - x3) - (x1 - x3) * (y4 - y3) != 0.0)
	return false;

      return ((x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3) == 0.0);
    }

    /**
     * Return the last segment colinear with this one.
     * Used in comparing paths.
     */
    Segment lastCoLinear()
    {
      Segment prev = this;
      Segment v = next;

      while (v instanceof LineSegment)
        {
	  if (isCoLinear((LineSegment) v))
	    {
	      prev = v;
	      v = v.next;
	    }
	  else
	    return prev;
        }
      return prev;
    }

    /**
     * Compare two segments.
     * We must take into account that the lines may be broken into colinear
     * subsegments and ignore them.
     */
    boolean equals(Segment b)
    {
      if (! (b instanceof LineSegment))
	return false;
      Point2D p1 = P1;
      Point2D p3 = b.P1;

      if (! p1.equals(p3))
	return false;

      Point2D p2 = lastCoLinear().P2;
      Point2D p4 = ((LineSegment) b).lastCoLinear().P2;
      return (p2.equals(p4));
    }

    /**
     * Returns a line segment
     */
    int pathIteratorFormat(double[] coords)
    {
      coords[0] = P2.getX();
      coords[1] = P2.getY();
      return (PathIterator.SEG_LINETO);
    }

    /**
     * Returns if the line has intersections.
     */
    boolean hasIntersections(Segment b)
    {
      if (b instanceof LineSegment)
	return (linesIntersect(this, (LineSegment) b) != null);

      if (b instanceof QuadSegment)
	return (lineQuadIntersect(this, (QuadSegment) b) != null);

      if (b instanceof CubicSegment)
	return (lineCubicIntersect(this, (CubicSegment) b) != null);

      return false;
    }

    /**
     * Splits intersections into nodes,
     * This one handles line-line, line-quadratic, line-cubic
     */
    int splitIntersections(Segment b)
    {
      if (b instanceof LineSegment)
        {
	  Intersection i = linesIntersect(this, (LineSegment) b);

	  if (i == null)
	    return 0;

	  return createNode(b, i);
        }

      Intersection[] x = null;

      if (b instanceof QuadSegment)
	x = lineQuadIntersect(this, (QuadSegment) b);

      if (b instanceof CubicSegment)
	x = lineCubicIntersect(this, (CubicSegment) b);

      if (x == null)
	return 0;

      if (x.length == 1)
	return createNode(b, (Intersection) x[0]);

      return createNodes(b, x);
    }

    /**
     * Returns the bounding box of this segment
     */
    Rectangle2D getBounds()
    {
      return (new Rectangle2D.Double(Math.min(P1.getX(), P2.getX()),
                                     Math.min(P1.getY(), P2.getY()),
                                     Math.abs(P1.getX() - P2.getX()),
                                     Math.abs(P1.getY() - P2.getY())));
    }

    /**
     * Returns the number of intersections on the positive X axis,
     * with the origin at (x,y), used for contains()-testing
     */
    int rayCrossing(double x, double y)
    {
      double x0 = P1.getX() - x;
      double y0 = P1.getY() - y;
      double x1 = P2.getX() - x;
      double y1 = P2.getY() - y;

      if (y0 * y1 > 0)
	return 0;

      if (x0 < 0 && x1 < 0)
	return 0;

      if (y0 == 0.0)
	y0 -= EPSILON;

      if (y1 == 0.0)
	y1 -= EPSILON;

      if (Line2D.linesIntersect(x0, y0, x1, y1, 
				EPSILON, 0.0, Double.MAX_VALUE, 0.0))
	return 1;
      return 0;
    }
  } // class LineSegment

  /**
   * Quadratic Bezier curve segment
   *
   * Note: Most peers don't support quadratics directly, so it might make
   * sense to represent them as cubics internally and just be done with it.
   * I think we should be peer-agnostic, however, and stay faithful to the
   * input geometry types as far as possible.
   */
  private class QuadSegment extends Segment
  {
    Point2D cp; // control point

    /**
     * Constructor, takes the coordinates of the start, control,
     * and end point, respectively.
     */
    QuadSegment(double x1, double y1, double cx, double cy, double x2,
                double y2)
    {
      super();
      P1 = new Point2D.Double(x1, y1);
      P2 = new Point2D.Double(x2, y2);
      cp = new Point2D.Double(cx, cy);
    }

    /**
     * Clones this segment
     */
    public Object clone()
    {
      return new QuadSegment(P1.getX(), P1.getY(), cp.getX(), cp.getY(),
                             P2.getX(), P2.getY());
    }

    /**
     * Returns twice the area of a curve, relative the P1-P2 line
     *
     * The area formula can be derived by using Green's formula in the
     * plane on the parametric form of the bezier.
     */
    double curveArea()
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp.getX();
      double y1 = cp.getY();
      double x2 = P2.getX();
      double y2 = P2.getY();

      double P = (y2 - 2 * y1 + y0);
      double Q = 2 * (y1 - y0);

      double A = (x2 - 2 * x1 + x0);
      double B = 2 * (x1 - x0);

      double area = (B * P - A * Q) / 3.0;
      return (area);
    }

    /**
     * Compare two segments.
     */
    boolean equals(Segment b)
    {
      if (! (b instanceof QuadSegment))
	return false;

      return (P1.equals(b.P1) && cp.equals(((QuadSegment) b).cp)
             && P2.equals(b.P2));
    }

    /**
     * Returns a Point2D corresponding to the parametric value t
     * of the curve
     */
    Point2D evaluatePoint(double t)
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp.getX();
      double y1 = cp.getY();
      double x2 = P2.getX();
      double y2 = P2.getY();

      return new Point2D.Double(t * t * (x2 - 2 * x1 + x0) + 2 * t * (x1 - x0)
                                + x0,
                                t * t * (y2 - 2 * y1 + y0) + 2 * t * (y1 - y0)
                                + y0);
    }

    /**
     * Returns the bounding box of this segment
     */
    Rectangle2D getBounds()
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp.getX();
      double y1 = cp.getY();
      double x2 = P2.getX();
      double y2 = P2.getY();
      double r0;
      double r1;

      double xmax = Math.max(x0, x2);
      double ymax = Math.max(y0, y2);
      double xmin = Math.min(x0, x2);
      double ymin = Math.min(y0, y2);

      r0 = 2 * (y1 - y0);
      r1 = 2 * (y2 - 2 * y1 + y0);
      if (r1 != 0.0)
        {
	  double t = -r0 / r1;
	  if (t > 0.0 && t < 1.0)
	    {
	      double y = evaluatePoint(t).getY();
	      ymax = Math.max(y, ymax);
	      ymin = Math.min(y, ymin);
	    }
        }
      r0 = 2 * (x1 - x0);
      r1 = 2 * (x2 - 2 * x1 + x0);
      if (r1 != 0.0)
        {
	  double t = -r0 / r1;
	  if (t > 0.0 && t < 1.0)
	    {
	      double x = evaluatePoint(t).getY();
	      xmax = Math.max(x, xmax);
	      xmin = Math.min(x, xmin);
	    }
        }

      return (new Rectangle2D.Double(xmin, ymin, xmax - xmin, ymax - ymin));
    }

    /**
     * Returns a cubic segment corresponding to this curve
     */
    CubicSegment getCubicSegment()
    {
      double x1 = P1.getX() + 2.0 * (cp.getX() - P1.getX()) / 3.0;
      double y1 = P1.getY() + 2.0 * (cp.getY() - P1.getY()) / 3.0;
      double x2 = cp.getX() + (P2.getX() - cp.getX()) / 3.0;
      double y2 = cp.getY() + (P2.getY() - cp.getY()) / 3.0;

      return new CubicSegment(P1.getX(), P1.getY(), x1, y1, x2, y2, P2.getX(),
                              P2.getY());
    }

    /**
     * Returns the segment's midpoint
     */
    Point2D getMidPoint()
    {
      return evaluatePoint(0.5);
    }

    /**
     * Returns the PathIterator type of a segment
     */
    int getType()
    {
      return (PathIterator.SEG_QUADTO);
    }

    /**
     * Returns the PathIterator coords of a segment
     */
    int pathIteratorFormat(double[] coords)
    {
      coords[0] = cp.getX();
      coords[1] = cp.getY();
      coords[2] = P2.getX();
      coords[3] = P2.getY();
      return (PathIterator.SEG_QUADTO);
    }

    /**
     * Returns the number of intersections on the positive X axis,
     * with the origin at (x,y), used for contains()-testing
     */
    int rayCrossing(double x, double y)
    {
      double x0 = P1.getX() - x;
      double y0 = P1.getY() - y;
      double x1 = cp.getX() - x;
      double y1 = cp.getY() - y;
      double x2 = P2.getX() - x;
      double y2 = P2.getY() - y;
      double[] r = new double[3];
      int nRoots;
      int nCrossings = 0;

      /* check if curve may intersect X+ axis. */
      if ((x0 > 0.0 || x1 > 0.0 || x2 > 0.0) && (y0 * y1 <= 0 || y1 * y2 <= 0))
        {
	  if (y0 == 0.0)
	    y0 -= EPSILON;
	  if (y2 == 0.0)
	    y2 -= EPSILON;

	  r[0] = y0;
	  r[1] = 2 * (y1 - y0);
	  r[2] = (y2 - 2 * y1 + y0);

	  nRoots = QuadCurve2D.solveQuadratic(r);
	  for (int i = 0; i < nRoots; i++)
	    if (r[i] > 0.0f && r[i] < 1.0f)
	      {
		double t = r[i];
		if (t * t * (x2 - 2 * x1 + x0) + 2 * t * (x1 - x0) + x0 > 0.0)
		  nCrossings++;
	      }
        }
      return nCrossings;
    }

    /**
     * Swap start and end points
     */
    void reverseCoords()
    {
      Point2D temp = P1;
      P1 = P2;
      P2 = temp;
    }

    /**
     * Splits intersections into nodes,
     * This one handles quadratic-quadratic only,
     * Quadratic-line is passed on to the LineSegment class,
     * Quadratic-cubic is passed on to the CubicSegment class
     */
    int splitIntersections(Segment b)
    {
      if (b instanceof LineSegment)
	return (b.splitIntersections(this));

      if (b instanceof CubicSegment)
	return (b.splitIntersections(this));

      if (b instanceof QuadSegment)
        {
	  // Use the cubic-cubic intersection routine for quads as well,
	  // Since a quadratic can be exactly described as a cubic, this
	  // should not be a problem; 
	  // The recursion depth will be the same in any case.
	  Intersection[] x = cubicCubicIntersect(getCubicSegment(),
	                                         ((QuadSegment) b)
	                                         .getCubicSegment());
	  if (x == null)
	    return 0;

	  if (x.length == 1)
	    return createNode(b, (Intersection) x[0]);

	  return createNodes(b, x);
        }
      return 0;
    }

    /**
     * Subdivides the segment at parametric value t, inserting
     * the new segment into the linked list after this,
     * such that this becomes [0,t] and this.next becomes [t,1]
     */
    void subdivideInsert(double t)
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp.getX();
      double y1 = cp.getY();
      double x2 = P2.getX();
      double y2 = P2.getY();

      double p10x = x0 + t * (x1 - x0);
      double p10y = y0 + t * (y1 - y0);
      double p11x = x1 + t * (x2 - x1);
      double p11y = y1 + t * (y2 - y1);
      double p20x = p10x + t * (p11x - p10x);
      double p20y = p10y + t * (p11y - p10y);

      insert(new QuadSegment(p20x, p20y, p11x, p11y, x2, y2));
      P2 = next.P1;
      cp.setLocation(p10x, p10y);

      next.node = node;
      node = null;
    }

    /**
     * Transforms the segment
     */
    void transform(AffineTransform at)
    {
      P1 = at.transform(P1, null);
      P2 = at.transform(P2, null);
      cp = at.transform(cp, null);
    }
  } // class QuadSegment

  /**
   * Cubic Bezier curve segment
   */
  private class CubicSegment extends Segment
  {
    Point2D cp1; // control points
    Point2D cp2; // control points

    /**
     * Constructor - takes coordinates of the starting point,
     * first control point, second control point and end point,
     * respecively.
     */
    public CubicSegment(double x1, double y1, double c1x, double c1y,
                        double c2x, double c2y, double x2, double y2)
    {
      super();
      P1 = new Point2D.Double(x1, y1);
      P2 = new Point2D.Double(x2, y2);
      cp1 = new Point2D.Double(c1x, c1y);
      cp2 = new Point2D.Double(c2x, c2y);
    }

    /**
     * Clones this segment
     */
    public Object clone()
    {
      return new CubicSegment(P1.getX(), P1.getY(), cp1.getX(), cp1.getY(),
                              cp2.getX(), cp2.getY(), P2.getX(), P2.getY());
    }

    /**
     * Returns twice the area of a curve, relative the P1-P2 line
     *
     * The area formula can be derived by using Green's formula in the
     * plane on the parametric form of the bezier.
     */
    double curveArea()
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp1.getX();
      double y1 = cp1.getY();
      double x2 = cp2.getX();
      double y2 = cp2.getY();
      double x3 = P2.getX();
      double y3 = P2.getY();

      double P = y3 - 3 * y2 + 3 * y1 - y0;
      double Q = 3 * (y2 + y0 - 2 * y1);
      double R = 3 * (y1 - y0);

      double A = x3 - 3 * x2 + 3 * x1 - x0;
      double B = 3 * (x2 + x0 - 2 * x1);
      double C = 3 * (x1 - x0);

      double area = (B * P - A * Q) / 5.0 + (C * P - A * R) / 2.0
                    + (C * Q - B * R) / 3.0;

      return (area);
    }

    /**
     * Compare two segments.
     */
    boolean equals(Segment b)
    {
      if (! (b instanceof CubicSegment))
	return false;

      return (P1.equals(b.P1) && cp1.equals(((CubicSegment) b).cp1)
             && cp2.equals(((CubicSegment) b).cp2) && P2.equals(b.P2));
    }

    /**
     * Returns a Point2D corresponding to the parametric value t
     * of the curve
     */
    Point2D evaluatePoint(double t)
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp1.getX();
      double y1 = cp1.getY();
      double x2 = cp2.getX();
      double y2 = cp2.getY();
      double x3 = P2.getX();
      double y3 = P2.getY();

      return new Point2D.Double(-(t * t * t) * (x0 - 3 * x1 + 3 * x2 - x3)
                                + 3 * t * t * (x0 - 2 * x1 + x2)
                                + 3 * t * (x1 - x0) + x0,
                                -(t * t * t) * (y0 - 3 * y1 + 3 * y2 - y3)
                                + 3 * t * t * (y0 - 2 * y1 + y2)
                                + 3 * t * (y1 - y0) + y0);
    }

    /**
     * Returns the bounding box of this segment
     */
    Rectangle2D getBounds()
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp1.getX();
      double y1 = cp1.getY();
      double x2 = cp2.getX();
      double y2 = cp2.getY();
      double x3 = P2.getX();
      double y3 = P2.getY();
      double[] r = new double[3];

      double xmax = Math.max(x0, x3);
      double ymax = Math.max(y0, y3);
      double xmin = Math.min(x0, x3);
      double ymin = Math.min(y0, y3);

      r[0] = 3 * (y1 - y0);
      r[1] = 6.0 * (y2 + y0 - 2 * y1);
      r[2] = 3.0 * (y3 - 3 * y2 + 3 * y1 - y0);

      int n = QuadCurve2D.solveQuadratic(r);
      for (int i = 0; i < n; i++)
        {
	  double t = r[i];
	  if (t > 0 && t < 1.0)
	    {
	      double y = evaluatePoint(t).getY();
	      ymax = Math.max(y, ymax);
	      ymin = Math.min(y, ymin);
	    }
        }

      r[0] = 3 * (x1 - x0);
      r[1] = 6.0 * (x2 + x0 - 2 * x1);
      r[2] = 3.0 * (x3 - 3 * x2 + 3 * x1 - x0);
      n = QuadCurve2D.solveQuadratic(r);
      for (int i = 0; i < n; i++)
        {
	  double t = r[i];
	  if (t > 0 && t < 1.0)
	    {
	      double x = evaluatePoint(t).getX();
	      xmax = Math.max(x, xmax);
	      xmin = Math.min(x, xmin);
	    }
        }
      return (new Rectangle2D.Double(xmin, ymin, (xmax - xmin), (ymax - ymin)));
    }

    /**
     * Returns a CubicCurve2D object corresponding to this segment.
     */
    CubicCurve2D getCubicCurve2D()
    {
      return new CubicCurve2D.Double(P1.getX(), P1.getY(), cp1.getX(),
                                     cp1.getY(), cp2.getX(), cp2.getY(),
                                     P2.getX(), P2.getY());
    }

    /**
     * Returns the parametric points of self-intersection if the cubic
     * is self-intersecting, null otherwise.
     */
    double[] getLoop()
    {
      double x0 = P1.getX();
      double y0 = P1.getY();
      double x1 = cp1.getX();
      double y1 = cp1.getY();
      double x2 = cp2.getX();
      double y2 = cp2.getY();
      double x3 = P2.getX();
      double y3 = P2.getY();
      double[] r = new double[4];
      double k;
      double R;
      double T;
      double A;
      double B;
      double[] results = new double[2];

      R = x3 - 3 * x2 + 3 * x1 - x0;
      T = y3 - 3 * y2 + 3 * y1 - y0;

      // A qudratic
      if (R == 0.0 && T == 0.0)
	return null;

      // true cubic
      if (R != 0.0 && T != 0.0)
        {
	  A = 3 * (x2 + x0 - 2 * x1) / R;
	  B = 3 * (x1 - x0) / R;

	  double P = 3 * (y2 + y0 - 2 * y1) / T;
	  double Q = 3 * (y1 - y0) / T;

	  if (A == P || Q == B)
	    return null;

	  k = (Q - B) / (A - P);
        }
      else
        {
	  if (R == 0.0)
	    {
	      // quadratic in x
	      k = -(3 * (x1 - x0)) / (3 * (x2 + x0 - 2 * x1));
	      A = 3 * (y2 + y0 - 2 * y1) / T;
	      B = 3 * (y1 - y0) / T;
	    }
	  else
	    {
	      // quadratic in y
	      k = -(3 * (y1 - y0)) / (3 * (y2 + y0 - 2 * y1));
	      A = 3 * (x2 + x0 - 2 * x1) / R;
	      B = 3 * (x1 - x0) / R;
	    }
        }

      r[0] = -k * k * k - A * k * k - B * k;
      r[1] = 3 * k * k + 2 * k * A + 2 * B;
      r[2] = -3 * k;
      r[3] = 2;

      int n = CubicCurve2D.solveCubic(r);
      if (n != 3)
	return null;

      // sort r
      double t;
      for (int i = 0; i < 2; i++)
	for (int j = i + 1; j < 3; j++)
	  if (r[j] < r[i])
	    {
	      t = r[i];
	      r[i] = r[j];
	      r[j] = t;
	    }

      if (Math.abs(r[0] + r[2] - k) < 1E-13)
	if (r[0] >= 0.0 && r[0] <= 1.0 && r[2] >= 0.0 && r[2] <= 1.0)
	  if (evaluatePoint(r[0]).distance(evaluatePoint(r[2])) < PE_EPSILON * 10)
	    { // we snap the points anyway
	      results[0] = r[0];
	      results[1] = r[2];
	      return (results);
	    }
      return null;
    }

    /**
     * Returns the segment's midpoint
     */
    Point2D getMidPoint()
    {
      return evaluatePoint(0.5);
    }

    /**
     * Returns the PathIterator type of a segment
     */
    int getType()
    {
      return (PathIterator.SEG_CUBICTO);
    }

    /**
     * Returns the PathIterator coords of a segment
     */
    int pathIteratorFormat(double[] coords)
    {
      coords[0] = cp1.getX();
      coords[1] = cp1.getY();
      coords[2] = cp2.getX();
      coords[3] = cp2.getY();
      coords[4] = P2.getX();
      coords[5] = P2.getY();
      return (PathIterator.SEG_CUBICTO);
    }

    /**
     * Returns the number of intersections on the positive X axis,
     * with the origin at (x,y), used for contains()-testing
     */
    int rayCrossing(double x, double y)
    {
      double x0 = P1.getX() - x;
      double y0 = P1.getY() - y;
      double x1 = cp1.getX() - x;
      double y1 = cp1.getY() - y;
      double x2 = cp2.getX() - x;
      double y2 = cp2.getY() - y;
      double x3 = P2.getX() - x;
      double y3 = P2.getY() - y;
      double[] r = new double[4];
      int nRoots;
      int nCrossings = 0;

      /* check if curve may intersect X+ axis. */
      if ((x0 > 0.0 || x1 > 0.0 || x2 > 0.0 || x3 > 0.0)
          && (y0 * y1 <= 0 || y1 * y2 <= 0 || y2 * y3 <= 0))
        {
	  if (y0 == 0.0)
	    y0 -= EPSILON;
	  if (y3 == 0.0)
	    y3 -= EPSILON;

	  r[0] = y0;
	  r[1] = 3 * (y1 - y0);
	  r[2] = 3 * (y2 + y0 - 2 * y1);
	  r[3] = y3 - 3 * y2 + 3 * y1 - y0;

	  if ((nRoots = CubicCurve2D.solveCubic(r)) > 0)
	    for (int i = 0; i < nRoots; i++)
	      {
		if (r[i] > 0.0 && r[i] < 1.0)
		  {
		    double t = r[i];
		    if (-(t * t * t) * (x0 - 3 * x1 + 3 * x2 - x3)
		        + 3 * t * t * (x0 - 2 * x1 + x2) + 3 * t * (x1 - x0)
		        + x0 > 0.0)
		      nCrossings++;
		  }
	      }
        }
      return nCrossings;
    }

    /**
     * Swap start and end points
     */
    void reverseCoords()
    {
      Point2D p = P1;
      P1 = P2;
      P2 = p;
      p = cp1; // swap control points
      cp1 = cp2;
      cp2 = p;
    }

    /**
     * Splits intersections into nodes,
     * This one handles cubic-cubic and cubic-quadratic intersections
     */
    int splitIntersections(Segment b)
    {
      if (b instanceof LineSegment)
	return (b.splitIntersections(this));

      Intersection[] x = null;

      if (b instanceof QuadSegment)
	x = cubicCubicIntersect(this, ((QuadSegment) b).getCubicSegment());

      if (b instanceof CubicSegment)
	x = cubicCubicIntersect(this, (CubicSegment) b);

      if (x == null)
	return 0;

      if (x.length == 1)
	return createNode(b, x[0]);

      return createNodes(b, x);
    }

    /**
     * Subdivides the segment at parametric value t, inserting
     * the new segment into the linked list after this,
     * such that this becomes [0,t] and this.next becomes [t,1]
     */
    void subdivideInsert(double t)
    {
      CubicSegment s = (CubicSegment) clone();
      double p1x = (s.cp1.getX() - s.P1.getX()) * t + s.P1.getX();
      double p1y = (s.cp1.getY() - s.P1.getY()) * t + s.P1.getY();

      double px = (s.cp2.getX() - s.cp1.getX()) * t + s.cp1.getX();
      double py = (s.cp2.getY() - s.cp1.getY()) * t + s.cp1.getY();

      s.cp2.setLocation((s.P2.getX() - s.cp2.getX()) * t + s.cp2.getX(),
                        (s.P2.getY() - s.cp2.getY()) * t + s.cp2.getY());

      s.cp1.setLocation((s.cp2.getX() - px) * t + px,
                        (s.cp2.getY() - py) * t + py);

      double p2x = (px - p1x) * t + p1x;
      double p2y = (py - p1y) * t + p1y;

      double p3x = (s.cp1.getX() - p2x) * t + p2x;
      double p3y = (s.cp1.getY() - p2y) * t + p2y;
      s.P1.setLocation(p3x, p3y);

      // insert new curve
      insert(s);

      // set this curve
      cp1.setLocation(p1x, p1y);
      cp2.setLocation(p2x, p2y);
      P2 = s.P1;
      next.node = node;
      node = null;
    }

    /**
     * Transforms the segment
     */
    void transform(AffineTransform at)
    {
      P1 = at.transform(P1, null);
      P2 = at.transform(P2, null);
      cp1 = at.transform(cp1, null);
      cp2 = at.transform(cp2, null);
    }
  } // class CubicSegment
} // class Area
