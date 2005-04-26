/* Arc2D.java -- represents an arc in 2-D space
   Copyright (C) 2002, 2003, 2004 Free Software Foundation

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

import java.util.NoSuchElementException;


/**
 * This class represents all arcs (segments of an ellipse in 2-D space). The
 * arcs are defined by starting angle and extent (arc length) in degrees, as
 * opposed to radians (like the rest of Java), and can be open, chorded, or
 * wedge shaped. The angles are skewed according to the ellipse, so that 45
 * degrees always points to the upper right corner (positive x, negative y)
 * of the bounding rectangle. A positive extent draws a counterclockwise arc,
 * and while the angle can be any value, the path iterator only traverses the
 * first 360 degrees. Storage is up to the subclasses.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Sven de Marothy (sven@physto.se)
 * @since 1.2
 */
public abstract class Arc2D extends RectangularShape
{
  /**
   * An open arc, with no segment connecting the endpoints. This type of
   * arc still contains the same points as a chorded version.
   */
  public static final int OPEN = 0;

  /**
   * A closed arc with a single segment connecting the endpoints (a chord).
   */
  public static final int CHORD = 1;

  /**
   * A closed arc with two segments, one from each endpoint, meeting at the
   * center of the ellipse.
   */
  public static final int PIE = 2;

  /** The closure type of this arc.  This is package-private to avoid an
   * accessor method.  */
  int type;

  /**
   * Create a new arc, with the specified closure type.
   *
   * @param type one of {@link #OPEN}, {@link #CHORD}, or {@link #PIE}.
   * @throws IllegalArgumentException if type is invalid
   */
  protected Arc2D(int type)
  {
    if (type < OPEN || type > PIE)
      throw new IllegalArgumentException();
    this.type = type;
  }

  /**
   * Get the starting angle of the arc in degrees.
   *
   * @return the starting angle
   * @see #setAngleStart(double)
   */
  public abstract double getAngleStart();

  /**
   * Get the extent angle of the arc in degrees.
   *
   * @return the extent angle
   * @see #setAngleExtent(double)
   */
  public abstract double getAngleExtent();

  /**
   * Return the closure type of the arc.
   *
   * @return the closure type
   * @see #OPEN
   * @see #CHORD
   * @see #PIE
   * @see #setArcType(int)
   */
  public int getArcType()
  {
    return type;
  }

  /**
   * Returns the starting point of the arc.
   *
   * @return the start point
   */
  public Point2D getStartPoint()
  {
    double angle = Math.toRadians(getAngleStart());
    double rx = getWidth() / 2;
    double ry = getHeight() / 2;
    double x = getX() + rx + rx * Math.cos(angle);
    double y = getY() + ry - ry * Math.sin(angle);
    return new Point2D.Double(x, y);
  }

  /**
   * Returns the ending point of the arc.
   *
   * @return the end point
   */
  public Point2D getEndPoint()
  {
    double angle = Math.toRadians(getAngleStart() + getAngleExtent());
    double rx = getWidth() / 2;
    double ry = getHeight() / 2;
    double x = getX() + rx + rx * Math.cos(angle);
    double y = getY() + ry - ry * Math.sin(angle);
    return new Point2D.Double(x, y);
  }

  /**
   * Set the parameters of the arc. The angles are in degrees, and a positive
   * extent sweeps counterclockwise (from the positive x-axis to the negative
   * y-axis).
   *
   * @param x the new x coordinate of the upper left of the bounding box
   * @param y the new y coordinate of the upper left of the bounding box
   * @param w the new width of the bounding box
   * @param h the new height of the bounding box
   * @param start the start angle, in degrees
   * @param extent the arc extent, in degrees
   * @param type one of {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
   * @throws IllegalArgumentException if type is invalid
   */
  public abstract void setArc(double x, double y, double w, double h,
                              double start, double extent, int type);

  /**
   * Set the parameters of the arc. The angles are in degrees, and a positive
   * extent sweeps counterclockwise (from the positive x-axis to the negative
   * y-axis).
   *
   * @param p the upper left point of the bounding box
   * @param d the dimensions of the bounding box
   * @param start the start angle, in degrees
   * @param extent the arc extent, in degrees
   * @param type one of {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
   * @throws IllegalArgumentException if type is invalid
   * @throws NullPointerException if p or d is null
   */
  public void setArc(Point2D p, Dimension2D d, double start, double extent,
                     int type)
  {
    setArc(p.getX(), p.getY(), d.getWidth(), d.getHeight(), start, extent, type);
  }

  /**
   * Set the parameters of the arc. The angles are in degrees, and a positive
   * extent sweeps counterclockwise (from the positive x-axis to the negative
   * y-axis).
   *
   * @param r the new bounding box
   * @param start the start angle, in degrees
   * @param extent the arc extent, in degrees
   * @param type one of {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
   * @throws IllegalArgumentException if type is invalid
   * @throws NullPointerException if r is null
   */
  public void setArc(Rectangle2D r, double start, double extent, int type)
  {
    setArc(r.getX(), r.getY(), r.getWidth(), r.getHeight(), start, extent, type);
  }

  /**
   * Set the parameters of the arc from the given one.
   *
   * @param a the arc to copy
   * @throws NullPointerException if a is null
   */
  public void setArc(Arc2D a)
  {
    setArc(a.getX(), a.getY(), a.getWidth(), a.getHeight(), a.getAngleStart(),
           a.getAngleExtent(), a.getArcType());
  }

  /**
   * Set the parameters of the arc. The angles are in degrees, and a positive
   * extent sweeps counterclockwise (from the positive x-axis to the negative
   * y-axis). This controls the center point and radius, so the arc will be
   * circular.
   *
   * @param x the x coordinate of the center of the circle
   * @param y the y coordinate of the center of the circle
   * @param r the radius of the circle
   * @param start the start angle, in degrees
   * @param extent the arc extent, in degrees
   * @param type one of {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
   * @throws IllegalArgumentException if type is invalid
   */
  public void setArcByCenter(double x, double y, double r, double start,
                             double extent, int type)
  {
    setArc(x - r, y - r, r + r, r + r, start, extent, type);
  }

  /**
   * Sets the parameters of the arc by finding the tangents of two lines, and
   * using the specified radius. The arc will be circular, will begin on the
   * tangent point of the line extending from p1 to p2, and will end on the
   * tangent point of the line extending from p2 to p3.
   *
   * XXX What happens if the points are colinear, or the radius negative?
   *
   * @param p1 the first point
   * @param p2 the tangent line intersection point
   * @param p3 the third point
   * @param r the radius of the arc
   * @throws NullPointerException if any point is null
   */
  public void setArcByTangent(Point2D p1, Point2D p2, Point2D p3, double r)
  {
    if ((p2.getX() - p1.getX()) * (p3.getY() - p1.getY())
        - (p3.getX() - p1.getX()) * (p2.getY() - p1.getY()) > 0)
      {
	Point2D p = p3;
	p3 = p1;
	p1 = p;
      }

    // normalized tangent vectors
    double dx1 = (p1.getX() - p2.getX()) / p1.distance(p2);
    double dy1 = (p1.getY() - p2.getY()) / p1.distance(p2);
    double dx2 = (p2.getX() - p3.getX()) / p3.distance(p2);
    double dy2 = (p2.getY() - p3.getY()) / p3.distance(p2);
    double theta1 = Math.atan2(dx1, dy1);
    double theta2 = Math.atan2(dx2, dy2);

    double dx = r * Math.cos(theta2) - r * Math.cos(theta1);
    double dy = -r * Math.sin(theta2) + r * Math.sin(theta1);

    if (theta1 < 0)
      theta1 += 2 * Math.PI;
    if (theta2 < 0)
      theta2 += 2 * Math.PI;
    if (theta2 < theta1)
      theta2 += 2 * Math.PI;

    // Vectors of the lines, not normalized, note we change 
    // the direction of line 2.
    dx1 = p1.getX() - p2.getX();
    dy1 = p1.getY() - p2.getY();
    dx2 = p3.getX() - p2.getX();
    dy2 = p3.getY() - p2.getY();

    // Calculate the tangent point to the second line
    double t2 = -(dx1 * dy - dy1 * dx) / (dx2 * dy1 - dx1 * dy2);
    double x2 = t2 * (p3.getX() - p2.getX()) + p2.getX();
    double y2 = t2 * (p3.getY() - p2.getY()) + p2.getY();

    // calculate the center point
    double x = x2 - r * Math.cos(theta2);
    double y = y2 + r * Math.sin(theta2);

    setArc(x - r, y - r, 2 * r, 2 * r, Math.toDegrees(theta1),
           Math.toDegrees(theta2 - theta1), getArcType());
  }

  /**
   * Set the start, in degrees.
   *
   * @param start the new start angle
   * @see #getAngleStart()
   */
  public abstract void setAngleStart(double start);

  /**
   * Set the extent, in degrees.
   *
   * @param extent the new extent angle
   * @see #getAngleExtent()
   */
  public abstract void setAngleExtent(double extent);

  /**
   * Sets the starting angle to the angle of the given point relative to
   * the center of the arc. The extent remains constant; in other words,
   * this rotates the arc.
   *
   * @param p the new start point
   * @throws NullPointerException if p is null
   * @see #getStartPoint()
   * @see #getAngleStart()
   */
  public void setAngleStart(Point2D p)
  {
    // Normalize.
    double x = p.getX() - (getX() + getWidth() / 2);
    double y = p.getY() - (getY() + getHeight() / 2);
    setAngleStart(Math.toDegrees(Math.atan2(-y, x)));
  }

  /**
   * Sets the starting and extent angles to those of the given points
   * relative to the center of the arc. The arc will be non-empty, and will
   * extend counterclockwise.
   *
   * @param x1 the first x coordinate
   * @param y1 the first y coordinate
   * @param x2 the second x coordinate
   * @param y2 the second y coordinate
   * @see #setAngleStart(Point2D)
   */
  public void setAngles(double x1, double y1, double x2, double y2)
  {
    // Normalize the points.
    double mx = getX();
    double my = getY();
    double mw = getWidth();
    double mh = getHeight();
    x1 = x1 - (mx + mw / 2);
    y1 = y1 - (my + mh / 2);
    x2 = x2 - (mx + mw / 2);
    y2 = y2 - (my + mh / 2);
    double start = Math.toDegrees(Math.atan2(-y1, x1));
    double extent = Math.toDegrees(Math.atan2(-y2, x2)) - start;
    if (extent < 0)
      extent += 360;
    setAngleStart(start);
    setAngleExtent(extent);
  }

  /**
   * Sets the starting and extent angles to those of the given points
   * relative to the center of the arc. The arc will be non-empty, and will
   * extend counterclockwise.
   *
   * @param p1 the first point
   * @param p2 the second point
   * @throws NullPointerException if either point is null
   * @see #setAngleStart(Point2D)
   */
  public void setAngles(Point2D p1, Point2D p2)
  {
    setAngles(p1.getX(), p1.getY(), p2.getX(), p2.getY());
  }

  /**
   * Set the closure type of this arc.
   *
   * @param type one of {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
   * @throws IllegalArgumentException if type is invalid
   * @see #getArcType()
   */
  public void setArcType(int type)
  {
    if (type < OPEN || type > PIE)
      throw new IllegalArgumentException();
    this.type = type;
  }

  /**
   * Sets the location and bounds of the ellipse of which this arc is a part.
   *
   * @param x the new x coordinate
   * @param y the new y coordinate
   * @param w the new width
   * @param h the new height
   * @see #getFrame()
   */
  public void setFrame(double x, double y, double w, double h)
  {
    setArc(x, y, w, h, getAngleStart(), getAngleExtent(), type);
  }

  /**
   * Gets the bounds of the arc. This is much tighter than
   * <code>getBounds</code>, as it takes into consideration the start and
   * end angles, and the center point of a pie wedge, rather than just the
   * overall ellipse.
   *
   * @return the bounds of the arc
   * @see #getBounds()
   */
  public Rectangle2D getBounds2D()
  {
    double extent = getAngleExtent();
    if (Math.abs(extent) >= 360)
      return makeBounds(getX(), getY(), getWidth(), getHeight());

    // Find the minimal bounding box.  This determined by its extrema,
    // which are the center, the endpoints of the arc, and any local
    // maximum contained by the arc.
    double rX = getWidth() / 2;
    double rY = getHeight() / 2;
    double centerX = getX() + rX;
    double centerY = getY() + rY;

    Point2D p1 = getStartPoint();
    Rectangle2D result = makeBounds(p1.getX(), p1.getY(), 0, 0);
    result.add(getEndPoint());

    if (type == PIE)
      result.add(centerX, centerY);
    if (containsAngle(0))
      result.add(centerX + rX, centerY);
    if (containsAngle(90))
      result.add(centerX, centerY - rY);
    if (containsAngle(180))
      result.add(centerX - rX, centerY);
    if (containsAngle(270))
      result.add(centerX, centerY + rY);

    return result;
  }

  /**
   * Construct a bounding box in a precision appropriate for the subclass.
   *
   * @param x the x coordinate
   * @param y the y coordinate
   * @param w the width
   * @param h the height
   * @return the rectangle for use in getBounds2D
   */
  protected abstract Rectangle2D makeBounds(double x, double y, double w,
                                            double h);

  /**
   * Tests if the given angle, in degrees, is included in the arc.
   * All angles are normalized to be between 0 and 360 degrees.
   *
   * @param a the angle to test
   * @return true if it is contained
   */
  public boolean containsAngle(double a)
  {
    double start = getAngleStart();
    double extent = getAngleExtent();
    double end = start + extent;

    if (extent == 0)
      return false;

    if (extent >= 360 || extent <= -360)
      return true;

    if (extent < 0)
      {
	end = start;
	start += extent;
      }

    start %= 360;
    while (start < 0)
      start += 360;

    end %= 360;
    while (end < start)
      end += 360;

    a %= 360;
    while (a < start)
      a += 360;

    return a >= start && a < end; // starting angle included, ending angle not
  }

  /**
   * Determines if the arc contains the given point. If the bounding box
   * is empty, then this will return false.
   *
   * The area considered 'inside' an arc of type OPEN is the same as the
   * area inside an equivalent filled CHORD-type arc. The area considered
   * 'inside' a CHORD-type arc is the same as the filled area.
   *
   * @param x the x coordinate to test
   * @param y the y coordinate to test
   * @return true if the point is inside the arc
   */
  public boolean contains(double x, double y)
  {
    double w = getWidth();
    double h = getHeight();
    double extent = getAngleExtent();
    if (w <= 0 || h <= 0 || extent == 0)
      return false;

    double mx = getX() + w / 2;
    double my = getY() + h / 2;
    double dx = (x - mx) * 2 / w;
    double dy = (y - my) * 2 / h;
    if ((dx * dx + dy * dy) >= 1.0)
      return false;

    double angle = Math.toDegrees(Math.atan2(-dy, dx));
    if (getArcType() == PIE)
      return containsAngle(angle);

    double a1 = Math.toRadians(getAngleStart());
    double a2 = Math.toRadians(getAngleStart() + extent);
    double x1 = mx + getWidth() * Math.cos(a1) / 2;
    double y1 = my - getHeight() * Math.sin(a1) / 2;
    double x2 = mx + getWidth() * Math.cos(a2) / 2;
    double y2 = my - getHeight() * Math.sin(a2) / 2;
    double sgn = ((x2 - x1) * (my - y1) - (mx - x1) * (y2 - y1)) * ((x2 - x1) * (y
                 - y1) - (x - x1) * (y2 - y1));

    if (Math.abs(extent) > 180)
      {
	if (containsAngle(angle))
	  return true;
	return sgn > 0;
      }
    else
      {
	if (! containsAngle(angle))
	  return false;
	return sgn < 0;
      }
  }

  /**
   * Tests if a given rectangle intersects the area of the arc.
   *
   * For a definition of the 'inside' area, see the contains() method.
   * @see #contains(double, double)
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @return true if the two shapes share common points
   */
  public boolean intersects(double x, double y, double w, double h)
  {
    double extent = getAngleExtent();
    if (extent == 0)
      return false;

    if (contains(x, y) || contains(x, y + h) || contains(x + w, y)
        || contains(x + w, y + h))
      return true;

    Rectangle2D rect = new Rectangle2D.Double(x, y, w, h);

    double a = getWidth() / 2.0;
    double b = getHeight() / 2.0;

    double mx = getX() + a;
    double my = getY() + b;
    double x1 = mx + a * Math.cos(Math.toRadians(getAngleStart()));
    double y1 = my - b * Math.sin(Math.toRadians(getAngleStart()));
    double x2 = mx + a * Math.cos(Math.toRadians(getAngleStart() + extent));
    double y2 = my - b * Math.sin(Math.toRadians(getAngleStart() + extent));

    if (getArcType() != CHORD)
      {
	// check intersections against the pie radii
	if (rect.intersectsLine(mx, my, x1, y1))
	  return true;
	if (rect.intersectsLine(mx, my, x2, y2))
	  return true;
      }
    else// check the chord
    if (rect.intersectsLine(x1, y1, x2, y2))
      return true;

    // Check the Arc segment against the four edges
    double dx;

    // Check the Arc segment against the four edges
    double dy;
    dy = y - my;
    dx = a * Math.sqrt(1 - ((dy * dy) / (b * b)));
    if (! java.lang.Double.isNaN(dx))
      {
	if (mx + dx >= x && mx + dx <= x + w
	    && containsAngle(Math.toDegrees(Math.atan2(-dy, dx))))
	  return true;
	if (mx - dx >= x && mx - dx <= x + w
	    && containsAngle(Math.toDegrees(Math.atan2(-dy, -dx))))
	  return true;
      }
    dy = (y + h) - my;
    dx = a * Math.sqrt(1 - ((dy * dy) / (b * b)));
    if (! java.lang.Double.isNaN(dx))
      {
	if (mx + dx >= x && mx + dx <= x + w
	    && containsAngle(Math.toDegrees(Math.atan2(-dy, dx))))
	  return true;
	if (mx - dx >= x && mx - dx <= x + w
	    && containsAngle(Math.toDegrees(Math.atan2(-dy, -dx))))
	  return true;
      }
    dx = x - mx;
    dy = b * Math.sqrt(1 - ((dx * dx) / (a * a)));
    if (! java.lang.Double.isNaN(dy))
      {
	if (my + dy >= y && my + dy <= y + h
	    && containsAngle(Math.toDegrees(Math.atan2(-dy, dx))))
	  return true;
	if (my - dy >= y && my - dy <= y + h
	    && containsAngle(Math.toDegrees(Math.atan2(dy, dx))))
	  return true;
      }

    dx = (x + w) - mx;
    dy = b * Math.sqrt(1 - ((dx * dx) / (a * a)));
    if (! java.lang.Double.isNaN(dy))
      {
	if (my + dy >= y && my + dy <= y + h
	    && containsAngle(Math.toDegrees(Math.atan2(-dy, dx))))
	  return true;
	if (my - dy >= y && my - dy <= y + h
	    && containsAngle(Math.toDegrees(Math.atan2(dy, dx))))
	  return true;
      }

    // Check whether the arc is contained within the box
    if (rect.contains(mx, my))
      return true;

    return false;
  }

  /**
   * Tests if a given rectangle is contained in the area of the arc.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @return true if the arc contains the rectangle
   */
  public boolean contains(double x, double y, double w, double h)
  {
    double extent = getAngleExtent();
    if (extent == 0)
      return false;

    if (! (contains(x, y) && contains(x, y + h) && contains(x + w, y)
        && contains(x + w, y + h)))
      return false;

    Rectangle2D rect = new Rectangle2D.Double(x, y, w, h);

    double a = getWidth() / 2.0;
    double b = getHeight() / 2.0;

    double mx = getX() + a;
    double my = getY() + b;
    double x1 = mx + a * Math.cos(Math.toRadians(getAngleStart()));
    double y1 = my - b * Math.sin(Math.toRadians(getAngleStart()));
    double x2 = mx + a * Math.cos(Math.toRadians(getAngleStart() + extent));
    double y2 = my - b * Math.sin(Math.toRadians(getAngleStart() + extent));
    if (getArcType() != CHORD)
      {
	// check intersections against the pie radii
	if (rect.intersectsLine(mx, my, x1, y1))
	  return false;

	if (rect.intersectsLine(mx, my, x2, y2))
	  return false;
      }
    else if (rect.intersectsLine(x1, y1, x2, y2))
      return false;
    return true;
  }

  /**
   * Tests if a given rectangle is contained in the area of the arc.
   *
   * @param r the rectangle
   * @return true if the arc contains the rectangle
   */
  public boolean contains(Rectangle2D r)
  {
    return contains(r.getX(), r.getY(), r.getWidth(), r.getHeight());
  }

  /**
   * Returns an iterator over this arc, with an optional transformation.
   * This iterator is threadsafe, so future modifications to the arc do not
   * affect the iteration.
   *
   * @param at the transformation, or null
   * @return a path iterator
   */
  public PathIterator getPathIterator(AffineTransform at)
  {
    return new ArcIterator(this, at);
  }

  /**
   * This class is used to iterate over an arc. Since ellipses are a subclass
   * of arcs, this is used by Ellipse2D as well.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  static final class ArcIterator implements PathIterator
  {
    /** The current iteration. */
    private int current;

    /** The last iteration. */
    private final int limit;

    /** The optional transformation. */
    private final AffineTransform xform;

    /** The x coordinate of the bounding box. */
    private final double x;

    /** The y coordinate of the bounding box. */
    private final double y;

    /** The width of the bounding box. */
    private final double w;

    /** The height of the bounding box. */
    private final double h;

    /** The start angle, in radians (not degrees). */
    private final double start;

    /** The extent angle, in radians (not degrees). */
    private final double extent;

    /** The arc closure type. */
    private final int type;

    /**
     * Construct a new iterator over an arc.
     *
     * @param a the arc
     * @param xform the transform
     */
    public ArcIterator(Arc2D a, AffineTransform xform)
    {
      this.xform = xform;
      x = a.getX();
      y = a.getY();
      w = a.getWidth();
      h = a.getHeight();
      double start = a.getAngleStart() * (Math.PI / 180);
      double extent = a.getAngleExtent() * (Math.PI / 180);

      if (extent < 0)
        {
	  extent = -extent;
	  start = 2 * Math.PI - extent + start;
        }
      this.start = start;
      this.extent = extent;

      type = a.type;
      if (w < 0 || h < 0)
	limit = -1;
      else if (extent == 0)
	limit = type;
      else if (extent <= Math.PI / 2.0)
	limit = type + 1;
      else if (extent <= Math.PI)
	limit = type + 2;
      else if (extent <= 3.0 * (Math.PI / 2.0))
	limit = type + 3;
      else
	limit = type + 4;
    }

    /**
     * Construct a new iterator over an ellipse.
     *
     * @param e the ellipse
     * @param xform the transform
     */
    public ArcIterator(Ellipse2D e, AffineTransform xform)
    {
      this.xform = xform;
      x = e.getX();
      y = e.getY();
      w = e.getWidth();
      h = e.getHeight();
      start = 0;
      extent = 2 * Math.PI;
      type = CHORD;
      limit = (w < 0 || h < 0) ? -1 : 5;
    }

    /**
     * Return the winding rule.
     *
     * @return {@link PathIterator#WIND_NON_ZERO}
     */
    public int getWindingRule()
    {
      return WIND_NON_ZERO;
    }

    /**
     * Test if the iteration is complete.
     *
     * @return true if more segments exist
     */
    public boolean isDone()
    {
      return current > limit;
    }

    /**
     * Advance the iterator.
     */
    public void next()
    {
      current++;
    }

    /**
     * Put the current segment into the array, and return the segment type.
     *
     * @param coords an array of 6 elements
     * @return the segment type
     * @throws NullPointerException if coords is null
     * @throws ArrayIndexOutOfBoundsException if coords is too small
     */
    public int currentSegment(float[] coords)
    {
      double[] double_coords = new double[6];
      int code = currentSegment(double_coords);
      for (int i = 0; i < 6; ++i)
	coords[i] = (float) double_coords[i];
      return code;
    }

    /**
     * Put the current segment into the array, and return the segment type.
     *
     * @param coords an array of 6 elements
     * @return the segment type
     * @throws NullPointerException if coords is null
     * @throws ArrayIndexOutOfBoundsException if coords is too small
     */
    public int currentSegment(double[] coords)
    {
      double rx = w / 2;
      double ry = h / 2;
      double xmid = x + rx;
      double ymid = y + ry;

      if (current > limit)
	throw new NoSuchElementException("arc iterator out of bounds");

      if (current == 0)
        {
	  coords[0] = xmid + rx * Math.cos(start);
	  coords[1] = ymid - ry * Math.sin(start);
	  if (xform != null)
	    xform.transform(coords, 0, coords, 0, 1);
	  return SEG_MOVETO;
        }

      if (type != OPEN && current == limit)
	return SEG_CLOSE;

      if ((current == limit - 1) && (type == PIE))
        {
	  coords[0] = xmid;
	  coords[1] = ymid;
	  if (xform != null)
	    xform.transform(coords, 0, coords, 0, 1);
	  return SEG_LINETO;
        }

      // note that this produces a cubic approximation of the arc segment,
      // not a true ellipsoid. there's no ellipsoid path segment code,
      // unfortunately. the cubic approximation looks about right, though.
      double kappa = (Math.sqrt(2.0) - 1.0) * (4.0 / 3.0);
      double quad = (Math.PI / 2.0);

      double curr_begin = start + (current - 1) * quad;
      double curr_extent = Math.min((start + extent) - curr_begin, quad);
      double portion_of_a_quadrant = curr_extent / quad;

      double x0 = xmid + rx * Math.cos(curr_begin);
      double y0 = ymid - ry * Math.sin(curr_begin);

      double x1 = xmid + rx * Math.cos(curr_begin + curr_extent);
      double y1 = ymid - ry * Math.sin(curr_begin + curr_extent);

      AffineTransform trans = new AffineTransform();
      double[] cvec = new double[2];
      double len = kappa * portion_of_a_quadrant;
      double angle = curr_begin;

      // in a hypothetical "first quadrant" setting, our first control
      // vector would be sticking up, from [1,0] to [1,kappa].
      //
      // let us recall however that in java2d, y coords are upside down
      // from what one would consider "normal" first quadrant rules, so we
      // will *subtract* the y value of this control vector from our first
      // point.
      cvec[0] = 0;
      cvec[1] = len;
      trans.scale(rx, ry);
      trans.rotate(angle);
      trans.transform(cvec, 0, cvec, 0, 1);
      coords[0] = x0 + cvec[0];
      coords[1] = y0 - cvec[1];

      // control vector #2 would, ideally, be sticking out and to the
      // right, in a first quadrant arc segment. again, subtraction of y.
      cvec[0] = 0;
      cvec[1] = -len;
      trans.rotate(curr_extent);
      trans.transform(cvec, 0, cvec, 0, 1);
      coords[2] = x1 + cvec[0];
      coords[3] = y1 - cvec[1];

      // end point
      coords[4] = x1;
      coords[5] = y1;

      if (xform != null)
	xform.transform(coords, 0, coords, 0, 3);

      return SEG_CUBICTO;
    }
  } // class ArcIterator

  /**
   * This class implements an arc in double precision.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.2
   */
  public static class Double extends Arc2D
  {
    /** The x coordinate of the box bounding the ellipse of this arc. */
    public double x;

    /** The y coordinate of the box bounding the ellipse of this arc. */
    public double y;

    /** The width of the box bounding the ellipse of this arc. */
    public double width;

    /** The height of the box bounding the ellipse of this arc. */
    public double height;

    /** The start angle of this arc, in degrees. */
    public double start;

    /** The extent angle of this arc, in degrees. */
    public double extent;

    /**
     * Create a new, open arc at (0,0) with 0 extent.
     */
    public Double()
    {
      super(OPEN);
    }

    /**
     * Create a new arc of the given type at (0,0) with 0 extent.
     *
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     */
    public Double(int type)
    {
      super(type);
    }

    /**
     * Create a new arc with the given dimensions.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     * @param start the start angle, in degrees
     * @param extent the extent, in degrees
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     */
    public Double(double x, double y, double w, double h, double start,
                  double extent, int type)
    {
      super(type);
      this.x = x;
      this.y = y;
      width = w;
      height = h;
      this.start = start;
      this.extent = extent;
    }

    /**
     * Create a new arc with the given dimensions.
     *
     * @param r the bounding box
     * @param start the start angle, in degrees
     * @param extent the extent, in degrees
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     * @throws NullPointerException if r is null
     */
    public Double(Rectangle2D r, double start, double extent, int type)
    {
      super(type);
      x = r.getX();
      y = r.getY();
      width = r.getWidth();
      height = r.getHeight();
      this.start = start;
      this.extent = extent;
    }

    /**
     * Return the x coordinate of the bounding box.
     *
     * @return the value of x
     */
    public double getX()
    {
      return x;
    }

    /**
     * Return the y coordinate of the bounding box.
     *
     * @return the value of y
     */
    public double getY()
    {
      return y;
    }

    /**
     * Return the width of the bounding box.
     *
     * @return the value of width
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Return the height of the bounding box.
     *
     * @return the value of height
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Return the start angle of the arc, in degrees.
     *
     * @return the value of start
     */
    public double getAngleStart()
    {
      return start;
    }

    /**
     * Return the extent of the arc, in degrees.
     *
     * @return the value of extent
     */
    public double getAngleExtent()
    {
      return extent;
    }

    /**
     * Tests if the arc contains points.
     *
     * @return true if the arc has no interior
     */
    public boolean isEmpty()
    {
      return width <= 0 || height <= 0;
    }

    /**
     * Sets the arc to the given dimensions.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     * @param start the start angle, in degrees
     * @param extent the extent, in degrees
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     */
    public void setArc(double x, double y, double w, double h, double start,
                       double extent, int type)
    {
      this.x = x;
      this.y = y;
      width = w;
      height = h;
      this.start = start;
      this.extent = extent;
      setArcType(type);
    }

    /**
     * Sets the start angle of the arc.
     *
     * @param start the new start angle
     */
    public void setAngleStart(double start)
    {
      this.start = start;
    }

    /**
     * Sets the extent angle of the arc.
     *
     * @param extent the new extent angle
     */
    public void setAngleExtent(double extent)
    {
      this.extent = extent;
    }

    /**
     * Creates a tight bounding box given dimensions that more precise than
     * the bounding box of the ellipse.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    protected Rectangle2D makeBounds(double x, double y, double w, double h)
    {
      return new Rectangle2D.Double(x, y, w, h);
    }
  } // class Double

  /**
   * This class implements an arc in float precision.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.2
   */
  public static class Float extends Arc2D
  {
    /** The x coordinate of the box bounding the ellipse of this arc. */
    public float x;

    /** The y coordinate of the box bounding the ellipse of this arc. */
    public float y;

    /** The width of the box bounding the ellipse of this arc. */
    public float width;

    /** The height of the box bounding the ellipse of this arc. */
    public float height;

    /** The start angle of this arc, in degrees. */
    public float start;

    /** The extent angle of this arc, in degrees. */
    public float extent;

    /**
     * Create a new, open arc at (0,0) with 0 extent.
     */
    public Float()
    {
      super(OPEN);
    }

    /**
     * Create a new arc of the given type at (0,0) with 0 extent.
     *
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     */
    public Float(int type)
    {
      super(type);
    }

    /**
     * Create a new arc with the given dimensions.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     * @param start the start angle, in degrees
     * @param extent the extent, in degrees
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     */
    public Float(float x, float y, float w, float h, float start,
                 float extent, int type)
    {
      super(type);
      this.x = x;
      this.y = y;
      width = w;
      height = h;
      this.start = start;
      this.extent = extent;
    }

    /**
     * Create a new arc with the given dimensions.
     *
     * @param r the bounding box
     * @param start the start angle, in degrees
     * @param extent the extent, in degrees
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     * @throws NullPointerException if r is null
     */
    public Float(Rectangle2D r, float start, float extent, int type)
    {
      super(type);
      x = (float) r.getX();
      y = (float) r.getY();
      width = (float) r.getWidth();
      height = (float) r.getHeight();
      this.start = start;
      this.extent = (float) extent;
    }

    /**
     * Return the x coordinate of the bounding box.
     *
     * @return the value of x
     */
    public double getX()
    {
      return x;
    }

    /**
     * Return the y coordinate of the bounding box.
     *
     * @return the value of y
     */
    public double getY()
    {
      return y;
    }

    /**
     * Return the width of the bounding box.
     *
     * @return the value of width
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Return the height of the bounding box.
     *
     * @return the value of height
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Return the start angle of the arc, in degrees.
     *
     * @return the value of start
     */
    public double getAngleStart()
    {
      return start;
    }

    /**
     * Return the extent of the arc, in degrees.
     *
     * @return the value of extent
     */
    public double getAngleExtent()
    {
      return extent;
    }

    /**
     * Tests if the arc contains points.
     *
     * @return true if the arc has no interior
     */
    public boolean isEmpty()
    {
      return width <= 0 || height <= 0;
    }

    /**
     * Sets the arc to the given dimensions.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     * @param start the start angle, in degrees
     * @param extent the extent, in degrees
     * @param type the arc type: {@link #OPEN}, {@link #CHORD}, or {@link #PIE}
     * @throws IllegalArgumentException if type is invalid
     */
    public void setArc(double x, double y, double w, double h, double start,
                       double extent, int type)
    {
      this.x = (float) x;
      this.y = (float) y;
      width = (float) w;
      height = (float) h;
      this.start = (float) start;
      this.extent = (float) extent;
      setArcType(type);
    }

    /**
     * Sets the start angle of the arc.
     *
     * @param start the new start angle
     */
    public void setAngleStart(double start)
    {
      this.start = (float) start;
    }

    /**
     * Sets the extent angle of the arc.
     *
     * @param extent the new extent angle
     */
    public void setAngleExtent(double extent)
    {
      this.extent = (float) extent;
    }

    /**
     * Creates a tight bounding box given dimensions that more precise than
     * the bounding box of the ellipse.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    protected Rectangle2D makeBounds(double x, double y, double w, double h)
    {
      return new Rectangle2D.Float((float) x, (float) y, (float) w, (float) h);
    }
  } // class Float
} // class Arc2D
