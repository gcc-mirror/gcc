/* FlatteningPathIterator.java -- performs interpolation of curved paths
   Copyright (C) 2002 Free Software Foundation

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

/**
 * This class can be used to perform the flattening required by the Shape
 * interface. It interpolates a curved path segment into a sequence of flat
 * ones within a certain flatness, up to a recursion limit.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Shape
 * @see RectangularShape#getPathIterator(AffineTransform, double)
 * @since 1.2
 * @status STUBS ONLY
 */
public class FlatteningPathIterator implements PathIterator
{
  // The iterator we are applied to.
  private PathIterator subIterator;
  private double flatness;
  private int limit;

  public FlatteningPathIterator(PathIterator src, double flatness)
  {
    this(src, flatness, 10);
  }
  public FlatteningPathIterator(PathIterator src, double flatness, int limit)
  {
    subIterator = src;
    this.flatness = flatness;
    this.limit = limit;
    if (flatness < 0 || limit < 0)
      throw new IllegalArgumentException();
  }

  public double getFlatness()
  {
    return flatness;
  }

  public int getRecursionLimit()
  {
    return limit;
  }

  public int getWindingRule()
  {
    return subIterator.getWindingRule();
  }

  public boolean isDone()
  {
    return subIterator.isDone();
  }

  public void next()
  {
    throw new Error("not implemented");
  }

  public int currentSegment(double[] coords)
  {
    throw new Error("not implemented");
  }
  public int currentSegment(float[] coords)
  {
    throw new Error("not implemented");
  }
} // class FlatteningPathIterator
