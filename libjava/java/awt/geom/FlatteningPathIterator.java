/* FlatteningPathIterator.java -- Approximates curves by straight lines
   Copyright (C) 2003 Free Software Foundation

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


package java.awt.geom;

import java.util.NoSuchElementException;


/**
 * A PathIterator for approximating curved path segments by sequences
 * of straight lines. Instances of this class will only return
 * segments of type {@link PathIterator#SEG_MOVETO}, {@link
 * PathIterator#SEG_LINETO}, and {@link PathIterator#SEG_CLOSE}.
 *
 * <p>The accuracy of the approximation is determined by two
 * parameters:
 *
 * <ul><li>The <i>flatness</i> is a threshold value for deciding when
 * a curved segment is consided flat enough for being approximated by
 * a single straight line. Flatness is defined as the maximal distance
 * of a curve control point to the straight line that connects the
 * curve start and end. A lower flatness threshold means a closer
 * approximation.  See {@link QuadCurve2D#getFlatness()} and {@link
 * CubicCurve2D#getFlatness()} for drawings which illustrate the
 * meaning of flatness.</li>
 *
 * <li>The <i>recursion limit</i> imposes an upper bound for how often
 * a curved segment gets subdivided. A limit of <i>n</i> means that
 * for each individual quadratic and cubic B&#xe9;zier spline
 * segment, at most 2<sup><small><i>n</i></small></sup> {@link
 * PathIterator#SEG_LINETO} segments will be created.</li></ul>
 *
 * <p><b>Memory Efficiency:</b> The memory consumption grows linearly
 * with the recursion limit. Neither the <i>flatness</i> parameter nor
 * the number of segments in the flattened path will affect the memory
 * consumption.
 *
 * <p><b>Thread Safety:</b> Multiple threads can safely work on
 * separate instances of this class. However, multiple threads should
 * not concurrently access the same instance, as no synchronization is
 * performed.
 *
 * @see <a href="doc-files/FlatteningPathIterator-1.html"
 * >Implementation Note</a>
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 *
 * @since 1.2
 */
public class FlatteningPathIterator
  implements PathIterator
{
  /**
   * The PathIterator whose curved segments are being approximated.
   */
  private final PathIterator srcIter;


  /**
   * The square of the flatness threshold value, which determines when
   * a curve segment is considered flat enough that no further
   * subdivision is needed.
   *
   * <p>Calculating flatness actually produces the squared flatness
   * value. To avoid the relatively expensive calculation of a square
   * root for each curve segment, we perform all flatness comparisons
   * on squared values.
   *
   * @see QuadCurve2D#getFlatnessSq()
   * @see CubicCurve2D#getFlatnessSq()
   */
  private final double flatnessSq;


  /**
   * The maximal number of subdivions that are performed to
   * approximate a quadratic or cubic curve segment.
   */
  private final int recursionLimit;


  /**
   * A stack for holding the coordinates of subdivided segments.
   *
   * @see <a href="doc-files/FlatteningPathIterator-1.html"
   * >Implementation Note</a>
   */
  private double[] stack;


  /**
   * The current stack size.
   *
   * @see <a href="doc-files/FlatteningPathIterator-1.html"
   * >Implementation Note</a>
   */
  private int stackSize;


  /**
   * The number of recursions that were performed to arrive at
   * a segment on the stack.
   *
   * @see <a href="doc-files/FlatteningPathIterator-1.html"
   * >Implementation Note</a>
   */
  private int[] recLevel;

  
  
  private final double[] scratch = new double[6];


  /**
   * The segment type of the last segment that was returned by
   * the source iterator.
   */
  private int srcSegType;


  /**
   * The current <i>x</i> position of the source iterator.
   */
  private double srcPosX;


  /**
   * The current <i>y</i> position of the source iterator.
   */
  private double srcPosY;


  /**
   * A flag that indicates when this path iterator has finished its
   * iteration over path segments.
   */
  private boolean done;


  /**
   * Constructs a new PathIterator for approximating an input
   * PathIterator with straight lines. The approximation works by
   * recursive subdivisons, until the specified flatness threshold is
   * not exceeded.
   *
   * <p>There will not be more than 10 nested recursion steps, which
   * means that a single <code>SEG_QUADTO</code> or
   * <code>SEG_CUBICTO</code> segment is approximated by at most
   * 2<sup><small>10</small></sup> = 1024 straight lines.
   */
  public FlatteningPathIterator(PathIterator src, double flatness)
  {
    this(src, flatness, 10);
  }


  /**
   * Constructs a new PathIterator for approximating an input
   * PathIterator with straight lines. The approximation works by
   * recursive subdivisons, until the specified flatness threshold is
   * not exceeded.  Additionally, the number of recursions is also
   * bound by the specified recursion limit.
   */
  public FlatteningPathIterator(PathIterator src, double flatness,
                                int limit)
  {
    if (flatness < 0 || limit < 0)
      throw new IllegalArgumentException();

    srcIter = src;
    flatnessSq = flatness * flatness;
    recursionLimit = limit;
    fetchSegment();
  }


  /**
   * Returns the maximally acceptable flatness.
   *
   * @see QuadCurve2D#getFlatness()
   * @see CubicCurve2D#getFlatness()
   */
  public double getFlatness()
  {
    return Math.sqrt(flatnessSq);
  }


  /**
   * Returns the maximum number of recursive curve subdivisions.
   */
  public int getRecursionLimit()
  {
    return recursionLimit;
  }


  // Documentation will be copied from PathIterator.
  public int getWindingRule()
  {
    return srcIter.getWindingRule();
  }


  // Documentation will be copied from PathIterator.
  public boolean isDone()
  {
    return done;
  }


  // Documentation will be copied from PathIterator.
  public void next()
  {
    if (stackSize > 0)
    {
      --stackSize;
      if (stackSize > 0)
      {
        switch (srcSegType)
        {
        case PathIterator.SEG_QUADTO:
          subdivideQuadratic();
          return;

        case PathIterator.SEG_CUBICTO:
          subdivideCubic();
          return;

        default:
          throw new IllegalStateException();
        }
      }
    }

    srcIter.next();
    fetchSegment();
  }


  // Documentation will be copied from PathIterator.
  public int currentSegment(double[] coords)
  {
    if (done)
      throw new NoSuchElementException();

    switch (srcSegType)
    {
    case PathIterator.SEG_CLOSE:
      return srcSegType;

    case PathIterator.SEG_MOVETO:
    case PathIterator.SEG_LINETO:
      coords[0] = srcPosX;
      coords[1] = srcPosY;
      return srcSegType;

    case PathIterator.SEG_QUADTO:
      if (stackSize == 0)
      {
        coords[0] = srcPosX;
        coords[1] = srcPosY;
      }
      else
      {
        int sp = stack.length - 4 * stackSize;
        coords[0] = stack[sp + 2];
        coords[1] = stack[sp + 3];
      }
      return PathIterator.SEG_LINETO;

    case PathIterator.SEG_CUBICTO:
      if (stackSize == 0)
      {
        coords[0] = srcPosX;
        coords[1] = srcPosY;
      }
      else
      {
        int sp = stack.length - 6 * stackSize;
        coords[0] = stack[sp + 4];
        coords[1] = stack[sp + 5];
      }
      return PathIterator.SEG_LINETO;
    }

    throw new IllegalStateException();
  }


  // Documentation will be copied from PathIterator.
  public int currentSegment(float[] coords)
  {
    if (done)
      throw new NoSuchElementException();

    switch (srcSegType)
    {
    case PathIterator.SEG_CLOSE:
      return srcSegType;

    case PathIterator.SEG_MOVETO:
    case PathIterator.SEG_LINETO:
      coords[0] = (float) srcPosX;
      coords[1] = (float) srcPosY;
      return srcSegType;

    case PathIterator.SEG_QUADTO:
      if (stackSize == 0)
      {
        coords[0] = (float) srcPosX;
        coords[1] = (float) srcPosY;
      }
      else
      {
        int sp = stack.length - 4 * stackSize;
        coords[0] = (float) stack[sp + 2];
        coords[1] = (float) stack[sp + 3];
      }
      return PathIterator.SEG_LINETO;

    case PathIterator.SEG_CUBICTO:
      if (stackSize == 0)
      {
        coords[0] = (float) srcPosX;
        coords[1] = (float) srcPosY;
      }
      else
      {
        int sp = stack.length - 6 * stackSize;
        coords[0] = (float) stack[sp + 4];
        coords[1] = (float) stack[sp + 5];
      }
      return PathIterator.SEG_LINETO;
    }

    throw new IllegalStateException();
  }


  /**
   * Fetches the next segment from the source iterator.
   */
  private void fetchSegment()
  {
    int sp;

    if (srcIter.isDone())
    {
      done = true;
      return;
    }

    srcSegType = srcIter.currentSegment(scratch);
    
    switch (srcSegType)
    {
    case PathIterator.SEG_CLOSE:
      return;

    case PathIterator.SEG_MOVETO:
    case PathIterator.SEG_LINETO:
      srcPosX = scratch[0];
      srcPosY = scratch[1];
      return;

    case PathIterator.SEG_QUADTO:
      if (recursionLimit == 0)
      {
        srcPosX = scratch[2];
        srcPosY = scratch[3];
        stackSize = 0;
        return;
      }
      sp = 4 * recursionLimit;
      stackSize = 1;
      if (stack == null)
      {
        stack = new double[sp + /* 4 + 2 */ 6];
        recLevel = new int[recursionLimit + 1];
      }
      recLevel[0] = 0;
      stack[sp] = srcPosX;                  // P1.x
      stack[sp + 1] = srcPosY;              // P1.y
      stack[sp + 2] = scratch[0];           // C.x
      stack[sp + 3] = scratch[1];           // C.y
      srcPosX = stack[sp + 4] = scratch[2]; // P2.x
      srcPosY = stack[sp + 5] = scratch[3]; // P2.y
      subdivideQuadratic();
      break;

    case PathIterator.SEG_CUBICTO:
      if (recursionLimit == 0)
      {
        srcPosX = scratch[4];
        srcPosY = scratch[5];
        stackSize = 0;
        return;
      }
      sp = 6 * recursionLimit;
      stackSize = 1;
      if ((stack == null) || (stack.length < sp + 8))
      {
        stack = new double[sp + /* 6 + 2 */ 8];
        recLevel = new int[recursionLimit + 1];
      }
      recLevel[0] = 0;
      stack[sp] = srcPosX;                  // P1.x
      stack[sp + 1] = srcPosY;              // P1.y
      stack[sp + 2] = scratch[0];           // C1.x
      stack[sp + 3] = scratch[1];           // C1.y
      stack[sp + 4] = scratch[2];           // C2.x
      stack[sp + 5] = scratch[3];           // C2.y
      srcPosX = stack[sp + 6] = scratch[4]; // P2.x
      srcPosY = stack[sp + 7] = scratch[5]; // P2.y
      subdivideCubic();
      return;
    }
  }


  /**
   * Repeatedly subdivides the quadratic curve segment that is on top
   * of the stack. The iteration terminates when the recursion limit
   * has been reached, or when the resulting segment is flat enough.
   */
  private void subdivideQuadratic()
  {
    int sp;
    int level;

    sp = stack.length - 4 * stackSize - 2;
    level = recLevel[stackSize - 1];
    while ((level < recursionLimit)
           && (QuadCurve2D.getFlatnessSq(stack, sp) >= flatnessSq))
    {
      recLevel[stackSize] = recLevel[stackSize - 1] = ++level;
      QuadCurve2D.subdivide(stack, sp, stack, sp - 4, stack, sp);
      ++stackSize;
      sp -= 4;
    }
  }


  /**
   * Repeatedly subdivides the cubic curve segment that is on top
   * of the stack. The iteration terminates when the recursion limit
   * has been reached, or when the resulting segment is flat enough.
   */
  private void subdivideCubic()
  {
    int sp;
    int level;

    sp = stack.length - 6 * stackSize - 2;
    level = recLevel[stackSize - 1];
    while ((level < recursionLimit)
           && (CubicCurve2D.getFlatnessSq(stack, sp) >= flatnessSq))
    {
      recLevel[stackSize] = recLevel[stackSize - 1] = ++level;
      
      CubicCurve2D.subdivide(stack, sp, stack, sp - 6, stack, sp);
      ++stackSize;
      sp -= 6;
    }
  }


  /* These routines were useful for debugging. Since they would
   * just bloat the implementation, they are commented out.
   *
   *

  private static String segToString(int segType, double[] d, int offset)
  {
    String s;

    switch (segType)
    {
    case PathIterator.SEG_CLOSE:
      return "SEG_CLOSE";

    case PathIterator.SEG_MOVETO:
      return "SEG_MOVETO (" + d[offset] + ", " + d[offset + 1] + ")";

    case PathIterator.SEG_LINETO:
      return "SEG_LINETO (" + d[offset] + ", " + d[offset + 1] + ")";

    case PathIterator.SEG_QUADTO:
      return "SEG_QUADTO (" + d[offset] + ", " + d[offset + 1]
        + ") (" + d[offset + 2] + ", " + d[offset + 3] + ")";

    case PathIterator.SEG_CUBICTO:
      return "SEG_CUBICTO (" + d[offset] + ", " + d[offset + 1]
        + ") (" + d[offset + 2] + ", " + d[offset + 3]
        + ") (" + d[offset + 4] + ", " + d[offset + 5] + ")";
    }

    throw new IllegalStateException();
  }


  private void dumpQuadraticStack(String msg)
  {
    int sp = stack.length - 4 * stackSize - 2;
    int i = 0;
    System.err.print("    " + msg + ":");
    while (sp < stack.length)
    {
      System.err.print(" (" + stack[sp] + ", " + stack[sp+1] + ")");
      if (i < recLevel.length)
        System.out.print("/" + recLevel[i++]);
      if (sp + 3 < stack.length)
        System.err.print(" [" + stack[sp+2] + ", " + stack[sp+3] + "]");
      sp += 4;
    }
    System.err.println();
  }


  private void dumpCubicStack(String msg)
  {
    int sp = stack.length - 6 * stackSize - 2;
    int i = 0;
    System.err.print("    " + msg + ":");
    while (sp < stack.length)
    {
      System.err.print(" (" + stack[sp] + ", " + stack[sp+1] + ")");
      if (i < recLevel.length)
        System.out.print("/" + recLevel[i++]);
      if (sp + 3 < stack.length)
      {
        System.err.print(" [" + stack[sp+2] + ", " + stack[sp+3] + "]");
        System.err.print(" [" + stack[sp+4] + ", " + stack[sp+5] + "]");
      }
      sp += 6;
    }
    System.err.println();
  }

  *
  *
  */
}
