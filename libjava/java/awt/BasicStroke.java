/* BasicStroke.java -- 
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.awt;

import java.util.Arrays;

/**
 * STUB CLASS ONLY
 */
public class BasicStroke implements Stroke
{
  public static final int JOIN_MITER = 0;
  public static final int JOIN_ROUND = 1;
  public static final int JOIN_BEVEL = 2;

  public static final int CAP_BUTT = 0;
  public static final int CAP_ROUND = 1;
  public static final int CAP_SQUARE = 2;

  private final float width;
  private final int cap;
  private final int join;
  private final float limit;
  private final float[] dash;
  private final float phase;

  /**
   * Creates a basic stroke.
   *
   * @param width May not be negative .
   * @param cap May be either CAP_BUTT, CAP_ROUND or CAP_SQUARE.
   * @param join May be either JOIN_ROUND, JOIN_BEVEL, or JOIN_MITER.
   * @param miterlimit the limit to trim the miter join. The miterlimit must be
   * greater than or equal to 1.0f.
   * @param dash The array representing the dashing pattern. There must be at
   * least one non-zero entry.
   * @param dashPhase is negative and dash is not null.
   *
   * @exception IllegalArgumentException If one input parameter doesn't meet
   * its needs.
   */
  public BasicStroke(float width, int cap, int join, float miterlimit,
                     float[] dash, float dashPhase)
  {
    if (width < 0.0f )
      throw new IllegalArgumentException("width " + width + " < 0");
    else if (cap < CAP_BUTT || cap > CAP_SQUARE)
      throw new IllegalArgumentException("cap " + cap + " out of range ["
					 + CAP_BUTT + ".." + CAP_SQUARE + "]");
    else if (miterlimit < 1.0f && join == JOIN_MITER)
      throw new IllegalArgumentException("miterlimit " + miterlimit
					 + " < 1.0f while join == JOIN_MITER");
    else if (join < JOIN_MITER || join > JOIN_BEVEL)
      throw new IllegalArgumentException("join " + join + " out of range ["
					 + JOIN_MITER + ".." + JOIN_BEVEL
					 + "]");
    else if (dashPhase < 0.0f && dash != null)
      throw new IllegalArgumentException("dashPhase " + dashPhase
					 + " < 0.0f while dash != null");
    else if (dash != null)
      if (dash.length == 0)
	throw new IllegalArgumentException("dash.length is 0");
      else
	{
	  boolean allZero = true;
	  
	  for ( int i = 0; i < dash.length; ++i)
	    {
	      if (dash[i] != 0.0f)
		{
		  allZero = false;
		  break;
		}
	    }
	  
	  if (allZero)
	    throw new IllegalArgumentException("all dashes are 0.0f");
	}

    this.width = width;
    this.cap = cap;
    this.join = join;
    limit = miterlimit;
    this.dash = dash == null ? null : (float[]) dash.clone();
    phase = dashPhase;
  }

  /**
   * Creates a basic stroke.
   *
   * @param width The width of the BasicStroke. May not be negative .
   * @param cap May be either CAP_BUTT, CAP_ROUND or CAP_SQUARE.
   * @param join May be either JOIN_ROUND, JOIN_BEVEL, or JOIN_MITER.
   * @param miterlimit the limit to trim the miter join. The miterlimit must be
   * greater than or equal to 1.0f.
   * 
   * @exception IllegalArgumentException If one input parameter doesn't meet
   * its needs.
   */
  public BasicStroke(float width, int cap, int join, float miterlimit)
  {
    this(width, cap, join, miterlimit, null, 0);
  }

  /**
   * Creates a basic stroke.
   *
   * @param width The width of the BasicStroke. May not be nehative.
   * @param cap May be either CAP_BUTT, CAP_ROUND or CAP_SQUARE.
   * @param join May be either JOIN_ROUND, JOIN_BEVEL, or JOIN_MITER.
   * 
   * @exception IllegalArgumentException If one input parameter doesn't meet
   * its needs.
   * @exception IllegalArgumentException FIXME
   */
  public BasicStroke(float width, int cap, int join)
  {
    this(width, cap, join, 10, null, 0);
  }

  /**
   * Creates a basic stroke.
   *
   * @param width The width of the BasicStroke.
   * 
   * @exception IllegalArgumentException If width is negative.
   */
  public BasicStroke(float width)
  {
    this(width, CAP_SQUARE, JOIN_MITER, 10, null, 0);
  }

  /**
   * Creates a basic stroke.
   */
  public BasicStroke()
  {
    this(1, CAP_SQUARE, JOIN_MITER, 10, null, 0);
  }
  
  public Shape createStrokedShape(Shape s)
  {
    throw new Error("not implemented");
  }

  public float getLineWidth()
  {
    return width;
  }

  public int getEndCap()
  {
    return cap;
  }

  public int getLineJoin()
  {
    return join;
  }

  public float getMiterLimit()
  {
    return limit;
  }

  public float[] getDashArray()
  {
    return dash;
  }

  public float getDashPhase()
  {
    return phase;
  }

  /**
   * Returns the hash code for this object. The hash is calculated by
   * xoring the hash, cap, join, limit, dash array and phase values
   * (converted to <code>int</code> first with
   * <code>Float.floatToIntBits()</code> if the value is a
   * <code>float</code>).
   */
  public int hashCode()
  {
    int hash = Float.floatToIntBits(width);
    hash ^= cap;
    hash ^= join;
    hash ^= Float.floatToIntBits(limit);
   
    if (dash != null)
      for (int i = 0; i < dash.length; i++)
	hash ^=  Float.floatToIntBits(dash[i]);

    hash ^= Float.floatToIntBits(phase);

    return hash;
  }

  /**
   * Returns true if the given Object is an instance of BasicStroke
   * and the width, cap, join, limit, dash array and phase are all
   * equal.
   */
  public boolean equals(Object o)
  {
    if (! (o instanceof BasicStroke))
      return false;
    BasicStroke s = (BasicStroke) o;
    return width == s.width && cap == s.cap && join == s.join
      && limit == s.limit && Arrays.equals(dash, s.dash) && phase == s.phase;
  }
} // class BasicStroke
