/* BasicStroke.java -- 
   Copyright (C) 2002, 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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


package java.awt;

import java.util.Arrays;

/**
 * A general purpose {@link Stroke} implementation that can represent a wide
 * variety of line styles for use with subclasses of {@link Graphics2D}.
 * <p>
 * The line cap and join styles can be set using the options illustrated 
 * here:
 * <p>
 * <img src="doc-files/capjoin.png" width="350" height="180"
 * alt="Illustration of line cap and join styles" />
 * <p>
 * A dash array can be used to specify lines with alternating opaque and
 * transparent sections.
 */
public class BasicStroke implements Stroke
{
  /** 
   * Indicates a mitered line join style. See the class overview for an
   * illustration.
   */
  public static final int JOIN_MITER = 0;
  
  /** 
   * Indicates a rounded line join style. See the class overview for an
   * illustration.
   */
  public static final int JOIN_ROUND = 1;
  
  /** 
   * Indicates a bevelled line join style. See the class overview for an
   * illustration.
   */
  public static final int JOIN_BEVEL = 2;

  /** 
   * Indicates a flat line cap style. See the class overview for an
   * illustration.
   */
  public static final int CAP_BUTT = 0;
  
  /** 
   * Indicates a rounded line cap style. See the class overview for an
   * illustration.
   */
  public static final int CAP_ROUND = 1;
  
  /** 
   * Indicates a square line cap style. See the class overview for an
   * illustration.
   */
  public static final int CAP_SQUARE = 2;

  /** The stroke width. */
  private final float width;
  
  /** The line cap style. */
  private final int cap;
  
  /** The line join style. */
  private final int join;
  
  /** The miter limit. */
  private final float limit;
  
  /** The dash array. */
  private final float[] dash;
  
  /** The dash phase. */
  private final float phase;

  /**
   * Creates a new <code>BasicStroke</code> instance with the given attributes.
   *
   * @param width  the line width (>= 0.0f).
   * @param cap  the line cap style (one of {@link #CAP_BUTT}, 
   *             {@link #CAP_ROUND} or {@link #CAP_SQUARE}).
   * @param join  the line join style (one of {@link #JOIN_ROUND}, 
   *              {@link #JOIN_BEVEL}, or {@link #JOIN_MITER}).
   * @param miterlimit  the limit to trim the miter join. The miterlimit must be
   * greater than or equal to 1.0f.
   * @param dash The array representing the dashing pattern. There must be at
   * least one non-zero entry.
   * @param dashPhase is negative and dash is not null.
   *
   * @throws IllegalArgumentException If one input parameter doesn't meet
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
   * Creates a new <code>BasicStroke</code> instance with the given attributes.
   *
   * @param width  the line width (>= 0.0f).
   * @param cap  the line cap style (one of {@link #CAP_BUTT}, 
   *             {@link #CAP_ROUND} or {@link #CAP_SQUARE}).
   * @param join  the line join style (one of {@link #JOIN_ROUND}, 
   *              {@link #JOIN_BEVEL}, or {@link #JOIN_MITER}).
   * @param miterlimit the limit to trim the miter join. The miterlimit must be
   * greater than or equal to 1.0f.
   * 
   * @throws IllegalArgumentException If one input parameter doesn't meet
   * its needs.
   */
  public BasicStroke(float width, int cap, int join, float miterlimit)
  {
    this(width, cap, join, miterlimit, null, 0);
  }

  /**
   * Creates a new <code>BasicStroke</code> instance with the given attributes.
   * The miter limit defaults to <code>10.0</code>.
   *
   * @param width  the line width (>= 0.0f).
   * @param cap  the line cap style (one of {@link #CAP_BUTT}, 
   *             {@link #CAP_ROUND} or {@link #CAP_SQUARE}).
   * @param join  the line join style (one of {@link #JOIN_ROUND}, 
   *              {@link #JOIN_BEVEL}, or {@link #JOIN_MITER}).
   * 
   * @throws IllegalArgumentException If one input parameter doesn't meet
   * its needs.
   */
  public BasicStroke(float width, int cap, int join)
  {
    this(width, cap, join, 10, null, 0);
  }

  /**
   * Creates a new <code>BasicStroke</code> instance with the given line
   * width.  The default values are:
   * <ul>
   * <li>line cap style: {@link #CAP_SQUARE};</li>
   * <li>line join style: {@link #JOIN_MITER};</li>
   * <li>miter limit: <code>10.0f</code>.
   * </ul>
   * 
   * @param width  the line width (>= 0.0f).
   * 
   * @throws IllegalArgumentException If <code>width</code> is negative.
   */
  public BasicStroke(float width)
  {
    this(width, CAP_SQUARE, JOIN_MITER, 10, null, 0);
  }

  /**
   * Creates a new <code>BasicStroke</code> instance.  The default values are:
   * <ul>
   * <li>line width: <code>1.0f</code>;</li>
   * <li>line cap style: {@link #CAP_SQUARE};</li>
   * <li>line join style: {@link #JOIN_MITER};</li>
   * <li>miter limit: <code>10.0f</code>.
   * </ul>
   */
  public BasicStroke()
  {
    this(1, CAP_SQUARE, JOIN_MITER, 10, null, 0);
  }
  
  /**
   * Creates a shape representing the stroked outline of the given shape.
   * THIS METHOD IS NOT YET IMPLEMENTED.
   * 
   * @param s  the shape.
   */
  public Shape createStrokedShape(Shape s)
  {
    // FIXME: Implement this
    throw new Error("not implemented");
  }

  /**
   * Returns the line width.
   * 
   * @return The line width.
   */
  public float getLineWidth()
  {
    return width;
  }

  /**
   * Returns a code indicating the line cap style (one of {@link #CAP_BUTT},
   * {@link #CAP_ROUND}, {@link #CAP_SQUARE}).
   * 
   * @return A code indicating the line cap style.
   */
  public int getEndCap()
  {
    return cap;
  }

  /**
   * Returns a code indicating the line join style (one of {@link #JOIN_BEVEL},
   * {@link #JOIN_MITER} or {@link #JOIN_ROUND}).
   * 
   * @return A code indicating the line join style.
   */
  public int getLineJoin()
  {
    return join;
  }

  /**
   * Returns the miter limit.
   * 
   * @return The miter limit.
   */
  public float getMiterLimit()
  {
    return limit;
  }

  /**
   * Returns the dash array, which defines the length of alternate opaque and 
   * transparent sections in lines drawn with this stroke.  If 
   * <code>null</code>, a continuous line will be drawn.
   * 
   * @return The dash array (possibly <code>null</code>).
   */
  public float[] getDashArray()
  {
    return dash;
  }

  /**
   * Returns the dash phase for the stroke.  This is the offset from the start
   * of a path at which the pattern defined by {@link #getDashArray()} is 
   * rendered.
   * 
   * @return The dash phase.
   */
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
   * 
   * @return The hash code.
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
   * Compares this <code>BasicStroke</code> for equality with an arbitrary 
   * object.  This method returns <code>true</code> if and only if:
   * <ul>
   * <li><code>o</code> is an instanceof <code>BasicStroke</code>;<li>
   * <li>this object has the same width, line cap style, line join style,
   * miter limit, dash array and dash phase as <code>o</code>.</li>
   * </ul>
   * 
   * @param o  the object (<code>null</code> permitted).
   * 
   * @return <code>true</code> if this stroke is equal to <code>o</code> and
   *         <code>false</code> otherwise.
   */
  public boolean equals(Object o)
  {
    if (! (o instanceof BasicStroke))
      return false;
    BasicStroke s = (BasicStroke) o;
    return width == s.width && cap == s.cap && join == s.join
      && limit == s.limit && Arrays.equals(dash, s.dash) && phase == s.phase;
  }
} 
