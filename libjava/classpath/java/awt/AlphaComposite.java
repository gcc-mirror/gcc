/* AlphaComposite.java -- provides a context for performing alpha compositing
   Copyright (C) 2002, 2005, 2006  Free Software Foundation, Inc.

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

import gnu.java.awt.java2d.AlphaCompositeContext;

import java.awt.image.ColorModel;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Composite
 * @see CompositeContext
 * @since 1.3
 * @status updated to 1.4 except for createContext, needs documentation
 */
public final class AlphaComposite implements Composite
{
  /** Map Long to AlphaComposites. See getInstance for details. */
  private static final LinkedHashMap cache = new LinkedHashMap(11, 0.75f, true)
  {
    /** The largest the alpha composite cache can grow. */
    private static final int MAX_CACHE_SIZE = 2048;

    /** Prune stale entries. */
    protected boolean removeEldestEntry(Map.Entry eldest)
    {
      return size() > MAX_CACHE_SIZE;
    }
  };

  public static final int CLEAR = 1;
  public static final int SRC = 2;
  public static final int DST = 9;
  public static final int SRC_OVER = 3;
  public static final int DST_OVER = 4;
  public static final int SRC_IN = 5;
  public static final int DST_IN = 6;
  public static final int SRC_OUT = 7;
  public static final int DST_OUT = 8;
  public static final int SRC_ATOP = 10;
  public static final int DST_ATOP = 11;
  public static final int XOR = 12;
  public static final AlphaComposite Clear = getInstance(CLEAR);
  public static final AlphaComposite Src = getInstance(SRC);
  public static final AlphaComposite Dst = getInstance(DST);
  public static final AlphaComposite SrcOver = getInstance(SRC_OVER);
  public static final AlphaComposite DstOver = getInstance(DST_OVER);
  public static final AlphaComposite SrcIn = getInstance(SRC_IN);
  public static final AlphaComposite DstIn = getInstance(DST_IN);
  public static final AlphaComposite SrcOut = getInstance(SRC_OUT);
  public static final AlphaComposite DstOut = getInstance(DST_OUT);
  public static final AlphaComposite SrcAtop = getInstance(SRC_ATOP);
  public static final AlphaComposite DstAtop = getInstance(DST_ATOP);
  public static final AlphaComposite Xor = getInstance(XOR);
  
  private final int rule;
  private final float alpha;
  private AlphaComposite(int rule, float alpha)
  {
    this.rule = rule;
    this.alpha = alpha;
  }

  /**
   * Creates an AlphaComposite object with the specified rule.
   *
   * @param rule The compositing rule.
   *
   * @exception IllegalArgumentException If rule is not one of the following:
   * CLEAR, SRC, DST, SRC_OVER, DST_OVER, SRC_IN, DST_IN, SRC_OUT, DST_OUT,
   * SRC_ATOP, DST_ATOP, or XOR.
   */
  public static AlphaComposite getInstance(int rule)
  {
    return getInstance(rule, 1);
  }
  
  /**
   * Creates an AlphaComposite object with the specified rule and the constant
   * alpha to multiply with the alpha of the source. The source is multiplied
   * with the specified alpha before being composited with the destination.
   *
   * @param rule The compositing rule.
   *
   * @exception IllegalArgumentException If rule is not one of the following:
   * CLEAR, SRC, DST, SRC_OVER, DST_OVER, SRC_IN, DST_IN, SRC_OUT, DST_OUT,
   * SRC_ATOP, DST_ATOP, or XOR.
   */
  public static AlphaComposite getInstance(int rule, float alpha)
  {
    if (rule < CLEAR || rule > XOR || ! (alpha >= 0 && alpha <= 1))
      throw new IllegalArgumentException();
    // This long is guaranteed unique for all valid alpha composites.
    Long l = new Long(rule + Double.doubleToLongBits(alpha));
    AlphaComposite a = (AlphaComposite) cache.get(l);
    if (a == null)
      {
        a = new AlphaComposite(rule, alpha);
        cache.put(l, a);
      }
    return a;
  }

  /**
   * Creates a {@link CompositeContext} that can be used to perform
   * compositing operations according to this AlphaComposite settings.
   *
   * @param srcColorModel the color model of the source raster
   * @param dstColorModel the color model of the destination raster
   * @param hints the rendering hints to use
   *
   * @return a {@link CompositeContext} that can be used to perform
   *         compositing operations according to this AlphaComposite settings
   */
  public CompositeContext createContext(ColorModel srcColorModel,
                                        ColorModel dstColorModel,
                                        RenderingHints hints)
  {
    return new AlphaCompositeContext(this, srcColorModel, dstColorModel);
  }

  /**
   * Return an <code>AlphaComposite</code> similar to <code>this</code>,
   * that uses the specified rule. If <code>rule</code> is the same as
   * <code>this.rule</code>, then <code>this</code> is returned.
   * 
   * @since 1.6
   */
  public AlphaComposite derive(int rule)
  {
    if (this.rule == rule)
      return this;
    else
      return AlphaComposite.getInstance(rule, this.getAlpha());
  }
  
  /**
   * Return an <code>AlphaComposite</code> similar to <code>this</code>,
   * that uses the specified <code>alpha</code>.
   * 
   * If <code>alph</code> is the same as <code>this.alpha</code>,
   * then <code>this</code> is returned.
   * 
   * @since 1.6
   */
  public AlphaComposite derive(float alpha)
  {
      if (this.getAlpha() == alpha)
        return this;
      else
        return AlphaComposite.getInstance(this.getRule(), alpha);
  }
  
  public float getAlpha()
  {
    return alpha;
  }
  
  public int getRule()
  {
    return rule;
  }
  
  public int hashCode()
  {
    return 31 * Float.floatToIntBits(alpha) + rule;
  }
  
  public boolean equals(Object o)
  {
    if (! (o instanceof AlphaComposite))
      return false;
    AlphaComposite a = (AlphaComposite) o;
    return rule == a.rule && alpha == a.alpha;
  }
} // class AlphaComposite
