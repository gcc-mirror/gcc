/* RenderingHints.java --
   Copyright (C) 2000, 2001, 2002  Free Software Foundation

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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * NEEDS DOCUMENTATION
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 * @author Eric Blake <ebb9@email.byu.edu>
 */
public class RenderingHints implements Map, Cloneable
{
  public abstract static class Key
  {
    private final int key;

    protected Key(int privateKey)
    {
      key = privateKey;
    }

    public abstract boolean isCompatibleValue(Object value);

    protected final int intKey()
    {
      return key;
    }

    public final int hashCode()
    {
      return System.identityHashCode(this);
    }

    public final boolean equals(Object other)
    {
      return this == other;
    }
  } // class Key

  private static final class KeyImpl extends Key
  {
    final String description;
    final Object v1;
    final Object v2;
    final Object v3;

    KeyImpl(int privateKey, String description,
            Object v1, Object v2, Object v3)
    {
      super(privateKey);
      this.description = description;
      this.v1 = v1;
      this.v2 = v2;
      this.v3 = v3;
    }

    public boolean isCompatibleValue(Object value)
    {
      return value == v1 || value == v2 || value == v3;
    }

    public String toString()
    {
      return description;
    }
  } // class KeyImpl

  private HashMap hintMap = new HashMap();

  public static final Key KEY_ANTIALIASING;

  public static final Object VALUE_ANTIALIAS_ON
    = "Antialiased rendering mode";

  public static final Object VALUE_ANTIALIAS_OFF
    = "Nonantialiased rendering mode";

  public static final Object VALUE_ANTIALIAS_DEFAULT
    = "Default antialiasing rendering mode";

  public static final Key KEY_RENDERING;

  public static final Object VALUE_RENDER_SPEED
    = "Fastest rendering methods";

  public static final Object VALUE_RENDER_QUALITY
    = "Highest quality rendering methods";

  public static final Object VALUE_RENDER_DEFAULT
    = "Default rendering methods";

  public static final Key KEY_DITHERING;

  public static final Object VALUE_DITHER_DISABLE
    = "Nondithered rendering mode";

  public static final Object VALUE_DITHER_ENABLE
    = "Dithered rendering mode";

  public static final Object VALUE_DITHER_DEFAULT
    = "Default dithering mode";

  public static final Key KEY_TEXT_ANTIALIASING;

  public static final Object VALUE_TEXT_ANTIALIAS_ON
    = "Antialiased text mode";

  public static final Object VALUE_TEXT_ANTIALIAS_OFF
    = "Nonantialiased text mode";

  public static final Object VALUE_TEXT_ANTIALIAS_DEFAULT
    = "Default antialiasing text mode";

  public static final Key KEY_FRACTIONALMETRICS;

  public static final Object VALUE_FRACTIONALMETRICS_OFF
    = "Integer text metrics mode";

  public static final Object VALUE_FRACTIONALMETRICS_ON
    = "Fractional text metrics mode";

  public static final Object VALUE_FRACTIONALMETRICS_DEFAULT
    = "Default fractional text metrics mode";

  public static final Key KEY_INTERPOLATION;

  public static final Object VALUE_INTERPOLATION_NEAREST_NEIGHBOR
    = "Nearest Neighbor image interpolation mode";

  public static final Object VALUE_INTERPOLATION_BILINEAR
    = "Bilinear image interpolation mode";

  public static final Object VALUE_INTERPOLATION_BICUBIC
    = "Bicubic image interpolation mode";

  public static final Key KEY_ALPHA_INTERPOLATION;

  public static final Object VALUE_ALPHA_INTERPOLATION_SPEED
    = "Fastest alpha blending methods";

  public static final Object VALUE_ALPHA_INTERPOLATION_QUALITY
    = "Highest quality alpha blending methods";

  public static final Object VALUE_ALPHA_INTERPOLATION_DEFAULT
    = "Default alpha blending methods";

  public static final Key KEY_COLOR_RENDERING;

  public static final Object VALUE_COLOR_RENDER_SPEED
    = "Fastest color rendering mode";

  public static final Object VALUE_COLOR_RENDER_QUALITY
    = "Highest quality color rendering mode";

  public static final Object VALUE_COLOR_RENDER_DEFAULT
    = "Default color rendering mode";

  public static final Key KEY_STROKE_CONTROL;

  public static final Object VALUE_STROKE_DEFAULT
    = "Default stroke normalization";

  public static final Object VALUE_STROKE_NORMALIZE
    = "Normalize strokes for consistent rendering";

  public static final Object VALUE_STROKE_PURE
    = "Pure stroke conversion for accurate paths";

  static
  {
    KEY_ANTIALIASING = new KeyImpl(1, "Global antialiasing enable key",
                                   VALUE_ANTIALIAS_ON,
                                   VALUE_ANTIALIAS_OFF,
                                   VALUE_ANTIALIAS_DEFAULT);
    KEY_RENDERING = new KeyImpl(2, "Global rendering quality key",
                                VALUE_RENDER_SPEED,
                                VALUE_RENDER_QUALITY,
                                VALUE_RENDER_DEFAULT);
    KEY_DITHERING = new KeyImpl(3, "Dithering quality key",
                                VALUE_DITHER_DISABLE,
                                VALUE_DITHER_ENABLE,
                                VALUE_DITHER_DEFAULT);
    KEY_TEXT_ANTIALIASING
      = new KeyImpl(4, "Text-specific antialiasing enable key",
                    VALUE_TEXT_ANTIALIAS_ON,
                    VALUE_TEXT_ANTIALIAS_OFF,
                    VALUE_TEXT_ANTIALIAS_DEFAULT);
    KEY_FRACTIONALMETRICS = new KeyImpl(5, "Fractional metrics enable key",
                                        VALUE_FRACTIONALMETRICS_OFF,
                                        VALUE_FRACTIONALMETRICS_ON,
                                        VALUE_FRACTIONALMETRICS_DEFAULT);
    KEY_INTERPOLATION = new KeyImpl(6, "Image interpolation method key",
                                    VALUE_INTERPOLATION_NEAREST_NEIGHBOR,
                                    VALUE_INTERPOLATION_BILINEAR,
                                    VALUE_INTERPOLATION_BICUBIC);
    KEY_ALPHA_INTERPOLATION
      = new KeyImpl(7, "Alpha blending interpolation method key",
                    VALUE_ALPHA_INTERPOLATION_SPEED,
                    VALUE_ALPHA_INTERPOLATION_QUALITY,
                    VALUE_ALPHA_INTERPOLATION_DEFAULT);
    KEY_COLOR_RENDERING = new KeyImpl(8, "Color rendering quality key",
                                      VALUE_COLOR_RENDER_SPEED,
                                      VALUE_COLOR_RENDER_QUALITY,
                                      VALUE_COLOR_RENDER_DEFAULT);
    KEY_STROKE_CONTROL = new KeyImpl(9, "Stroke normalization control key",
                                     VALUE_STROKE_DEFAULT,
                                     VALUE_STROKE_NORMALIZE,
                                     VALUE_STROKE_PURE);
  }

  public RenderingHints(Map init)
  {
    putAll(init);
  }

  public RenderingHints(Key key, Object value)
  {
    put(key, value);
  }

  public int size()
  {
    return hintMap.size();
  }

  public boolean isEmpty()
  {
    return hintMap.isEmpty();
  }

  public boolean containsKey(Object key)
  {
    if (key == null)
      throw new NullPointerException();
    return hintMap.containsKey((Key) key);
  }

  public boolean containsValue(Object value)
  {
    return hintMap.containsValue(value);
  }

  public Object get(Object key)
  {
    return hintMap.get((Key) key);
  }

  public Object put(Object key, Object value)
  {
    if (key == null || value == null)
      throw new NullPointerException();
    if (! ((Key) key).isCompatibleValue(value))
      throw new IllegalArgumentException();
    return hintMap.put(key, value);
  }

  public void add(RenderingHints hints)
  {
    hintMap.putAll(hints);
  }

  public void clear()
  {
    hintMap.clear();
  }

  public Object remove(Object key)
  {
    return remove((Key) key);
  }

  public void putAll(Map m)
  {
    hintMap.putAll(m);
  }

  public Set keySet()
  {
    return hintMap.keySet();
  }

  public Collection values()
  {
    return hintMap.values();
  }

  public Set entrySet()
  {
    return Collections.unmodifiableSet(hintMap.entrySet());
  }

  public boolean equals(Object o)
  {
    return hintMap.equals(o);
  }

  public int hashCode()
  {
    return hintMap.hashCode();
  }

  public Object clone()
  {
    try
      {
        RenderingHints copy = (RenderingHints) super.clone();
        copy.hintMap = (HashMap) hintMap.clone();
        return copy;
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e); // Impossible
      }
  }

  public String toString()
  {
    return hintMap.toString();
  }
} // class RenderingHints
