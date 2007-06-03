/* RenderingHints.java --
   Copyright (C) 2000, 2001, 2002, 2004, 2005  Free Software Foundation

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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * A collection of (key, value) items that provide 'hints' for the 
 * {@link java.awt.Graphics2D} rendering pipeline.  Because these
 * items are hints only, they may be ignored by a particular
 * {@link java.awt.Graphics2D} implementation.
 *
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 * @author Eric Blake (ebb9@email.byu.edu)
 */
public class RenderingHints
  implements Map<Object,Object>, Cloneable
{
  /**
   * The base class used to represent keys.
   */
  public abstract static class Key
  {
    private final int key;

    /**
     * Creates a new key.
     * 
     * @param privateKey  the private key.
     */
    protected Key(int privateKey)
    {
      key = privateKey;
    }

    /**
     * Returns <code>true</code> if the specified value is compatible with
     * this key, and <code>false</code> otherwise.
     * 
     * @param value  the value (<code>null</code> permitted).
     * 
     * @return A boolean.
     */
    public abstract boolean isCompatibleValue(Object value);

    /**
     * Returns the private key for this instance.
     * 
     * @return The private key.
     */
    protected final int intKey()
    {
      return key;
    }

    /**
     * Returns a hash code for the key.
     * 
     * @return A hash code.
     */
    public final int hashCode()
    {
      return System.identityHashCode(this);
    }

    /**
     * Checks this key for equality with an arbitrary object.
     * 
     * @param other  the object (<code>null</code> permitted)
     * 
     * @return A boolean.
     */
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

    /**
     * Returns <code>true</code> if the specified value is compatible with
     * this key, and <code>false</code> otherwise.
     * 
     * @param value  the value (<code>null</code> permitted).
     * 
     * @return A boolean.
     */
    public boolean isCompatibleValue(Object value)
    {
      return value == v1 || value == v2 || value == v3;
    }

    /**
     * Returns a string representation of the key.
     * 
     * @return A string.
     */
    public String toString()
    {
      return description;
    }
  } // class KeyImpl

  private HashMap<Object,Object> hintMap = new HashMap<Object,Object>();

  /**
   * A key for the 'antialiasing' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_ANTIALIAS_OFF}</td>
   *   <td>Render without antialiasing (better speed).</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_ANTIALIAS_ON}</td>
   *   <td>Render with antialiasing (better quality).</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_ANTIALIAS_DEFAULT}</td>
   *   <td>Use the default value for antialiasing.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_ANTIALIASING;

  /**
   * This value is for use with the {@link #KEY_ANTIALIASING} key.
   */
  public static final Object VALUE_ANTIALIAS_ON
    = "Antialiased rendering mode";

  /**
   * This value is for use with the {@link #KEY_ANTIALIASING} key.
   */
  public static final Object VALUE_ANTIALIAS_OFF
    = "Nonantialiased rendering mode";

  /**
   * This value is for use with the {@link #KEY_ANTIALIASING} key.
   */
  public static final Object VALUE_ANTIALIAS_DEFAULT
    = "Default antialiasing rendering mode";

  /**
   * A key for the 'rendering' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_RENDER_SPEED}</td>
   *   <td>Prefer speed over quality when rendering.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_RENDER_QUALITY}</td>
   *   <td>Prefer quality over speed when rendering.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_RENDER_DEFAULT}</td>
   *   <td>Use the default value for quality vs. speed when rendering.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_RENDERING;

  /**
   * This value is for use with the {@link #KEY_RENDERING} key.
   */
  public static final Object VALUE_RENDER_SPEED
    = "Fastest rendering methods";

  /**
   * This value is for use with the {@link #KEY_RENDERING} key.
   */
  public static final Object VALUE_RENDER_QUALITY
    = "Highest quality rendering methods";

  /**
   * This value is for use with the {@link #KEY_RENDERING} key.
   */
  public static final Object VALUE_RENDER_DEFAULT
    = "Default rendering methods";

  /**
   * A key for the 'dithering' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_DITHER_DISABLE}</td>
   *   <td>Disable dithering.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_DITHER_ENABLE}</td>
   *   <td>Enable dithering.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_DITHER_DEFAULT}</td>
   *   <td>Use the default value for dithering.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_DITHERING;

  /**
   * This value is for use with the {@link #KEY_DITHERING} key.
   */
  public static final Object VALUE_DITHER_DISABLE
    = "Nondithered rendering mode";

  /**
   * This value is for use with the {@link #KEY_DITHERING} key.
   */
  public static final Object VALUE_DITHER_ENABLE
    = "Dithered rendering mode";

  /**
   * This value is for use with the {@link #KEY_DITHERING} key.
   */
  public static final Object VALUE_DITHER_DEFAULT
    = "Default dithering mode";

  /**
   * A key for the 'text antialiasing' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_TEXT_ANTIALIAS_ON}</td>
   *   <td>Render text with antialiasing (better quality usually).</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_TEXT_ANTIALIAS_OFF}</td>
   *   <td>Render test without antialiasing (better speed).</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_TEXT_ANTIALIAS_DEFAULT}</td>
   *   <td>Use the default value for text antialiasing.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_TEXT_ANTIALIASING;

  /**
   * This value is for use with the {@link #KEY_TEXT_ANTIALIASING} key.
   */
  public static final Object VALUE_TEXT_ANTIALIAS_ON
    = "Antialiased text mode";

  /**
   * This value is for use with the {@link #KEY_TEXT_ANTIALIASING} key.
   */
  public static final Object VALUE_TEXT_ANTIALIAS_OFF
    = "Nonantialiased text mode";

  /**
   * This value is for use with the {@link #KEY_TEXT_ANTIALIASING} key.
   */
  public static final Object VALUE_TEXT_ANTIALIAS_DEFAULT
    = "Default antialiasing text mode";

  /**
   * A key for the 'fractional metrics' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_FRACTIONALMETRICS_OFF}</td>
   *   <td>Render text with fractional metrics off.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_FRACTIONALMETRICS_ON}</td>
   *   <td>Render text with fractional metrics on.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_FRACTIONALMETRICS_DEFAULT}</td>
   *   <td>Use the default value for fractional metrics.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_FRACTIONALMETRICS;

  /**
   * This value is for use with the {@link #KEY_FRACTIONALMETRICS} key.
   */
  public static final Object VALUE_FRACTIONALMETRICS_OFF
    = "Integer text metrics mode";

  /**
   * This value is for use with the {@link #KEY_FRACTIONALMETRICS} key.
   */
  public static final Object VALUE_FRACTIONALMETRICS_ON
    = "Fractional text metrics mode";

  /**
   * This value is for use with the {@link #KEY_FRACTIONALMETRICS} key.
   */
  public static final Object VALUE_FRACTIONALMETRICS_DEFAULT
    = "Default fractional text metrics mode";

  /**
   * A key for the 'interpolation' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_INTERPOLATION_NEAREST_NEIGHBOR}</td>
   *   <td>Use nearest neighbour interpolation.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_INTERPOLATION_BILINEAR}</td>
   *   <td>Use bilinear interpolation.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_INTERPOLATION_BICUBIC}</td>
   *   <td>Use bicubic interpolation.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_INTERPOLATION;

  /**
   * This value is for use with the {@link #KEY_INTERPOLATION} key.
   */
  public static final Object VALUE_INTERPOLATION_NEAREST_NEIGHBOR
    = "Nearest Neighbor image interpolation mode";

  /**
   * This value is for use with the {@link #KEY_INTERPOLATION} key.
   */
  public static final Object VALUE_INTERPOLATION_BILINEAR
    = "Bilinear image interpolation mode";

  /**
   * This value is for use with the {@link #KEY_INTERPOLATION} key.
   */
  public static final Object VALUE_INTERPOLATION_BICUBIC
    = "Bicubic image interpolation mode";

  /**
   * A key for the 'alpha interpolation' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_ALPHA_INTERPOLATION_SPEED}</td>
   *   <td>Prefer speed over quality.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_ALPHA_INTERPOLATION_QUALITY}</td>
   *   <td>Prefer quality over speed.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_ALPHA_INTERPOLATION_DEFAULT}</td>
   *   <td>Use the default setting.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_ALPHA_INTERPOLATION;

  /**
   * This value is for use with the {@link #KEY_ALPHA_INTERPOLATION} key.
   */
  public static final Object VALUE_ALPHA_INTERPOLATION_SPEED
    = "Fastest alpha blending methods";

  /**
   * This value is for use with the {@link #KEY_ALPHA_INTERPOLATION} key.
   */
  public static final Object VALUE_ALPHA_INTERPOLATION_QUALITY
    = "Highest quality alpha blending methods";

  /**
   * This value is for use with the {@link #KEY_ALPHA_INTERPOLATION} key.
   */
  public static final Object VALUE_ALPHA_INTERPOLATION_DEFAULT
    = "Default alpha blending methods";

  /**
   * A key for the 'color rendering' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_COLOR_RENDER_SPEED}</td>
   *   <td>Prefer speed over quality.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_COLOR_RENDER_QUALITY}</td>
   *   <td>Prefer quality over speed.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_COLOR_RENDER_DEFAULT}</td>
   *   <td>Use the default setting.</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_COLOR_RENDERING;

  /**
   * This value is for use with the {@link #KEY_COLOR_RENDERING} key.
   */
  public static final Object VALUE_COLOR_RENDER_SPEED
    = "Fastest color rendering mode";

  /**
   * This value is for use with the {@link #KEY_COLOR_RENDERING} key.
   */
  public static final Object VALUE_COLOR_RENDER_QUALITY
    = "Highest quality color rendering mode";

  /**
   * This value is for use with the {@link #KEY_COLOR_RENDERING} key.
   */
  public static final Object VALUE_COLOR_RENDER_DEFAULT
    = "Default color rendering mode";

  /**
   * A key for the 'stroke control' hint.  Permitted values are:
   * <p>
   * <table>
   * <tr>
   *   <td>{@link #VALUE_STROKE_DEFAULT}</td>
   *   <td>Use the default setting.</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_STROKE_NORMALIZE}</td>
   *   <td>XXX</td>
   * </tr>
   * <tr>
   *   <td>{@link #VALUE_STROKE_PURE}</td>
   *   <td>XXX</td>
   * </tr>
   * </table>
   */
  public static final Key KEY_STROKE_CONTROL;

  /**
   * This value is for use with the {@link #KEY_STROKE_CONTROL} key.
   */
  public static final Object VALUE_STROKE_DEFAULT
    = "Default stroke normalization";

  /**
   * This value is for use with the {@link #KEY_STROKE_CONTROL} key.
   */
  public static final Object VALUE_STROKE_NORMALIZE
    = "Normalize strokes for consistent rendering";

  /**
   * This value is for use with the {@link #KEY_STROKE_CONTROL} key.
   */
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

  /**
   * Creates a new collection of hints containing all the (key, value) pairs
   * in the specified map.
   * 
   * @param init  a map containing a collection of hints (<code>null</code> 
   *              permitted).
   */
  public RenderingHints(Map<Key,?> init)
  {
    if (init != null)
      putAll(init);
  }

  /**
   * Creates a new collection containing a single (key, value) pair.
   * 
   * @param key  the key.
   * @param value  the value.
   */
  public RenderingHints(Key key, Object value)
  {
    put(key, value);
  }

  /**
   * Returns the number of hints in the collection.
   * 
   * @return The number of hints.
   */
  public int size()
  {
    return hintMap.size();
  }

  /**
   * Returns <code>true</code> if there are no hints in the collection,
   * and <code>false</code> otherwise.
   * 
   * @return A boolean.
   */
  public boolean isEmpty()
  {
    return hintMap.isEmpty();
  }

  /**
   * Returns <code>true</code> if the collection of hints contains the
   * specified key, and <code>false</code> otherwise.
   * 
   * @param key  the key (<code>null</code> not permitted).
   * 
   * @return A boolean.
   * 
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   * @throws ClassCastException if <code>key</code> is not a {@link Key}.
   */
  public boolean containsKey(Object key)
  {
    if (key == null)
      throw new NullPointerException();
    // don't remove the cast, it is necessary to throw the required exception
    return hintMap.containsKey((Key) key);
  }

  /**
   * Returns <code>true</code> if the collection of hints contains the
   * specified value, and <code>false</code> otherwise.
   * 
   * @param value  the value.
   * 
   * @return A boolean.
   */
  public boolean containsValue(Object value)
  {
    return hintMap.containsValue(value);
  }

  /**
   * Returns the value associated with the specified key, or <code>null</code>
   * if there is no value defined for the key.
   * 
   * @param key  the key (<code>null</code> permitted).
   * 
   * @return The value (possibly <code>null</code>).
   * 
   * @throws ClassCastException if <code>key</code> is not a {@link Key}.
   * 
   * @see #containsKey(Object)
   */
  public Object get(Object key)
  {
    // don't remove the cast, it is necessary to throw the required exception
    return hintMap.get((Key) key);
  }

  /**
   * Adds a (key, value) pair to the collection of hints (if the
   * collection already contains the specified key, then the 
   * value is updated).
   * 
   * @param key  the key.
   * @param value  the value.
   * 
   * @return  the previous value of the key or <code>null</code> if the key
   * didn't have a value yet.
   */
  public Object put(Object key, Object value)
  {
    if (key == null || value == null)
      throw new NullPointerException();
    if (! ((Key) key).isCompatibleValue(value))
      throw new IllegalArgumentException();
    return hintMap.put(key, value);
  }

  /**
   * Adds all the hints from a collection to this collection.
   * 
   * @param hints  the hint collection.
   */
  public void add(RenderingHints hints)
  {
    hintMap.putAll(hints);
  }

  /**
   * Clears all the hints from this collection.
   */
  public void clear()
  {
    hintMap.clear();
  }

  /**
   * Removes a hint from the collection.
   * 
   * @param key  the key.
   * 
   * @return The value that was associated with the key, or <code>null</code> if 
   *         the key was not part of the collection
   * 
   * @throws ClassCastException if the key is not a subclass of 
   *         {@link RenderingHints.Key}.
   */
  public Object remove(Object key)
  {
    // don't remove the (Key) cast, it is necessary to throw the exception
    // required by the spec
    return hintMap.remove((Key) key);  
  }

  /**
   * Adds a collection of (key, value) pairs to the collection.
   * 
   * @param m  a map containing (key, value) items.
   * 
   * @throws ClassCastException if the map contains a key that is not
   *         a subclass of {@link RenderingHints.Key}.
   * @throws IllegalArgumentException if the map contains a value that is
   *         not compatible with its key.
   */
  public void putAll(Map<?,?> m)
  {
    // preprocess map to generate appropriate exceptions
    Iterator iterator = m.keySet().iterator();
    while (iterator.hasNext())
      {
        Key key = (Key) iterator.next();
        if (!key.isCompatibleValue(m.get(key)))
          throw new IllegalArgumentException();
      }
    // map is OK, update
    hintMap.putAll(m);
  }

  /**
   * Returns a set containing the keys from this collection.
   * 
   * @return A set of keys.
   */
  public Set<Object> keySet()
  {
    return hintMap.keySet();
  }

  /**
   * Returns a collection of the values from this hint collection.  The
   * collection is backed by the <code>RenderingHints</code> instance, 
   * so updates to one will affect the other.
   * 
   * @return A collection of values.
   */
  public Collection<Object> values()
  {
    return hintMap.values();
  }

  /**
   * Returns a set of entries from the collection.
   * 
   * @return A set of entries.
   */
  public Set<Map.Entry<Object,Object>> entrySet()
  {
    return Collections.unmodifiableSet(hintMap.entrySet());
  }

  /**
   * Checks this collection for equality with an arbitrary object.
   * 
   * @param o  the object (<code>null</code> permitted)
   * 
   * @return A boolean.
   */
  public boolean equals(Object o)
  {
    return hintMap.equals(o);
  }

  /**
   * Returns a hash code for the collection of hints.
   * 
   * @return A hash code.
   */
  public int hashCode()
  {
    return hintMap.hashCode();
  }

  /**
   * Creates a clone of this instance.
   * 
   * @return A clone.
   */
  public Object clone()
  {
    try
      {
        RenderingHints copy = (RenderingHints) super.clone();
        copy.hintMap = new HashMap<Object,Object>(hintMap);
        return copy;
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e); // Impossible
      }
  }

  /**
   * Returns a string representation of this instance.
   * 
   * @return A string.
   */
  public String toString()
  {
    return hintMap.toString();
  }
} // class RenderingHints
