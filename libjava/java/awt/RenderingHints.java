/* Copyright (C) 2000, 2001, 2002  Free Software Foundation

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

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class RenderingHints implements
    //java.util.Map,
    Cloneable
{

  public abstract static class Key
  {
    private int intKey;

    protected Key(int privateKey)
    {
      intKey = privateKey;
    }

    public abstract boolean isCompatibleValue(Object value);
    
    protected final int intKey() 
    {
      return intKey;
    }    

    public final int hashCode() 
    {
      return System.identityHashCode(this);
    }
    
    public final boolean equals(Object other) 
    {
      return (this == other);
    }
  }

  private static class KeyImpl extends Key
  {
    String description;
    Object v1, v2, v3;

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
      return (value == v1) || (value == v2) || (value == v3);
    }

    public String toString() 
    {
      return description;
    }
  }


  //java.util.HashMap hintMap;

  public static final Key KEY_ANTIALIASING;
  public static final Object
    VALUE_ANTIALIAS_ON = "Antialiased rendering mode",
    VALUE_ANTIALIAS_DEFAULT = "Default antialiasing rendering mode";

  static 
  {
    KEY_ANTIALIASING = new KeyImpl(1, "Global antialiasing enable key",
				   VALUE_ANTIALIAS_ON,
				   VALUE_ANTIALIAS_DEFAULT,
				   VALUE_ANTIALIAS_DEFAULT);
  }

  public static final Key KEY_RENDERING;
  public static final Object 
    VALUE_RENDER_SPEED = "Fastest rendering methods",
    VALUE_RENDER_QUALITY = "Highest quality rendering methods",
    VALUE_RENDER_DEFAULT = "Default rendering methods";

  static
  {
    KEY_RENDERING = new KeyImpl(2, "Global rendering quality key",
				VALUE_RENDER_SPEED,
				VALUE_RENDER_QUALITY,
				VALUE_RENDER_DEFAULT);
  }
  
  public static final Key KEY_DITHERING;
  public static final Object
    VALUE_DITHER_DISABLE = "Nondithered rendering mode",
    VALUE_DITHER_ENABLE = "Dithered rendering mode",
    VALUE_DITHER_DEFAULT = "Default dithering mode";

  static
  {
    KEY_DITHERING = new KeyImpl(3, "Dithering quality key",
				VALUE_DITHER_DISABLE,
				VALUE_DITHER_ENABLE,
				VALUE_DITHER_DEFAULT);
  }
  
  public static final Key KEY_TEXT_ANTIALIASING;
  public static final Object
    VALUE_TEXT_ANTIALIAS_ON = "Antialiased text mode",
    VALUE_TEXT_ANTIALIAS_OFF = "Nonantialiased text mode",
    VALUE_TEXT_ANTIALIAS_DEFAULT = "Default antialiasing text mode";

  static 
  {
    KEY_TEXT_ANTIALIASING = new KeyImpl(4, "Text-specific antialiasing enable key",
					VALUE_TEXT_ANTIALIAS_ON,
					VALUE_TEXT_ANTIALIAS_OFF,
					VALUE_TEXT_ANTIALIAS_DEFAULT);
  }
  
  public static final Key KEY_FRACTIONALMETRICS;
  public static final Object
    VALUE_FRACTIONALMETRICS_OFF = "Integer text metrics mode",
    VALUE_FRACTIONALMETRICS_ON = "Fractional text metrics mode",
    VALUE_FRACTIONALMETRICS_DEFAULT = "Default fractional text metrics mode";

  static 
  {
    KEY_FRACTIONALMETRICS = new KeyImpl(5, "Fractional metrics enable key",
					VALUE_FRACTIONALMETRICS_OFF,
					VALUE_FRACTIONALMETRICS_ON,
					VALUE_FRACTIONALMETRICS_DEFAULT);
  }
  
  public static final Key KEY_INTERPOLATION;
  public static final Object
    VALUE_INTERPOLATION_NEAREST_NEIGHBOR = "Nearest Neighbor image interpolation mode",
    VALUE_INTERPOLATION_BILINEAR = "Bilinear image interpolation mode",
    VALUE_INTERPOLATION_BICUBIC = "Bicubic image interpolation mode";

  static 
  {
    KEY_INTERPOLATION = new KeyImpl(6, "Image interpolation method key",
				    VALUE_INTERPOLATION_NEAREST_NEIGHBOR,
				    VALUE_INTERPOLATION_BILINEAR,
				    VALUE_INTERPOLATION_BICUBIC);
  }
  
  public static final Key KEY_ALPHA_INTERPOLATION;
  public static final Object
    VALUE_ALPHA_INTERPOLATION_SPEED = "Fastest alpha blending methods",
    VALUE_ALPHA_INTERPOLATION_QUALITY = "Highest quality alpha blending methods",
    VALUE_ALPHA_INTERPOLATION_DEFAULT = "Default alpha blending methods";

  static
  {
    KEY_ALPHA_INTERPOLATION = new KeyImpl(7, "Alpha blending interpolation method key",
					  VALUE_ALPHA_INTERPOLATION_SPEED,
					  VALUE_ALPHA_INTERPOLATION_QUALITY,
					  VALUE_ALPHA_INTERPOLATION_DEFAULT);
  }
  
  public static final Key KEY_COLOR_RENDERING;
  public static final Object
    VALUE_COLOR_RENDER_SPEED = "Fastest color rendering mode",
    VALUE_COLOR_RENDER_QUALITY = "Highest quality color rendering mode",
    VALUE_COLOR_RENDER_DEFAULT = "Default color rendering mode";

  static 
  {
    KEY_COLOR_RENDERING = new KeyImpl(8, "Color rendering quality key",
				      VALUE_COLOR_RENDER_SPEED,
				      VALUE_COLOR_RENDER_QUALITY,
				      VALUE_COLOR_RENDER_DEFAULT);
  }

  public static final Key KEY_STROKE_CONTROL;
  public static final Object
    VALUE_STROKE_DEFAULT = "Default stroke control mode",
    VALUE_STROKE_NORMALIZE = "Normalize stroke control mode",
    VALUE_STROKE_PURE = "Pure stroke control mode";

  static 
  {
    KEY_STROKE_CONTROL = new KeyImpl(9, "Stroke normalization control key",
				     VALUE_STROKE_DEFAULT,
				     VALUE_STROKE_NORMALIZE,
				     VALUE_STROKE_PURE);
  }
  
  //public RenderingHints(Map init);

  public RenderingHints(Key key, Object value)
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }

  public int size() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public boolean isEmpty() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }

  public boolean containsKey(Object key) 
  {      
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public boolean containsValue(Object value) 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public Object get(Object key)
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public Object put(Object key, Object value) 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public void add(RenderingHints hints) 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }

  public void clear() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public Object remove(Object key) 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  /*
  public void putAll(Map m) 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  */
  
  /*
  public Set keySet() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  */
  
  /*
  public Collection values() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  */
  
  /*
  public Set entrySet() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  */
  
  public boolean equals(Object o) 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public int hashCode() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public Object clone() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
  
  public String toString() 
  {
    throw new UnsupportedOperationException("FIXME, not implemented yet");
  }
}
