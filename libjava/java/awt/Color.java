/* Color.java -- represents a color in Java
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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

import java.awt.color.ColorSpace;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;
import java.io.Serializable;

/**
 * This class represents a color value in the AWT system. It uses the sRGB
 * (standard Red-Green-Blue) system, along with an alpha value ranging from
 * transparent (0.0f or 0) and opaque (1.0f or 255). The color is not
 * pre-multiplied by the alpha value an any of the accessor methods. Further
 * information about sRGB can be found at
 * <a href="http://www.w3.org/pub/WWW/Graphics/Color/sRGB.html">
 * http://www.w3.org/pub/WWW/Graphics/Color/sRGB.html</a>.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @see ColorSpace
 * @see AlphaComposite
 * @since 1.0
 * @status updated to 1.4
 */
public class Color implements Paint, Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 118526816881161077L;

  /** Constant for the color white: R=255, G=255, B=255. */
  public static final Color white = new Color(0xffffff, false);

  /**
   * Constant for the color white: R=255, G=255, B=255.
   *
   * @since 1.4
   */
  public static final Color WHITE = white;

  /** Constant for the color light gray: R=192, G=192, B=192. */
  public static final Color lightGray = new Color(0xc0c0c0, false);

  /**
   * Constant for the color light gray: R=192, G=192, B=192.
   *
   * @since 1.4
   */
  public static final Color LIGHT_GRAY = lightGray;

  /** Constant for the color gray: R=128, G=128, B=128. */
  public static final Color gray = new Color(0x808080, false);

  /**
   * Constant for the color gray: R=128, G=128, B=128.
   *
   * @since 1.4
   */
  public static final Color GRAY = gray;

  /** Constant for the color dark gray: R=64, G=64, B=64. */
  public static final Color darkGray = new Color(0x404040, false);

  /**
   * Constant for the color dark gray: R=64, G=64, B=64.
   *
   * @since 1.4
   */
  public static final Color DARK_GRAY = darkGray;

  /** Constant for the color black: R=0, G=0, B=0. */
  public static final Color black = new Color(0x000000, false);

  /**
   * Constant for the color black: R=0, G=0, B=0.
   *
   * @since 1.4
   */
  public static final Color BLACK = black;

  /** Constant for the color red: R=255, G=0, B=0. */
  public static final Color red = new Color(0xff0000, false);

  /**
   * Constant for the color red: R=255, G=0, B=0.
   *
   * @since 1.4
   */
  public static final Color RED = red;

  /** Constant for the color pink: R=255, G=175, B=175. */
  public static final Color pink = new Color(0xffafaf, false);

  /**
   * Constant for the color pink: R=255, G=175, B=175.
   *
   * @since 1.4
   */
  public static final Color PINK = pink;

  /** Constant for the color orange: R=255, G=200, B=0. */
  public static final Color orange = new Color(0xffc800, false);

  /**
   * Constant for the color orange: R=255, G=200, B=0.
   *
   * @since 1.4
   */
  public static final Color ORANGE = orange;

  /** Constant for the color yellow: R=255, G=255, B=0. */
  public static final Color yellow = new Color(0xffff00, false);

  /**
   * Constant for the color yellow: R=255, G=255, B=0.
   *
   * @since 1.4
   */
  public static final Color YELLOW = yellow;

  /** Constant for the color green: R=0, G=255, B=0. */
  public static final Color green = new Color(0x00ff00, false);

  /**
   * Constant for the color green: R=0, G=255, B=0.
   *
   * @since 1.4
   */
  public static final Color GREEN = green;

  /** Constant for the color magenta: R=255, G=0, B=255. */
  public static final Color magenta = new Color(0xff00ff, false);

  /**
   * Constant for the color magenta: R=255, G=0, B=255.
   *
   * @since 1.4
   */
  public static final Color MAGENTA = magenta;

  /** Constant for the color cyan: R=0, G=255, B=255. */
  public static final Color cyan = new Color(0x00ffff, false);

  /**
   * Constant for the color cyan: R=0, G=255, B=255.
   *
   * @since 1.4
   */
  public static final Color CYAN = cyan;

  /** Constant for the color blue: R=0, G=0, B=255. */
  public static final Color blue = new Color(0x0000ff, false);

  /**
   * Constant for the color blue: R=0, G=0, B=255.
   *
   * @since 1.4
   */
  public static final Color BLUE = blue;

  /** Internal mask for red. */
  private static final int RED_MASK = 255 << 16;

  /** Internal mask for green. */
  private static final int GREEN_MASK = 255 << 8;

  /** Internal mask for blue. */
  private static final int BLUE_MASK = 255;

  /** Internal mask for alpha. Package visible for use in subclass. */
  static final int ALPHA_MASK = 255 << 24;

  /** Amount to scale a color by when brightening or darkening. */
  private static final float BRIGHT_SCALE = 0.7f;

  /**
   * The color value, in sRGB. Note that the actual color may be more
   * precise if frgbvalue or fvalue is non-null. This class stores alpha, red,
   * green, and blue, each 0-255, packed in an int. However, the subclass
   * SystemColor stores an index into an array. Therefore, for serial
   * compatibility (and because of poor design on Sun's part), this value
   * cannot be used directly; instead you must use <code>getRGB()</code>.
   *
   * @see #getRGB()
   * @serial the value of the color, whether an RGB literal or array index
   */
  final int value;

  /**
   * The color value, in sRGB. This may be null if the color was constructed
   * with ints; and it does not include alpha. This stores red, green, and
   * blue, in the range 0.0f - 1.0f.
   *
   * @see #getRGBColorComponents(float[])
   * @see #getRGBComponents(float[])
   * @serial the rgb components of the value
   * @since 1.2
   */
  private float[] frgbvalue;

  /**
   * The color value, in the native ColorSpace components. This may be null
   * if the color was constructed with ints or in the sRGB color space; and
   * it does not include alpha.
   *
   * @see #getRGBColorComponents(float[])
   * @see #getRGBComponents(float[])
   * @serial the original color space components of the color
   * @since 1.2
   */
  private float[] fvalue;

  /**
   * The alpha value. This is in the range 0.0f - 1.0f, but is invalid if
   * deserialized as 0.0 when frgbvalue is null.
   *
   * @see #getRGBComponents(float[])
   * @see #getComponents(float[])
   * @serial the alpha component of this color
   * @since 1.2
   */
  private final float falpha;

  /**
   * The ColorSpace. Null means the default sRGB space.
   *
   * @see #getColor(String)
   * @see #getColorSpace()
   * @see #getColorComponents(float[])
   * @serial the color space for this color
   * @since 1.2
   */
  private final ColorSpace cs;

  /**
   * The paint context for this solid color. Package visible for use in
   * subclass.
   */
  transient ColorPaintContext context;

  /**
   * Initializes a new instance of <code>Color</code> using the specified
   * red, green, and blue values, which must be given as integers in the
   * range of 0-255. Alpha will default to 255 (opaque). When drawing to
   * screen, the actual color may be adjusted to the best match of hardware
   * capabilities.
   *
   * @param red the red component of the RGB value
   * @param green the green component of the RGB value
   * @param blue the blue component of the RGB value
   * @throws IllegalArgumentException if the values are out of range 0-255
   * @see #getRed()
   * @see #getGreen()
   * @see #getBlue()
   * @see #getRGB()
   * @see #Color(int, int, int, int)
   */
  public Color(int red, int green, int blue)
  {
    this(red, green, blue, 255);
  }

  /**
   * Initializes a new instance of <code>Color</code> using the specified
   * red, green, blue, and alpha values, which must be given as integers in
   * the range of 0-255. When drawing to screen, the actual color may be
   * adjusted to the best match of hardware capabilities.
   *
   * @param red the red component of the RGB value
   * @param green the green component of the RGB value
   * @param blue the blue component of the RGB value
   * @param alpha the alpha value of the color
   * @throws IllegalArgumentException if the values are out of range 0-255
   * @see #getRed()
   * @see #getGreen()
   * @see #getBlue()
   * @see #getAlpha()
   * @see #getRGB()
   */
  public Color(int red, int green, int blue, int alpha)
  {
    if ((red & 255) != red || (green & 255) != green || (blue & 255) != blue
        || (alpha & 255) != alpha)
      throw new IllegalArgumentException("Bad RGB values");
    value = (alpha << 24) | (red << 16) | (green << 8) | blue;
    falpha = 1;
    cs = null;
  }

  /**
   * Initializes a new instance of <code>Color</code> using the specified
   * RGB value. The blue value is in bits 0-7, green in bits 8-15, and
   * red in bits 16-23. The other bits are ignored. The alpha value is set
   * to 255 (opaque). When drawing to screen, the actual color may be
   * adjusted to the best match of hardware capabilities.
   *
   * @param value the RGB value
   * @see ColorModel#getRGBdefault()
   * @see #getRed()
   * @see #getGreen()
   * @see #getBlue()
   * @see #getRGB()
   * @see #Color(int, boolean)
   */
  public Color(int value)
  {
    this(value, false);
  }

  /**
   * Initializes a new instance of <code>Color</code> using the specified
   * RGB value. The blue value is in bits 0-7, green in bits 8-15, and
   * red in bits 16-23. The alpha value is in bits 24-31, unless hasalpha
   * is false, in which case alpha is set to 255. When drawing to screen, the
   * actual color may be adjusted to the best match of hardware capabilities.
   *
   * @param value the RGB value
   * @param hasalpha true if value includes the alpha
   * @see ColorModel#getRGBdefault()
   * @see #getRed()
   * @see #getGreen()
   * @see #getBlue()
   * @see #getAlpha()
   * @see #getRGB()
   */
  public Color(int value, boolean hasalpha)
  {
    // Note: SystemColor calls this constructor, setting falpha to 0; but
    // code in getRGBComponents correctly reports falpha as 1.0 to the user
    // for all instances of SystemColor since frgbvalue is left null here.
    if (hasalpha)
      falpha = ((value & ALPHA_MASK) >> 24) / 255f;
    else
      {
        value |= ALPHA_MASK;
        falpha = 1;
      }
    this.value = value;
    cs = null;
  }

  /**
   * Initializes a new instance of <code>Color</code> using the specified
   * RGB values. These must be in the range of 0.0-1.0. Alpha is assigned
   * the value of 1.0 (opaque). When drawing to screen, the actual color may
   * be adjusted to the best match of hardware capabilities.
   *
   * @param red the red component of the RGB value
   * @param green the green component of the RGB value
   * @param blue the blue component of the RGB value
   * @throws IllegalArgumentException tf the values are out of range 0.0f-1.0f
   * @see #getRed()
   * @see #getGreen()
   * @see #getBlue()
   * @see #getRGB()
   * @see #Color(float, float, float, float)
   */
  public Color(float red, float green, float blue)
  {
    this(red, green, blue, 1.0f);
  }

  /**
   * Initializes a new instance of <code>Color</code> using the specified
   * RGB and alpha values. These must be in the range of 0.0-1.0. When drawing
   * to screen, the actual color may be adjusted to the best match of
   * hardware capabilities.
   *
   * @param red the red component of the RGB value
   * @param green the green component of the RGB value
   * @param blue the blue component of the RGB value
   * @param alpha the alpha value of the color
   * @throws IllegalArgumentException tf the values are out of range 0.0f-1.0f
   * @see #getRed()
   * @see #getGreen()
   * @see #getBlue()
   * @see #getAlpha()
   * @see #getRGB()
   */
  public Color(float red, float green, float blue, float alpha)
  {
    value = convert(red, green, blue, alpha);
    frgbvalue = new float[] {red, green, blue};
    falpha = alpha;
    cs = null;
  }

  /**
   * Creates a color in the given ColorSpace with the specified alpha. The
   * array must be non-null and have enough elements for the color space
   * (for example, RGB requires 3 elements, CMYK requires 4). When drawing
   * to screen, the actual color may be adjusted to the best match of
   * hardware capabilities.
   *
   * @param space the color space of components
   * @param components the color components, except alpha
   * @param alpha the alpha value of the color
   * @throws NullPointerException if cpsace or components is null
   * @throws ArrayIndexOutOfBoundsException if components is too small
   * @throws IllegalArgumentException if alpha or any component is out of range
   * @see #getComponents(float[])
   * @see #getColorComponents(float[])
   */
  public Color(ColorSpace space, float[] components, float alpha)
  {
    frgbvalue = space.toRGB(components);
    fvalue = components;
    falpha = alpha;
    cs = space;
    value = convert(frgbvalue[0], frgbvalue[1], frgbvalue[2], alpha);
  }

  /**
   * Returns the red value for this color, as an integer in the range 0-255
   * in the sRGB color space.
   *
   * @return the red value for this color
   * @see #getRGB()
   */
  public int getRed()
  {
    // Do not inline getRGB() to value, because of SystemColor.
    return (getRGB() & RED_MASK) >> 16;
  }

  /**
   * Returns the green value for this color, as an integer in the range 0-255
   * in the sRGB color space.
   *
   * @return the green value for this color
   * @see #getRGB()
   */
  public int getGreen()
  {
    // Do not inline getRGB() to value, because of SystemColor.
    return (getRGB() & GREEN_MASK) >> 8;
  }

  /**
   * Returns the blue value for this color, as an integer in the range 0-255
   * in the sRGB color space.
   *
   * @return the blue value for this color
   * @see #getRGB()
   */
  public int getBlue()
  {
    // Do not inline getRGB() to value, because of SystemColor.
    return getRGB() & BLUE_MASK;
  }

  /**
   * Returns the alpha value for this color, as an integer in the range 0-255.
   *
   * @return the alpha value for this color
   * @see #getRGB()
   */
  public int getAlpha()
  {
    // Do not inline getRGB() to value, because of SystemColor.
    return (getRGB() & ALPHA_MASK) >>> 24;
  }

  /**
   * Returns the RGB value for this color, in the sRGB color space. The blue
   * value will be in bits 0-7, green in 8-15, red in 6-23, and alpha value in
   * 24-31.
   *
   * @return the RGB value for this color
   * @see ColorModel#getRGBdefault()
   * @see #getRed()
   * @see #getGreen()
   * @see #getBlue()
   * @see #getAlpha()
   */
  public int getRGB()
  {
    return value;
  }

  /**
   * Returns a brighter version of this color. This is done by increasing the
   * RGB values by an arbitrary scale factor. The new color is opaque (an
   * alpha of 255). Note that this method and the <code>darker()</code>
   * method are not necessarily inverses.
   *
   * @return a brighter version of this color
   * @see #darker()
   */
  public Color brighter()
  {
    // Do not inline getRGB() to this.value, because of SystemColor.
    int value = getRGB();
    int red = (value & RED_MASK) >> 16;
    int green = (value & GREEN_MASK) >> 8;
    int blue = value & BLUE_MASK;
    // We have to special case 0-2 because they won't scale by division.
    red = red < 3 ? 3 : (int) Math.min(255, red / BRIGHT_SCALE);
    green = green < 3 ? 3 : (int) Math.min(255, green / BRIGHT_SCALE);
    blue = blue < 3 ? 3 : (int) Math.min(255, blue / BRIGHT_SCALE);
    return new Color(red, green, blue, 255);
  }

  /**
   * Returns a darker version of this color. This is done by decreasing the
   * RGB values by an arbitrary scale factor. The new color is opaque (an
   * alpha of 255). Note that this method and the <code>brighter()</code>
   * method are not necessarily inverses.
   *
   * @return a darker version of this color
   * @see #brighter()
   */
  public Color darker()
  {
    // Do not inline getRGB() to this.value, because of SystemColor.
    int value = getRGB();
    return new Color((int) (((value & RED_MASK) >> 16) * BRIGHT_SCALE),
                     (int) (((value & GREEN_MASK) >> 8) * BRIGHT_SCALE),
                     (int) ((value & BLUE_MASK) * BRIGHT_SCALE), 255);
  }

  /**
   * Returns a hash value for this color. This is simply the color in 8-bit
   * precision, in the format 0xAARRGGBB (alpha, red, green, blue).
   *
   * @return a hash value for this color
   */
  public int hashCode()
  {
    return value;
  }

  /**
   * Tests this object for equality against the specified object.  This will
   * be true if and only if the specified object is an instance of
   * <code>Color</code> and has the same 8-bit integer red, green, and blue
   * values as this object. Note that two colors may be slightly different
   * as float values, but round to the same integer values. Also note that
   * this does not accurately compare SystemColors, since that class does
   * not store its internal data in RGB format like regular colors.
   *
   * @param obj the object to compare to
   * @return true if the specified object is semantically equal to this one
   */
  public boolean equals(Object obj)
  {
    return obj instanceof Color && ((Color) obj).value == value;
  }

  /**
   * Returns a string representation of this object. Subclasses may return
   * any desired format, except for null, but this implementation returns
   * <code>getClass().getName() + "[r=" + getRed() + ",g=" + getGreen()
   * + ",b=" + getBlue() + ']'</code>.
   *
   * @return a string representation of this object
   */
  public String toString()
  {
    return getClass().getName() + "[r=" + ((value & RED_MASK) >> 16)
      + ",g=" + ((value & GREEN_MASK) >> 8) + ",b=" + (value & BLUE_MASK)
      + ']';
  }

  /**
   * Converts the specified string to a number, using Integer.decode, and
   * creates a new instance of <code>Color</code> from the value. The alpha
   * value will be 255 (opaque).
   *
   * @param str the numeric color string
   * @return a new instance of <code>Color</code> for the string
   * @throws NumberFormatException if the string cannot be parsed
   * @throws NullPointerException if the string is null
   * @see Integer#decode(String)
   * @see #Color(int)
   * @since 1.1
   */
  public static Color decode(String str)
  {
    return new Color(Integer.decode(str).intValue(), false);
  }

  /**
   * Returns a new instance of <code>Color</code> from the value of the
   * system property named by the specified string.  If the property does not
   * exist, or cannot be parsed, then <code>null</code> will be returned.
   *
   * @param prop the system property to retrieve
   * @throws SecurityException if getting the property is denied
   * @see #getColor(String, Color)
   * @see Integer#getInteger(String)
   */
  public static Color getColor(String prop)
  {
    return getColor(prop, null);
  }

  /**
   * Returns a new instance of <code>Color</code> from the value of the
   * system property named by the specified string.  If the property does
   * not exist, or cannot be parsed, then the default color value will be
   * returned.
   *
   * @param prop the system property to retrieve
   * @param defcolor the default color
   * @throws SecurityException if getting the property is denied
   * @see Integer#getInteger(String)
   */
  public static Color getColor(String prop, Color defcolor)
  {
    Integer val = Integer.getInteger(prop, null);
    return val == null ? defcolor
      : new Color(val.intValue(), false);
  }

  /**
   * Returns a new instance of <code>Color</code> from the value of the
   * system property named by the specified string.  If the property does
   * not exist, or cannot be parsed, then the default RGB value will be
   * used to create a return value.
   *
   * @param prop the system property to retrieve
   * @param defrgb the default RGB value
   * @throws SecurityException if getting the property is denied
   * @see #getColor(String, Color)
   * @see Integer#getInteger(String, int)
   */
  public static Color getColor(String prop, int defrgb)
  {
    Color c = getColor(prop, null);
    return c == null ? new Color(defrgb, false) : c;
  }

  /**
   * Converts from the HSB (hue, saturation, brightness) color model to the
   * RGB (red, green, blue) color model. The hue may be any floating point;
   * it's fractional portion is used to select the angle in the HSB model.
   * The saturation and brightness must be between 0 and 1. The result is
   * suitable for creating an RGB color with the one-argument constructor.
   *
   * @param hue the hue of the HSB value
   * @param saturation the saturation of the HSB value
   * @param brightness the brightness of the HSB value
   * @return the RGB value
   * @see #getRGB()
   * @see #Color(int)
   * @see ColorModel#getRGBdefault()
   */
  public static int HSBtoRGB(float hue, float saturation, float brightness)
  {
    if (saturation == 0)
      return convert(brightness, brightness, brightness, 0);
    if (saturation < 0 || saturation > 1 || brightness < 0 || brightness > 1)
      throw new IllegalArgumentException();
    hue = hue - (float) Math.floor(hue);
    int i = (int) (6 * hue);
    float f = 6 * hue - i;
    float p = brightness * (1 - saturation);
    float q = brightness * (1 - saturation * f);
    float t = brightness * (1 - saturation * (1 - f));
    switch (i)
      {
      case 0:
        return convert(brightness, t, p, 0);
      case 1:
        return convert(q, brightness, p, 0);
      case 2:
        return convert(p, brightness, t, 0);
      case 3:
        return convert(p, q, brightness, 0);
      case 4:
        return convert(t, p, brightness, 0);
      case 5:
        return convert(brightness, p, q, 0);
      default:
        throw new InternalError("impossible");
      }
  }

  /**
   * Converts from the RGB (red, green, blue) color model to the HSB (hue,
   * saturation, brightness) color model. If the array is null, a new one
   * is created, otherwise it is recycled. The results will be in the range
   * 0.0-1.0 if the inputs are in the range 0-255.
   *
   * @param red the red part of the RGB value
   * @param green the green part of the RGB value
   * @param blue the blue part of the RGB value
   * @param array an array for the result (at least 3 elements), or null
   * @return the array containing HSB value
   * @throws ArrayIndexOutOfBoundsException of array is too small
   * @see #getRGB()
   * @see #Color(int)
   * @see ColorModel#getRGBdefault()
   */
  public static float[] RGBtoHSB(int red, int green, int blue, float array[])
  {
    if (array == null)
      array = new float[3];
    // Calculate brightness.
    int min;
    int max;
    if (red < green)
      {
        min = red;
        max = green;
      }
    else
      {
        min = green;
        max = red;
      }
    if (blue > max)
      max = blue;
    else if (blue < min)
      min = blue;
    array[2] = max / 255f;
    // Calculate saturation.
    if (max == 0)
      array[1] = 0;
    else
      array[1] = (max - min) / max;
    // Calculate hue.
    if (array[1] == 0)
      array[0] = 0;
    else
      {
        float delta = (max - min) * 6;
        if (red == max)
          array[0] = (green - blue) / delta;
        else if (green == max)
          array[0] = 1 / 3 + (blue - red) / delta;
        else
          array[0] = 2 / 3 + (red - green) / delta;
        if (array[0] < 0)
          array[0]++;
      }
    return array;
  }

  /**
   * Returns a new instance of <code>Color</code> based on the specified
   * HSB values. The hue may be any floating point; it's fractional portion
   * is used to select the angle in the HSB model. The saturation and
   * brightness must be between 0 and 1.
   *
   * @param hue the hue of the HSB value
   * @param saturation the saturation of the HSB value
   * @param brightness the brightness of the HSB value
   * @return the new <code>Color</code> object
   */
  public static Color getHSBColor(float hue, float saturation,
                                  float brightness)
  {
    return new Color(HSBtoRGB(hue, saturation, brightness), false);
  }

  /**
   * Returns a float array with the red, green, and blue components, and the
   * alpha value, in the default sRGB space, with values in the range 0.0-1.0.
   * If the array is null, a new one is created, otherwise it is recycled.
   *
   * @param array the array to put results into (at least 4 elements), or null
   * @return the RGB components and alpha value
   * @throws ArrayIndexOutOfBoundsException if array is too small
   */
  public float[] getRGBComponents(float[] array)
  {
    if (array == null)
      array = new float[4];
    getRGBColorComponents(array);
    // Stupid serialization issues require this check.
    array[3] = (falpha == 0 && frgbvalue == null
                ? ((getRGB() & ALPHA_MASK) >> 24) / 255f : falpha);
    return array;
  }

  /**
   * Returns a float array with the red, green, and blue components, in the
   * default sRGB space, with values in the range 0.0-1.0. If the array is
   * null, a new one is created, otherwise it is recycled.
   *
   * @param array the array to put results into (at least 3 elements), or null
   * @return the RGB components
   * @throws ArrayIndexOutOfBoundsException if array is too small
   */
  public float[] getRGBColorComponents(float[] array)
  {
    if (array == null)
      array = new float[3];
    else if (array == frgbvalue)
      return array; // Optimization for getColorComponents(float[]).
    if (frgbvalue == null)
      {
        // Do not inline getRGB() to this.value, because of SystemColor.
        int value = getRGB();
        frgbvalue = new float[] { ((value & RED_MASK) >> 16) / 255f,
                                  ((value & GREEN_MASK) >> 8) / 255f,
                                  (value & BLUE_MASK) / 255f };
      }
    array[0] = frgbvalue[0];
    array[1] = frgbvalue[1];
    array[2] = frgbvalue[2];
    return array;
  }

  /**
   * Returns a float array containing the color and alpha components of this
   * color in the ColorSpace it was created with (the constructors which do
   * not take a ColorSpace parameter use a default sRGB ColorSpace). If the
   * array is null, a new one is created, otherwise it is recycled, and must
   * have at least one more position than components used in the color space.
   *
   * @param array the array to put results into, or null
   * @return the original color space components and alpha value
   * @throws ArrayIndexOutOfBoundsException if array is too small
   */
  public float[] getComponents(float[] array)
  {
    int numComponents = cs == null ? 3 : cs.getNumComponents();
    if (array == null)
      array = new float[1 + numComponents];
    getColorComponents(array);
    // Stupid serialization issues require this check.
    array[numComponents] = (falpha == 0 && frgbvalue == null
                            ? ((getRGB() & ALPHA_MASK) >> 24) / 255f : falpha);
    return array;
  }

  /**
   * Returns a float array containing the color components of this color in
   * the ColorSpace it was created with (the constructors which do not take
   * a ColorSpace parameter use a default sRGB ColorSpace). If the array is
   * null, a new one is created, otherwise it is recycled, and must have at
   * least as many positions as used in the color space.
   *
   * @param array the array to put results into, or null
   * @return the original color space components
   * @throws ArrayIndexOutOfBoundsException if array is too small
   */
  public float[] getColorComponents(float[] array)
  {
    int numComponents = cs == null ? 3 : cs.getNumComponents();
    if (array == null)
      array = new float[numComponents];
    if (fvalue == null) // If fvalue is null, cs should be null too.
      fvalue = getRGBColorComponents(frgbvalue);
    System.arraycopy(fvalue, 0, array, 0, numComponents);
    return array;
  }

  /**
   * Returns a float array containing the color and alpha components of this
   * color in the given ColorSpace. If the array is null, a new one is
   * created, otherwise it is recycled, and must have at least one more
   * position than components used in the color space.
   *
   * @param space the color space to translate to
   * @param array the array to put results into, or null
   * @return the color space components and alpha value
   * @throws ArrayIndexOutOfBoundsException if array is too small
   * @throws NullPointerException if space is null
   */
  public float[] getComponents(ColorSpace space, float[] array)
  {
    int numComponents = space.getNumComponents();
    if (array == null)
      array = new float[1 + numComponents];
    getColorComponents(space, array);
    // Stupid serialization issues require this check.
    array[numComponents] = (falpha == 0 && frgbvalue == null
                            ? ((getRGB() & ALPHA_MASK) >> 24) / 255f : falpha);
    return array;
  }

  /**
   * Returns a float array containing the color components of this color in
   * the given ColorSpace. If the array is null, a new one is created,
   * otherwise it is recycled, and must have at least as many positions as
   * used in the color space.
   *
   * @param space the color space to translate to
   * @return the color space components
   * @throws ArrayIndexOutOfBoundsException if array is too small
   * @throws NullPointerException if space is null
   */
  public float[] getColorComponents(ColorSpace space, float[] array)
  {
    float[] components = space.fromRGB(getRGBColorComponents(frgbvalue));
    if (array == null)
      return components;
    System.arraycopy(components, 0, array, 0, components.length);
    return array;
  }

  /**
   * Returns the color space of this color. Except for the constructor which
   * takes a ColorSpace argument, this will be an implementation of
   * ColorSpace.CS_sRGB.
   *
   * @return the color space
   */
  public ColorSpace getColorSpace()
  {
    return cs == null ? ColorSpace.getInstance(ColorSpace.CS_sRGB) : cs;
  }

  /**
   * Returns a paint context, used for filling areas of a raster scan with
   * this color. Since the color is constant across the entire rectangle, and
   * since it is always in sRGB space, this implementation returns the same
   * object, regardless of the parameters. Subclasses, however, may have a
   * mutable result.
   *
   * @param cm the requested color model, ignored
   * @param deviceBounds the bounding box in device coordinates, ignored
   * @param userBounds the bounding box in user coordinates, ignored
   * @param xform the bounds transformation, ignored
   * @param hints any rendering hints, ignored
   * @return a context for painting this solid color
   */
  public PaintContext createContext(ColorModel cm, Rectangle deviceBounds,
                                    Rectangle2D userBounds,
                                    AffineTransform xform,
                                    RenderingHints hints)
  {
    if (context == null)
      context = new ColorPaintContext(value);
    return context;
  }

  /**
   * Returns the transparency level of this color.
   *
   * @return one of {@link #OPAQUE}, {@link #BITMASK}, or {@link #TRANSLUCENT}
   */
  public int getTransparency()
  {
    // Do not inline getRGB() to this.value, because of SystemColor.
    int alpha = getRGB() & ALPHA_MASK;
    return alpha == (255 << 24) ? OPAQUE : alpha == 0 ? BITMASK : TRANSLUCENT;
  }

  /**
   * Converts float values to integer value.
   *
   * @param red the red value
   * @param green the green value
   * @param blue the blue value
   * @param alpha the alpha value
   * @return the integer value made of 8-bit sections
   * @throws IllegalArgumentException if parameters are out of range 0.0-1.0
   */
  private static int convert(float red, float green, float blue, float alpha)
  {
    if (red < 0 || red > 1 || green < 0 || green > 1 || blue < 0 || blue > 1
        || alpha < 0 || alpha > 1)
      throw new IllegalArgumentException("Bad RGB values");
    int redval = Math.round(255 * red);
    int greenval = Math.round(255 * green);
    int blueval = Math.round(255 * blue);
    int alphaval = Math.round(255 * alpha);
    return (alphaval << 24) | (redval << 16) | (greenval << 8) | blueval;
  }
} // class Color
