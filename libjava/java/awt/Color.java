/* Color.java -- Class representing a color in Java
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

/**
  * This class represents a color value in the AWT system.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Color implements java.io.Serializable
{

/*
 * Static Variables
 */

/**
  * Constant for the color white
  */
public static final Color white = new Color(255,255,255,255);

/**
  * Constant for the color light gray
  */
public static final Color lightGray = new Color(192,192,192,255);

/**
  * Constant for the color gray
  */
public static final Color gray = new Color(128,128,128,255);

/**
  * Constant for the color dark gray
  */
public static final Color darkGray = new Color(64,64,64,255);

/**
  * Constant for the color black
  */
public static final Color black = new Color(0,0,0,255);

/**
  * Constant for the color red
  */
public static final Color red = new Color(255,0,0,255);

/**
  * Constant for the color pink
  */
public static final Color pink = new Color(255, 175, 175,255);

/**
  * Constant for the color orange
  */
public static final Color orange = new Color(255, 200, 0,255);

/**
  * Constant for the color yellow
  */
public static final Color yellow = new Color(255,255,0,255);

/**
  * Constant for the color green
  */
public static final Color green = new Color(0,255,0,255);

/**
  * Constant for the color magenta
  */
public static final Color magenta = new Color(255,0,255,255);

/**
  * Constant for the color cyan
  */
public static final Color cyan = new Color(0,255,255,255);

/**
  * Constant for the color blue
  */
public static final Color blue = new Color(0,0,255,255);

// Serialization Constant
private static final long serialVersionUID = 118526816881161077L;

// Masks for individual color components
private static final int redmask = 255 << 16;
private static final int greenmask = 255 << 8;
private static final int bluemask = 255;
private static final int alphamask = 255 << 24;

private static final int BRIGHT_STEP = 0x30;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The RGB value of the color.
  */
private int value = 0xFFFFFFFF;

/*************************************************************************/

/*
 * Static Methods
 */

/**
  * Converts the specified string to a number and creates a new instance
  * of <code>Color</code> from the value.
  *
  * @param str The numeric color string.
  *
  * @return A new instance of <code>Color</code> for the string.
  *
  * @exception NumberFormatException If the string cannot be parsed.
  */
public static Color
decode(String str) throws NumberFormatException
{
  Integer i = Integer.decode(str);
  return(new Color(i.intValue()));
}

/*************************************************************************/

/**
  * Returns a new instance of <code>Color</code> from the value of
  * the system property named by the specified string.  If the property
  * does not exist, or cannot be parsed, then <code>null</code> will be
  * returned.
  *
  * @param prop The system property to retrieve.
  */
public static Color
getColor(String prop)
{
  return(getColor(prop, null));
}

/*************************************************************************/

/**
  * Returns a new instance of <code>Color</code> from the value of the
  * system property named by the specified string.  If the property does
  * not exist, or cannot be parsed, then the default RGB value will be
  * used to create a return value.
  *
  * @param prop The system property to retrieve.
  * @param defrgb The default RGB value.
  */
public static Color
getColor(String prop, int defrgb)
{
  return(getColor(prop, new Color(defrgb)));
}

/*************************************************************************/

/**
  * Returns a new instance of <code>Color</code> from the value of the
  * system property named by the specified string.  If the property does
  * not exist, or cannot be parsed, then the default color value will be
  * returned
  *
  * @param prop The system property to retrieve.
  * @param defcolor The default color
  */
public static Color
getColor(String prop, Color defcolor)
{
  String val = System.getProperty(prop);
  if (val == null)
    return(defcolor);

  try
    {
      return(decode(val));
    }
  catch(NumberFormatException e)
    {
      return(defcolor);
    }
}

/*************************************************************************/

/**
  * Converts from the HSB (hue, saturation, brightness) color model to
  * the RGB (red, green, blue) color model.
  *
  * @param hue The hue of the HSB value.
  * @param saturation The saturation of the HSB value.
  * @param brightness The brightness of the HSB value.
  *
  * @return The RGB value.
  */
public static int
HSBtoRGB(float hue, float saturation, float brightness)
{
  // FIXME: Implement
  throw new RuntimeException("Not implemented yet");
}

/*************************************************************************/

/**
  * Converts from the RGB (red, green, blue) color model to the HSB
  * (hue, saturation, brightness) color model.
  *
  * @param red The red part of the RGB value.
  * @param green The green part of the RGB value.
  * @param blue The blue part of the RGB value.
  * @param hsbvals An array of three floats used for storing the HSB values,
  * or <code>null</code> if this return mechanism is not used.
  *
  * @return The HSB value.
  */
public static float[]
RGBtoHSB(int red, int green, int blue, float hsbvals[])
{
  // FIXME: Implement
  throw new RuntimeException("Not implemented yet");
}

/*************************************************************************/

/**
  * Returns a new instance of <code>Color</code> based on the specified
  * HSB values.
  *
  * @param hue The hue of the HSB value.
  * @param saturation The saturation of the HSB value.
  * @param brightness The brightness of the HSB value.
  *
  * @return The new <code>Color</code> object.
  */
public static Color
getHSBColor(float hue, float saturation, float brightness)
{
  return(new Color(HSBtoRGB(hue, saturation, brightness)));
}

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>Color</code> using the specified
  * red, green, and blue values, which must be given as integers in the
  * range of 0-255.
  *
  * @param red The red component of the RGB value.
  * @param green The green component of the RGB value.
  * @param blue The blue component of the RGB value.
  *
  * @exception IllegalArgumentException If the values are out of range.
  */
public
Color(int red, int green, int blue)
{
  if ((red < 0) || (red > 255) || (green < 0) || (green > 255) ||
      (blue < 0) || (blue > 255))
    throw new IllegalArgumentException("Bad RGB values");

  value = blue + (green << 8) + (red << 16);
}

public
Color(int red, int green, int blue, int alpha)
{
  if ((red < 0) || (red > 255) || (green < 0) || (green > 255) ||
      (blue < 0) || (blue > 255))
    throw new IllegalArgumentException("Bad RGB values");

  value = blue + (green << 8) + (red << 16) + (alpha << 24);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Color</code> using the specified
  * RGB value.  The blue value is in bits 0-7, green in bits 8-15, and 
  * red in bits 16-23.  The other bits are ignored.
  *
  * @param value The RGB value
  */
public
Color(int value)
{
  this.value = value;
}

public
Color(int value, boolean hasalpha)
{
  this.value = value;
  if (! hasalpha)
    this.value |= 0xFF000000;
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Color</code> using the specified
  * RGB values.  These must be in the range of 0.0-1.0.
  *
  * @param red The red component of the RGB value.
  * @param green The green component of the RGB value.
  * @param blue The blue component of the RGB value.
  *
  * @exception IllegalArgumentException If the values are out of range.
  */
public
Color(float red, float green, float blue)
{
  if ((red < 0.0) || (red > 1.0) || (green < 0.0) || (green > 1.0) ||
      (blue < 0.0) || (blue > 1.0))
    throw new IllegalArgumentException("Bad RGB values");

  int redval = (int)(255 * red);
  int greenval = (int)(255 * green);
  int blueval = (int)(255 * blue);

  value = blueval + (greenval << 8) + (redval << 16);
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the red value for this color.
  *
  * @return The red value for this color.
  */
public int
getRed()
{
  int redval = (value & redmask) >> 16;

  return(redval);
}

/*************************************************************************/

/**
  * Returns the green value for this color.
  *
  * @return The green value for this color.
  */
public int
getGreen()
{
  int greenval = (value & greenmask) >> 8;

  return(greenval);
}

/*************************************************************************/

/**
  * Returns the blue value for this color.
  *
  * @return The blue value for this color.
  */
public int
getBlue()
{
  int blueval = (value & bluemask);

  return(blueval);
}

public int
getAlpha()
{
  int alphaval = (value & alphamask);

  return(alphaval);
}

public int
getTransparency()
{
  if (getAlpha() == 0xFF)
    return Transparency.OPAQUE;
  else
    return Transparency.TRANSLUCENT;
}

/*************************************************************************/

/**
  * Returns the RGB value for this color.  The blue value will be in bits
  * 0-7, green in 8-15, and red in 6-23.  The upper bits should be ignored.
  *
  * @return The RGB value for this color.
  */
public int
getRGB()
{
  return(value);
}

/*************************************************************************/

/**
  * Returns a brighter version of this color.  This is done by increasing
  * the RGB values by an arbitrary scale factor.  Note that this method
  * and the <code>darker()</code> method are not necessarily inverses.
  *
  * @return A brighter version of this color.
  */
public Color
brighter()
{
  return new Color(Math.min(255, getRed()   + BRIGHT_STEP),
		   Math.min(255, getGreen() + BRIGHT_STEP),
		   Math.min(255, getBlue()  + BRIGHT_STEP),
		   getAlpha());
}

/*************************************************************************/

/**
  * Returns a darker version of this color.  This is done by decreasing
  * the RGB values by an arbitrary scale factor.  Note that this method
  * and the <code>brighter()</code> method are not necessarily inverses.
  *
  * @return A darker version of this color.
  */
public Color
darker()
{
  return new Color(Math.max(0, getRed()   - BRIGHT_STEP),
		   Math.max(0, getGreen() - BRIGHT_STEP),
		   Math.max(0, getBlue()  - BRIGHT_STEP),
		   getAlpha());
}

/*************************************************************************/

/**
  * Returns a hash value for this color.
  *
  * @return A hash value for this color.
  */
public int
hashCode()
{
  return(value);
}

/*************************************************************************/

/**
  * Tests this object for equality against the specified object.  This will
  * be true if and only if the specified object is an instance of
  * <code>Color</code> and has the same red, green, and blue values as
  * this object.
  *
  * @return <code>true</code> if the specified object is equal to this one,
  * <code>false</code> otherwise.
  */
public boolean
equals(Object obj)
{
  if (!(obj instanceof Color))
    return(false);

  Color c = (Color)obj;
  return value == c.value;
}

/*************************************************************************/

/**
  * Returns a string representation of this object.
  * 
  * @return A string representation of this object.
  */
public String
toString()
{
  return(getClass().getName() + "(red=" + getRed() + ",green=" + getGreen() +
         ",blue=" + getBlue() + ")");
}

} // class Color

