/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 15, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Stubbed; A very incomplete implementation.
 */

public class Color extends Object implements Paint, Serializable
{
  public static final Color white =	new Color(0xFFFFFFFF, true);
  public static final Color lightGray =	new Color(0xFFC0C0C0, true);
  public static final Color gray =	new Color(0xFF808080, true);
  public static final Color darkGray =	new Color(0xFF404040, true);
  public static final Color black =	new Color(0xFF000000, true);
  public static final Color red =	new Color(0xFFFF0000, true);
  public static final Color pink =	new Color(0xFFFFAFAF, true);
  public static final Color orange =	new Color(0xFFFFC800, true);
  public static final Color yellow =	new Color(0xFFFFFF00, true);
  public static final Color green =	new Color(0xFF00FF00, true);
  public static final Color magenta =	new Color(0xFFFF00FF, true);
  public static final Color cyan =	new Color(0xFF00FFFF, true);
  public static final Color blue =	new Color(0xFF0000FF, true);
  
  // The internal sRGB representation.
  // Alpha is bits 24-31, if hasalpha is true.
  // Red is bits 16-23; Green is bits 8-15; Blue is bits 0-7.
  private int rgba = 0xFFFFFFFF;

  public Color(int rgb)
  {
    this(rgb, false);
  } 

  public Color(int rgba, boolean hasalpha)
  {
    this.rgba = rgba;
    if (!hasalpha)
      rgba |= 0xFF000000;
  }

  public Color(int r, int g, int b)
  {
    this(r, g, b, 0xFF);
  }

  public Color(int r, int g, int b, int a)
  {
    rgba = a << 24 | ((r << 16) & 0x00FF0000) | ((g << 8) & 0x0000FF00) |
    	   (b & 0x000000FF);
  }

  public int getRed()
  {
    return (rgba >> 16) & 0xFF;
  }

  public int getGreen()
  {
    return (rgba >> 8) & 0xFF;
  }

  public int getBlue()
  {
    return rgba & 0xFF;
  }

  public int getAlpha()
  {
    return (rgba >> 24) & 0xFF;
  }

  public int getRGB()
  {
    return rgba;
  }
}
