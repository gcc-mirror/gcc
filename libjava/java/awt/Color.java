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
  public static final Color white =	new Color(0xff, 0xff, 0xff);
  public static final Color lightGray =	new Color(0xc0, 0xc0, 0xc0);
  public static final Color gray =	new Color(0x80, 0x80, 0x80);
  public static final Color darkGray =	new Color(0x40, 0x40, 0x40);
  public static final Color black =	new Color(0x00, 0x00, 0x00);
  public static final Color red =	new Color(0xff, 0x00, 0x00);
  public static final Color pink =	new Color(0xff, 0xaf, 0xaf);
  public static final Color orange =	new Color(0xff, 0xc8, 0x00);
  public static final Color yellow =	new Color(0xff, 0xff, 0x00);
  public static final Color green =	new Color(0x00, 0xff, 0x00);
  public static final Color magenta =	new Color(0xff, 0x00, 0xff);
  public static final Color cyan =	new Color(0x00, 0xff, 0xff);
  public static final Color blue =	new Color(0x00, 0x00, 0xff);
  
  // The internal sRGB representation.
  private float r;
  private float g;
  private float b;
  private int alpha = 255;

  public Color(int rgb)
  {
    this(rgb, false);
  } 

  public Color(int rgba, boolean hasalpha)
  {
    // Alpha is bits 24-31, if hasalpha is true.
    // Red is bits 16-23; Green is bits 8-15; Blue is bits 0-7.
    b = rgb & 0xFF;
    g = (rgb >>= 8) & 0xFF;
    r = (rgb >>= 8) & 0xFF;
    if (hasalpha)
      alpha = (rgb >>= 8) & 0xFF;
  }

  public int getRGB()
  {
    return alpha << 24 | r << 16 | g << 8 | b;
  }
}
