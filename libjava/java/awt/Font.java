/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 16, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Stubbed; A very incomplete implementation.
 */

public class Font
{
  // FIXME

  public static final int PLAIN = 0;
  public static final int BOLD = 1;
  public static final int ITALIC = 2;
  public static final int ROMAN_BASELINE = 0;
  public static final int CENTER_BASELINE = 1;
  public static final int HANGING_BASELINE = 2;
  protected String name;
  protected int style;
  protected int size;
  protected float pointSize;

  public Font(String name, int style, int size)
  {
    this.name = name;
    this.style = style & 0x3;	// Only use lowest 2 bits.
    this.size = size;
    pointSize = size;		// Assume some subclass can set a different val.
  }

  public boolean isPlain()
  {
    if (style == PLAIN)
      return true;

    return false;
  }

  public boolean isBold()
  {
    if ((style & BOLD) == BOLD)
      return true;

    return false;
  }

  public boolean isItalic()
  {
    if ((style & ITALIC) == ITALIC)
      return true;

    return false;
  }
  
  public String getName()
  {
    return name;
  }

  public int getStyle()
  {
    return style;
  }

  public int getSize()
  {
    return size;
  }

  public float getSize2D()
  {
    return pointSize;
  }

  public static Font decode(String str) { return null; } // FIXME
}
