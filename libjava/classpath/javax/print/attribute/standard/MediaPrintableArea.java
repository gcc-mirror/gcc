/* MediaPrintableArea.java -- 
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import javax.print.attribute.DocAttribute;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public final class MediaPrintableArea
  implements DocAttribute, PrintJobAttribute, PrintRequestAttribute
{
  private static final long serialVersionUID = -1597171464050795793L;

  public static final int INCH = 25400;
  public static final int MM = 1000;
  
  private float x;
  private float y;
  private float width;
  private float height;
  
  /**
   * Creates a new <code>MediaPrintableArea</code> object.
   * 
   * @throws IllegalArgumentException if x i&lt; 0 or y i&lt; 0 or w i&lt;= 0
   * or h i&lt;= 0 or units i&lt; 1
   */
  public MediaPrintableArea(float x, float y, float w, float h, int units)
  {
    if (x < 0.0f || y < 0.0f || w <= 0.0f || h <= 0.0f)
      throw new IllegalArgumentException();

    this.x = x;
    this.y = y;
    this.width = w;
    this.height = h;
  }

  /**
   * Creates a new <code>MediaPrintableArea</code> object.
   * 
   * @throws IllegalArgumentException if x i&lt; 0 or y i&lt; 0 or w i&lt;= 0
   * or h i&lt;= 0 or units i&lt; 1
   */
  public MediaPrintableArea(int x, int y, int w, int h, int units)
  {
    if (x < 0 || y < 0 || w <= 0 || h <= 0)
      throw new IllegalArgumentException();

    this.x = x;
    this.y = y;
    this.width = w;
    this.height = h;
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>MediaPrintableArea</code> itself
   */
  public Class getCategory()
  {
    return MediaPrintableArea.class;
  }

  /**
   * Returns name of this class.
   *
   * @return the string "media-printable-area"
   */
  public String getName()
  {
    return "media-printable-area";
  }

  public float getHeight(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException();

    return height * units;
  }

  public float getWidth(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException();

    return width * units;
  }

  public float getX(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException();

    return x * units;
  }

  public float getY(int units)
  {
    if (units < 1)
      throw new IllegalArgumentException();

    return y * units;
  }
}
