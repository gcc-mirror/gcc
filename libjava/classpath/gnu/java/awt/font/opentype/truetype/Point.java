/* Point.java -- Holds information for one point on a glyph outline
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.font.opentype.truetype;

/**
 * Encapsulates information regarding one point on a glyph outline.
 */
public class Point
{
  public static final short FLAG_TOUCHED_X = 1;
  public static final short FLAG_TOUCHED_Y = 2;
  public static final short FLAG_ON_CURVE = 4;
  public static final short FLAG_CONTOUR_END = 8;
  public static final short FLAG_WEAK_INTERPOLATION = 16;
  public static final short FLAG_INFLECTION = 32;
  public static final short FLAG_DONE_X = 64;
  public static final short FLAG_DONE_Y = 128;
  
  /**
   * Right direction.
   */
  public static final int DIR_RIGHT = 1;

  /**
   * Left direction.
   */
  public static final int DIR_LEFT = -1;

  /**
   * Up direction.
   */
  public static final int DIR_UP = 2;

  /**
   * Down direction.
   */
  public static final int DIR_DOWN = -2;

  /**
   * The original x coordinate in font units.
   */
  int origX;

  /**
   * The original y coordinate in font units.
   */
  int origY;

  /**
   * The x coordinate scaled to the target.
   */
  int scaledX;

  /**
   * The y coordinate scaled to the target.
   */
  int scaledY;

  /**
   * The final hinted and scaled x coordinate.
   */
  int x;

  /**
   * The final hinted and scaled y coordinate.
   */
  int y;

  int u;
  int v;

  /**
   * The glyph flags.
   */
  short flags;

  /**
   * The previous point in the contour.
   */
  private Point prev;

  /**
   * The next point in the contour.
   */
  private Point next;

  /**
   * The in-direction of the point, according to the DIR_* constants of this
   * class.
   */
  int inDir;

  /**
   * The out-direction of the point, according to the DIR_* constants of this
   * class.
   */
  int outDir;

  public Point getNext()
  {
    return next;
  }

  public void setNext(Point next)
  {
    this.next = next;
  }

  public Point getPrev()
  {
    return prev;
  }

  public void setPrev(Point prev)
  {
    this.prev = prev;
  }

  public int getOrigX()
  {
    return origX;
  }

  public void setOrigX(int origX)
  {
    this.origX = origX;
  }

  public int getOrigY()
  {
    return origY;
  }

  public void setOrigY(int origY)
  {
    this.origY = origY;
  }

  public int getInDir()
  {
    return inDir;
  }

  public void setInDir(int inDir)
  {
    this.inDir = inDir;
  }

  public int getOutDir()
  {
    return outDir;
  }

  public void setOutDir(int outDir)
  {
    this.outDir = outDir;
  }

  public short getFlags()
  {
    return flags;
  }

  public void setFlags(short flags)
  {
    this.flags = flags;
  }

  public void addFlags(short flags)
  {
    this.flags |= flags;
  }

  public boolean isControlPoint()
  {
    return (flags & FLAG_ON_CURVE) == 0;
  }

  public int getU()
  {
    return u;
  }

  public void setU(int u)
  {
    this.u = u;
  }

  public int getV()
  {
    return v;
  }

  public void setV(int v)
  {
    this.v = v;
  }

  public String toString()
  {
    StringBuilder s = new StringBuilder();
    s.append("[Point] origX: ");
    s.append(origX);
    s.append(", origY: ");
    s.append(origY);
    // TODO: Add more info when needed.
    return s.toString();
  }

  public int getX()
  {
    return x;
  }

  public void setX(int x)
  {
    this.x = x;
  }

  public int getY()
  {
    return y;
  }

  public void setY(int y)
  {
    this.y = y;
  }

  public int getScaledX()
  {
    return scaledX;
  }

  public void setScaledX(int scaledX)
  {
    this.scaledX = scaledX;
  }

  public int getScaledY()
  {
    return scaledY;
  }

  public void setScaledY(int scaledY)
  {
    this.scaledY = scaledY;
  }
}
