/* DebugGraphics.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.image.ImageObserver;
import java.io.PrintStream;
import java.text.AttributedCharacterIterator;


/**
 * DebugGraphics
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DebugGraphics extends Graphics
{

  /**
   * graphics
   */
  Graphics graphics;

  /**
   * buffer
   */
  Image buffer;

  /**
   * debugOptions
   */
  int debugOptions;

  /**
   * graphicsID
   */
  int graphicsID;

  /**
   * xOffset
   */
  int xOffset;

  /**
   * yOffset
   */
  int yOffset;

  /**
   * LOG_OPTION
   */
  public static final int LOG_OPTION = 1;

  /**
   * FLASH_OPTION
   */
  public static final int FLASH_OPTION = 2;

  /**
   * BUFFERED_OPTION
   */
  public static final int BUFFERED_OPTION = 4;

  /**
   * NONE_OPTION
   */
  public static final int NONE_OPTION = -1;

  /**
   * Creates a <code>DebugGraphics</code> object.
   */
  public DebugGraphics()
  {
    // TODO
  }

  /**
   * Constructor DebugGraphics
   * @param graphics TODO
   * @param component TODO
   */
  public DebugGraphics(Graphics graphics, JComponent component)
  {
    // TODO
  }

  /**
   * Constructor DebugGraphics
   * @param graphics TODO
   */
  public DebugGraphics(Graphics graphics)
  {
    // TODO
  }

  /**
   * setColor
   * @param value0 TODO
   */
  public void setColor(Color color)
  {
    // TODO
  }

  /**
   * create
   *
   * @return Graphics
   */
  public Graphics create()
  {
    return null; // TODO
  }

  /**
   * create
   *
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @returns Graphics
   */
  public Graphics create(int valx, int y, int w, int h)
  {
    return null; // TODO
  }

  /**
   * flashColor
   *
   * @return Color
   */
  public static Color flashColor()
  {
    return null; // TODO
  }

  /**
   * setFlashColor
   *
   * @param color the color to use for flashing
   */
  public static void setFlashColor(Color color)
  {
    // TODO
  }

  /**
   * flashTime
   *
   * @return The time in milliseconds
   */
  public static int flashTime()
  {
    return 0; // TODO
  }

  /**
   * setFlashTime
   *
   * @param time The time in milliseconds
   */
  public static void setFlashTime(int time)
  {
    // TODO
  }

  /**
   * flashCount
   * @returns int
   */
  public static int flashCount()
  {
    return 0; // TODO
  }

  /**
   * setFlashCount
   *
   * @param count The number of flashes
   */
  public static void setFlashCount(int count)
  {
    // TODO
  }

  /**
   * logStream
   *
   * @return The <code>PrintStream</code> to write logging messages to
   */
  public static PrintStream logStream()
  {
    return null; // TODO
  }

  /**
   * setLogStream
   *
   * @param stream The currently set <code>PrintStream</code>.
   */
  public static void setLogStream(PrintStream stream)
  {
    // TODO
  }

  /**
   * getFont
   * @returns Font
   */
  public Font getFont()
  {
    return null; // TODO
  }

  /**
   * setFont
   *
   * @param font The font to use for drawing text
   */
  public void setFont(Font font)
  {
    // TODO
  }

  /**
   * getColor
   * @returns Color
   */
  public Color getColor()
  {
    return null; // TODO
  }

  /**
   * getFontMetrics
   * @returns FontMetrics
   */
  public FontMetrics getFontMetrics()
  {
    return null; // TODO
  }

  /**
   * getFontMetrics
   * @param font TODO
   * @returns FontMetrics
   */
  public FontMetrics getFontMetrics(Font font)
  {
    return null; // TODO
  }

  /**
   * translate
   *
   * @param x TODO
   * @param y TODO
   */
  public void translate(int x, int y)
  {
    // TODO
  }

  /**
   * setPaintMode
   */
  public void setPaintMode()
  {
    // TODO
  }

  /**
   * setXORMode
   *
   * @param color TODO
   */
  public void setXORMode(Color color)
  {
    // TODO
  }

  /**
   * getClipBounds
   * @returns Rectangle
   */
  public Rectangle getClipBounds()
  {
    return null; // TODO
  }

  /**
   * clipRect
   * @param value0 TODO
   * @param value1 TODO
   * @param value2 TODO
   * @param value3 TODO
   */
  public void clipRect(int value0, int value1, int value2, int value3)
  {
    // TODO
  }

  /**
   * setClip
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   */
  public void setClip(int x, int y, int w, int h)
  {
    // TODO
  }

  /**
   * getClip
   * @returns Shape
   */
  public Shape getClip()
  {
    return null; // TODO
  }

  /**
   * setClip
   * @param shape TODO
   */
  public void setClip(Shape shape)
  {
    // TODO
  }

  /**
   * drawRect
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param valh TODO
   */
  public void drawRect(int x, int y, int w, int h)
  {
    // TODO
  } // drawRect()

  /**
   * fillRect
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   */
  public void fillRect(int x, int y, int w, int h)
  {
    // TODO
  } // fillRect()

  /**
   * clearRect
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   */
  public void clearRect(int x, int y, int w, int h)
  {
    // TODO
  }

  /**
   * drawRoundRect
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param arcWidth TODO
   * @param arcHeight TODO
   */
  public void drawRoundRect(int x, int y, int w, int h, int arcWidth,
                            int arcHeight)
  {
    // TODO
  }

  /**
   * fillRoundRect
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param arcWidth TODO
   * @param arcHeight TODO
   */
  public void fillRoundRect(int x, int y, int w, int h, int arcWidth,
                            int arcHeight)
  {
    // TODO
  }

  /**
   * drawLine
   * @param x1 TODO
   * @param y1 TODO
   * @param x2 TODO
   * @param y2 TODO
   */
  public void drawLine(int x1, int y1, int x2, int y2)
  {
    // TODO
  }

  /**
   * draw3DRect
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param raised TODO
   */
  public void draw3DRect(int x, int y, int w, int h, boolean raised)
  {
    // TODO
  }

  /**
   * fill3DRect
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param raised TODO
   */
  public void fill3DRect(int x, int y, int w, int h, boolean raised)
  {
    // TODO
  }

  /**
   * drawOval
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   */
  public void drawOval(int x, int y, int w, int h)
  {
    // TODO
  }

  /**
   * fillOval
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   */
  public void fillOval(int x, int y, int w, int h)
  {
    // TODO
  }

  /**
   * drawArc
   *
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param startAngle TODO
   * @param arcAngle TODO
   */
  public void drawArc(int x, int y, int w, int h, int startAngle, int arcAngle)
  {
    // TODO
  }

  /**
   * fillArc
   *
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param startAngle TODO
   * @param arcAngle TODO
   */
  public void fillArc(int x, int y, int w, int h, int startAngle, int arcAngle)
  {
    // TODO
  }

  /**
   * drawPolyline
   * @param xpoints TODO
   * @param ypoints TODO
   * @param npoints TODO
   */
  public void drawPolyline(int[] xpoints, int[] ypoints, int npoints)
  {
    // TODO
  }

  /**
   * drawPolygon
   *
   * @param xpoints TODO
   * @param ypoints TODO
   * @param npoints TODO
   */
  public void drawPolygon(int[] xpoints, int[] ypoints, int npoints)
  {
    // TODO
  }

  /**
   * fillPolygon
   *
   * @param xpoints TODO
   * @param ypoints TODO
   * @param npoints TODO
   */
  public void fillPolygon(int[] xpoints, int[] ypoints, int npoints)
  {
    // TODO
  }

  /**
   * drawString
   *
   * @param string TODO
   * @param x TODO
   * @param y TODO
   */
  public void drawString(String string, int s, int y)
  {
    // TODO
  }

  /**
   * drawString
   *
   * @param iterator TODO
   * @param x TODO
   * @param y TODO
   */
  public void drawString(AttributedCharacterIterator iterator, int x, int y)
  {
    // TODO
  }

  /**
   * drawBytes
   * 
   * @param data TODO
   * @param offset TODO
   * @param length TODO
   * @param x TODO
   * @param y TODO
   */
  public void drawBytes(byte[] data, int offset, int length, int x, int y)
  {
    // TODO
  }

  /**
   * drawChars
   * @param data TODO
   * @param offset TODO
   * @param length TODO
   * @param value3 TODO
   * @param value4 TODO
   */
  public void drawChars(char[] data, int offset, int value2, int x, int y)
  {
    // TODO
  } // drawChars()

  /**
   * drawImage
   * @param image TODO
   * @param x TODO
   * @param y TODO
   * @param observer TODO
   * @returns boolean
   */
  public boolean drawImage(Image image, int x, int y, ImageObserver observer)
  {
    return false; // TODO
  }

  /**
   * drawImage
   * @param image TODO
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param observer TODO
   * @returns boolean
   */
  public boolean drawImage(Image image, int x, int y, int w, int h,
                           ImageObserver observer)
  {
    return false; // TODO
  }

  /**
   * drawImage
   * @param image TODO
   * @param x TODO
   * @param y TODO
   * @param background TODO
   * @param observer TODO
   * @returns boolean
   */
  public boolean drawImage(Image image, int x, int y, Color background,
                           ImageObserver observer)
  {
    return false; // TODO
  }

  /**
   * drawImage
   * @param image TODO
   * @param x TODO
   * @param y TODO
   * @param w TODO
   * @param h TODO
   * @param background TODO
   * @param observer TODO
   * @returns boolean
   */
  public boolean drawImage(Image image, int x, int y, int w, int h,
                           Color background, ImageObserver observer)
  {
    return false; // TODO
  }

  /**
   * drawImage
   * 
   * @param image The image to draw
   * @param dx1 TODO
   * @param dy1 TODO
   * @param dx2 TODO
   * @param dy2 TODO
   * @param sx1 TODO
   * @param sy1 TODO
   * @param sx2 TODO
   * @param sy2 TODO
   * @param observer TODO
   * @returns boolean
   */
  public boolean drawImage(Image image, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           ImageObserver observer)
  {
    return false; // TODO
  }

  /**
   * drawImage
   *
   * @param image The image to draw
   * @param dx1 TODO
   * @param dy1 TODO
   * @param dx2 TODO
   * @param dy2 TODO
   * @param sx1 TODO
   * @param sy1 TODO
   * @param sx2 TODO
   * @param sy2 TODO
   * @param background TODO
   * @param observer TODO
   * @returns boolean
   */
  public boolean drawImage(Image image, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           Color background, ImageObserver observer)
  {
    return false; // TODO
  }

  /**
   * copyArea
   *
   * @param x The x position of the source area
   * @param y The y position of the source area
   * @param width The width of the area
   * @param height The height of the area
   * @param destx The x position of the destination area
   * @param desty The y posiiton of the destination area
   */
  public void copyArea(int x, int y, int w, int h, int destx, int desty)
  {
    // TODO
  }

  /**
   * Releases all system resources that this <code>Graphics</code> is using.
   */
  public void dispose()
  {
    // TODO
  }

  /**
   * isDrawingBuffer
   *
   * @return boolean
   */
  public boolean isDrawingBuffer()
  {
    return false; // TODO
  }

  /**
   * toShortString
   * @returns String
   */
  String toShortString()
  {
    return null; // TODO
  } // toShortString()

  /**
   * setDebugOptions
   *
   * @param options the debug options
   */
  public void setDebugOptions(int options)
  {
    // TODO
  }

  /**
   * getDebugOptions
   *
   * @return the debug options
   */
  public int getDebugOptions()
  {
    return 0; // TODO
  }
}
