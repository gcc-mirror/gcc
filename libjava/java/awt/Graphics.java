/* Graphics.java -- Abstract Java drawing class
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.

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

import java.awt.image.ImageObserver;

/**
  * This is the abstract superclass of classes for drawing to graphics
  * devices such as the screen or printers.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy <warrenl@cygnus.com>
  */
public abstract class Graphics
{

/*
 * Instance Variables
 */

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Default constructor for subclasses.
  */
protected
Graphics()
{
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns a copy of this <code>Graphics</code> object.
  *
  * @return A copy of this object.
  */
public abstract Graphics
create();

/*************************************************************************/

/**
  * Returns a copy of this <code>Graphics</code> object.  The origin point
  * will be translated to the point (x, y) and the cliping rectangle set
  * to the intersection of the clipping rectangle in this object and the
  * rectangle specified by the parameters to this method.
  *
  * @param x The new X coordinate of the clipping region rect.
  * @param y The new Y coordinate of the clipping region rect.
  * @param width The width of the clipping region intersect rectangle. 
  * @param height The height of the clipping region intersect rectangle. 
  *
  * @return A copy of this object, modified as specified.
  */
public Graphics
create(int x, int y, int width, int height)
{
  Graphics g = create();

  g.translate(x, y);
  // FIXME: I'm not sure if this will work.  Are the old clip rect bounds
  // translated above?
  g.clipRect(0, 0, width, height);

  return(g);
}

/*************************************************************************/

/**
  * Translates this context so that its new origin point is the point
  * (x, y).
  *
  * @param x The new X coordinate of the origin.
  * @param y The new Y coordinate of the origin.
  */
public abstract void
translate(int x, int y);

/*************************************************************************/

/**
  * Returns the current color for this object.
  *
  * @return The color for this object.
  */
public abstract Color
getColor();

/*************************************************************************/

/**
  * Sets the current color for this object.
  *
  * @param color The new color.
  */
public abstract void
setColor(Color color);

/*************************************************************************/

/**
  * Sets this context into "paint" mode, where the target pixels are
  * completely overwritten when drawn on.
  */
public abstract void
setPaintMode();

/*************************************************************************/

/**
  * Sets this context info "XOR" mode, where the targe pixles are
  * XOR-ed when drawn on. 
  *
  * @param color The color to XOR against.
  */
public abstract void
setXORMode(Color color);
  
/*************************************************************************/

/**
  * Returns the current font for this graphics context.
  *
  * @return The current font.
  */
public abstract Font
getFont();

/*************************************************************************/

/**
  * Sets the font for this graphics context to the specified value.
  *
  * @param font The new font.
  */
public abstract void
setFont(Font font);

/*************************************************************************/

/**
  * Returns the font metrics for the current font.
  *
  * @return The font metrics for the current font.
  */
public FontMetrics
getFontMetrics()
{
  return(getFontMetrics(getFont()));
}

/*************************************************************************/

/**
  * Returns the font metrics for the specified font.
  *
  * @param font The font to return metrics for.
  *
  * @return The requested font metrics.
  */
public abstract FontMetrics
getFontMetrics(Font font);

/*************************************************************************/

/**
  * Returns the bounding rectangle of the clipping region for this 
  * graphics context.
  *
  * @return The bounding rectangle for the clipping region.
  */
public abstract Rectangle
getClipBounds();

/*************************************************************************/

/**
  * Returns the bounding rectangle of the clipping region for this 
  * graphics context.
  *
  * @return The bounding rectangle for the clipping region.
  *
  * @deprecated This method is deprecated in favor of
  * <code>getClipBounds()</code>.
  */
public Rectangle
getClipRect()
{
  return(getClipBounds());
}

/*************************************************************************/

/**
  * Sets the clipping region to the intersection of the current clipping
  * region and the rectangle determined by the specified parameters.
  *
  * @param x The X coordinate of the upper left corner of the intersect rect.
  * @param Y The Y coordinate of the upper left corner of the intersect rect.
  * @param width The width of the intersect rect.
  * @param height The height of the intersect rect.
  */
public abstract void
clipRect(int x, int y, int width, int height);

/*************************************************************************/

/**
  * Sets the clipping region to the rectangle determined by the specified
  * parameters.
  *
  * @param x The X coordinate of the upper left corner of the rect.
  * @param y The Y coordinate of the upper left corner of the rect.
  * @param width The width of the rect.
  * @param height The height of the rect.
  */
public abstract void
setClip(int x, int y, int width, int height);

/*************************************************************************/

/**
  * Returns the current clipping region as a <code>Shape</code> object.
  *
  * @return The clipping region as a <code>Shape</code>.
  */
public abstract Shape
getClip();

/*************************************************************************/

/**
  * Sets the clipping region to the specified <code>Shape</code>.
  *
  * @param shape The new clipping region.
  */
public abstract void
setClip(Shape clip);

/*************************************************************************/

/**
  * Copies the specified rectangle to the specified offset location.
  *
  * @param x The X coordinate of the upper left corner of the copy rect.
  * @param y The Y coordinate of the upper left corner of the copy rect.
  * @param width The width of the copy rect.
  * @param height The height of the copy rect.
  * @param dx The offset from the X value to start drawing.
  * @param dy The offset from the Y value to start drawing.
  */
public abstract void
copyArea(int x, int y, int width, int height, int dx, int dy);

/*************************************************************************/

/**
  * Draws a line between the two specified points.
  *
  * @param x1 The X coordinate of the first point.
  * @param y1 The Y coordinate of the first point.
  * @param x2 The X coordinate of the second point.
  * @param y2 The Y coordinate of the second point.
  */
public abstract void
drawLine(int x1, int y1, int x2, int y2);

/*************************************************************************/

/**
  * Fills the area bounded by the specified rectangle.
  *
  * @param x The X coordinate of the upper left corner of the fill rect.
  * @param y The Y coordinate of the upper left corner of the fill rect.
  * @param width The width of the fill rect.
  * @param height The height of the fill rect.
  */
public abstract void
fillRect(int x, int y, int width, int height); 

/*************************************************************************/

/**
  * Draws the outline of the specified rectangle.
  *
  * @param x The X coordinate of the upper left corner of the draw rect.
  * @param y The Y coordinate of the upper left corner of the draw rect.
  * @param width The width of the draw rect.
  * @param height The height of the draw rect.
  */
public void
drawRect(int x, int y, int width, int height)
{
  int x1 = x;
  int y1 = y;
  int x2 = x + width;
  int y2 = y + height;
  drawLine(x1, y1, x2, y1);
  drawLine(x2, y1, x2, y2);
  drawLine(x2, y2, x1, y2);
  drawLine(x1, y2, x1, y1);
}

/*************************************************************************/

/**
  * Clears the specified rectangle.
  *
  * @param x The X coordinate of the upper left corner of the clear rect.
  * @param y The Y coordinate of the upper left corner of the clear rect.
  * @param width The width of the clear rect.
  * @param height The height of the clear rect.
  */
public abstract void
clearRect(int x, int y, int width, int height);

/*************************************************************************/

/**
  * Draws the outline of the specified rectangle with rounded cornders.
  *
  * @param x The X coordinate of the upper left corner of the draw rect.
  * @param y The Y coordinate of the upper left corner of the draw rect.
  * @param width The width of the draw rect.
  * @param height The height of the draw rect.
  * @param arcWidth The width of the corner arcs.
  * @param arcHeigth The height of the corner arcs.
  */
public abstract void
drawRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight);

/*************************************************************************/

/**
  * Fills the specified rectangle with rounded cornders.
  *
  * @param x The X coordinate of the upper left corner of the fill rect.
  * @param y The Y coordinate of the upper left corner of the fill rect.
  * @param width The width of the fill rect.
  * @param height The height of the fill rect.
  * @param arcWidth The width of the corner arcs.
  * @param arcHeigth The height of the corner arcs.
  */
public abstract void
fillRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight);

/*************************************************************************/

public void
draw3DRect(int x, int y, int width, int height, boolean raised)
{
  Color color = getColor();
  Color tl = color.brighter();
  Color br = color.darker();
    
  if (!raised)
    {
      Color tmp = tl;
      tl = br;
      br = tmp;
    }
    
  int x1 = x;
  int y1 = y;
  int x2 = x + width;
  int y2 = y + height;
    
  setColor(tl);
  drawLine(x1, y1, x2, y1);
  drawLine(x1, y2, x1, y1);
  setColor(br);
  drawLine(x2, y1, x2, y2);
  drawLine(x2, y1, x1, y2);
  setColor(color);
}

/**
  * Fills the specified rectangle with a 3D effect
  *
  * @param x The X coordinate of the upper left corner of the fill rect.
  * @param y The Y coordinate of the upper left corner of the fill rect.
  * @param width The width of the fill rect.
  * @param height The height of the fill rect.
  * @param raised <code>true</code> if the rectangle appears raised,
  * <code>false</code> if it should appear etched.
  */
public void
fill3DRect(int x, int y, int width, int height, boolean raised)
{
  fillRect(x, y, width, height);
  draw3DRect(x, y, width-1, height-1, raised);
}

/*************************************************************************/

/**
  * Draws the outline of the specified rectangle with a 3D effect
  *
  * @param x The X coordinate of the upper left corner of the draw rect.
  * @param y The Y coordinate of the upper left corner of the draw rect.
  * @param width The width of the draw rect.
  * @param height The height of the draw rect.
  * @param raised <code>true</code> if the rectangle appears raised,
  * <code>false</code> if it should appear etched.
  */
public void
drawRoundRect(int x, int y, int width, int height, boolean raised)
{
  // FIXME: ???
}

/*************************************************************************/

/**
  * Draws an oval that just fits within the specified rectangle.
  *
  * @param x The X coordinate of the upper left corner of the rect.
  * @param y The Y coordinate of the upper left corner of the rect.
  * @param width The width of the rect.
  * @param height The height of the rect.
  */
public abstract void
drawOval(int x, int y, int width, int height);

/*************************************************************************/

/**
  * Fills an oval that just fits within the specified rectangle.
  *
  * @param x The X coordinate of the upper left corner of the rect.
  * @param y The Y coordinate of the upper left corner of the rect.
  * @param width The width of the rect.
  * @param height The height of the rect.
  */
public abstract void
fillOval(int x, int y, int width, int height);

/*************************************************************************/

/**
  * Draws an arc using the specified bounding rectangle and the specified
  * angle parameter.  The arc is centered at the center of the rectangle.
  * The arc starts at the arcAngle position and extend for arcAngle
  * degrees.  The degree origin is at the 3 o'clock position.
  *
  * @param x The X coordinate of the upper left corner of the rect.
  * @param y The Y coordinate of the upper left corner of the rect.
  * @param width The width of the rect.
  * @param height The height of the rect.
  * @param arcStart The beginning angle of the arc.
  * @param arcAngle The extent of the arc.
  */
public abstract void
drawArc(int x, int y, int width, int height, int startAngle, int arcAngle);

/*************************************************************************/

/**
  * Fills the arc define by the specified bounding rectangle and the specified
  * angle parameter.  The arc is centered at the center of the rectangle.
  * The arc starts at the arcAngle position and extend for arcAngle
  * degrees.  The degree origin is at the 3 o'clock position.
  *
  * @param x The X coordinate of the upper left corner of the rect.
  * @param y The Y coordinate of the upper left corner of the rect.
  * @param width The width of the rect.
  * @param height The height of the rect.
  * @param arcStart The beginning angle of the arc.
  * @param arcAngle The extent of the arc.
  */
public abstract void
fillArc(int x, int y, int width, int height, int startAngle, int arcAngle);

/*************************************************************************/

/**
  * Draws a series of interconnected lines determined by the arrays
  * of corresponding x and y coordinates.
  *
  * @param xPoints The X coordinate array.
  * @param yPoints The Y coordinate array.
  * @param npoints The number of points to draw.
  */
public abstract void
drawPolyline(int xPoints[], int yPoints[], int npoints);

/*************************************************************************/

/**
  * Draws a series of interconnected lines determined by the arrays
  * of corresponding x and y coordinates.  The figure is closed if necessary
  * by connecting the first and last points.
  *
  * @param xPoints The X coordinate array.
  * @param yPoints The Y coordinate array.
  * @param npoints The number of points to draw.
  */
public abstract void
drawPolygon(int xPoints[], int yPoints[], int npoints);

/*************************************************************************/

/**
  * Draws the specified polygon.
  *
  * @param polygon The polygon to draw.
  */
public void
drawPolygon(Polygon polygon)
{
  drawPolygon(polygon.xpoints, polygon.ypoints, polygon.npoints);
}

/*************************************************************************/

/**
  * Fills the polygon determined by the arrays
  * of corresponding x and y coordinates.
  *
  * @param xPoints The X coordinate array.
  * @param yPoints The Y coordinate array.
  * @param npoints The number of points to draw.
  */
public abstract void
fillPolygon(int xPoints[], int yPoints[], int npoints);

/*************************************************************************/

/**
  * Fills the specified polygon
  *
  * @param polygon The polygon to fill.
  */
public void
fillPolygon(Polygon polygon)
{
  fillPolygon(polygon.xpoints, polygon.ypoints, polygon.npoints);
}

/*************************************************************************/

/**
  * Draws the specified string starting at the specified point.
  *
  * @param string The string to draw.
  * @param x The X coordinate of the point to draw at.
  * @param y The Y coordinate of the point to draw at.
  */
public abstract void
drawString(String string, int x, int y);

/*************************************************************************/

/**
  * Draws the specified characters starting at the specified point.
  *
  * @param data The array of characters to draw.
  * @param offset The offset into the array to start drawing characters from.
  * @param length The number of characters to draw.
  * @param x The X coordinate of the point to draw at.
  * @param y The Y coordinate of the point to draw at.
  */
public void
drawChars(char data[], int offset, int length, int x, int y)
{
  drawString(new String(data, offset, length), x, y);
}

/*************************************************************************/

/**
  * Draws the specified bytes as text starting at the specified point.
  *
  * @param data The array of bytes to draw.
  * @param offset The offset into the array to start drawing bytes from.
  * @param length The number of bytes to draw.
  * @param x The X coordinate of the point to draw at.
  * @param y The Y coordinate of the point to draw at.
  */
public void
drawChars(byte data[], int offset, int length, int x, int y)
{
  drawString(new String(data, offset, length), x, y);
}

/*
public abstract void drawString(AttributedCharacterIterator iterator,
	        		  int x, int y)
*/

public void
drawBytes(byte[] data, int offset, int length, int x, int y)
{
  String str = new String(data, offset, length);
  drawString(str, x, y);
}

/*************************************************************************/

/**
  * Draws all of the image that is available and returns.  If the image
  * is not completely loaded, <code>false</code> is returned and 
  * the specified iamge observer is notified as more data becomes 
  * available.
  *
  * @param image The image to draw.
  * @param x The X coordinate of the point to draw at.
  * @param y The Y coordinate of the point to draw at.
  * @param observer The image observer to notify as data becomes available.
  *
  * @return <code>true</code> if all the image data is available,
  * <code>false</code> otherwise.
  */
public abstract boolean
drawImage(Image image, int x, int y, ImageObserver observer);
 
/*************************************************************************/

/**
  * Draws all of the image that is available and returns.  The image
  * is scaled to fit in the specified rectangle.  If the image
  * is not completely loaded, <code>false</code> is returned and 
  * the specified iamge observer is notified as more data becomes 
  * available.
  *
  * @param image The image to draw.
  * @param x The X coordinate of the point to draw at.
  * @param y The Y coordinate of the point to draw at.
  * @param width The width of the rectangle to draw in.
  * @param height The height of the rectangle to draw in.
  * @param observer The image observer to notify as data becomes available.
  *
  * @return <code>true</code> if all the image data is available,
  * <code>false</code> otherwise.
  */
public abstract boolean
drawImage(Image image, int x, int y, int width, int height, 
          ImageObserver observer);
 
/*************************************************************************/

/**
  * Draws all of the image that is available and returns.  If the image
  * is not completely loaded, <code>false</code> is returned and 
  * the specified iamge observer is notified as more data becomes 
  * available.
  *
  * @param image The image to draw.
  * @param x The X coordinate of the point to draw at.
  * @param y The Y coordinate of the point to draw at.
  * @param bgcolor The background color to use for the image.
  * @param observer The image observer to notify as data becomes available.
  *
  * @return <code>true</code> if all the image data is available,
  * <code>false</code> otherwise.
  */
public abstract boolean
drawImage(Image image, int x, int y, Color bgcolor, ImageObserver observer);
 
/*************************************************************************/

/**
  * Draws all of the image that is available and returns.  The image
  * is scaled to fit in the specified rectangle.  If the image
  * is not completely loaded, <code>false</code> is returned and 
  * the specified iamge observer is notified as more data becomes 
  * available.
  *
  * @param image The image to draw.
  * @param x The X coordinate of the point to draw at.
  * @param y The Y coordinate of the point to draw at.
  * @param width The width of the rectangle to draw in.
  * @param height The height of the rectangle to draw in.
  * @param bgcolor The background color to use for the image.
  * @param observer The image observer to notify as data becomes available.
  *
  * @return <code>true</code> if all the image data is available,
  * <code>false</code> otherwise.
  */
public abstract boolean
drawImage(Image image, int x, int y, int width, int height, Color bgcolor,
          ImageObserver observer);
 
/*************************************************************************/

/**
  * FIXME: Write Javadocs for this when you understand it.
  */
public abstract boolean
drawImage(Image image, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1,
          int sx2, int sy2, ImageObserver observer);

/*************************************************************************/

/**
  * FIXME: Write Javadocs for this when you understand it.
  */
public abstract boolean
drawImage(Image image, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1,
          int sx2, int sy2, Color bgcolor, ImageObserver observer);

/*************************************************************************/

/**
  * Free any resources held by this graphics context immediately instead
  * of waiting for the object to be garbage collected and finalized.
  */
public abstract void
dispose();

/*************************************************************************/

/**
  * Frees the resources held by this graphics context when it is
  * garbage collected.
  */
public void
finalize()
{
  dispose();
}

/*************************************************************************/

/**
  * Returns a string representation of this object.
  *
  * @param A string representation of this object. 
  */
public String
toString()
{
  return(super.toString());
}

public boolean
hitClip(int x, int y, int width, int height)
{
  throw new UnsupportedOperationException("not implemented yet");
}

public Rectangle
getClipBounds(Rectangle r)
{
  Rectangle clipBounds = getClipBounds();
  
  if (r == null)
    return clipBounds;

  r.x      = clipBounds.x;
  r.y      = clipBounds.y;
  r.width  = clipBounds.width;
  r.height = clipBounds.height;
  return r;
}

} // class Graphics

