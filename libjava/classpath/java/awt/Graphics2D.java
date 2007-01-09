/* Copyright (C) 2000, 2002, 2004, 2006,  Free Software Foundation

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


package java.awt;

import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImageObserver;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.RenderableImage;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.text.AttributedCharacterIterator;
import java.util.Map;

/**
 * An abstract class defining a device independent two-dimensional vector 
 * graphics API.  Concrete subclasses implement this API for output of 
 * vector graphics to:
 * <p>
 * <ul>
 * <li>a {@link javax.swing.JComponent} - in the 
 *     {@link javax.swing.JComponent#paint(Graphics)} method, the incoming 
 *     {@link Graphics} should always be an instance of 
 *     <code>Graphics2D</code>;</li> 
 * <li>a {@link BufferedImage} - see 
 *     {@link BufferedImage#createGraphics()};</li>
 * <li>a {@link java.awt.print.PrinterJob} - in the 
 *     {@link Printable#print(Graphics, PageFormat, int)} method, the incoming
 *     {@link Graphics} should always be an instance of 
 *     <code>Graphics2D</code>.</li>
 * </ul>
 * <p>
 * Third party libraries provide support for output to other formats via this 
 * API, including encapsulated postscript (EPS), portable document format (PDF),
 * and scalable vector graphics (SVG).
 * 
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public abstract class Graphics2D extends Graphics
{

  protected Graphics2D()
  {
  }
  
  public void draw3DRect(int x, int y, int width, int height,
			 boolean raised)
  {
    super.draw3DRect(x, y, width, height, raised);
  }
  
  public void fill3DRect(int x, int y, int width, int height,
			 boolean raised)
  {
    super.fill3DRect(x, y, width, height, raised);
  }

  /**
   * Draws an outline around a shape using the current stroke and paint.
   * 
   * @param shape  the shape (<code>null</code> not permitted).
   * 
   * @see #getStroke()
   * @see #getPaint()
   */
  public abstract void draw(Shape shape);

  public abstract boolean drawImage(Image image, AffineTransform xform,
				    ImageObserver obs);

  public abstract void drawImage(BufferedImage image,
				 BufferedImageOp op,
				 int x,
				 int y);

  public abstract void drawRenderedImage(RenderedImage image,
					 AffineTransform xform);

  public abstract void drawRenderableImage(RenderableImage image,
                                           AffineTransform xform);

  /**
   * Draws a string at the specified location, using the current font.
   * 
   * @param text  the string to draw.
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * 
   * @see Graphics#setFont(Font)
   */
  public abstract void drawString(String text, int x, int y);

  /**
   * Draws a string at the specified location, using the current font.
   * 
   * @param text  the string to draw.
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * 
   * @see Graphics#setFont(Font)
   */
  public abstract void drawString(String text, float x, float y);
    
  /**
   * Draws an attributed string at the specified location.
   * 
   * @param iterator  the source of the attributed text.
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   */
  public abstract void drawString(AttributedCharacterIterator iterator,
                                  int x, int y);

  /**
   * Draws an attributed string at the specified location.
   * 
   * @param iterator  the source of the attributed text.
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   */
  public abstract void drawString(AttributedCharacterIterator iterator,
				  float x, float y);

  /**
   * Fills the interior of the specified <code>shape</code> using the current
   * paint.
   * 
   * @param shape  the shape to fill (<code>null</code> not permitted).
   * 
   * @see #draw(Shape)
   * @see #getPaint()
   */
  public abstract void fill(Shape shape);
    
  public abstract boolean hit(Rectangle rect, Shape text,
			      boolean onStroke);

  public abstract GraphicsConfiguration getDeviceConfiguration();

  /**
   * Sets the current compositing rule.
   * 
   * @param comp  the composite.
   * 
   * @see #getComposite()
   */
  public abstract void setComposite(Composite comp);
    
  /**
   * Sets the paint to be used for subsequent drawing operations.
   * 
   * @param paint  the paint (<code>null</code> not permitted).
   * 
   * @see #getPaint()
   */
  public abstract void setPaint(Paint paint);

  /**
   * Sets the stroke to be used for subsequent drawing operations.
   * 
   * @param stroke  the stroke (<code>null</code> not permitted).
   * 
   * @see #getStroke()
   */
  public abstract void setStroke(Stroke stroke);

  /**
   * Adds or updates a hint in the current rendering hints table.
   * 
   * @param hintKey  the hint key.
   * @param hintValue  the hint value.
   */
  public abstract void setRenderingHint(RenderingHints.Key hintKey,
                                        Object hintValue);

  /**
   * Returns the current value of a rendering hint.
   * 
   * @param hintKey  the key for the hint.
   * 
   * @return The value for the specified hint.
   */
  public abstract Object getRenderingHint(RenderingHints.Key hintKey);
  
  /**
   * Replaces the current rendering hints with the supplied hints.
   * 
   * @param hints  the hints.
   * 
   * @see #addRenderingHints(Map)
   */
  public abstract void setRenderingHints(Map<?,?> hints);

  /**
   * Adds/updates the rendering hint.
   * 
   * @param hints  the hints to add or update.
   */
  public abstract void addRenderingHints(Map<?,?> hints);

  /**
   * Returns the current rendering hints.
   * 
   * @return The current rendering hints.
   */
  public abstract RenderingHints getRenderingHints();

  public abstract void translate(int x, int y);

  public abstract void translate(double tx, double ty);
    
  public abstract void rotate(double theta);

  public abstract void rotate(double theta, double x, double y);

  public abstract void scale(double scaleX, double scaleY);

  public abstract void shear(double shearX, double shearY);

  /**
   * Sets the current transform to a concatenation of <code>transform</code>
   * and the existing transform.
   * 
   * @param transform  the transform.
   */
  public abstract void transform(AffineTransform transform);
  
  /**
   * Sets the current transform.  If the caller specifies a <code>null</code>
   * transform, this method should set the current transform to the 
   * identity transform.
   * 
   * @param transform  the transform (<code>null</code> permitted).
   * 
   * @see #getTransform()
   */
  public abstract void setTransform(AffineTransform transform);

  /**
   * Returns the current transform.
   * 
   * @return The current transform.
   * 
   * @see #setTransform(AffineTransform)
   */
  public abstract AffineTransform getTransform();

  /**
   * Returns the current paint.
   * 
   * @return The current paint.
   * 
   * @see #setPaint(Paint)
   */
  public abstract Paint getPaint();

  /**
   * Returns the current compositing rule.
   * 
   * @return The current compositing rule.
   * 
   * @see #setComposite(Composite)
   */
  public abstract Composite getComposite();

  /**
   * Sets the background color (used by the 
   * {@link Graphics#clearRect(int, int, int, int)} method).
   * 
   * @param color  the color.
   * 
   * @see #getBackground()
   */
  public abstract void setBackground(Color color);

  /**
   * Returns the color used by the 
   * {@link Graphics#clearRect(int, int, int, int)} method.
   * 
   * @return The background color.
   * 
   * @see #setBackground(Color)
   */
  public abstract Color getBackground();

  /**
   * Returns the current stroke.
   * 
   * @return The current stroke.
   * 
   * @see #setStroke(Stroke)
   */
  public abstract Stroke getStroke();    

  /**
   * Sets the clip region to the intersection of the current clipping region 
   * and <code>s</code>.
   * 
   * @param s  the shape to intersect with the current clipping region.
   * 
   * @see Graphics#setClip(Shape)
   */
  public abstract void clip(Shape s);

  /**
   * Returns the font render context.
   * 
   * @return The font render context.
   */
  public abstract FontRenderContext getFontRenderContext();

  /**
   * Draws a glyph vector at the specified location.
   * 
   * @param g  the glyph vector.
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   */
  public abstract void drawGlyphVector(GlyphVector g, float x, float y);
}
