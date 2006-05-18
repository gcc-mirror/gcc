/* AbstractGraphics2D.java -- Abstract Graphics2D implementation
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

package gnu.java.awt.java2d;

import java.awt.AWTError;
import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.CompositeContext;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Paint;
import java.awt.PaintContext;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.Toolkit;
import java.awt.RenderingHints.Key;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.PathIterator;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.WritableRaster;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Implements general and shared behaviour for Graphics2D implementation.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public abstract class AbstractGraphics2D
  extends Graphics2D
  implements Cloneable
{

  /**
   * Accuracy of the sampling in the anti-aliasing shape filler.
   * Lower values give more speed, while higher values give more quality.
   * It is advisable to choose powers of two.
   */
  private static final int AA_SAMPLING = 8;

  /**
   * The transformation for this Graphics2D instance
   */
  private AffineTransform transform;

  /**
   * The foreground.
   */
  private Paint paint;

  /**
   * The background.
   */
  private Color background;

  /**
   * The current font.
   */
  private Font font;

  /**
   * The current composite setting.
   */
  private Composite composite;

  /**
   * The current stroke setting.
   */
  private Stroke stroke;

  /**
   * The current clip. This clip is in user coordinate space.
   */
  private Shape clip;

  /**
   * The rendering hints.
   */
  private RenderingHints renderingHints;

  /**
   * The paint raster.
   */
  private Raster paintRaster;

  /**
   * A cached pixel array.
   */
  private int[] pixel;

  /**
   * The raster of the destination surface. This is where the painting is
   * performed.
   */
  private WritableRaster destinationRaster;

  /**
   * Stores the alpha values for a scanline in the anti-aliasing shape
   * renderer.
   */
  private transient int[] alpha;

  /**
   * The edge table for the scanline conversion algorithms.
   */
  private transient ArrayList[] edgeTable;

  /**
   * Indicates if cerain graphics primitives can be rendered in an optimized
   * fashion. This will be the case if the following conditions are met:
   * - The transform may only be a translation, no rotation, shearing or
   *   scaling.
   * - The paint must be a solid color.
   * - The composite must be an AlphaComposite.SrcOver.
   * - The clip must be a Rectangle.
   * - The stroke must be a plain BasicStroke().
   *
   * These conditions represent the standard settings of a new
   * AbstractGraphics2D object and will be the most commonly used setting
   * in Swing rendering and should therefore be optimized as much as possible.
   */
  private boolean isOptimized;

  /**
   * Creates a new AbstractGraphics2D instance.
   */
  protected AbstractGraphics2D()
  {
    transform = new AffineTransform();
    background = Color.WHITE;
    composite = AlphaComposite.SrcOver;
    stroke = new BasicStroke();
    HashMap hints = new HashMap();
    hints.put(RenderingHints.KEY_TEXT_ANTIALIASING,
              RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT);
    hints.put(RenderingHints.KEY_ANTIALIASING,
              RenderingHints.VALUE_ANTIALIAS_DEFAULT);
    renderingHints = new RenderingHints(hints);

    pixel = new int[4];
  }

  /**
   * Draws the specified shape. The shape is passed through the current stroke
   * and is then forwarded to {@link #fillShape}.
   *
   * @param shape the shape to draw
   */
  public void draw(Shape shape)
  {
    // Stroke the shape.
    Shape strokedShape = stroke.createStrokedShape(shape);

    // Clip the stroked shape.
//    Shape clipped = clipShape(strokedShape);
//    if (clipped != null)
//      {
//        // Fill the shape.
//        fillShape(clipped, false);
//      }
    // FIXME: Clipping doesn't seem to work.
    fillShape(strokedShape, false);
  }

  public boolean drawImage(Image image, AffineTransform xform, ImageObserver obs)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public void drawImage(BufferedImage image, BufferedImageOp op, int x, int y)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public void drawRenderableImage(RenderableImage image, AffineTransform xform)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  /**
   * Draws the specified string at the specified location.
   *
   * @param text the string to draw
   * @param x the x location, relative to the bounding rectangle of the text
   * @param y the y location, relative to the bounding rectangle of the text
   */
  public void drawString(String text, int x, int y)
  {
    FontRenderContext ctx = getFontRenderContext();
    GlyphVector gv = font.createGlyphVector(ctx, text.toCharArray());
    drawGlyphVector(gv, x, y);
  }

  /**
   * Draws the specified string at the specified location.
   *
   * @param text the string to draw
   * @param x the x location, relative to the bounding rectangle of the text
   * @param y the y location, relative to the bounding rectangle of the text
   */
  public void drawString(String text, float x, float y)
  {
    FontRenderContext ctx = getFontRenderContext();
    GlyphVector gv = font.createGlyphVector(ctx, text.toCharArray());
    drawGlyphVector(gv, x, y);
  }

  /**
   * Draws the specified string (as AttributedCharacterIterator) at the
   * specified location.
   *
   * @param iterator the string to draw
   * @param x the x location, relative to the bounding rectangle of the text
   * @param y the y location, relative to the bounding rectangle of the text
   */
  public void drawString(AttributedCharacterIterator iterator, int x, int y)
  {
    FontRenderContext ctx = getFontRenderContext();
    GlyphVector gv = font.createGlyphVector(ctx, iterator);
    drawGlyphVector(gv, x, y);
  }

  /**
   * Draws the specified string (as AttributedCharacterIterator) at the
   * specified location.
   *
   * @param iterator the string to draw
   * @param x the x location, relative to the bounding rectangle of the text
   * @param y the y location, relative to the bounding rectangle of the text
   */
  public void drawString(AttributedCharacterIterator iterator, float x, float y)
  {
    FontRenderContext ctx = getFontRenderContext();
    GlyphVector gv = font.createGlyphVector(ctx, iterator);
    drawGlyphVector(gv, x, y);
  }

  /**
   * Fills the specified shape with the current foreground.
   *
   * @param shape the shape to fill
   */
  public void fill(Shape shape)
  {
//    Shape clipped = clipShape(shape);
//    if (clipped != null)
//      fillShape(clipped, false);
    fillShape(shape, false);
  }

  public boolean hit(Rectangle rect, Shape text, boolean onStroke)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  /**
   * Sets the composite.
   *
   * @param comp the composite to set
   */
  public void setComposite(Composite comp)
  {
    composite = comp;
    if (! (comp.equals(AlphaComposite.SrcOver)))
      isOptimized = false;
    else
      updateOptimization();
  }

  /**
   * Sets the current foreground.
   *
   * @param p the foreground to set.
   */
  public void setPaint(Paint p)
  {
    if (p != null)
      {
        paint = p;

        if (! (paint instanceof Color))
          isOptimized = false;
        else
          {
            updateOptimization();
            rawSetForeground((Color) paint);
          }
      }
  }

  /**
   * Sets the stroke for this graphics object.
   *
   * @param s the stroke to set
   */
  public void setStroke(Stroke s)
  {
    stroke = s;
    if (! stroke.equals(new BasicStroke()))
      isOptimized = false;
    else
      updateOptimization();
  }

  /**
   * Sets the specified rendering hint.
   *
   * @param hintKey the key of the rendering hint
   * @param hintValue the value
   */
  public void setRenderingHint(Key hintKey, Object hintValue)
  {
    renderingHints.put(hintKey, hintValue);
  }

  /**
   * Returns the rendering hint for the specified key.
   *
   * @param hintKey the rendering hint key
   *
   * @return the rendering hint for the specified key
   */
  public Object getRenderingHint(Key hintKey)
  {
    return renderingHints.get(hintKey);
  }

  /**
   * Sets the specified rendering hints.
   *
   * @param hints the rendering hints to set
   */
  public void setRenderingHints(Map hints)
  {
    renderingHints.clear();
    renderingHints.putAll(hints);
  }

  /**
   * Adds the specified rendering hints.
   *
   * @param hints the rendering hints to add
   */
  public void addRenderingHints(Map hints)
  {
    renderingHints.putAll(hints);
  }

  /**
   * Returns the current rendering hints.
   *
   * @return the current rendering hints
   */
  public RenderingHints getRenderingHints()
  {
    return (RenderingHints) renderingHints.clone();
  }

  /**
   * Translates the coordinate system by (x, y).
   *
   * @param x the translation X coordinate
   * @param y the translation Y coordinate 
   */
  public void translate(int x, int y)
  {
    transform.translate(x, y);

    // Update the clip. We special-case rectangular clips here, because they
    // are so common (e.g. in Swing).
    if (clip != null)
      {
        if (clip instanceof Rectangle)
          {
            Rectangle r = (Rectangle) clip;
            r.x -= x;
            r.y -= y;
            setClip(r);
          }
        else
          {
            AffineTransform clipTransform = new AffineTransform();
            clipTransform.translate(-x, -y);
            updateClip(clipTransform);
          }
      }
  }

  /**
   * Translates the coordinate system by (tx, ty).
   *
   * @param tx the translation X coordinate
   * @param ty the translation Y coordinate 
   */
  public void translate(double tx, double ty)
  {
    transform.translate(tx, ty);

    // Update the clip. We special-case rectangular clips here, because they
    // are so common (e.g. in Swing).
    if (clip != null)
      {
        if (clip instanceof Rectangle)
          {
            Rectangle r = (Rectangle) clip;
            r.x -= tx;
            r.y -= ty;
          }
        else
          {
            AffineTransform clipTransform = new AffineTransform();
            clipTransform.translate(-tx, -ty);
            updateClip(clipTransform);
          }
      }
  }

  /**
   * Rotates the coordinate system by <code>theta</code> degrees.
   *
   * @param theta the angle be which to rotate the coordinate system
   */
  public void rotate(double theta)
  {
    transform.rotate(theta);
    if (clip != null)
      {
        AffineTransform clipTransform = new AffineTransform();
        clipTransform.rotate(-theta);
        updateClip(clipTransform);
      }
    updateOptimization();
  }

  /**
   * Rotates the coordinate system by <code>theta</code> around the point
   * (x, y).
   *
   * @param theta the angle by which to rotate the coordinate system
   * @param x the point around which to rotate, X coordinate
   * @param y the point around which to rotate, Y coordinate
   */
  public void rotate(double theta, double x, double y)
  {
    transform.rotate(theta, x, y);
    if (clip != null)
      {
        AffineTransform clipTransform = new AffineTransform();
        clipTransform.rotate(-theta, x, y);
        updateClip(clipTransform);
      }
    updateOptimization();
  }

  /**
   * Scales the coordinate system by the factors <code>scaleX</code> and
   * <code>scaleY</code>.
   *
   * @param scaleX the factor by which to scale the X axis
   * @param scaleY the factor by which to scale the Y axis
   */
  public void scale(double scaleX, double scaleY)
  {
    transform.scale(scaleX, scaleY);
    if (clip != null)
      {
        AffineTransform clipTransform = new AffineTransform();
        clipTransform.scale(-scaleX, -scaleY);
        updateClip(clipTransform);
      }
    updateOptimization();
  }

  /**
   * Shears the coordinate system by <code>shearX</code> and
   * <code>shearY</code>.
   *
   * @param shearX the X shearing
   * @param shearY the Y shearing
   */
  public void shear(double shearX, double shearY)
  {
    transform.shear(shearX, shearY);
    if (clip != null)
      {
        AffineTransform clipTransform = new AffineTransform();
        clipTransform.shear(-shearX, -shearY);
        updateClip(clipTransform);
      }
    updateOptimization();
  }

  /**
   * Transforms the coordinate system using the specified transform
   * <code>t</code>.
   *
   * @param t the transform
   */
  public void transform(AffineTransform t)
  {
    transform.concatenate(t);
    try
      {
        AffineTransform clipTransform = t.createInverse();
        updateClip(clipTransform);
      }
    catch (NoninvertibleTransformException ex)
      {
        // TODO: How can we deal properly with this?
        ex.printStackTrace();
      }
    updateOptimization();
  }

  /**
   * Sets the transformation for this Graphics object.
   *
   * @param t the transformation to set
   */
  public void setTransform(AffineTransform t)
  {
    // Transform clip into target space using the old transform.
    updateClip(transform);
    transform.setTransform(t);
    // Transform the clip back into user space using the inverse new transform.
    try
      {
        updateClip(transform.createInverse());
      }
    catch (NoninvertibleTransformException ex)
      {
        // TODO: How can we deal properly with this?
        ex.printStackTrace();
      }
    updateOptimization();
  }

  /**
   * Returns the transformation of this coordinate system.
   *
   * @return the transformation of this coordinate system
   */
  public AffineTransform getTransform()
  {
    return (AffineTransform) transform.clone();
  }

  /**
   * Returns the current foreground.
   *
   * @return the current foreground
   */
  public Paint getPaint()
  {
    return paint;
  }


  /**
   * Returns the current composite.
   *
   * @return the current composite
   */
  public Composite getComposite()
  {
    return composite;
  }

  /**
   * Sets the current background.
   *
   * @param color the background to set.
   */
  public void setBackground(Color color)
  {
    background = color;
  }

  /**
   * Returns the current background.
   *
   * @return the current background
   */
  public Color getBackground()
  {
    return background;
  }

  /**
   * Returns the current stroke.
   *
   * @return the current stroke
   */
  public Stroke getStroke()
  {
    return stroke;
  }

  /**
   * Intersects the clip of this graphics object with the specified clip.
   *
   * @param s the clip with which the current clip should be intersected
   */
  public void clip(Shape s)
  {
    // Initialize clip if not already present.
    if (clip == null)
      clip = s;
    
    // This is so common, let's optimize this. 
    else if (clip instanceof Rectangle && s instanceof Rectangle)
      {
        Rectangle clipRect = (Rectangle) clip;
        Rectangle r = (Rectangle) s;
        computeIntersection(r.x, r.y, r.width, r.height, clipRect);
        // Call setClip so that subclasses get notified.
        setClip(clipRect);
      }
   else
     {
       Area current;
       if (clip instanceof Area)
         current = (Area) clip;
       else
         current = new Area(clip);

       Area intersect;
       if (s instanceof Area)
         intersect = (Area) s;
       else
         intersect = new Area(s);

       current.intersect(intersect);
       clip = current;
       isOptimized = false;
       // Call setClip so that subclasses get notified.
       setClip(clip);
     }
  }

  public FontRenderContext getFontRenderContext()
  {
    //return new FontRenderContext(transform, false, false);
    return new FontRenderContext(new AffineTransform(), false, false);
  }

  /**
   * Draws the specified glyph vector at the specified location.
   *
   * @param gv the glyph vector to draw
   * @param x the location, x coordinate
   * @param y the location, y coordinate
   */
  public void drawGlyphVector(GlyphVector gv, float x, float y)
  {
    int numGlyphs = gv.getNumGlyphs();
    AffineTransform t = new AffineTransform();
    t.translate(x, y);

//    // TODO: We could use fill(gv.getOutline()), but that seems to be
      // slightly more inefficient. 
    for (int i = 0; i < numGlyphs; i++)
    {
      //fill(gv.getGlyphVisualBounds(i));
      GeneralPath p = new GeneralPath(gv.getGlyphOutline(i));
      p.transform(t);
      //Shape clipped = clipShape(p);
      //if (clipped != null)
      //  fillShape(clipped, true);
      // FIXME: Clipping doesn't seem to work correctly.
      fillShape(p, true);
    }
  }

  /**
   * Creates a copy of this graphics object.
   *
   * @return a copy of this graphics object
   */
  public Graphics create()
  {
    AbstractGraphics2D copy = (AbstractGraphics2D) clone();
    return copy;
  }

  /**
   * Creates and returns a copy of this Graphics object. This should
   * be overridden by subclasses if additional state must be handled when
   * cloning. This is called by {@link #create()}.
   *
   * @return a copy of this Graphics object
   */
  protected Object clone()
  {
    try
      {
        AbstractGraphics2D copy = (AbstractGraphics2D) super.clone();
        // Copy the clip. If it's a Rectangle, preserve that for optimization.
        if (clip instanceof Rectangle)
          copy.clip = new Rectangle((Rectangle) clip);
        else
          copy.clip = new GeneralPath(clip);

        copy.renderingHints = new RenderingHints(renderingHints);
        copy.transform = new AffineTransform(transform);
        // The remaining state is inmmutable and doesn't need to be copied.
        return copy;
      }
    catch (CloneNotSupportedException ex)
      {
        AWTError err = new AWTError("Unexpected exception while cloning");
        err.initCause(ex);
        throw err;
      }
  }

  /**
   * Returns the current foreground.
   */
  public Color getColor()
  {
    Color c = null;
    if (paint instanceof Color)
      c = (Color) paint;
    return c;
  }

  /**
   * Sets the current foreground.
   *
   * @param color the foreground to set
   */
  public void setColor(Color color)
  {
    setPaint(color);
  }

  public void setPaintMode()
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public void setXORMode(Color color)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  /**
   * Returns the current font.
   *
   * @return the current font
   */
  public Font getFont()
  {
    return font;
  }

  /**
   * Sets the font on this graphics object. When <code>f == null</code>, the
   * current setting is not changed.
   *
   * @param f the font to set
   */
  public void setFont(Font f)
  {
    if (f != null)
      font = f;
  }

  /**
   * Returns the font metrics for the specified font.
   *
   * @param font the font for which to fetch the font metrics
   *
   * @return the font metrics for the specified font
   */
  public FontMetrics getFontMetrics(Font font)
  {
    return Toolkit.getDefaultToolkit().getFontMetrics(font);
  }

  /**
   * Returns the bounds of the current clip.
   *
   * @return the bounds of the current clip
   */
  public Rectangle getClipBounds()
  {
    Rectangle b = null;
    if (clip != null)
      b = clip.getBounds();
    return b;
  }

  /**
   * Intersects the current clipping region with the specified rectangle.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param width the width of the rectangle
   * @param height the height of the rectangle
   */
  public void clipRect(int x, int y, int width, int height)
  {
    clip(new Rectangle(x, y, width, height));
  }

  /**
   * Sets the clip to the specified rectangle.
   *
   * @param x the x coordinate of the clip rectangle
   * @param y the y coordinate of the clip rectangle
   * @param width the width of the clip rectangle
   * @param height the height of the clip rectangle
   */
  public void setClip(int x, int y, int width, int height)
  {
    setClip(new Rectangle(x, y, width, height));
  }

  /**
   * Returns the current clip.
   *
   * @return the current clip
   */
  public Shape getClip()
  {
    return clip;
  }

  /**
   * Sets the current clipping area to <code>clip</code>.
   *
   * @param c the clip to set
   */
  public void setClip(Shape c)
  {
    clip = c;
    if (! (clip instanceof Rectangle))
      isOptimized = false;
    else
      updateOptimization();
  }

  public void copyArea(int x, int y, int width, int height, int dx, int dy)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  /**
   * Draws a line from (x1, y1) to (x2, y2).
   *
   * This implementation transforms the coordinates and forwards the call to
   * {@link #rawDrawLine}.
   */
  public void drawLine(int x1, int y1, int x2, int y2)
  {
    if (isOptimized)
      {
        int tx = (int) transform.getTranslateX();
        int ty = (int) transform.getTranslateY();
        rawDrawLine(x1 + tx, y1 + ty, x2 + tx, y2 + ty);
      }
    else
      {
        Line2D line = new Line2D.Double(x1, y1, x2, y2);
        draw(line);
      }
  }

  /**
   * Fills a rectangle with the current paint.
   *
   * @param x the upper left corner, X coordinate
   * @param y the upper left corner, Y coordinate
   * @param width the width of the rectangle
   * @param height the height of the rectangle
   */
  public void fillRect(int x, int y, int width, int height)
  {
    if (isOptimized)
      {
        int tx = (int) transform.getTranslateX();
        int ty = (int) transform.getTranslateY();
        rawFillRect(x + tx, y + ty, width, height);
      }
    else
      {
        fill(new Rectangle(x, y, width, height));
      }
  }

  /**
   * Fills a rectangle with the current background color.
   *
   * This implementation temporarily sets the foreground color to the 
   * background and forwards the call to {@link #fillRect(int, int, int, int)}.
   *
   * @param x the upper left corner, X coordinate
   * @param y the upper left corner, Y coordinate
   * @param width the width of the rectangle
   * @param height the height of the rectangle
   */
  public void clearRect(int x, int y, int width, int height)
  {
    Paint savedForeground = getPaint();
    setPaint(getBackground());
    //System.err.println("clearRect transform type: " + transform.getType());
    fillRect(x, y, width, height);
    setPaint(savedForeground);
  }

  /**
   * Draws a rounded rectangle.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param width the width of the rectangle
   * @param height the height of the rectangle
   * @param arcWidth the width of the arcs
   * @param arcHeight the height of the arcs
   */
  public void drawRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    draw(new RoundRectangle2D.Double(x, y, width, height, arcWidth,
                                     arcHeight));
  }

  /**
   * Fills a rounded rectangle.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param width the width of the rectangle
   * @param height the height of the rectangle
   * @param arcWidth the width of the arcs
   * @param arcHeight the height of the arcs
   */
  public void fillRoundRect(int x, int y, int width, int height, int arcWidth,
                            int arcHeight)
  {
    fill(new RoundRectangle2D.Double(x, y, width, height, arcWidth,
                                     arcHeight));
  }

  /**
   * Draws the outline of an oval.
   *
   * @param x the upper left corner of the bounding rectangle of the ellipse
   * @param y the upper left corner of the bounding rectangle of the ellipse
   * @param width the width of the ellipse
   * @param height the height of the ellipse
   */
  public void drawOval(int x, int y, int width, int height)
  {
    draw(new Ellipse2D.Double(x, y, width, height));
  }

  /**
   * Fills an oval.
   *
   * @param x the upper left corner of the bounding rectangle of the ellipse
   * @param y the upper left corner of the bounding rectangle of the ellipse
   * @param width the width of the ellipse
   * @param height the height of the ellipse
   */
  public void fillOval(int x, int y, int width, int height)
  {
    fill(new Ellipse2D.Double(x, y, width, height));
  }

  /**
   * Draws an arc.
   */
  public void drawArc(int x, int y, int width, int height, int arcStart,
                      int arcAngle)
  {
    draw(new Arc2D.Double(x, y, width, height, arcStart, arcAngle,
                          Arc2D.OPEN));
  }

  /**
   * Fills an arc.
   */
  public void fillArc(int x, int y, int width, int height, int arcStart,
                      int arcAngle)
  {
    fill(new Arc2D.Double(x, y, width, height, arcStart, arcAngle,
                          Arc2D.OPEN));
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int npoints)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  /**
   * Draws the outline of a polygon.
   */
  public void drawPolygon(int[] xPoints, int[] yPoints, int npoints)
  {
    draw(new Polygon(xPoints, yPoints, npoints));
  }

  /**
   * Fills the outline of a polygon.
   */
  public void fillPolygon(int[] xPoints, int[] yPoints, int npoints)
  {
    fill(new Polygon(xPoints, yPoints, npoints));
  }

  public boolean drawImage(Image image, int x, int y, ImageObserver observer)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public boolean drawImage(Image image, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public boolean drawImage(Image image, int x, int y, Color bgcolor,
                           ImageObserver observer)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public boolean drawImage(Image image, int x, int y, int width, int height,
                           Color bgcolor, ImageObserver observer)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public boolean drawImage(Image image, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           ImageObserver observer)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public boolean drawImage(Image image, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2, Color bgcolor,
                           ImageObserver observer)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  /**
   * Disposes this graphics object.
   */
  public void dispose()
  {
    // Nothing special to do here.
  }

  /**
   * Fills the specified shape. The shape has already been clipped against the
   * current clip.
   *
   * @param s the shape to fill
   * @param isFont <code>true</code> if the shape is a font outline
   */
  protected void fillShape(Shape s, boolean isFont)
  {
    // Determine if we need to antialias stuff.
    boolean antialias = false;
    if (isFont)
      {
        Object v = renderingHints.get(RenderingHints.KEY_TEXT_ANTIALIASING);
        // We default to antialiasing on for text as long as we have no
        // good hinting implemented.
        antialias = (v == RenderingHints.VALUE_TEXT_ANTIALIAS_ON
                     || v == RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT);
      }
    else
      {
        Object v = renderingHints.get(RenderingHints.KEY_ANTIALIASING);
        antialias = (v == RenderingHints.VALUE_ANTIALIAS_ON);
      }

    Rectangle2D userBounds = s.getBounds2D();

    // Flatten the path. TODO: Determine the best flattening factor
    // wrt to speed and quality.
    PathIterator path = s.getPathIterator(getTransform(), 1.0);

    // Build up polygons and let the native backend render this using
    // rawFillShape() which would provide a default implementation for
    // drawPixel using a PolyScan algorithm.
    double[] seg = new double[6];

    // TODO: Use ArrayList<PolyEdge> here when availble.
    ArrayList segs = new ArrayList();
    double segX = 0.; // The start point of the current edge.
    double segY = 0.; 
    double polyX = 0.; // The start point of the current polygon.
    double polyY = 0.;

    double minX = Integer.MAX_VALUE;
    double maxX = Integer.MIN_VALUE;
    double minY = Integer.MAX_VALUE;
    double maxY = Integer.MIN_VALUE;

    //System.err.println("fill polygon");
    while (! path.isDone())
      {
        int segType = path.currentSegment(seg);
        minX = Math.min(minX, seg[0]);
        maxX = Math.max(maxX, seg[0]);
        minY = Math.min(minY, seg[1]);
        maxY = Math.max(maxY, seg[1]);

        //System.err.println("segment: " + segType + ", " + seg[0] + ", " + seg[1]);
        if (segType == PathIterator.SEG_MOVETO)
          {
            segX = seg[0];
            segY = seg[1];
            polyX = seg[0];
            polyY = seg[1];
          }
        else if (segType == PathIterator.SEG_CLOSE)
          {
            // Close the polyline.
            PolyEdge edge = new PolyEdge(segX, segY, polyX, polyY);
            segs.add(edge);
          }
        else if (segType == PathIterator.SEG_LINETO)
          {
            PolyEdge edge = new PolyEdge(segX, segY, seg[0], seg[1]);
            segs.add(edge);
            segX = seg[0];
            segY = seg[1];
          }
        path.next();
      }
    if (segs.size() > 0)
      {
        if (antialias)
          fillShapeAntialias(segs, minX, minY, maxX, maxY, userBounds);
        else
          rawFillShape(segs, minX, minY, maxX, maxY, userBounds);
      }
  }

  /**
   * Draws one pixel in the target coordinate space. This method draws the
   * specified pixel by getting the painting pixel for that coordinate
   * from the paintContext and compositing the pixel with the compositeContext.
   * The resulting pixel is then set by calling {@link #rawSetPixel}.
   *
   * @param x the x coordinate
   * @param y the y coordinate
   */
  protected void drawPixel(int x, int y)
  {
    // FIXME: Implement efficient compositing.
    if (! (paint instanceof Color))
      {
        int[] paintPixel = paintRaster.getPixel(x, y, pixel);
        Color c = new Color(paintPixel[0], paintPixel[1], paintPixel[2]);
        rawSetForeground(c);
      }
    rawSetPixel(x, y);
  }

  /**
   * Draws a pixel in the target coordinate space using the specified color.
   * 
   * @param x the x coordinate
   * @param y the y coordinate
   */
  protected void rawSetPixel(int x, int y)
  {
    // FIXME: Provide default implementation or remove method.
  }

  /**
   * Sets the foreground color for drawing.
   *
   * @param c the color to set
   */
  protected void rawSetForeground(Color c)
  {
    // Probably remove method.
  }

  protected void rawSetForeground(int r, int g, int b)
  {
    rawSetForeground(new Color(r, g, b));
  }

  /**
   * Returns the color model of this Graphics object.
   *
   * @return the color model of this Graphics object
   */
  protected abstract ColorModel getColorModel();

  /**
   * Returns the bounds of the target.
   *
   * @return the bounds of the target
   */
  protected Rectangle getDeviceBounds()
  {
    return destinationRaster.getBounds();
  }

  /**
   * Returns the bounds of the drawing area in user space.
   *
   * @return the bounds of the drawing area in user space
   */
  protected Rectangle2D getUserBounds()
  {
    PathIterator pathIter = getDeviceBounds().getPathIterator(getTransform());
    GeneralPath path = new GeneralPath();
    path.append(pathIter, true);
    return path.getBounds();

  }
  /**
   * Draws a line in optimization mode. The implementation should respect the
   * clip but can assume that it is a rectangle.
   *
   * @param x0 the starting point, X coordinate
   * @param y0 the starting point, Y coordinate
   * @param x1 the end point, X coordinate 
   * @param y1 the end point, Y coordinate
   */
  protected void rawDrawLine(int x0, int y0, int x1, int y1)
  {
    // This is an implementation of Bresenham's line drawing algorithm.
    int dy = y1 - y0;
    int dx = x1 - x0;
    int stepx, stepy;

    if (dy < 0)
      {
        dy = -dy;
        stepy = -1;
      }
    else
      {
        stepy = 1;
      }
    if (dx < 0)
      {
        dx = -dx;
        stepx = -1;
        }
    else
      {
        stepx = 1;
      }
    dy <<= 1;
    dx <<= 1;

    drawPixel(x0, y0);
    if (dx > dy)
      {
        int fraction = dy - (dx >> 1); // same as 2*dy - dx
        while (x0 != x1)
          {
            if (fraction >= 0)
              {
                y0 += stepy;
                fraction -= dx;
              }
            x0 += stepx;
            fraction += dy;
            drawPixel(x0, y0);
          }
      }
    else
      {
        int fraction = dx - (dy >> 1);
        while (y0 != y1)
          {
            if (fraction >= 0)
              {
                x0 += stepx;
                fraction -= dy;
              }
            y0 += stepy;
            fraction += dx;
            drawPixel(x0, y0);
          }
      }
  }

  /**
   * Fills a rectangle in optimization mode. The implementation should respect
   * the clip but can assume that it is a rectangle.
   *
   * @param x the upper left corner, X coordinate
   * @param y the upper left corner, Y coordinate
   * @param w the width
   * @param h the height
   */
  protected void rawFillRect(int x, int y, int w, int h)
  {
    int x2 = x + w;
    int y2 = y + h;
    for (int xc = x; xc < x2; xc++)
      {
        for (int yc = y; yc < y2; yc++)
          {
            drawPixel(xc, yc);
          }
      }
  }

  /**
   * Fills the specified polygon. This should be overridden by backends
   * that support accelerated (native) polygon filling, which is the
   * case for most toolkit window and offscreen image implementations.
   *
   * The polygon is already clipped when this method is called.
   */
  protected void rawFillShape(ArrayList segs, double minX, double minY,
                              double maxX, double maxY, Rectangle2D userBounds)
  {
    // This is an implementation of a polygon scanline conversion algorithm
    // described here:
    // http://www.cs.berkeley.edu/~ug/slide/pipeline/assignments/scan/

    // Create table of all edges.
    // The edge buckets, sorted and indexed by their Y values.

    Rectangle deviceBounds = new Rectangle((int) minX, (int) minY,
                                           (int) Math.ceil(maxX) - (int) minX,
                                           (int) Math.ceil(maxY) - (int) minY);
    PaintContext pCtx = paint.createContext(getColorModel(), deviceBounds,
                                            userBounds, transform, renderingHints);

    ArrayList[] edgeTable = new ArrayList[(int) Math.ceil(maxY)
                                          - (int) Math.ceil(minY) + 1];

    for (Iterator i = segs.iterator(); i.hasNext();)
      {
        PolyEdge edge = (PolyEdge) i.next();
        int yindex = (int) ((int) Math.ceil(edge.y0) - (int) Math.ceil(minY));
        if (edgeTable[yindex] == null) // Create bucket when needed.
          edgeTable[yindex] = new ArrayList();
        edgeTable[yindex].add(edge); // Add edge to the bucket of its line.
      }

    // TODO: The following could be useful for a future optimization.
//    // Sort all the edges in the edge table within their buckets.
//    for (int y = 0; y < edgeTable.length; y++)
//      {
//        if (edgeTable[y] != null)
//          Collections.sort(edgeTable[y]);
//      }

    // The activeEdges list contains all the edges of the current scanline
    // ordered by their intersection points with this scanline.
    ArrayList activeEdges = new ArrayList();
    PolyEdgeComparator comparator = new PolyEdgeComparator();

    // Scan all relevant lines.
    int minYInt = (int) Math.ceil(minY);
    for (int y = minYInt; y <= maxY; y++)
      {
        ArrayList bucket = edgeTable[y - minYInt];
        // Update all the x intersections in the current activeEdges table
        // and remove entries that are no longer in the scanline.
        for (Iterator i = activeEdges.iterator(); i.hasNext();)
          {
            PolyEdge edge = (PolyEdge) i.next();
            if (y > edge.y1)
              i.remove();
            else
              {
                edge.xIntersection += edge.slope;
                //edge.xIntersection = edge.x0 + edge.slope * (y - edge.y0);
                //System.err.println("edge.xIntersection: " + edge.xIntersection);
              }
          }

        if (bucket != null)
          activeEdges.addAll(bucket);

        // Sort current edges. We are using a bubble sort, because the order
        // of the intersections will not change in most situations. They
        // will only change, when edges intersect each other.
        int size = activeEdges.size();
        if (size > 1)
          {
            for (int i = 1; i < size; i++)
              {
                PolyEdge e1 = (PolyEdge) activeEdges.get(i - 1);
                PolyEdge e2 = (PolyEdge) activeEdges.get(i);
                if (comparator.compare(e1, e2) > 0)
                  {
                    // Swap e2 with its left neighbor until it 'fits'.
                    int j = i;
                    do
                      {
                        activeEdges.set(j, e1);
                        activeEdges.set(j - 1, e2);
                        j--;
                        if (j >= 1)
                          e1 = (PolyEdge) activeEdges.get(j - 1);
                      } while (j >= 1 && comparator.compare(e1, e2) > 0);
                  }
              }
          }

        // Now draw all pixels inside the polygon.
        // This is the last edge that intersected the scanline.
        PolyEdge previous = null; // Gets initialized below.
        boolean active = false;
        //System.err.println("scanline: " + y);
        for (Iterator i = activeEdges.iterator(); i.hasNext();)
          {
            PolyEdge edge = (PolyEdge) i.next();
            // Only fill scanline, if the current edge actually intersects
            // the scanline. There may be edges that lie completely
            // within the current scanline.
            //System.err.println("previous: " + previous);
            //System.err.println("edge: " + edge);
            if (active)
              {
                if (edge.y1 > y)
                  {
                    int x0 = (int) previous.xIntersection;
                    int x1 = (int) edge.xIntersection;
                    fillScanline(pCtx, x0, x1, y);
                    previous = edge;
                    active = false;
                  }
              }
            else
              {
                if (edge.y1 > y)
                  {
                    previous = edge;
                    active = true;
                  }
              }
          }
      }
    pCtx.dispose();
  }

  /**
   * Paints a scanline between x0 and x1.
   *
   * @param x0 the left offset
   * @param x1 the right offset
   * @param y the scanline
   */
  protected void fillScanline(PaintContext pCtx, int x0, int x1, int y)
  {
    Raster paintRaster = pCtx.getRaster(x0, y, x1 - x0, 1);
    ColorModel paintColorModel = pCtx.getColorModel();
    CompositeContext cCtx = composite.createContext(paintColorModel,
                                                    getColorModel(),
                                                    renderingHints);
    cCtx.compose(paintRaster, destinationRaster, destinationRaster);
    updateRaster(destinationRaster, x0, y, x1 - x0, 1);
    cCtx.dispose();
  }

  /**
   * Fills arbitrary shapes in an anti-aliased fashion.
   *
   * @param segs the line segments which define the shape which is to be filled
   * @param minX the bounding box, left X
   * @param minY the bounding box, upper Y
   * @param maxX the bounding box, right X
   * @param maxY the bounding box, lower Y
   */
  private void fillShapeAntialias(ArrayList segs, double minX, double minY,
                                  double maxX, double maxY,
                                  Rectangle2D userBounds)
  {
    // This is an implementation of a polygon scanline conversion algorithm
    // described here:
    // http://www.cs.berkeley.edu/~ug/slide/pipeline/assignments/scan/
    // The antialiasing is implemented using a sampling technique, we do
    // not scan whole lines but fractions of the line.

    Rectangle deviceBounds = new Rectangle((int) minX, (int) minY,
                                           (int) Math.ceil(maxX) - (int) minX,
                                           (int) Math.ceil(maxY) - (int) minY);
    PaintContext pCtx = paint.createContext(getColorModel(), deviceBounds,
                                            userBounds, transform,
                                            renderingHints);

    // This array will contain the oversampled transparency values for
    // each pixel in the scanline.
    int numScanlines = (int) Math.ceil(maxY) - (int) minY;
    int numScanlinePixels = (int) Math.ceil(maxX) - (int) minX + 1;
    if (alpha == null || alpha.length < (numScanlinePixels + 1))
      alpha = new int[numScanlinePixels + 1];
    
    int firstLine = (int) minY;
    //System.err.println("minY: " + minY);
    int firstSubline = (int) (Math.ceil((minY - Math.floor(minY)) * AA_SAMPLING));
    double firstLineDouble = firstLine + firstSubline / (double) AA_SAMPLING;
    //System.err.println("firstSubline: " + firstSubline);

    // Create table of all edges.
    // The edge buckets, sorted and indexed by their Y values.
    //System.err.println("numScanlines: " + numScanlines);
    if (edgeTable == null
        || edgeTable.length < numScanlines * AA_SAMPLING + AA_SAMPLING)
      edgeTable = new ArrayList[numScanlines * AA_SAMPLING + AA_SAMPLING];

    //System.err.println("firstLineDouble: " + firstLineDouble);
    
    for (Iterator i = segs.iterator(); i.hasNext();)
      {
        PolyEdge edge = (PolyEdge) i.next();
        int yindex = (int) (Math.ceil((edge.y0 - firstLineDouble) * AA_SAMPLING));
        //System.err.println("yindex: " + yindex + " for y0: " + edge.y0);
        // Initialize edge's slope and initial xIntersection.
        edge.slope = ((edge.x1 - edge.x0) / (edge.y1 - edge.y0)) / AA_SAMPLING;
        if (edge.y0 == edge.y1) // Horizontal edge.
          edge.xIntersection = Math.min(edge.x0, edge.x1);
        else
          {
            double alignedFirst = Math.ceil(edge.y0 * AA_SAMPLING) / AA_SAMPLING;
            edge.xIntersection = edge.x0 + (edge.slope * AA_SAMPLING) * (alignedFirst - edge.y0);
          }
        //System.err.println(edge);
        // FIXME: Sanity check should not be needed when clipping works.
        if (yindex >= 0 && yindex < edgeTable.length)
          {
            if (edgeTable[yindex] == null) // Create bucket when needed.
              edgeTable[yindex] = new ArrayList();
            edgeTable[yindex].add(edge); // Add edge to the bucket of its line.
          }
      }
    
    // The activeEdges list contains all the edges of the current scanline
    // ordered by their intersection points with this scanline.
    ArrayList activeEdges = new ArrayList();
    PolyEdgeComparator comparator = new PolyEdgeComparator();
    
    // Scan all lines.
    int yindex = 0;
    //System.err.println("firstLine: " + firstLine + ", maxY: " + maxY + ", firstSubline: " + firstSubline);
    for (int y = firstLine; y <= maxY; y++)
      {
        for (int subY = firstSubline; subY < AA_SAMPLING; subY++)
          {
            //System.err.println("scanline: " + y + ", subScanline: " + subY);
            ArrayList bucket = edgeTable[yindex];
            // Update all the x intersections in the current activeEdges table
            // and remove entries that are no longer in the scanline.
            for (Iterator i = activeEdges.iterator(); i.hasNext();)
              {
                PolyEdge edge = (PolyEdge) i.next();
                // TODO: Do the following using integer arithmetics.
                if ((y + ((double) subY / (double) AA_SAMPLING)) > edge.y1)
                  i.remove();
                else
                  {
                    edge.xIntersection += edge.slope;
                    //System.err.println("edge: " + edge);
                    //edge.xIntersection = edge.x0 + edge.slope * (y - edge.y0);
                    //System.err.println("edge.xIntersection: " + edge.xIntersection);
                  }
              }

            if (bucket != null)
              {
                activeEdges.addAll(bucket);
                edgeTable[yindex].clear();
              }

            // Sort current edges. We are using a bubble sort, because the order
            // of the intersections will not change in most situations. They
            // will only change, when edges intersect each other.
            int size = activeEdges.size();
            if (size > 1)
              {
                for (int i = 1; i < size; i++)
                  {
                    PolyEdge e1 = (PolyEdge) activeEdges.get(i - 1);
                    PolyEdge e2 = (PolyEdge) activeEdges.get(i);
                    if (comparator.compare(e1, e2) > 0)
                      {
                        // Swap e2 with its left neighbor until it 'fits'.
                        int j = i;
                        do
                          {
                            activeEdges.set(j, e1);
                            activeEdges.set(j - 1, e2);
                            j--;
                            if (j >= 1)
                              e1 = (PolyEdge) activeEdges.get(j - 1);
                          } while (j >= 1 && comparator.compare(e1, e2) > 0);
                      }
                  }
              }
        
            // Now draw all pixels inside the polygon.
            // This is the last edge that intersected the scanline.
            PolyEdge previous = null; // Gets initialized below.
            boolean active = false;
            //System.err.println("scanline: " + y + ", subscanline: " + subY);
            for (Iterator i = activeEdges.iterator(); i.hasNext();)
              {
                PolyEdge edge = (PolyEdge) i.next();
                // Only fill scanline, if the current edge actually intersects
                // the scanline. There may be edges that lie completely
                // within the current scanline.
                //System.err.println("previous: " + previous);
                //System.err.println("edge: " + edge);
                if (active)
                  {
                    // TODO: Use integer arithmetics here.
                    if (edge.y1 > (y + (subY / (double) AA_SAMPLING)))
                      {
                        //System.err.println(edge);
                        // TODO: Eliminate the aligments.
                        int x0 = (int) Math.min(Math.max(previous.xIntersection, minX), maxX);
                        int x1 = (int) Math.min(Math.max(edge.xIntersection, minX), maxX);
                        //System.err.println("minX: " + minX + ", x0: " + x0 + ", x1: " + x1 + ", maxX: " + maxX);
                        // TODO: Pull out cast.
                        alpha[x0 - (int) minX]++;
                        alpha[x1 - (int) minX + 1]--;
                        previous = edge;
                        active = false;
                      }
                  }
                else
                  {
                    // TODO: Use integer arithmetics here.
                    if (edge.y1 > (y + (subY / (double) AA_SAMPLING)))
                      {
                        //System.err.println(edge);
                        previous = edge;
                        active = true;
                      }
                  }
              }
            yindex++;
          }
        firstSubline = 0;
        // Render full scanline.
        //System.err.println("scanline: " + y);
        fillScanlineAA(alpha, (int) minX, (int) y, numScanlinePixels, pCtx);
      }
    if (paint instanceof Color && composite == AlphaComposite.SrcOver)
      rawSetForeground((Color) paint);

    pCtx.dispose();
  }

  /**
   * Fills a horizontal line between x0 and x1 for anti aliased rendering.
   * the alpha array contains the deltas of the alpha values from one pixel
   * to the next.
   *
   * @param alpha the alpha values in the scanline
   * @param x0 the beginning of the scanline
   * @param y the y coordinate of the line
   */
  private void fillScanlineAA(int[] alpha, int x0, int y, int numScanlinePixels,
                              PaintContext pCtx)
  {
    // FIXME: This doesn't work. Fixit.
    CompositeContext cCtx = composite.createContext(pCtx.getColorModel(),
                                                    getColorModel(),
                                                    renderingHints);
    Raster paintRaster = pCtx.getRaster(x0, y, numScanlinePixels, 1);
    System.err.println("paintColorModel: " + pCtx.getColorModel());
    WritableRaster aaRaster = paintRaster.createCompatibleWritableRaster();
    int numBands = paintRaster.getNumBands();
    int[] pixels = new int[numScanlinePixels + paintRaster.getNumBands()];
    pixels = paintRaster.getPixels(x0, y, numScanlinePixels, 1, pixels);
    ColorModel cm = pCtx.getColorModel();
    
    double lastAlpha = 0.;
    int lastAlphaInt = 0;
    int[] components = new int[4];
    
    for (int i = 0; i < pixels.length; i++)
      {
        if (alpha[i] != 0)
          {
            lastAlphaInt += alpha[i];
            lastAlpha = lastAlphaInt / AA_SAMPLING;
          }
        components = cm.getComponents(pixel[i], components, 0);
        components[0] = (int) (components[0] * lastAlpha);
        pixel[i] = cm.getDataElement(components, 0);
      }

    aaRaster.setPixels(0, 0, numScanlinePixels, 1, pixels);
    cCtx.compose(aaRaster, destinationRaster, destinationRaster);
    updateRaster(destinationRaster, x0, y, numScanlinePixels, 1);

    cCtx.dispose();
  }


  /**
   * Initializes this graphics object. This must be called by subclasses in
   * order to correctly initialize the state of this object.
   */
  protected void init()
  {
    setPaint(Color.BLACK);
    setFont(new Font("SansSerif", Font.PLAIN, 12));
    isOptimized = true;

    // FIXME: Should not be necessary. A clip of null should mean
    // 'clip against device bounds.
    clip = getDeviceBounds();
    destinationRaster = getDestinationRaster();
  }

  /**
   * Returns a WritableRaster that is used by this class to perform the
   * rendering in. It is not necessary that the target surface immediately
   * reflects changes in the raster. Updates to the raster are notified via
   * {@link #updateRaster}.
   *
   * @return the destination raster
   */
  protected abstract WritableRaster getDestinationRaster();

  /**
   * Notifies the backend that the raster has changed in the specified
   * rectangular area. The raster that is provided in this method is always
   * the same as the one returned in {@link #getDestinationRaster}.
   * Backends that reflect changes to this raster directly don't need to do
   * anything here.
   *
   * @param raster the updated raster, identical to the raster returned
   *        by {@link #getDestinationRaster()}
   * @param x the upper left corner of the updated region, X coordinate
   * @param y the upper lef corner of the updated region, Y coordinate
   * @param w the width of the updated region
   * @param h the height of the updated region
   */
  protected void updateRaster(Raster raster, int x, int y, int w, int h)
  {
    // Nothing to do here. Backends that need to update their surface
    // to reflect the change should override this method.
  }

  // Some helper methods.

  /**
   * Helper method to check and update the optimization conditions.
   */
  private void updateOptimization()
  {
    int transformType = transform.getType();
    boolean optimizedTransform = false;
    if (transformType == AffineTransform.TYPE_IDENTITY
        || transformType == AffineTransform.TYPE_TRANSLATION)
      optimizedTransform = true;

    boolean optimizedClip = (clip == null || clip instanceof Rectangle);
    isOptimized = optimizedClip
                  && optimizedTransform && paint instanceof Color
                  && composite == AlphaComposite.SrcOver
                  && stroke.equals(new BasicStroke());
  }

  /**
   * Calculates the intersection of two rectangles. The result is stored
   * in <code>rect</code>. This is basically the same
   * like {@link Rectangle#intersection(Rectangle)}, only that it does not
   * create new Rectangle instances. The tradeoff is that you loose any data in
   * <code>rect</code>.
   *
   * @param x upper-left x coodinate of first rectangle
   * @param y upper-left y coodinate of first rectangle
   * @param w width of first rectangle
   * @param h height of first rectangle
   * @param rect a Rectangle object of the second rectangle
   *
   * @throws NullPointerException if rect is null
   *
   * @return a rectangle corresponding to the intersection of the
   *         two rectangles. An empty rectangle is returned if the rectangles
   *         do not overlap
   */
  private static Rectangle computeIntersection(int x, int y, int w, int h,
                                               Rectangle rect)
  {
    int x2 = (int) rect.x;
    int y2 = (int) rect.y;
    int w2 = (int) rect.width;
    int h2 = (int) rect.height;

    int dx = (x > x2) ? x : x2;
    int dy = (y > y2) ? y : y2;
    int dw = (x + w < x2 + w2) ? (x + w - dx) : (x2 + w2 - dx);
    int dh = (y + h < y2 + h2) ? (y + h - dy) : (y2 + h2 - dy);

    if (dw >= 0 && dh >= 0)
      rect.setBounds(dx, dy, dw, dh);
    else
      rect.setBounds(0, 0, 0, 0);

    return rect;
  }

  /**
   * Helper method to transform the clip. This is called by the various
   * transformation-manipulation methods to update the clip (which is in
   * userspace) accordingly.
   *
   * The transform usually is the inverse transform that was applied to the
   * graphics object.
   *
   * @param t the transform to apply to the clip
   */
  private void updateClip(AffineTransform t)
  {
    if (! (clip instanceof GeneralPath))
      clip = new GeneralPath(clip);

    GeneralPath p = (GeneralPath) clip;
    p.transform(t);
  }

  /**
   * Clips the specified shape using the current clip. If the resulting shape
   * is empty, this will return <code>null</code>.
   *
   * @param s the shape to clip
   *
   * @return the clipped shape or <code>null</code> if the result is empty
   */
  private Shape clipShape(Shape s)
  {
    Shape clipped = null;

    // Clip the shape if necessary.
    if (clip != null)
      {
        Area a;
        if (! (s instanceof Area))
          a = new Area(s);
        else
          a = (Area) s;

        Area clipArea;
        if (! (clip instanceof Area))
          clipArea = new Area(clip);
        else
          clipArea = (Area) clip;

        a.intersect(clipArea);
        if (! a.isEmpty())
          clipped = a;
      }
    else
      {
        clipped = s;
      }
    return clipped;
  }
}
