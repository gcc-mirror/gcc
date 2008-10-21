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

import gnu.java.util.LRUCache;

import java.awt.AWTError;
import java.awt.AlphaComposite;
import java.awt.AWTPermission;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.CompositeContext;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Paint;
import java.awt.PaintContext;
import java.awt.Point;
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
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.ReplicateScaleFilter;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.WeakHashMap;

/**
 * This is a 100% Java implementation of the Java2D rendering pipeline. It is
 * meant as a base class for Graphics2D implementations.
 *
 * <h2>Backend interface</h2>
 * <p>
 * The backend must at the very least provide a Raster which the the rendering
 * pipeline can paint into. This must be implemented in
 * {@link #getDestinationRaster()}. For some backends that might be enough, like
 * when the target surface can be directly access via the raster (like in
 * BufferedImages). Other targets need some way to synchronize the raster with
 * the surface, which can be achieved by implementing the
 * {@link #updateRaster(Raster, int, int, int, int)} method, which always gets
 * called after a chunk of data got painted into the raster.
 * </p>
 * <p>Alternativly the backend can provide a method for filling Shapes by
 * overriding the protected method fillShape(). This can be accomplished
 * by a polygon filling function of the backend. Keep in mind though that
 * Shapes can be quite complex (i.e. non-convex and containing holes, etc)
 * which is not supported by all polygon fillers. Also it must be noted
 * that fillShape() is expected to handle painting and compositing as well as
 * clipping and transformation. If your backend can't support this natively,
 * then you can fallback to the implementation in this class. You'll need
 * to provide a writable Raster then, see above.</p>
 * <p>Another alternative is to implement fillScanline() which only requires
 * the backend to be able to draw horizontal lines in device space,
 * which is usually very cheap.
 * The implementation should still handle painting and compositing,
 * but no more clipping and transformation is required by the backend.</p>
 * <p>The backend is free to provide implementations for the various raw*
 * methods for optimized AWT 1.1 style painting of some primitives. This should
 * accelerate painting of Swing greatly. When doing so, the backend must also
 * keep track of the clip and translation, probably by overriding
 * some clip and translate methods. Don't forget to message super in such a
 * case.</p>
 *
 * <h2>Acceleration options</h2>
 * <p>
 * The fact that it is
 * pure Java makes it a little slow. However, there are several ways of
 * accelerating the rendering pipeline:
 * <ol>
 * <li><em>Optimization hooks for AWT 1.1 - like graphics operations.</em>
 *   The most important methods from the {@link java.awt.Graphics} class
 *   have a corresponding <code>raw*</code> method, which get called when
 *   several optimization conditions are fullfilled. These conditions are
 *   described below. Subclasses can override these methods and delegate
 *   it directly to a native backend.</li>
 * <li><em>Native PaintContexts and CompositeContext.</em> The implementations
 *   for the 3 PaintContexts and AlphaCompositeContext can be accelerated
 *   using native code. These have proved to two of the most performance
 *   critical points in the rendering pipeline and cannot really be done quickly
 *   in plain Java because they involve lots of shuffling around with large
 *   arrays. In fact, you really would want to let the graphics card to the
 *   work, they are made for this.</li>
 * <li>Provide an accelerated implementation for fillShape(). For instance,
 * OpenGL can fill shapes very efficiently. There are some considerations
 * to be made though, see above for details.</li>
 * </ol>
 * </p>
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public abstract class AbstractGraphics2D
  extends Graphics2D
  implements Cloneable, Pixelizer
{
  /**
   * Caches scaled versions of an image.
   *
   * @see #drawImage(Image, int, int, int, int, ImageObserver)
   */
  protected static final WeakHashMap<Image, HashMap<Dimension,Image>> imageCache =
    new WeakHashMap<Image, HashMap<Dimension, Image>>();
  
  /**
   * Wether we use anti aliasing for rendering text by default or not.
   */
  private static final boolean DEFAULT_TEXT_AA =
    Boolean.getBoolean("gnu.java2d.default_text_aa");

  /**
   * The default font to use on the graphics object.
   */
  private static final Font FONT = new Font("SansSerif", Font.PLAIN, 12);

  /**
   * The size of the LRU cache used for caching GlyphVectors.
   */
  private static final int GV_CACHE_SIZE = 50;

  /**
   * Caches certain shapes to avoid massive creation of such Shapes in
   * the various draw* and fill* methods.
   */
  private static final ShapeCache shapeCache = new ShapeCache();

  /**
   * A pool of scanline converters. It is important to reuse scanline
   * converters because they keep their datastructures in place. We pool them
   * for use in multiple threads.
   */
  private static final LinkedList<ScanlineConverter> scanlineConverters =
    new LinkedList<ScanlineConverter>();

  /**
   * Caches glyph vectors for better drawing performance.
   */
  private static final Map<TextCacheKey,GlyphVector> gvCache =
    Collections.synchronizedMap(new LRUCache<TextCacheKey,GlyphVector>(GV_CACHE_SIZE));

  /**
   * This key is used to search in the gvCache without allocating a new
   * key each time.
   */
  private static final TextCacheKey searchTextKey = new TextCacheKey();

  /**
   * The transformation for this Graphics2D instance
   */
  protected AffineTransform transform;

  /**
   * The foreground.
   */
  private Paint paint;

  /**
   * The paint context during rendering.
   */
  private PaintContext paintContext = null;

  /**
   * The background.
   */
  private Color background = Color.WHITE;

  /**
   * Foreground color, as set by setColor.
   */
  private Color foreground = Color.BLACK;
  private boolean isForegroundColorNull = true;
  
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
   * The raster of the destination surface. This is where the painting is
   * performed.
   */
  private WritableRaster destinationRaster;

  /**
   * Indicates if certain graphics primitives can be rendered in an optimized
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
  private boolean isOptimized = true;

  private static final BasicStroke STANDARD_STROKE = new BasicStroke();

  private static final HashMap<Key, Object> STANDARD_HINTS;
  static
    {
    
      HashMap<Key, Object> hints = new HashMap<Key, Object>();
      hints.put(RenderingHints.KEY_TEXT_ANTIALIASING,
                RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT);
      hints.put(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_DEFAULT);
    
      STANDARD_HINTS = hints;
    }
  
  /**
   * Creates a new AbstractGraphics2D instance.
   */
  protected AbstractGraphics2D()
  {
    transform = new AffineTransform();
    background = Color.WHITE;
    composite = AlphaComposite.SrcOver;
    stroke = STANDARD_STROKE;
    renderingHints = new RenderingHints(STANDARD_HINTS);
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
    // Fill the stroked shape.
    fillShape(strokedShape, false);
  }


  /**
   * Draws the specified image and apply the transform for image space ->
   * user space conversion.
   *
   * This method is implemented to special case RenderableImages and
   * RenderedImages and delegate to
   * {@link #drawRenderableImage(RenderableImage, AffineTransform)} and
   * {@link #drawRenderedImage(RenderedImage, AffineTransform)} accordingly.
   * Other image types are not yet handled.
   *
   * @param image the image to be rendered
   * @param xform the transform from image space to user space
   * @param obs the image observer to be notified
   */
  public boolean drawImage(Image image, AffineTransform xform,
                           ImageObserver obs)
  {
    Rectangle areaOfInterest = new Rectangle(0, 0, image.getWidth(obs),
                                             image.getHeight(obs));
    return drawImageImpl(image, xform, obs, areaOfInterest);
  }

  /**
   * Draws the specified image and apply the transform for image space ->
   * user space conversion. This method only draw the part of the image
   * specified by <code>areaOfInterest</code>.
   *
   * This method is implemented to special case RenderableImages and
   * RenderedImages and delegate to
   * {@link #drawRenderableImage(RenderableImage, AffineTransform)} and
   * {@link #drawRenderedImage(RenderedImage, AffineTransform)} accordingly.
   * Other image types are not yet handled.
   *
   * @param image the image to be rendered
   * @param xform the transform from image space to user space
   * @param obs the image observer to be notified
   * @param areaOfInterest the area in image space that is rendered
   */
  private boolean drawImageImpl(Image image, AffineTransform xform,
                             ImageObserver obs, Rectangle areaOfInterest)
  {
    boolean ret;
    if (image == null)
      {
        ret = true;
      }
    else if (image instanceof RenderedImage)
      {
        // FIXME: Handle the ImageObserver.
        drawRenderedImageImpl((RenderedImage) image, xform, areaOfInterest);
        ret = true;
      }
    else if (image instanceof RenderableImage)
      {
        // FIXME: Handle the ImageObserver.
        drawRenderableImageImpl((RenderableImage) image, xform, areaOfInterest);
        ret = true;
      }
    else
      {
        // FIXME: Implement rendering of other Image types.
        ret = false;
      }
    return ret;
  }

  /**
   * Renders a BufferedImage and applies the specified BufferedImageOp before
   * to filter the BufferedImage somehow. The resulting BufferedImage is then
   * passed on to {@link #drawRenderedImage(RenderedImage, AffineTransform)}
   * to perform the final rendering.
   *
   * @param image the source buffered image
   * @param op the filter to apply to the buffered image before rendering
   * @param x the x coordinate to render the image to 
   * @param y the y coordinate to render the image to 
   */
  public void drawImage(BufferedImage image, BufferedImageOp op, int x, int y)
  {
    BufferedImage filtered =
      op.createCompatibleDestImage(image, image.getColorModel());
    AffineTransform t = new AffineTransform();
    t.translate(x, y);
    drawRenderedImage(filtered, t);
  }

  /**
   * Renders the specified image to the destination raster. The specified
   * transform is used to convert the image into user space. The transform
   * of this AbstractGraphics2D object is used to transform from user space
   * to device space.
   * 
   * The rendering is performed using the scanline algorithm that performs the
   * rendering of other shapes and a custom Paint implementation, that supplies
   * the pixel values of the rendered image.
   *
   * @param image the image to render to the destination raster
   * @param xform the transform from image space to user space
   */
  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    Rectangle areaOfInterest = new Rectangle(image.getMinX(),
                                             image.getHeight(),
                                             image.getWidth(),
                                             image.getHeight());
    drawRenderedImageImpl(image, xform, areaOfInterest);
  }

  /**
   * Renders the specified image to the destination raster. The specified
   * transform is used to convert the image into user space. The transform
   * of this AbstractGraphics2D object is used to transform from user space
   * to device space. Only the area specified by <code>areaOfInterest</code>
   * is finally rendered to the target.
   * 
   * The rendering is performed using the scanline algorithm that performs the
   * rendering of other shapes and a custom Paint implementation, that supplies
   * the pixel values of the rendered image.
   *
   * @param image the image to render to the destination raster
   * @param xform the transform from image space to user space
   */
  private void drawRenderedImageImpl(RenderedImage image,
                                     AffineTransform xform,
                                     Rectangle areaOfInterest)
  {
    // First we compute the transformation. This is made up of 3 parts:
    // 1. The areaOfInterest -> image space transform.
    // 2. The image space -> user space transform.
    // 3. The user space -> device space transform.
    AffineTransform t = new AffineTransform();
    t.translate(- areaOfInterest.x - image.getMinX(),
                - areaOfInterest.y - image.getMinY());
    t.concatenate(xform);
    t.concatenate(transform);
    AffineTransform it = null;
    try
      {
        it = t.createInverse();
      }
    catch (NoninvertibleTransformException ex)
      {
        // Ignore -- we return if the transform is not invertible.
      }
    if (it != null)
      {
        // Transform the area of interest into user space.
        GeneralPath aoi = new GeneralPath(areaOfInterest);
        aoi.transform(xform);
        // Render the shape using the standard renderer, but with a temporary
        // ImagePaint.
        ImagePaint p = new ImagePaint(image, it);
        Paint savedPaint = paint;
        try
          {
            paint = p;
            fillShape(aoi, false);
          }
        finally
          {
            paint = savedPaint;
          }
      }
  }

  /**
   * Renders a renderable image. This produces a RenderedImage, which is
   * then passed to {@link #drawRenderedImage(RenderedImage, AffineTransform)}
   * to perform the final rendering.
   *
   * @param image the renderable image to be rendered
   * @param xform the transform from image space to user space
   */
  public void drawRenderableImage(RenderableImage image, AffineTransform xform)
  {
    Rectangle areaOfInterest = new Rectangle((int) image.getMinX(),
                                             (int) image.getHeight(),
                                             (int) image.getWidth(),
                                             (int) image.getHeight());
    drawRenderableImageImpl(image, xform, areaOfInterest);
                                                       
  }

  /**
   * Renders a renderable image. This produces a RenderedImage, which is
   * then passed to {@link #drawRenderedImage(RenderedImage, AffineTransform)}
   * to perform the final rendering. Only the area of the image specified
   * by <code>areaOfInterest</code> is rendered.
   *
   * @param image the renderable image to be rendered
   * @param xform the transform from image space to user space
   */
  private void drawRenderableImageImpl(RenderableImage image,
                                       AffineTransform xform,
                                       Rectangle areaOfInterest)
  {
    // TODO: Maybe make more clever usage of a RenderContext here.
    RenderedImage rendered = image.createDefaultRendering();
    drawRenderedImageImpl(rendered, xform, areaOfInterest);
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
    GlyphVector gv;
    synchronized (searchTextKey)
      {
        TextCacheKey tck = searchTextKey;
        FontRenderContext frc = getFontRenderContext();
        tck.setString(text);
        tck.setFont(font);
        tck.setFontRenderContext(frc);
        if (gvCache.containsKey(tck))
          {
            gv = gvCache.get(tck);
          }
        else
          {
            gv = font.createGlyphVector(frc, text.toCharArray());
            gvCache.put(new TextCacheKey(text, font, frc), gv);
          }
      }
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
    if (! (comp instanceof AlphaComposite))
      {
        // FIXME: this check is only required "if this Graphics2D
        // context is drawing to a Component on the display screen".
        SecurityManager sm = System.getSecurityManager();
        if (sm != null)
          sm.checkPermission(new AWTPermission("readDisplayPixels"));
      }

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
          {
            isOptimized = false;
          }
        else
          {
            this.foreground = (Color) paint;
            isForegroundColorNull = false;
            updateOptimization();
          }
      }
    else
      {
        this.foreground = Color.BLACK;
        isForegroundColorNull = true;
      }
    
    // free resources if needed, then put the paint context to null
    if (this.paintContext != null)
      this.paintContext.dispose();
    
    this.paintContext = null;
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
        clipTransform.scale(1 / scaleX, 1 / scaleY);
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
      setClip(s);

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
    // Protect our own transform from beeing modified.
    AffineTransform tf = new AffineTransform(transform);
    // TODO: Determine antialias and fractionalmetrics parameters correctly.
    return new FontRenderContext(tf, false, true);
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
    translate(x, y);
    fillShape(gv.getOutline(), true);
    translate(-x, -y);
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
        else if (clip != null)
          copy.clip = new GeneralPath(clip);
        else
          copy.clip = null;

	copy.renderingHints = new RenderingHints(null);
	copy.renderingHints.putAll(renderingHints);
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
    if (isForegroundColorNull)
      return null;
    
    return this.foreground;
  }

  /**
   * Sets the current foreground.
   *
   * @param color the foreground to set
   */
  public void setColor(Color color)
  { 
    this.setPaint(color);
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
    if (isOptimized)
      rawCopyArea(x, y, width, height, dx, dy);
    else
      copyAreaImpl(x, y, width, height, dx, dy);
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
        ShapeCache sc = shapeCache;
        if (sc.line == null)
          sc.line = new Line2D.Float();
        sc.line.setLine(x1, y1, x2, y2);
        draw(sc.line);
      }
  }

  public void drawRect(int x, int y, int w, int h)
  {
    if (isOptimized)
      {
        int tx = (int) transform.getTranslateX();
        int ty = (int) transform.getTranslateY();
        rawDrawRect(x + tx, y + ty, w, h);
      }
    else
      {
        ShapeCache sc = shapeCache;
        if (sc.rect == null)
          sc.rect = new Rectangle();
        sc.rect.setBounds(x, y, w, h);
        draw(sc.rect);
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
        rawFillRect(x + (int) transform.getTranslateX(),
                    y + (int) transform.getTranslateY(), width, height);
      }
    else
      {
        ShapeCache sc = shapeCache;
        if (sc.rect == null)
          sc.rect = new Rectangle();
        sc.rect.setBounds(x, y, width, height);
        fill(sc.rect);
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
    if (isOptimized)
      rawClearRect(x, y, width, height);
    else
      {
        Paint savedForeground = getPaint();
        setPaint(getBackground());
        fillRect(x, y, width, height);
        setPaint(savedForeground);
      }
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
    ShapeCache sc = shapeCache;
    if (sc.roundRect == null)
      sc.roundRect = new RoundRectangle2D.Float();
    sc.roundRect.setRoundRect(x, y, width, height, arcWidth, arcHeight);
    draw(sc.roundRect);
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
    ShapeCache sc = shapeCache;
    if (sc.roundRect == null)
      sc.roundRect = new RoundRectangle2D.Float();
    sc.roundRect.setRoundRect(x, y, width, height, arcWidth, arcHeight);
    fill(sc.roundRect);
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
    ShapeCache sc = shapeCache;
    if (sc.ellipse == null)
      sc.ellipse = new Ellipse2D.Float();
    sc.ellipse.setFrame(x, y, width, height);
    draw(sc.ellipse);
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
    ShapeCache sc = shapeCache;
    if (sc.ellipse == null)
      sc.ellipse = new Ellipse2D.Float();
    sc.ellipse.setFrame(x, y, width, height);
    fill(sc.ellipse);
  }

  /**
   * Draws an arc.
   */
  public void drawArc(int x, int y, int width, int height, int arcStart,
                      int arcAngle)
  {
    ShapeCache sc = shapeCache;
    if (sc.arc == null)
      sc.arc = new Arc2D.Float();
    sc.arc.setArc(x, y, width, height, arcStart, arcAngle, Arc2D.OPEN);
    draw(sc.arc);
  }

  /**
   * Fills an arc.
   */
  public void fillArc(int x, int y, int width, int height, int arcStart,
                      int arcAngle)
  {
    ShapeCache sc = shapeCache;
    if (sc.arc == null)
      sc.arc = new Arc2D.Float();
    sc.arc.setArc(x, y, width, height, arcStart, arcAngle, Arc2D.PIE);
    draw(sc.arc);
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int npoints)
  {
    ShapeCache sc = shapeCache;
    if (sc.polyline == null)
      sc.polyline = new GeneralPath();
    GeneralPath p = sc.polyline;
    p.reset();
    if (npoints > 0)
      p.moveTo(xPoints[0], yPoints[0]);
    for (int i = 1; i < npoints; i++)
      p.lineTo(xPoints[i], yPoints[i]);
    fill(p);
  }

  /**
   * Draws the outline of a polygon.
   */
  public void drawPolygon(int[] xPoints, int[] yPoints, int npoints)
  {
    ShapeCache sc = shapeCache;
    if (sc.polygon == null)
      sc.polygon = new Polygon();
    sc.polygon.reset();
    sc.polygon.xpoints = xPoints;
    sc.polygon.ypoints = yPoints;
    sc.polygon.npoints = npoints;
    draw(sc.polygon);
  }

  /**
   * Fills the outline of a polygon.
   */
  public void fillPolygon(int[] xPoints, int[] yPoints, int npoints)
  {
    ShapeCache sc = shapeCache;
    if (sc.polygon == null)
      sc.polygon = new Polygon();
    sc.polygon.reset();
    sc.polygon.xpoints = xPoints;
    sc.polygon.ypoints = yPoints;
    sc.polygon.npoints = npoints;
    fill(sc.polygon);
  }

  /**
   * Draws the specified image at the specified location. This forwards
   * to {@link #drawImage(Image, AffineTransform, ImageObserver)}.
   *
   * @param image the image to render
   * @param x the x location to render to
   * @param y the y location to render to
   * @param observer the image observer to receive notification
   */
  public boolean drawImage(Image image, int x, int y, ImageObserver observer)
  {
    boolean ret;
    if (isOptimized)
      {
        ret = rawDrawImage(image, x + (int) transform.getTranslateX(),
                           y + (int) transform.getTranslateY(), observer);
      }
    else
      {
        AffineTransform t = new AffineTransform();
        t.translate(x, y);
        ret = drawImage(image, t, observer);
      }
    return ret;
  }

  /**
   * Draws the specified image at the specified location. The image
   * is scaled to the specified width and height. This forwards
   * to {@link #drawImage(Image, AffineTransform, ImageObserver)}.
   *
   * @param image the image to render
   * @param x the x location to render to
   * @param y the y location to render to
   * @param width the target width of the image
   * @param height the target height of the image
   * @param observer the image observer to receive notification
   */
  public boolean drawImage(Image image, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    AffineTransform t = new AffineTransform();
    int imWidth = image.getWidth(observer);
    int imHeight = image.getHeight(observer);
    if (imWidth == width && imHeight == height)
      {
        // No need to scale, fall back to non-scaling loops.
        return drawImage(image, x, y, observer);
      }
    else
      {
        Image scaled = prepareImage(image, width, height);
        // Ideally, this should notify the observer about the scaling progress.
        return drawImage(scaled, x, y, observer);
      }
  }

  /**
   * Draws the specified image at the specified location. This forwards
   * to {@link #drawImage(Image, AffineTransform, ImageObserver)}.
   *
   * @param image the image to render
   * @param x the x location to render to
   * @param y the y location to render to
   * @param bgcolor the background color to use for transparent pixels
   * @param observer the image observer to receive notification
   */
  public boolean drawImage(Image image, int x, int y, Color bgcolor,
                           ImageObserver observer)
  {
    AffineTransform t = new AffineTransform();
    t.translate(x, y);
    // TODO: Somehow implement the background option.
    return drawImage(image, t, observer);
  }

  /**
   * Draws the specified image at the specified location. The image
   * is scaled to the specified width and height. This forwards
   * to {@link #drawImage(Image, AffineTransform, ImageObserver)}.
   *
   * @param image the image to render
   * @param x the x location to render to
   * @param y the y location to render to
   * @param width the target width of the image
   * @param height the target height of the image
   * @param bgcolor the background color to use for transparent pixels
   * @param observer the image observer to receive notification
   */
  public boolean drawImage(Image image, int x, int y, int width, int height,
                           Color bgcolor, ImageObserver observer)
  {
    AffineTransform t = new AffineTransform();
    t.translate(x, y);
    double scaleX = (double) image.getWidth(observer) / (double) width;
    double scaleY = (double) image.getHeight(observer) / (double) height;
    t.scale(scaleX, scaleY);
    // TODO: Somehow implement the background option.
    return drawImage(image, t, observer);
  }

  /**
   * Draws an image fragment to a rectangular area of the target.
   *
   * @param image the image to render
   * @param dx1 the first corner of the destination rectangle
   * @param dy1 the first corner of the destination rectangle
   * @param dx2 the second corner of the destination rectangle
   * @param dy2 the second corner of the destination rectangle
   * @param sx1 the first corner of the source rectangle
   * @param sy1 the first corner of the source rectangle
   * @param sx2 the second corner of the source rectangle
   * @param sy2 the second corner of the source rectangle
   * @param observer the image observer to be notified
   */
  public boolean drawImage(Image image, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           ImageObserver observer)
  {
    int sx = Math.min(sx1, sx1);
    int sy = Math.min(sy1, sy2);
    int sw = Math.abs(sx1 - sx2);
    int sh = Math.abs(sy1 - sy2);
    int dx = Math.min(dx1, dx1);
    int dy = Math.min(dy1, dy2);
    int dw = Math.abs(dx1 - dx2);
    int dh = Math.abs(dy1 - dy2);
    
    AffineTransform t = new AffineTransform();
    t.translate(sx - dx, sy - dy);
    double scaleX = (double) sw / (double) dw;
    double scaleY = (double) sh / (double) dh;
    t.scale(scaleX, scaleY);
    Rectangle areaOfInterest = new Rectangle(sx, sy, sw, sh);
    return drawImageImpl(image, t, observer, areaOfInterest);
  }

  /**
   * Draws an image fragment to a rectangular area of the target.
   *
   * @param image the image to render
   * @param dx1 the first corner of the destination rectangle
   * @param dy1 the first corner of the destination rectangle
   * @param dx2 the second corner of the destination rectangle
   * @param dy2 the second corner of the destination rectangle
   * @param sx1 the first corner of the source rectangle
   * @param sy1 the first corner of the source rectangle
   * @param sx2 the second corner of the source rectangle
   * @param sy2 the second corner of the source rectangle
   * @param bgcolor the background color to use for transparent pixels
   * @param observer the image observer to be notified
   */
  public boolean drawImage(Image image, int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2, Color bgcolor,
                           ImageObserver observer)
  {
    // FIXME: Do something with bgcolor.
    return drawImage(image, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, observer);
  }

  /**
   * Disposes this graphics object.
   */
  public void dispose()
  {
    // Nothing special to do here.
  }

  /**
   * Fills the specified shape. Override this if your backend can efficiently
   * fill shapes. This is possible on many systems via a polygon fill
   * method or something similar. But keep in mind that Shapes can be quite
   * complex (non-convex, with holes etc), which is not necessarily supported
   * by all polygon fillers. Also note that you must perform clipping
   * before filling the shape.
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
        // We default to antialiasing for text rendering.
        antialias = v == RenderingHints.VALUE_TEXT_ANTIALIAS_ON
                    || (v == RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT
                         && DEFAULT_TEXT_AA);
      }
    else
      {
        Object v = renderingHints.get(RenderingHints.KEY_ANTIALIASING);
        antialias = (v == RenderingHints.VALUE_ANTIALIAS_ON);
      }
    ScanlineConverter sc = getScanlineConverter();
    int resolution = 0;
    int yRes = 0;
    if (antialias)
      {
        // Adjust resolution according to rendering hints.
        resolution = 2;
        yRes = 4;
      }
    sc.renderShape(this, s, clip, transform, resolution, yRes, renderingHints);
    freeScanlineConverter(sc);
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
  protected abstract Rectangle getDeviceBounds();

  /**
   * Draws a line in optimization mode. The implementation should respect the
   * clip and translation. It can assume that the clip is a rectangle and that
   * the transform is only a translating transform.
   *
   * @param x0 the starting point, X coordinate
   * @param y0 the starting point, Y coordinate
   * @param x1 the end point, X coordinate 
   * @param y1 the end point, Y coordinate
   */
  protected void rawDrawLine(int x0, int y0, int x1, int y1)
  {
    ShapeCache sc = shapeCache;
    if (sc.line == null)
      sc.line = new Line2D.Float();
    sc.line.setLine(x0, y0, x1, y1);
    draw(sc.line);
  }

  protected void rawDrawRect(int x, int y, int w, int h)
  {
    ShapeCache sc = shapeCache;
    if (sc.rect == null)
      sc.rect = new Rectangle();
    sc.rect.setBounds(x, y, w, h);
    draw(sc.rect);
  }

  /**
   * Clears a rectangle in optimization mode. The implementation should respect the
   * clip and translation. It can assume that the clip is a rectangle and that
   * the transform is only a translating transform.
   *
   * @param x the upper left corner, X coordinate
   * @param y the upper left corner, Y coordinate
   * @param w the width
   * @param h the height
   */
  protected void rawClearRect(int x, int y, int w, int h)
  {
    Paint savedForeground = getPaint();
    setPaint(getBackground());
    rawFillRect(x, y, w, h);
    setPaint(savedForeground);
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
    ShapeCache sc = shapeCache;
    if (sc.rect == null)
      sc.rect = new Rectangle();
    sc.rect.setBounds(x, y, w, h);
    fill(sc.rect);
  }

  /**
   * Draws an image in optimization mode. The implementation should respect
   * the clip but can assume that it is a rectangle.
   *
   * @param image the image to be painted
   * @param x the location, X coordinate
   * @param y the location, Y coordinate
   * @param obs the image observer to be notified
   *
   * @return <code>true</code> when the image is painted completely,
   *         <code>false</code> if it is still rendered
   */
  protected boolean rawDrawImage(Image image, int x, int y, ImageObserver obs)
  {
    AffineTransform t = new AffineTransform();
    t.translate(x, y);
    return drawImage(image, t, obs);
  }

  /**
   * Copies a rectangular region to another location.
   *
   * @param x the upper left corner, X coordinate
   * @param y the upper left corner, Y coordinate
   * @param w the width
   * @param h the height
   * @param dx
   * @param dy
   */
  protected void rawCopyArea(int x, int y, int w, int h, int dx, int dy)
  {
    copyAreaImpl(x, y, w, h, dx, dy);
  }

  // Private implementation methods.

  /**
   * Copies a rectangular area of the target raster to a different location.
   */
  private void copyAreaImpl(int x, int y, int w, int h, int dx, int dy)
  {
    // FIXME: Implement this properly.
    throw new UnsupportedOperationException("Not implemented yet.");
  }

  /**
   * Paints a scanline between x0 and x1. Override this when your backend
   * can efficiently draw/fill horizontal lines.
   *
   * @param x0 the left offset
   * @param x1 the right offset
   * @param y the scanline
   */
  public void renderScanline(int y, ScanlineCoverage c)
  {
    PaintContext pCtx = getPaintContext();
    
    int x0 = c.getMinX();
    int x1 = c.getMaxX();
    Raster paintRaster = pCtx.getRaster(x0, y, x1 - x0, 1);

    // Do the anti aliasing thing.
    float coverageAlpha = 0;
    float maxCoverage = c.getMaxCoverage();
    ColorModel cm = pCtx.getColorModel();
    DataBuffer db = paintRaster.getDataBuffer();
    Point loc = new Point(paintRaster.getMinX(), paintRaster.getMinY());
    SampleModel sm = paintRaster.getSampleModel();
    WritableRaster writeRaster = Raster.createWritableRaster(sm, db, loc);
    WritableRaster alphaRaster = cm.getAlphaRaster(writeRaster);
    int pixel;
    ScanlineCoverage.Iterator iter = c.iterate();
    while (iter.hasNext())
      {
        ScanlineCoverage.Range range = iter.next();
        coverageAlpha = range.getCoverage() / maxCoverage;
        if (coverageAlpha < 1.0)
          {
            for (int x = range.getXPos(); x < range.getXPosEnd(); x++)
              {
                pixel = alphaRaster.getSample(x, y, 0);
                pixel = (int) (pixel * coverageAlpha);
                alphaRaster.setSample(x, y, 0, pixel);
              }
          }
      }
    ColorModel paintColorModel = pCtx.getColorModel();
    CompositeContext cCtx = composite.createContext(paintColorModel,
                                                    getColorModel(),
                                                    renderingHints);
    WritableRaster raster = getDestinationRaster();
    WritableRaster targetChild = raster.createWritableTranslatedChild(-x0, -y);
    
    cCtx.compose(paintRaster, targetChild, targetChild);
    updateRaster(raster, x0, y, x1 - x0, 1);
    cCtx.dispose();
  }


  /**
   * Initializes this graphics object. This must be called by subclasses in
   * order to correctly initialize the state of this object.
   */
  protected void init()
  {
    setPaint(Color.BLACK);
    setFont(FONT);
    isOptimized = true;
  }

  /**
   * Returns a WritableRaster that is used by this class to perform the
   * rendering in. It is not necessary that the target surface immediately
   * reflects changes in the raster. Updates to the raster are notified via
   * {@link #updateRaster}.
   *
   * @return the destination raster
   */
  protected WritableRaster getDestinationRaster()
  {
    // TODO: Ideally we would fetch the xdrawable's surface pixels for
    // initialization of the raster.
    Rectangle db = getDeviceBounds();
    if (destinationRaster == null)
      {
        int[] bandMasks = new int[]{ 0xFF0000, 0xFF00, 0xFF };
        destinationRaster = Raster.createPackedRaster(DataBuffer.TYPE_INT,
                                                      db.width, db.height,
                                                      bandMasks, null);
        // Initialize raster with white.
        int x0 = destinationRaster.getMinX();
        int x1 = destinationRaster.getWidth() + x0;
        int y0 = destinationRaster.getMinY();
        int y1 = destinationRaster.getHeight() + y0;
        int numBands = destinationRaster.getNumBands();
        for (int y = y0; y < y1; y++)
          {
            for (int x = x0; x < x1; x++)
              {
                for (int b = 0; b < numBands; b++)
                  destinationRaster.setSample(x, y, b, 255);
              }
          }
      }
    return destinationRaster;
  }

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
    int x2 = rect.x;
    int y2 = rect.y;
    int w2 = rect.width;
    int h2 = rect.height;

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
   * Returns a free scanline converter from the pool.
   *
   * @return a scanline converter
   */
  private ScanlineConverter getScanlineConverter()
  {
    synchronized (scanlineConverters)
      {
        ScanlineConverter sc;
        if (scanlineConverters.size() > 0)
          {
            sc = scanlineConverters.removeFirst();
          }
        else
          {
            sc = new ScanlineConverter();
          }
        return sc;
      }
  }

  /**
   * Puts a scanline converter back in the pool.
   *
   * @param sc
   */
  private void freeScanlineConverter(ScanlineConverter sc)
  {
    synchronized (scanlineConverters)
      {
        scanlineConverters.addLast(sc);
      }
  }

  private PaintContext getPaintContext()
  {
    if (this.paintContext == null)
      {
        this.paintContext =
          this.foreground.createContext(getColorModel(),
                                        getDeviceBounds(),
                                        getClipBounds(),
                                        getTransform(),
                                        getRenderingHints());
      }
   
    return this.paintContext;
  }

  /**
   * Scales an image to the specified width and height. This should also
   * be used to implement
   * {@link Toolkit#prepareImage(Image, int, int, ImageObserver)}.
   * This uses {@link Toolkit#createImage(ImageProducer)} to create the actual
   * image.
   *
   * @param image the image to prepare
   * @param w the width
   * @param h the height
   *
   * @return the scaled image
   */
  public static Image prepareImage(Image image, int w, int h)
  {
    // Try to find cached scaled image.
    HashMap<Dimension,Image> scaledTable = imageCache.get(image);
    Dimension size = new Dimension(w, h);
    Image scaled = null;
    if (scaledTable != null)
      {
        scaled = scaledTable.get(size);
      }
    if (scaled == null)
      {
        // No cached scaled image. Start scaling image now.
        ImageProducer source = image.getSource();
        ReplicateScaleFilter scaler = new ReplicateScaleFilter(w, h);
        FilteredImageSource filteredSource =
          new FilteredImageSource(source, scaler);
        // Ideally, this should asynchronously scale the image.
        Image scaledImage =
          Toolkit.getDefaultToolkit().createImage(filteredSource);
        scaled = scaledImage;
        // Put scaled image in cache.
        if (scaledTable == null)
          {
            scaledTable = new HashMap<Dimension,Image>();
            imageCache.put(image, scaledTable);
          }
        scaledTable.put(size, scaledImage);
      }
    return scaled;
  }

}
