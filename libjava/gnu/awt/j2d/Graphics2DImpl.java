/* Copyright (C) 2000, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.j2d;

import java.awt.Color;
import java.awt.Composite;
import java.awt.Image;
import java.awt.Shape;
import java.awt.Rectangle;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Paint;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.image.ImageObserver;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.Map;

/**
 * Delegates almost all work to a state object, that allows us to
 * hot-swap rendering strategies based on state changes inflicted on
 * this Graphics object. This class keeps track of properties that are
 * not affected by the state, (such as clip shape,
 * foreground/background color, font, etc.).
 *
 * <p>The far front-end of the rendering pipeline consists of the
 * Graphics2D API. In the far back-end, lies the native graphics
 * libraries. In most cases the native graphics libraries only have
 * direct support for a subset of the properties of Graphics2D. To
 * make up missing features in the native graphics libraries, the
 * pipeline between the front-end and the back-end need to translate
 * drawing request to primitive operations that are supported by the
 * back-end. E.g. for X11, drawing a straight line will translate to
 * an XDrawLine, drawing a bezier curve will trigger flattening of the
 * curve and will result in a call to XDrawLines.
 *
 * <p>This is the basic strategy for the rendering pipeline: Whenever
 * a graphics property change occurs, that causes the current pipeline
 * to be insufficient, amend or replace parts of the pipeline so that
 * the pipeline will once again be able to translate requests to the
 * set of primitives supported by the native graphics library.
 *
 * <p>Most graphics libraries share common subsets of
 * functionality. To be able to reuse pieces of the rendering pipeline
 * for several backends, we define interfaces that describe subsets of
 * characteristics supported by the backends. A wrapper for the native
 * library can implement several interfaces to describe its range of
 * functionality.
 *
 * <p>Typically, most painting is done with a graphics object with
 * simple properties. Unless one is using a complex Look & Feel, the
 * painting of Swing components will never require affine transforms,
 * alpha blending, non-rectangular clipping, etc. When graphics
 * objects are created, they start off in a state where all the
 * properties are simple. Most graphics objects experience only
 * trivial property changes, and never leave this simple state. It is
 * therefore wise to ensure that the rendering pipeline for this
 * initial state is lean and as much as possible plugs directly into
 * the backend.
 *
 * <p>The initial state for graphics object of most raster displays
 * would call for two levels of indirection:
 *
 * <pre>
 * Graphics2D object ---> IntegerGraphicsState ---> DirectRasterGraphics
 * </pre>
 */
public class Graphics2DImpl extends Graphics2D implements Cloneable
{
  GraphicsConfiguration config;

  AbstractGraphicsState state;
    
  Color fg;
  Color bg;

  Font font;
  
  public Graphics2DImpl(GraphicsConfiguration config)
  {
    this.config = config;
  }
  
  public void setState(AbstractGraphicsState state)
  {
    this.state = state;
    this.state.setFrontend(this);
  }
    
  public Object clone()
  {
    try
      {
	Graphics2DImpl gfxCopy = (Graphics2DImpl) super.clone();
	AbstractGraphicsState stateCopy =
	  (AbstractGraphicsState) state.clone();
	gfxCopy.setState(stateCopy);
	
	return gfxCopy;
      }
    catch (CloneNotSupportedException ex)
      {
	// This should never happen.
	throw new InternalError ();
      }
  }


  // -------- Graphics methods:

  public Graphics create()
  {
    Graphics2DImpl gfxCopy = (Graphics2DImpl) clone();
    return gfxCopy;
  }

  public Color getColor()
  {
    return fg;
  }
  
  public void setColor(Color color)
  {
    fg = color;
    state.setColor(color);
  }

  public void setPaintMode()
  {
    state.setPaintMode();
  }

  public void setXORMode(Color altColor)
  {
    state.setXORMode(altColor);
  }

  public Font getFont()
  {
    return font;
  }

  public void setFont(Font font)
  {
    this.font = font;
    state.setFont(font);
  }
    
  public FontMetrics getFontMetrics(Font font)
  {
    return state.getFontMetrics(font);
  }

  public Rectangle getClipBounds()
  {
    return state.getClipBounds();
  }
    
  public void clipRect(int x, int y, int width, int height)
  {
    Shape clip = state.getClip();
    if (clip == null)
    {
      clip = new Rectangle (x,y,width,height);
      setClip (clip);
      return;
    }
    if (clip instanceof Rectangle)
      {
	Rectangle clipRect = (Rectangle) clip;
	clip = clipRect.intersection(new Rectangle(x, y, width, height));
	setClip(clip);
	return;
      }
	
    String msg =
      "intersecting current clip shape " + clip + " with new rectangle " +
      "has not been implemented yet";
    throw new UnsupportedOperationException(msg);
  }

  public void setClip(int x, int y, int width, int height)
  {
    Rectangle clip = new Rectangle(x, y, width, height);
    setClip(clip);
  }

  public Shape getClip()
  {
    return state.getClip();
  }

  public void setClip(Shape clip)
  {
    state.setClip(clip);
  }

  public void copyArea(int x, int y, int width, int height,
		       int dx, int dy)
  {
    state.copyArea(x, y, width, height, dx, dy);
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    state.drawLine(x1, y1, x2, y2);
  }
    
  public void fillRect(int x, int y, int width, int height)
  {
    state.fillRect(x, y, width, height);
  }
    
  public void clearRect(int x, int y, int width, int height)
  {
    state.clearRect(x, y, width, height);
  }
  
  public void drawRoundRect(int x, int y, int width, int height,
			    int arcWidth, int arcHeight)
  {
    state.drawRoundRect(x, y, width, height, arcWidth, arcHeight);
  }
    
  public void fillRoundRect(int x, int y, int width, int height,
			    int arcWidth, int arcHeight)
  {
    state.fillRoundRect(x, y, width, height, arcWidth, arcHeight);
  }

  public void drawOval(int x, int y, int width, int height)
  {
    state.drawOval(x, y, width, height);
  }

  public void fillOval(int x, int y, int width, int height)
  {
    state.fillOval(x, y, width, height);
  }

  public void drawArc(int x, int y, int width, int height,
		      int startAngle, int arcAngle)
  {
    state.drawArc(x, y, width, height, startAngle, arcAngle);
  }

  public void fillArc(int x, int y, int width, int height,
		      int startAngle, int arcAngle)
  {
    state.fillArc(x, y, width, height, startAngle, arcAngle);
  }

  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints)
  {
    state.drawPolyline(xPoints, yPoints, nPoints);
  }
  
  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    state.drawPolygon(xPoints, yPoints, nPoints);
  }
    
  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    state.fillPolygon(xPoints, yPoints, nPoints);
  }
    
  public boolean drawImage(Image image, int x, int y,
			   ImageObserver observer)
  {
    return state.drawImage(image, x, y, observer);
  }

  public boolean drawImage(Image img, int x, int y,
			   int width, int height,
			   ImageObserver observer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public boolean drawImage(Image img, int x, int y, Color bgcolor,
			   ImageObserver observer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public boolean drawImage(Image img, int x, int y,
			   int width, int height, Color bgcolor,
			   ImageObserver observer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public boolean drawImage(Image img,
			   int dx1, int dy1, int dx2, int dy2,
			   int sx1, int sy1, int sx2, int sy2,
			   ImageObserver observer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public boolean drawImage(Image img,
			   int dx1, int dy1, int dx2, int dy2,
			   int sx1, int sy1, int sx2, int sy2,
			   Color bgcolor, ImageObserver observer)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public void dispose()
  {
    AbstractGraphicsState lState = state;
    
    state = null;
    config = null;
    font = null;
    fg = null;
    bg = null;
    
    if (lState != null)
      lState.dispose();
  }
    


  // -------- Graphics2D methods:
    
  public void draw(Shape shape)
  {
    state.draw(shape);
  }
  
  public boolean drawImage(Image image, AffineTransform xform,
			   ImageObserver obs)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }


  public void drawString(String text, int x, int y)
  {
    state.drawString(text, x, y);
  }

  public void drawString(String text, float x, float y)
  {
    state.drawString(text, x, y);
  }

  public void fill(Shape shape)
  {
    state.fill(shape);
  }

  public boolean hit(Rectangle rect, Shape text, boolean onStroke)
  {
    return state.hit(rect, text, onStroke);
  }
    
  public GraphicsConfiguration getDeviceConfiguration()
  {
    return config;
  }

  public void setPaint(Paint paint)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void setRenderingHint(RenderingHints.Key hintKey,
			       Object hintValue)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public Object getRenderingHint(RenderingHints.Key hintKey)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public RenderingHints getRenderingHints()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
    
  public void translate(int x, int y)
  {
    state.translate(x, y);
  }
  
  public void translate(double tx, double ty)
  {
    state.translate(tx, ty);
  }
    
  public void rotate(double theta)
  {
    state.rotate(theta);
  }

  public void rotate(double theta, double x, double y)
  {
    state.rotate(theta, x, y);
  }
  
  public void scale(double scaleX, double scaleY)
  {
    state.scale(scaleX, scaleY);
  }
  
  public void shear(double shearX, double shearY)
  {
    state.shear(shearX, shearY);
  }

  public void transform(AffineTransform Tx)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void setTransform(AffineTransform Tx)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
    
  public AffineTransform getTransform()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public Paint getPaint()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
    
  public void setBackground(Color color)
  {
    bg = color;
  }

  public Color getBackground()
  {
    return bg;
  }

  public void clip(Shape shape)
  {
    Shape clip = state.getClip();
    
    if ((shape instanceof Rectangle) && (clip instanceof Rectangle))
      {
	clip = ((Rectangle) clip).intersection((Rectangle) shape);
	state.setClip(clip);
	return;
      }
	
    String msg =
      "intersecting current clip shape " + clip + " with new shape " + shape +
      "has not been implemented yet";
    throw new UnsupportedOperationException(msg);
  }
  
  public void drawImage(BufferedImage image, BufferedImageOp op, int x, int y)
  {
    throw new UnsupportedOperationException("not implemented yet");  
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    throw new UnsupportedOperationException("not implemented yet");  
  }

  public void drawRenderableImage(RenderableImage image, AffineTransform xform)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void drawString(AttributedCharacterIterator iterator,
                		  int x, int y)
  {
    throw new UnsupportedOperationException("not implemented yet");  
  }

  public void drawString(AttributedCharacterIterator iterator, float x, 
			 float y)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void setComposite(Composite comp)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void setStroke(Stroke stroke)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void setRenderingHints(Map hints)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void addRenderingHints(Map hints)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public Composite getComposite()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public Stroke getStroke()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public FontRenderContext getFontRenderContext ()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void drawGlyphVector (GlyphVector g, float x, float y)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
}
