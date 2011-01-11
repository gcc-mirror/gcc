/* QtGraphics.java --
   Copyright (C)  2005, 2006  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.AlphaComposite;
import java.awt.AWTPermission;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.GradientPaint;
import java.awt.GraphicsConfiguration;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Rectangle;
import java.awt.Paint;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImageObserver;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.RenderableImage;

import java.text.AttributedCharacterIterator;
import java.text.CharacterIterator;
import java.util.Map;

/**
 * QtGraphics is an abstract implementation of Graphics2D over a QPainter
 * object. This is to be subclassed for different drawing contexts,
 * which may have different requirements.
 */
public abstract class QtGraphics extends Graphics2D
{
  /**
   * Native QPainter pointer.
   */
  protected long nativeObject;

  private static final AffineTransform identity = new AffineTransform();

  // Graphics state
  protected Font font;              // Current font.
  protected Color color, bgcolor;   // Current color and background color.
  protected Shape clip;             // Current clipping area.
  protected Shape initialClip;      // Initial clip bounds
  protected AffineTransform xform;  // Current transform
  protected Stroke currentStroke;   // the current stroke
  protected boolean nativeStroking; // whether we're using Qt's stroking or not
  protected Composite composite; // current composite operator
  protected double currentAlpha; // current alpha
  protected Paint currentPaint;  // current paint
  protected RenderingHints renderingHints; // the rendering hints.

  /**
   * Owner Graphics, used by subcontext created by create()
   * to avoid GC of the original context.
   */
  Graphics parent;

  /**
   * Do-nothing constructor.
   */
  QtGraphics()
  {
  }

  /**
   * Copying constructor - used by copy() and subclasses.
   */
  QtGraphics(QtGraphics parent)
  {
    cloneNativeContext( parent );
    setFont( parent.getFont() );
    setAlpha( parent.currentAlpha );
    setBackground( parent.getBackground() );
    setColor( parent.getColor() );
    setClip( (initialClip = parent.getClip()) );
    setTransform( parent.getTransform() );
    setStroke( parent.getStroke() );
    setComposite( parent.getComposite() );
    setPaint( parent.getPaint() );
    setRenderingHints( parent.getRenderingHints() );
  }

  /**
   * Set up some generic defaults.
   */
  protected void setup()
  {
    font = new Font ("Dialog", Font.PLAIN, 12);
    setTransform( identity );
    setStroke( new BasicStroke() );
    renderingHints = new RenderingHints( null );
  }

  public synchronized native void delete();

  public void dispose()
  {
  }

  // ********************** etc *******************************

  private void resetClip()
  {
    AffineTransform current = getTransform();
    setTransform( identity );
    setClip( initialClip );
    setTransform( current );
  }

  protected native void initImage(QtImage image);
  protected native void initVolatileImage(QtVolatileImage image);

  // Creates a new native QPainter object on the same context.
  private native void cloneNativeContext( QtGraphics parent );
  private native void setColor(int r, int g, int b, int a);
  private native void drawNative( QPainterPath p );
  private native void fillNative( QPainterPath p );
  private native void setClipNative( QPainterPath p );
  private native void setClipRectNative( int x, int y, int w, int h );
  private native void intersectClipNative( QPainterPath p );
  private native void intersectClipRectNative( int x, int y, int w, int h );
  private native void setQtTransform(QMatrix m);
  private native void setNativeStroke(QPen p);
  private native void setNativeComposite(int alphaMode);
  private native void drawStringNative(String string, double x, double y);
  private native void setLinearGradient(int r1, int g1, int b1,
                                        int r2, int g2, int b2,
                                        double x1, double y1,
                                        double x2, double y2, boolean cyclic);
  private native void setAlphaNative(double alpha);
  private native void setFontNative(QtFontPeer font);
  private native QPainterPath getClipNative();

  void setAlpha(double alpha)
  {
    currentAlpha = alpha;
    setAlphaNative(currentAlpha);
  }

  // ************ Public methods *********************

  /**
   * Context-sensitive methods are declared abstract.
   */
  public abstract Graphics create();

  public abstract void copyArea(int x, int y, int width, int height,
                                int dx, int dy);

  public abstract GraphicsConfiguration getDeviceConfiguration();


  public Color getColor()
  {
    return new Color(color.getRed(), color.getGreen(), color.getBlue());
  }

  public void setColor(Color c)
  {
    if( c == null )
      c = Color.white;
    this.color = c;
    int alpha = (int)(c.getAlpha() * currentAlpha);
    setColor(c.getRed(), c.getGreen(), c.getBlue(), alpha);
  }

  public void setBackground(Color color)
  {
    bgcolor = new Color(color.getRed(), color.getGreen(), color.getBlue());
  }

  public Color getBackground()
  {
    return new Color(bgcolor.getRed(), bgcolor.getGreen(), bgcolor.getBlue());
  }

  public void setPaintMode()
  {
  }

  public void setXORMode(Color color)
  {
    // FIXME
  }

  public boolean hit(Rectangle rect, Shape s, boolean onStroke)
  {
    if( onStroke )
      {
        Shape stroked = currentStroke.createStrokedShape( s );
        return stroked.intersects( (double)rect.x, (double)rect.y,
                                   (double)rect.width, (double)rect.height );
      }
    return s.intersects( (double)rect.x, (double)rect.y,
                         (double)rect.width, (double)rect.height );
  }

  // ******************* Font ***********************
  public Font getFont()
  {
    return font;
  }

  public void setFont(Font font)
  {
    if( font == null )
      return;
    this.font = font;
    if(font.getPeer() != null && font.getPeer() instanceof QtFontPeer)
      setFontNative( (QtFontPeer)font.getPeer() );
  }

  public FontMetrics getFontMetrics(Font font)
  {
    return new QtFontMetrics(font, this);
  }

  // ***************** Clipping *********************

  /**
   * Intersects the current clip with the shape
   */
  public void clip(Shape s)
  {
    intersectClipNative( new QPainterPath( s ) );
  }

  public void clipRect(int x, int y, int width, int height)
  {
    intersectClipRectNative( x, y, width, height );
  }

  public void setClip(int x, int y, int width, int height)
  {
    setClipRectNative( x, y, width, height );
  }

  public Shape getClip()
  {
    return getClipNative().getPath();
  }

  public native Rectangle getClipBounds();

  /**
   * Sets the clip
   */
  public void setClip(Shape clip)
  {
    if (clip == null)
      resetClip();
    else
      setClipNative(new QPainterPath( clip ));
  }

  // ***************** Drawing primitives *********************

  public void draw(Shape s)
  {
    if( nativeStroking )
      drawNative( new QPainterPath(s) );
    else
      fillNative( new QPainterPath( currentStroke.createStrokedShape( s ) ) );
  }

  public void fill(Shape s)
  {
    fillNative( new QPainterPath(s) );
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    if( nativeStroking )
      drawNative( new QPainterPath((double)x1, (double)y1, (double)x2, (double)y2, true) );
    else
      draw( new Line2D.Double((double)x1, (double)y1, (double)x2, (double)y2) );
  }

  public void drawRect(int x, int y, int width, int height)
  {
    if( nativeStroking )
      drawNative( new QPainterPath((double)x, (double)y,
                                   (double)width, (double)height) );
    else
      fillNative( new QPainterPath
                  ( currentStroke.createStrokedShape
                    (new Rectangle2D.Double
                     ((double)x, (double)y,
                      (double)width, (double)height) ) ) );
  }

  public void fillRect(int x, int y, int width, int height)
  {
    fillNative( new QPainterPath( x, y, width, height ) );
  }

  public void clearRect(int x, int y, int width, int height)
  {
    Color c = color;
    setColor( bgcolor ); // FIXME
    fillRect( x, y, width, height );
    setColor( c );
  }

  public void drawRoundRect(int x, int y, int width, int height,
                            int arcWidth, int arcHeight)
  {
    draw( new RoundRectangle2D.Double(x, y, width, height,
                                      arcWidth, arcHeight) );
  }

  public void fillRoundRect(int x, int y, int width, int height,
                            int arcWidth, int arcHeight)
  {
    fill( new RoundRectangle2D.Double(x, y, width, height,
                                      arcWidth, arcHeight) );
  }

  public void drawOval(int x, int y, int width, int height)
  {
    draw( new Ellipse2D.Double((double)x, (double)y,
                               (double)width, (double)height) );
  }

  public void fillOval(int x, int y, int width, int height)
  {
    fill( new Ellipse2D.Double(x, y, width, height) );
  }

  public void drawArc(int x, int y, int width, int height,
                      int arcStart, int arcAngle)
  {
    draw( new Arc2D.Double(x, y, width, height, arcStart, arcAngle,
                           Arc2D.OPEN) );
  }

  public void fillArc(int x, int y, int width, int height,
                      int arcStart, int arcAngle)
  {
    fill( new Arc2D.Double(x, y, width, height, arcStart, arcAngle,
                           Arc2D.CHORD) );
  }

  public void drawPolyline(int xPoints[], int yPoints[], int npoints)
  {
    for( int i = 0; i < npoints - 1; i++)
      drawLine(xPoints[i], yPoints[i], xPoints[i + 1], yPoints[i + 1]);
  }

  public void drawPolygon(int xPoints[], int yPoints[], int npoints)
  {
    draw( new Polygon(xPoints, yPoints, npoints) );
  }

  public void fillPolygon(int xPoints[], int yPoints[], int npoints)
  {
    fill( new Polygon(xPoints, yPoints, npoints) );
  }

  public native void fill3DRect(int x, int y, int width, int height, boolean raised);

  public native void draw3DRect(int x, int y, int width, int height, boolean raised);

  // *********************** Text rendering *************************

  public void drawString(String string, int x, int y)
  {
    drawStringNative(string, (double)x, (double)y);
  }

  public void drawString(String string, float x, float y)
  {
    drawStringNative(string, (double)x, (double)y);
  }

  public void drawString (AttributedCharacterIterator ci, int x, int y)
  {
    // FIXME - to something more correct ?
    String s = "";
    for(char c = ci.first(); c != CharacterIterator.DONE; c = ci.next())
      s += c;
    drawString(s, x, y);
  }

  public void drawString(AttributedCharacterIterator ci,
                         float x, float y)
  {
    // FIXME - to something more correct ?
    String s = "";
    for(char c = ci.first(); c != CharacterIterator.DONE; c = ci.next())
      s += c;
    drawString(s, x, y);
  }

  public void drawGlyphVector(GlyphVector v, float x, float y)
  {
    throw new RuntimeException("Not implemented");
  }

  // ******************* Image drawing ******************************
  public boolean drawImage(Image image,
                           AffineTransform Tx,
                           ImageObserver obs)
  {
    if (image instanceof QtImage)
      return ((QtImage)image).drawImage(this, new QMatrix( Tx ), obs);

    return (new QtImage(image.getSource())).drawImage(this,
                                                      new QMatrix( Tx ),
                                                      obs);
  }

  public boolean drawImage(Image image, int x, int y, Color bgcolor,
                           ImageObserver observer)
  {
    if (image instanceof QtImage)
      return ((QtImage)image).drawImage (this, x, y, bgcolor, observer);
    return (new QtImage(image.getSource())).drawImage (this, x, y,
                                                       bgcolor, observer);
  }

  public boolean drawImage(Image image,
                           int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           Color bgcolor, ImageObserver observer)
  {
    if (image instanceof QtImage)
      return ((QtImage)image).drawImage(this, dx1, dy1, dx2, dy2,
                                        sx1, sy1, sx2, sy2, bgcolor, observer);

    return (new QtImage(image.getSource())).drawImage(this, dx1, dy1,
                                                      dx2, dy2,
                                                      sx1, sy1, sx2, sy2,
                                                      bgcolor, observer);
  }

  public boolean drawImage(Image image, int x, int y,
                           int width, int height, Color bgcolor,
                           ImageObserver observer)
  {
    if (image instanceof QtImage)
      return ((QtImage)image).drawImage (this, x, y, width, height,
                                         bgcolor, observer);
    return (new QtImage(image.getSource())).drawImage (this, x, y,
                                                       width, height,
                                                       bgcolor, observer);
  }

  public boolean drawImage(Image image, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    return drawImage(image, x, y, width, height, null, observer);
  }

  public boolean drawImage(Image image, int x, int y, ImageObserver observer)
  {
    return drawImage(image, x, y, null, observer);
  }

  public boolean drawImage(Image image, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, ImageObserver observer)
  {
    return drawImage(image, dx1, dy1, dx2, dy2,
                     sx1, sy1, sx2, sy2, null, observer);
  }

  // *********************** Transform methods *************************
  public AffineTransform getTransform()
  {
    return new AffineTransform( xform );
  }

  public void setTransform(AffineTransform Tx)
  {
    xform = new AffineTransform( Tx );
    setQtTransform( new QMatrix( xform ) );
  }

  public void rotate(double theta)
  {
    xform.rotate( theta );
    setQtTransform( new QMatrix( xform ) );
  }

  public void rotate(double theta, double x, double y)
  {
    xform.rotate(theta, x, y);
    setQtTransform( new QMatrix( xform ) );
  }

  public void scale(double sx, double sy)
  {
    xform.scale(sx, sy);
    setQtTransform( new QMatrix( xform ) );
  }

  public void shear(double shx, double shy)
  {
    xform.shear(shx, shy);
    setQtTransform( new QMatrix( xform ) );
  }

  public void transform(AffineTransform Tx)
  {
    xform.concatenate( Tx );
    setQtTransform( new QMatrix( xform ) );
  }

  public void translate(double tx, double ty)
  {
    xform.translate( tx, ty );
    setQtTransform( new QMatrix( xform ) );
  }

  public void translate(int x, int y)
  {
    translate((double)x, (double)y);
  }

  // *************** Stroking, Filling, Compositing *****************
  public void setStroke(Stroke s)
  {
    try  // ..to convert the stroke into a native one.
      {
        QPen pen = new QPen( s );
        nativeStroking = true;
        setNativeStroke( pen );
        setColor( color );
      }
    catch (IllegalArgumentException e)
      {
        nativeStroking = false;
      }
    currentStroke = s;
  }

  public Stroke getStroke()
  { // FIXME: return copy?
    return currentStroke;
  }

  public void setComposite(Composite comp)
  {
    if( comp == null)
      {
        setNativeComposite( AlphaComposite.SRC_OVER );
        return;
      }

    if( comp instanceof AlphaComposite )
      {
        if( ((AlphaComposite)comp).getRule() != AlphaComposite.XOR )
          setAlpha( ((AlphaComposite)comp).getAlpha() );
        setNativeComposite( ((AlphaComposite)comp).getRule() );
        composite = comp;
      }
    else
      {
        // FIXME: this check is only required "if this Graphics2D
        // context is drawing to a Component on the display screen".
        SecurityManager sm = System.getSecurityManager();
        if (sm != null)
          sm.checkPermission(new AWTPermission("readDisplayPixels"));

        throw new UnsupportedOperationException("We don't support custom"+
                                                " composites yet.");
      }
  }

  public Composite getComposite()
  {
    return composite;
  }

  public void setPaint(Paint p)
  {
    if( p == null )
      return;

    // FIXME
    currentPaint = p;
    if( p instanceof GradientPaint )
      {
        GradientPaint lg = (GradientPaint)p;
        setLinearGradient(lg.getColor1().getRed(), lg.getColor1().getGreen(),
                          lg.getColor1().getBlue(), lg.getColor2().getRed(),
                          lg.getColor2().getGreen(), lg.getColor2().getBlue(),
                          lg.getPoint1().getX(), lg.getPoint1().getY(),
                          lg.getPoint2().getX(), lg.getPoint2().getY(),
                          lg.isCyclic() );
        return;
      }
    if( p instanceof Color )
      {
        setColor((Color) p);
        return;
      }
    throw new UnsupportedOperationException("We don't support custom"+
                                            " paints yet.");
  }

  public Paint getPaint()
  {
    // FIXME
    return currentPaint;
  }

  // ********************** Rendering Hints *************************

  public void addRenderingHints(Map hints)
  {
    renderingHints.putAll( hints );
  }

  public Object getRenderingHint(RenderingHints.Key hintKey)
  {
    return renderingHints.get( hintKey );
  }

  public RenderingHints getRenderingHints()
  {
    return (RenderingHints) renderingHints.clone();
  }

  public void setRenderingHints(Map<?,?> hints)
  {
    renderingHints = new RenderingHints( null );
    renderingHints.putAll(hints);
    updateRenderingHints();
  }

  public void setRenderingHint(RenderingHints.Key hintKey, Object hintValue)
  {
    renderingHints.put( hintKey, hintValue );
    updateRenderingHints();
  }

  private void updateRenderingHints()
  {
    // FIXME - update native settings.
  }

  ////////////////////////////// unimplemented /////////////////////

  public FontRenderContext getFontRenderContext()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void drawRenderableImage(RenderableImage image, AffineTransform xform)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void drawImage(BufferedImage image, BufferedImageOp op, int x, int y)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }
}
