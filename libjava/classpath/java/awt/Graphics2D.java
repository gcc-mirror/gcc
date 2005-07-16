/* Copyright (C) 2000, 2002, 2004  Free Software Foundation

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
import java.text.AttributedCharacterIterator;
import java.util.Map;

/**
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

  public abstract void drawString(String text, int x, int y);

  public abstract void drawString(String text, float x, float y);
    
  public abstract void drawString(AttributedCharacterIterator iterator,
                                  int x, int y);

  public abstract void drawString(AttributedCharacterIterator iterator,
				  float x, float y);

  // public abstract void drawGlyphVector(GlyphVector g, float x, float y);

  public abstract void fill(Shape shape);
    
  public abstract boolean hit(Rectangle rect, Shape text,
			      boolean onStroke);

  public abstract GraphicsConfiguration getDeviceConfiguration();

  public abstract void setComposite(Composite comp);
    
  public abstract void setPaint(Paint paint);

  public abstract void setStroke(Stroke stroke);

  public abstract void setRenderingHint(RenderingHints.Key hintKey,
                                        Object hintValue);

  public abstract Object getRenderingHint(RenderingHints.Key hintKey);
  
  public abstract void setRenderingHints(Map hints);

  public abstract void addRenderingHints(Map hints);

  public abstract RenderingHints getRenderingHints();

  public abstract void translate(int x, int y);

  public abstract void translate(double tx, double ty);
    
  public abstract void rotate(double theta);

  public abstract void rotate(double theta, double x, double y);

  public abstract void scale(double scaleX, double scaleY);

  public abstract void shear(double shearX, double shearY);

  public abstract void transform(AffineTransform Tx);
  
  public abstract void setTransform(AffineTransform Tx);

  public abstract AffineTransform getTransform();

  public abstract Paint getPaint();

  public abstract Composite getComposite();

  public abstract void setBackground(Color color);

  public abstract Color getBackground();

  public abstract Stroke getStroke();    

  public abstract void clip(Shape s);

  public abstract FontRenderContext getFontRenderContext ();

  public abstract void drawGlyphVector (GlyphVector g, float x, float y);
}
