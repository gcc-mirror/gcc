/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.geom.AffineTransform;
import java.awt.image.ImageObserver;

//import java.util.Map;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
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

  /*
  public abstract void drawImage(BufferedImage image,
				 BufferedImageOp op,
				 int x,
				 int y);
  */

  /*
  public abstract void drawRenderedImage(RenderedImage image,
					 AffineTransform xform);
  */

  /*
  public abstract void drawRenderableImage(RenderableImage image,
                                           AffineTransform xform);
  */

  public abstract void drawString(String text, int x, int y);

  public abstract void drawString(String text, float x, float y);
    
  /*
  public abstract void drawString(AttributedCharacterIterator iterator,
                                  int x, int y);
  */

  /*
  public abstract void drawString(AttributedCharacterIterator iterator,
				  float x, float y);
  */

  /*
  public abstract void drawGlyphVector(GlyphVector g, float x, float y);
  */

  public abstract void fill(Shape shape);
    
  public abstract boolean hit(Rectangle rect, Shape text,
			      boolean onStroke);

  public abstract GraphicsConfiguration getDeviceConfiguration();

  //public abstract void setComposite(Composite comp);
    
  public abstract void setPaint(Paint paint);

  //public abstract void setStroke(Stroke stroke)

  public abstract void setRenderingHint(RenderingHints.Key hintKey,
                                        Object hintValue);

  public abstract Object getRenderingHint(RenderingHints.Key hintKey);
  
  //public abstract void setRenderingHints(Map hints);

  //public abstract void addRenderingHints(Map hints);

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

  //public abstract Composite getComposite();

  public abstract void setBackground(Color color);

  public abstract Color getBackground();

  //public abstract Stroke getStroke();    

  public abstract void clip(Shape s);

  //public abstract FontRenderContext getFontRenderContext()
}
