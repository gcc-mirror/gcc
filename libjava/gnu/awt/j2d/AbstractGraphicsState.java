/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.j2d;

import java.awt.Color;
import java.awt.Image;
import java.awt.Shape;
import java.awt.Rectangle;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.image.ImageObserver;

/**
 * Base class for graphics state objects (State pattern, GOF book)
 * that represents the current pipeline configuration. The Graphics2D
 * object forwards most of the requests to the state object. The
 * Graphics2D object itself only administers properties that are not
 * specific for a certain state.
 */
public abstract class AbstractGraphicsState implements Cloneable
{
  Graphics2DImpl frontend;

  public void setFrontend(Graphics2DImpl frontend)
  {
    this.frontend = frontend;
  }
  
  public void dispose()
  {
    frontend = null;
  }

  // -------- Graphics methods:
  
  public abstract void setColor(Color color);

  public abstract void setPaintMode();

  public abstract void setXORMode(Color altColor);

  public abstract void setFont(Font font);

  public abstract FontMetrics getFontMetrics(Font font);

  public abstract void setClip(Shape clip);

  public abstract Shape getClip();
  public abstract Rectangle getClipBounds();

  public abstract void copyArea(int x, int y, 
				int width, int height,
				int dx, int dy);

  public abstract void drawLine(int x1, int y1,
				int x2, int y2);
  
  public abstract void fillRect(int x, int y,
				int width, int height);
  
  public abstract void clearRect(int x, int y,
				 int width, int height);
  
  public abstract void drawRoundRect(int x, int y,
				     int width, int height,
				     int arcWidth, int arcHeight);
  
  public abstract void fillRoundRect(int x, int y,
				     int width, int height,
				     int arcWidth, int arcHeight);
  
  public abstract void drawOval(int x, int y,
				int width, int height);
  
  public abstract void fillOval(int x, int y,
				int width, int height);
  
  public abstract void drawArc(int x, int y,
			       int width, int height,
			       int startAngle, int arcAngle);
  
  public abstract void fillArc(int x, int y,
			       int width, int height,
			       int startAngle, int arcAngle);
  
  public abstract void drawPolyline(int[] xPoints, int[] yPoints,int nPoints);

  public abstract void drawPolygon(int[] xPoints, int[] yPoints, int nPoints);
    
  public abstract void fillPolygon(int[] xPoints, int[] yPoints, int nPoints);

  public abstract boolean drawImage(Image image, int x, int y,
				    ImageObserver observer);
    

  // -------- Graphics2D methods:

  public abstract void draw(Shape shape);

  public abstract void fill(Shape shape);
    
  public abstract boolean hit(Rectangle rect, Shape text, boolean onStroke);
  
  public abstract void drawString(String text, int x, int y);
  
  public abstract void drawString(String text, float x, float y);
  
  public abstract void translate(int x, int y);
  
  public abstract void translate(double tx, double ty);
  
  public abstract void rotate(double theta);
  
  public abstract void rotate(double theta, double x, double y);
  
  public abstract void scale(double scaleX, double scaleY);
  
  public abstract void shear(double shearX, double shearY);

  public Object clone ()
  {
    try
      {
	return super.clone ();
      }
    catch (CloneNotSupportedException ex)
      {
	// This should never happen.
	throw new InternalError ();
      }
  }
}
