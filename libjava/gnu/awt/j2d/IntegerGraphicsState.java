/* Copyright (C) 2000  Free Software Foundation

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
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.awt.image.ColorModel;

/**
 * IntegerGraphicsState is one of several graphics state
 * implementations.  This graphics state is used when the graphics
 * object has simple properties, (coordinate translation only, no
 * transform) and the backend supports integer coordinates (pixel
 * based). For primitive paint operations, this object translates the
 * coordinates and forwards the request to the backend. For requests
 * to draw arbitrary shapes and paths, this object translates the
 * requests to primitive drawing operations supported by the
 * backend. IntegerGraphicsState is meant to support the most common
 * state of an graphics object. The degree of functionality is roughly
 * equivalent with the old java.awt.Graphics API.
 */
public class IntegerGraphicsState extends AbstractGraphicsState
{
  int tx;
  int ty;
  
  DirectRasterGraphics directGfx;
  Shape clip;
  
  public IntegerGraphicsState(DirectRasterGraphics directGfx)
  {
    this.directGfx = directGfx;
  }
  
  public Object clone()
  {
    IntegerGraphicsState clone = (IntegerGraphicsState) super.clone();
    clone.directGfx = (DirectRasterGraphics) directGfx.clone();
    
    return clone;
  }

  public void dispose()
  {
    DirectRasterGraphics lDeviceGfx = directGfx;
    
    directGfx = null;
    
    if (lDeviceGfx != null)
      lDeviceGfx.dispose();
    
    super.dispose();
  }
  
  // -------- Graphics methods:
  
  public void setColor(Color color)
  {
    directGfx.setColor(color);
  }
  
  public void setPaintMode()
  {
    directGfx.setPaintMode();
  }

  public void setXORMode(Color altColor)
  {
    directGfx.setXORMode(altColor);
  }
  
  public void setFont(Font font)
  {
    directGfx.setFont(font);
  }
  
  public FontMetrics getFontMetrics(Font font)
  {
    return directGfx.getFontMetrics(font);
  }

  public void setClip(Shape clip)
  {
    if (clip instanceof Rectangle)
      {
	Rectangle clipRect = (Rectangle) ((Rectangle) clip).clone();
	clipRect.x += tx;
	clipRect.y += ty;
	
	this.clip = clipRect;
	
	directGfx.setClip(clipRect);
	return;
      }
    
    String msg =
      "translating clip shape " + clip + " into device " +
      "coordinate space has not been implemented yet";
    
    throw new UnsupportedOperationException(msg);
  }

  public Shape getClip()
  {
    if (clip instanceof Rectangle)
      {
	Rectangle clipRect = (Rectangle) clip;
	clipRect.x -= tx;
	clipRect.y -= ty;
	return clipRect;
      }

    String msg =
      "translating clip shape " + clip + " into user " +
      "coordinate space has not been implemented yet";
    
    throw new UnsupportedOperationException(msg);
  }

  public Rectangle getClipBounds()
  {
    Rectangle clipRect = clip.getBounds();
    
    clipRect.x -= tx;
    clipRect.y -= ty;
    return clipRect;
  }

  public void copyArea(int x, int y, 
		       int width, int height,
		       int dx, int dy)
  {
    directGfx.copyArea(x+tx, y+ty, width, height, dx, dy);
  }

  public void drawLine(int x1, int y1,
		       int x2, int y2)
  {
    directGfx.drawLine(x1+tx, y1+ty, x2+tx, y2+ty);
  }
    
  public void fillRect(int x, int y,
		       int width, int height)
  {
    directGfx.fillRect(x+tx, y+ty, width, height);
  }
    
  public void clearRect(int x, int y,
			int width, int height)
  {
    directGfx.setColor(frontend.getBackground());
    directGfx.fillRect(x+tx, y+ty, width, height);
    directGfx.setColor(frontend.getColor());
  }

  public void drawRoundRect(int x, int y,
			    int width, int height,
			    int arcWidth, int arcHeight)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public void fillRoundRect(int x, int y,
			    int width, int height,
			    int arcWidth, int arcHeight)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void drawOval(int x, int y,
		       int width, int height)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public void fillOval(int x, int y,
		       int width, int height)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public void drawArc(int x, int y,
		      int width, int height,
		      int startAngle, int arcAngle)
  {
    directGfx.drawArc(x+tx, y+ty, width, height, startAngle, arcAngle);
  }    

  public void fillArc(int x, int y,
		      int width, int height,
		      int startAngle, int arcAngle)
  {
    directGfx.fillArc(x+tx, y+ty, width, height, startAngle, arcAngle);
  }
  
  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints)
  {
    if ((tx == 0) || (ty == 0))
      {
	directGfx.drawPolyline(xPoints, yPoints, nPoints);
	return;
      }
	    
    throw new UnsupportedOperationException("translate not implemented");
  }

  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    if ((tx == 0) || (ty == 0))
      {
	directGfx.drawPolygon(xPoints, yPoints, nPoints);
	return;
      }

    throw new UnsupportedOperationException("translate not implemented");
  }
  
  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints)
  {
    if ((tx == 0) || (ty == 0))
      {
	directGfx.fillPolygon(xPoints, yPoints, nPoints);
	return;
      }
    
    throw new UnsupportedOperationException("translate not implemented");
  }

  public boolean drawImage(Image image, int x, int y,
			   ImageObserver observer)
  {
    x += tx;
    y += ty;
    
    if (image instanceof BufferedImage)
      {
	BufferedImage bImage = (BufferedImage) image;
	Object config =
	  bImage.getProperty("java.awt.GraphicsConfiguration");
	
	if (config == frontend.config)
	  return directGfx.drawImage(image, x, y, observer);
	
	int width = image.getWidth(null);
	int height = image.getHeight(null);	
	
	Rectangle bounds = new Rectangle(x, y, width, height);
	
	MappedRaster mr = directGfx.mapRaster(bounds);
	
	// manipulate raster here...
	ColorModel colorModel = mr.getColorModel();
	WritableRaster raster = mr.getRaster();
	
	int xEnd = x + width;
	int yEnd = y + height;
	
	// FIXME: Use the following code only as a fallback. It's SLOW!

	Object rgbElem = null;
	for (int yy=0; yy<height; yy++)
	  {
	    for (int xx=0; xx<width; xx++)
	      {
		int srgb = bImage.getRGB(xx, yy);
		int sa = ((srgb >>> 24) & 0xff) + 1;
		int sr = ((srgb >>> 16) & 0xff) + 1;
		int sg = ((srgb >>> 8) & 0xff) + 1;
		int sb = (srgb & 0xff) + 1;
		
		rgbElem = raster.getDataElements(xx+x, yy+y, rgbElem);
		int drgb = colorModel.getRGB(rgbElem);
		int dr = ((drgb >>> 16) & 0xff) + 1;
		int dg = ((drgb >>> 8) & 0xff) + 1;
		int db = (drgb & 0xff) + 1;		    
		int da = 256 - sa;
		
		dr = ((sr*sa + dr*da) >>> 8) - 1;
		dg = ((sg*sa + dg*da) >>> 8) - 1;
		db = ((sb*sa + db*da) >>> 8) - 1;
		
		drgb = (dr<<16) | (dg<<8) | db;
		
		rgbElem = colorModel.getDataElements(drgb, rgbElem);
		
		raster.setDataElements(xx+x, yy+y, rgbElem);
	      }
	  }
	directGfx.unmapRaster(mr);
	return true;
	
      }
    throw new UnsupportedOperationException("drawing image " + image +
					    "not implemented");
  }
  

  // -------- Graphics2D methods:
  
  public void draw(Shape shape)
  {
    if (shape instanceof Rectangle)
      {
	Rectangle rect = (Rectangle) shape;
	directGfx.drawRect(rect.x+tx, rect.y+ty, rect.width, rect.height);
	return;
      } 
    
    throw new UnsupportedOperationException("shape not implemented");
  }

  public void fill(Shape shape)
  {
    if (shape instanceof Rectangle)
      {
	Rectangle rect = (Rectangle) shape;
	directGfx.fillRect(rect.x+tx, rect.y+ty, rect.width, rect.height);
	return;
      }
    
    throw new UnsupportedOperationException("not implemented");
  }
    
  public boolean hit(Rectangle rect, Shape text,
		     boolean onStroke)
  {
    throw new UnsupportedOperationException("not implemented");
  }

  public void drawString(String text, int x, int y)
  {
    directGfx.drawString(text, x+tx, y+ty);
  }

  public void drawString(String text, float x, float y)
  {
    drawString(text, (int) x, (int) y);
  }
  
  public void translate(int x, int y)
  {
    tx += x;
    ty += y;
  }
    
  public void translate(double tx, double ty)
  {
    if ((tx == 0) && (ty == 0))
      return;
    
    needAffineTransform();
  }

  public void rotate(double theta)
  {
    if (theta == 0)
      return;
    
    needAffineTransform();
  }

  public void rotate(double theta, double x, double y)
  {
    if (theta == 0)
      return;
    
    needAffineTransform();
  }

  public void scale(double scaleX, double scaleY)
  {
    if ((scaleX == 1) && (scaleY == 1))
      return;

    needAffineTransform();
  }
  
  public void shear(double shearX, double shearY)
  {
    if ((shearX == 0) && (shearY == 0))
      return;
    
    needAffineTransform();
  }   
  
  private void needAffineTransform()
  {
    throw new UnsupportedOperationException("state with affine " +
					    "transform not implemented");
  }
}
