/* Copyright (C) 2000, 2003  Free Software Foundation

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
import java.awt.image.Raster;
import java.awt.image.ImageObserver;

/**
 * Interface for a simple pixel based backend graphics object that
 * does not handle translation/transforms, curves, nor advanced
 * compositing.
 */
public interface DirectRasterGraphics extends Cloneable
{
  void dispose();
  
  void setColor(Color color);
  
  void setPaintMode();
  
  void setXORMode(Color altColor);
  
  void setFont(Font font);
  
  FontMetrics getFontMetrics(Font font);
  
  // supports rects, multi-rects and polygons
  void setClip(Shape clip);
  
  void copyArea(int x, int y, int width, int height,
		       int dx, int dy);
  
  void drawLine(int x1, int y1, int x2, int y2);
  
  void drawRect(int x, int y, int width, int height);
  
  void fillRect(int x, int y, int width, int height);
  
  void drawArc(int x, int y, int width, int height,
		      int startAngle, int arcAngle);
  
  void fillArc(int x, int y, int width, int height,
		      int startAngle, int arcAngle);
  
  void drawPolyline(int[] xPoints, int[] yPoints, int nPoints);
  
  void drawPolygon(int[] xPoints, int[] yPoints, int nPoints);
  
  void fillPolygon(int[] xPoints, int[] yPoints, int nPoints,
			  int translateX, int translateY);
  
  void drawString(String str, int x, int y);
  
  boolean drawImage(Image image, int x, int y,
			   ImageObserver observer);

  /**
   * Map the data for screen pixels in the requested bounds to a
   * raster object.  This gives read/write access to the screen
   * pixels, allowing neat alpha and composite tricks.
   */
  MappedRaster mapRaster(Rectangle bounds);
  
  /**
   * Detach previously mapped pixel data from a raster object.
   */
  void unmapRaster(MappedRaster mappedRaster);
  
  Object clone();
}
