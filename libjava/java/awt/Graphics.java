/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.image.ImageObserver;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 15, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Almost complete
 */

public abstract class Graphics
{
  protected Graphics() {}
  
  public abstract Graphics create();

  public Graphics create(int x, int y, int width, int height)
  {
    Graphics gfx = create();
    gfx.translate(x, y);
    gfx.setClip(0, y, width, height);
    return gfx;
  }

  public abstract void translate(int x, int y);

  public abstract Color getColor();

  public abstract void setColor(Color color);

  public abstract void setPaintMode();

  public abstract void setXORMode(Color altColor);

  public abstract Font getFont();

  public abstract void setFont(Font font);

  public FontMetrics getFontMetrics()
  {
    return getFontMetrics(getFont());
  }

  public abstract FontMetrics getFontMetrics(Font font);

  public abstract Rectangle getClipBounds();

  public abstract void clipRect(int x, int y, int width, int height);

  public abstract void setClip(int x, int y, int width, int height);

  public abstract Shape getClip();

  public abstract void setClip(Shape clip);

  public abstract void copyArea(int x, int y, int width, int height,
				int dx, int dy);

  public abstract void drawLine(int x1, int y1, int x2, int y2);
    
  public abstract void fillRect(int x, int y, int width, int height);

  public void drawRect(int x, int y, int width, int height)
  {
    int x1 = x;
    int y1 = y;
    int x2 = x + width;
    int y2 = y + height;
    drawLine(x1, y1, x2, y1);
    drawLine(x2, y1, x2, y2);
    drawLine(x2, y2, x1, y2);
    drawLine(x1, y2, x1, y1);
  }

  public abstract void clearRect(int x, int y, int width, int height);
  
  public abstract void drawRoundRect(int x, int y, int width, int height,
				     int arcWidth, int arcHeight);
  
  public abstract void fillRoundRect(int x, int y, int width, int height,
				     int arcWidth, int arcHeight);
  
  public void draw3DRect(int x, int y, int width, int height,
			 boolean raised)
  {
    Color color = getColor();
    Color tl = color.brighter();
    Color br = color.darker();
    
    if (!raised)
      {
	Color tmp = tl;
	tl = br;
	br = tmp;
      }
    
    int x1 = x;
    int y1 = y;
    int x2 = x + width;
    int y2 = y + height;
    
    setColor(tl);
    drawLine(x1, y1, x2, y1);
    drawLine(x1, y2, x1, y1);
    setColor(br);
    drawLine(x2, y1, x2, y2);
    drawLine(x2, y1, x1, y2);
    setColor(color);
  }

  public void fill3DRect(int x, int y, int width, int height,
			 boolean raised)
  {
    fillRect(x, y, width, height);
    draw3DRect(x, y, width-1, height-1, raised);
  }

  public abstract void drawOval(int x, int y, int width, int height);
  
  public abstract void fillOval(int x, int y, int width, int height);
  
  public abstract void drawArc(int x, int y, int width, int height,
			       int startAngle, int arcAngle);
  
  public abstract void fillArc(int x, int y, int width, int height,
			       int startAngle, int arcAngle);
  
  public abstract void drawPolyline(int[] xPoints, int[] yPoints,
				    int nPoints);
  
  public abstract void drawPolygon(int[] xPoints, int[] yPoints,
				   int nPoints);
  
  //public void drawPolygon(Polygon p);
    
  public abstract void fillPolygon(int[] xPoints, int[] yPoints,
				   int nPoints);

  //public void fillPolygon(Polygon p);

  public abstract void drawString(String str, int x, int y);

  /*
  public abstract void drawString(AttributedCharacterIterator iterator,
	        		  int x, int y)
  */

  public void drawChars(char[] data, int offset, int length,
			int x, int y)
  {
    String str = new String(data, offset, length);
    drawString(str, x, y);
  }

  public void drawBytes(byte[] data, int offset, int length,
			int x, int y)
  {
    String str = new String(data, offset, length);
    drawString(str, x, y);
  }

  public abstract boolean drawImage(Image img, int x, int y,
				    ImageObserver observer);

  public abstract boolean drawImage(Image img, int x, int y,
				    int width, int height,
				    ImageObserver observer);

  public abstract boolean drawImage(Image img, int x, int y, Color bgcolor,
				    ImageObserver observer);
  
  public abstract boolean drawImage(Image img, int x, int y,
				    int width, int height, Color bgcolor,
				    ImageObserver observer);

  public abstract boolean drawImage(Image img,
				    int dx1, int dy1, int dx2, int dy2,
				    int sx1, int sy1, int sx2, int sy2,
				    ImageObserver observer);

  public abstract boolean drawImage(Image img,
				    int dx1, int dy1, int dx2, int dy2,
				    int sx1, int sy1, int sx2, int sy2,
				    Color bgcolor, ImageObserver observer);

  public abstract void dispose();
  
  public void finalize()
  {
    dispose();
  }

  public String toString()
  {
    return super.toString(); // FIXME
  }
    
  /** @deprecated */
  public Rectangle getClipRect()
  {
    return getClipBounds(null);
  }

  public boolean hitClip(int x, int y, int width, int height)
  {
    throw new UnsupportedOperationException("not implemented yet");
  }

  public Rectangle getClipBounds(Rectangle r)
  {
    Rectangle clipBounds = getClipBounds();

    if (r == null)
      return clipBounds;

    r.x      = clipBounds.x;
    r.y      = clipBounds.y;
    r.width  = clipBounds.width;
    r.height = clipBounds.height;
    return r;
  }
}
