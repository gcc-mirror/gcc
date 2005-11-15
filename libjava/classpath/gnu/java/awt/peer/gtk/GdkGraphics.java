/* GdkGraphics.java
   Copyright (C) 1998, 1999, 2002, 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import gnu.classpath.Configuration;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.SystemColor;
import java.awt.image.ImageObserver;
import java.text.AttributedCharacterIterator;

public class GdkGraphics extends Graphics
{
  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("gtkpeer");
      }
    initStaticState ();
  }
  
  static native void initStaticState();
  private final int native_state = GtkGenericPeer.getUniqueInteger ();

  Color color, xorColor;
  GtkComponentPeer component;
  Font font = new Font ("Dialog", Font.PLAIN, 12);
  Rectangle clip;
  GtkImage image; 

  int xOffset = 0;
  int yOffset = 0;

  static final int GDK_COPY = 0, GDK_XOR = 2;

  native void initState (GtkComponentPeer component);
  native void initStateUnlocked (GtkComponentPeer component);
  native void initState (int width, int height);
  native void initFromImage (GtkImage image);
  native void copyState (GdkGraphics g);

  GdkGraphics (GdkGraphics g)
  {
    color = g.color;
    xorColor = g.xorColor;
    font = g.font;
    if (font == null)
      font = new Font ("Dialog", Font.PLAIN, 12);
    clip = new Rectangle (g.clip);
    component = g.component;

    copyState (g);
  }

  GdkGraphics (int width, int height)
  {
    initState (width, height);
    color = Color.black;
    clip = new Rectangle (0, 0, width, height);
    font = new Font ("Dialog", Font.PLAIN, 12);
  }

  GdkGraphics (GtkImage image)
  {
    this.image = image;
    initFromImage (image);
    color = Color.black;
    clip = new Rectangle (0, 0, 
			  image.getWidth(null), image.getHeight(null));
    font = new Font ("Dialog", Font.PLAIN, 12);
  }

  GdkGraphics (GtkComponentPeer component)
  {
    this.component = component;
    color = Color.black;

    if (component.isRealized ())
      initComponentGraphics ();
    else
      connectSignals (component);
  }

  void initComponentGraphics ()
  {
    initState (component);
    color = component.awtComponent.getForeground ();
    if (color == null)
      color = Color.BLACK;
    Dimension d = component.awtComponent.getSize ();
    clip = new Rectangle (0, 0, d.width, d.height);
  }

  // called back by native side: realize_cb
  void initComponentGraphicsUnlocked ()
  {
    initStateUnlocked (component);
    color = component.awtComponent.getForeground ();
    if (color == null)
      color = Color.BLACK;
    Dimension d = component.awtComponent.getSize ();
    clip = new Rectangle (0, 0, d.width, d.height);
  }

  native void connectSignals (GtkComponentPeer component);

  public native void clearRect(int x, int y, int width, int height);

  public void clipRect (int x, int y, int width, int height)
  {
    if (component != null && ! component.isRealized ())
      return;

    clip = clip.intersection (new Rectangle (x, y, width, height));
    setClipRectangle (clip.x, clip.y, clip.width, clip.height);
  }

  public native void copyArea(int x, int y, int width, int height, 
			      int dx, int dy);

  public Graphics create ()
  {
    return new GdkGraphics (this);
  }

  public native void dispose();

  public boolean drawImage (Image img, int x, int y, 
			    Color bgcolor, ImageObserver observer)
  {
    return drawImage(img, x, y, img.getWidth(null), img.getHeight(null),
		     bgcolor, observer);
  }

  public boolean drawImage (Image img, int x, int y, ImageObserver observer)
  {
    return drawImage (img, x, y, null, observer);
  }

  public boolean drawImage (Image img, int x, int y, int width, int height, 
			    Color bgcolor, ImageObserver observer)
  {
    if (img instanceof GtkImage)
      return ((GtkImage)img).drawImage (this, x, y, width, height, 
					bgcolor, observer);
    else
      return (new GtkImage(img.getSource())).drawImage (this, x, y, 
							width, height, 
							bgcolor, observer);
  }

  public boolean drawImage (Image img, int x, int y, int width, int height, 
			    ImageObserver observer)
  {
    return drawImage (img, x, y, width, height,  null, observer);
  }

  public boolean drawImage (Image img, int dx1, int dy1, int dx2, int dy2, 
			    int sx1, int sy1, int sx2, int sy2, 
			    Color bgcolor, ImageObserver observer)
  {
    if (img instanceof GtkImage)
      return ((GtkImage)img).drawImage(this, dx1, dy1, dx2, dy2, 
				       sx1, sy1, sx2, sy2, bgcolor, observer);
    else
      return (new GtkImage(img.getSource())).drawImage(this, dx1, dy1, 
						       dx2, dy2, 
						       sx1, sy1, sx2, sy2, 
						       bgcolor, observer);
  }

  public boolean drawImage (Image img, int dx1, int dy1, int dx2, int dy2, 
			    int sx1, int sy1, int sx2, int sy2, 
			    ImageObserver observer) 
  {
    return drawImage (img, dx1, dy1, dx2, dy2, 
		      sx1, sy1, sx2, sy2, 
		      null, observer);
  }

  public native void drawLine(int x1, int y1, int x2, int y2);

  public native void drawArc(int x, int y, int width, int height,
			     int startAngle, int arcAngle);
  public native void fillArc(int x, int y, int width, int height, 
			     int startAngle, int arcAngle);
  public native void drawOval(int x, int y, int width, int height);
  public native void fillOval(int x, int y, int width, int height);

  public native void drawPolygon(int[] xPoints, int[] yPoints, int nPoints);
  public native void fillPolygon(int[] xPoints, int[] yPoints, int nPoints);

  public native void drawPolyline(int[] xPoints, int[] yPoints, int nPoints);

  public native void drawRect(int x, int y, int width, int height);
  public native void fillRect(int x, int y, int width, int height);

  GdkFontPeer getFontPeer() 
  {
    return (GdkFontPeer) getFont().getPeer(); 
  }

  native void drawString (GdkFontPeer f, String str, int x, int y);
  public void drawString (String str, int x, int y)
  {
    drawString(getFontPeer(), str, x, y);
  }
  

  public void drawString (AttributedCharacterIterator ci, int x, int y)
  {
    throw new Error ("not implemented");
  }

  public void drawRoundRect(int x, int y, int width, int height, 
			    int arcWidth, int arcHeight)
  {
    if (arcWidth > width)
      arcWidth = width;
    if (arcHeight > height)
      arcHeight = height;

    int xx = x + width - arcWidth;
    int yy = y + height - arcHeight;

    drawArc (x, y, arcWidth, arcHeight, 90, 90);
    drawArc (xx, y, arcWidth, arcHeight, 0, 90);
    drawArc (xx, yy, arcWidth, arcHeight, 270, 90);
    drawArc (x, yy, arcWidth, arcHeight, 180, 90);

    int y1 = y + arcHeight / 2;
    int y2 = y + height - arcHeight / 2;
    drawLine (x, y1, x, y2);
    drawLine (x + width, y1, x + width, y2);

    int x1 = x + arcWidth / 2;
    int x2 = x + width - arcWidth / 2;
    drawLine (x1, y, x2, y);
    drawLine (x1, y + height, x2, y + height);
  }

  public void fillRoundRect (int x, int y, int width, int height, 
			     int arcWidth, int arcHeight)
  {
    if (arcWidth > width)
      arcWidth = width;
    if (arcHeight > height)
      arcHeight = height;

    int xx = x + width - arcWidth;
    int yy = y + height - arcHeight;

    fillArc (x, y, arcWidth, arcHeight, 90, 90);
    fillArc (xx, y, arcWidth, arcHeight, 0, 90);
    fillArc (xx, yy, arcWidth, arcHeight, 270, 90);
    fillArc (x, yy, arcWidth, arcHeight, 180, 90);

    fillRect (x, y + arcHeight / 2, width, height - arcHeight + 1);
    fillRect (x + arcWidth / 2, y, width - arcWidth + 1, height);
  }

  public Shape getClip ()
  {
    return getClipBounds ();
  }

  public Rectangle getClipBounds ()
  {
    if (clip == null)
      return null;
    else
      return clip.getBounds();
  }

  public Color getColor ()
  {
    return color;
  }

  public Font getFont ()
  {
    return font;
  }

  public FontMetrics getFontMetrics (Font font)
  {
    return new GdkFontMetrics (font);
  }

  native void setClipRectangle (int x, int y, int width, int height);

  public void setClip (int x, int y, int width, int height)
  {
    if ((component != null && ! component.isRealized ())
        || clip == null)
      return;

    clip.x = x;
    clip.y = y;
    clip.width = width;
    clip.height = height;
    
    setClipRectangle (x, y, width, height);
  }

  public void setClip (Rectangle clip)
  {
    setClip (clip.x, clip.y, clip.width, clip.height);
  }

  public void setClip (Shape clip)
  {
    if (clip == null)
      {
	// Reset clipping.
	Dimension d = component.awtComponent.getSize();
	setClip(new Rectangle (0, 0, d.width, d.height));
      }
    else
      setClip(clip.getBounds());
  }

  private native void setFGColor(int red, int green, int blue);

  public void setColor (Color c)
  {
    if (c == null)
      color = Color.BLACK;
    else
      color = c;

    if (xorColor == null) /* paint mode */
      setFGColor (color.getRed (), color.getGreen (), color.getBlue ());
    else		  /* xor mode */
      setFGColor (color.getRed   () ^ xorColor.getRed (),
		  color.getGreen () ^ xorColor.getGreen (),
		  color.getBlue  () ^ xorColor.getBlue ());
  }
  
  public void setFont (Font font)
  {
    if (font != null)
      this.font = font;
  }

  native void setFunction (int gdk_func);

  public void setPaintMode ()
  {
    xorColor = null;

    setFunction (GDK_COPY);
    setFGColor (color.getRed (), color.getGreen (), color.getBlue ());
  }

  public void setXORMode (Color c)
  {
    xorColor = c;

    setFunction (GDK_XOR);
    setFGColor (color.getRed   () ^ xorColor.getRed (),
		color.getGreen () ^ xorColor.getGreen (),
		color.getBlue  () ^ xorColor.getBlue ());
  }

  public native void translateNative(int x, int y);

  public void translate (int x, int y)
  {
    if (component != null && ! component.isRealized ())
      return;

    clip.x -= x;
    clip.y -= y;

    translateNative (x, y);
  }
}
