/* GdkGraphics.java
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.image.ImageObserver;
import java.text.AttributedCharacterIterator;

public class GdkGraphics extends Graphics
{
  private final int native_state = GtkGenericPeer.getUniqueInteger();

  Color color, xorColor;
  GtkComponentPeer component;
  Font font;
  Rectangle clip;

  int xOffset = 0;
  int yOffset = 0;

  static final int GDK_COPY = 0, GDK_XOR = 2;

  native int[] initState (GtkComponentPeer component);
  native void initState (int width, int height);
  native void copyState (GdkGraphics g);

  GdkGraphics (GdkGraphics g)
  {
    color = g.color;
    xorColor = g.xorColor;
    font = g.font;
    clip = new Rectangle (g.clip);
    component = g.component;

    copyState (g);
  }

  GdkGraphics (int width, int height)
  {
    initState (width, height);
    color = Color.black;
    clip = new Rectangle (0, 0, width, height);
    font = new Font ("Dialog", Font.PLAIN, 10);
  }

  GdkGraphics (GtkComponentPeer component)
  {
    this.component = component;
    int rgb[] = initState (component);
    color = new Color (rgb[0], rgb[1], rgb[2]);
    font = new Font ("Dialog", Font.PLAIN, 10);
    Dimension d = component.awtComponent.getSize ();
    clip = new Rectangle (0, 0, d.width, d.height);
  }

  public native void clearRect (int x, int y, int width, int height);

  public void clipRect (int x, int y, int width, int height)
  {
    clip = clip.intersection (new Rectangle (x, y, width, height));
    setClipRectangle (clip.x, clip.y, clip.width, clip.height);
  }

  native public void copyArea (int x, int y, int width, int height, 
			       int dx, int dy);

  public Graphics create ()
  {
    return new GdkGraphics (this);
  }

//    public Graphics create (int x, int y, int width, int height)
//    {
//      GdkGraphics g = new GdkGraphics (this);
//      System.out.println ("translating by: " + x +" " + y);
//      g.translate (x, y);
//      g.clipRect (0, 0, width, height);

//      return g;
//    }
  
  native public void dispose ();

  native void copyPixmap (Graphics g, int x, int y, int width, int height);
  public boolean drawImage (Image img, int x, int y, 
			    Color bgcolor, ImageObserver observer)
  {
    if (img instanceof GtkOffScreenImage)
      {
	copyPixmap (img.getGraphics (), 
		    x, y, img.getWidth (null), img.getHeight (null));
	return true;
      }

    GtkImage image = (GtkImage) img;
    new GtkImagePainter (image, this, x, y, -1, -1, bgcolor);
    return image.isLoaded ();
  }

  public boolean drawImage (Image img, int x, int y, ImageObserver observer)
  {
    if (img instanceof GtkOffScreenImage)
      {
	copyPixmap (img.getGraphics (), 
		    x, y, img.getWidth (null), img.getHeight (null));
	return true;
      }

    return drawImage (img, x, y, component.getBackground (), observer);
  }

  public boolean drawImage (Image img, int x, int y, int width, int height, 
			    Color bgcolor, ImageObserver observer)
  {
    if (img instanceof GtkOffScreenImage)
      {
	throw new RuntimeException ();
      }

    GtkImage image = (GtkImage) img;
    new GtkImagePainter (image, this, x, y, width, height, bgcolor);
    return image.isLoaded ();
  }

  public boolean drawImage (Image img, int x, int y, int width, int height, 
			    ImageObserver observer)
  {
    return drawImage (img, x, y, width, height, component.getBackground (),
		      observer);
  }

  public boolean drawImage (Image img, int dx1, int dy1, int dx2, int dy2, 
			    int sx1, int sy1, int sx2, int sy2, 
			    Color bgcolor, ImageObserver observer)
  {
    if (img instanceof GtkOffScreenImage)
      {
	throw new RuntimeException ();
      }

    GtkImage image = (GtkImage) img;
    new GtkImagePainter (image, this, dx1, dy1, dx2, dy2, 
			 sx1, sy1, sx2, sy2, bgcolor);
    return image.isLoaded ();
  }

  public boolean drawImage (Image img, int dx1, int dy1, int dx2, int dy2, 
			    int sx1, int sy1, int sx2, int sy2, 
			    ImageObserver observer) 
  {
    return drawImage (img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2,
		      component.getBackground (), observer);
  }

  native public void drawLine (int x1, int y1, int x2, int y2);

  native public void drawArc (int x, int y, int width, int height,
			      int startAngle, int arcAngle);
  native public void fillArc (int x, int y, int width, int height, 
			      int startAngle, int arcAngle);
  native public void drawOval(int x, int y, int width, int height);
  native public void fillOval(int x, int y, int width, int height);

  native public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints);
  native public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints);

  native public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints);

  native public void drawRect(int x, int y, int width, int height);
  native public void fillRect (int x, int y, int width, int height);

  native void drawString (String str, int x, int y, String fname, int size);
  public void drawString (String str, int x, int y)
  {
    drawString (str, x, y, font.getName(), font.getSize());
  }

  public void drawString (AttributedCharacterIterator ci, int x, int y)
  {
    throw new Error ("not implemented");
  }

  public void drawRoundRect(int x, int y, int width, int height, 
			    int arcWidth, int arcHeight)
  {
    // System.out.println ("drawRoundRect called [UNIMPLEMENTED]");
  }

  public void fillRoundRect (int x, int y, int width, int height, 
			     int arcWidth, int arcHeight)
  {
    // System.out.println ("fillRoundRect called [UNIMPLEMENTED]");
  }

  public Shape getClip ()
  {
    return getClipBounds ();
  }

  public Rectangle getClipBounds ()
  {
//      System.out.println ("returning CLIP: " + clip);
    return new Rectangle (clip.x, clip.y, clip.width, clip.height);
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
    setClip (clip.getBounds ());
  }

  native private void setFGColor (int red, int green, int blue);

  public void setColor (Color c)
  {
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

  native public void translateNative (int x, int y);

  public void translate (int x, int y)
  {
    clip.x -= x;
    clip.y -= y;

    translateNative (x, y);
  }
}
