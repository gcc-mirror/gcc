/* GtkImagePainter.java
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
import java.awt.Rectangle;
import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageObserver;
import java.util.Hashtable;

public class GtkImagePainter implements Runnable, ImageConsumer
{
  GtkImage image;
  GdkGraphics gc;
  int startX, startY;
  int redBG;
  int greenBG;
  int blueBG;
  double affine[];
  int width, height;
  boolean flipX, flipY;
  Rectangle clip;
  int s_width, s_height;
  ImageObserver observer;

  public
  GtkImagePainter (GtkImage image, GdkGraphics gc, int x, int y, 
		   int width, int height, Color bgcolor, ImageObserver o)
  {
    this.image = image;
    this.gc = (GdkGraphics) gc.create ();
    startX = x;
    startY = y;
    redBG = bgcolor.getRed ();
    greenBG = bgcolor.getGreen ();
    blueBG = bgcolor.getBlue ();
    this.width = width;
    this.height = height;
    flipX = flipY = false;
    s_width = s_height = 0;
    clip = null;
    observer = o;

    run ();
  }

  public
  GtkImagePainter (GtkImage image, GdkGraphics gc, 
		   int dx1, int dy1, int dx2, int dy2,
		   int sx1, int sy1, int sx2, int sy2,
		   Color bgcolor, ImageObserver o)
  {
    this.image = image;
    this.gc = (GdkGraphics) gc.create ();
    startX = (dx1 < dx2) ? dx1 : dx2;
    startY = dy1;
    redBG = bgcolor.getRed ();
    greenBG = bgcolor.getGreen ();
    blueBG = bgcolor.getBlue ();
    observer = o;

    this.width = Math.abs (dx2 - dx1);
    this.height = Math.abs (dy2 - dy1);

    flipX = ((dx1 > dx2 && sx2 > sx1)
	     || (dx1 < dx2 && sx2 < sx1));

    flipY = ((dy1 > dy2 && sy2 > sy1)
	     || (dy1 < dy2 && sy2 < sy1));

    s_width = Math.abs (sx2 - sx1);
    s_height = Math.abs (sy2 - sy1);
    clip = new Rectangle (sx1, sy1, s_width, s_height);

    run ();
  }

  public void
  run ()
  {
    image.startProduction (this);
    gc.dispose ();
  }

  /* Convert pixel data into a format that gdkrgb can understand */
  static int[] 
  convertPixels (int[] pixels, ColorModel model)
  {
    if (pixels == null || model == null)
    {
      return null;
    }

    if (model.equals (ColorModel.getRGBdefault ()))
      return pixels;

    int ret[] = new int[pixels.length];

    for (int i = 0; i < pixels.length; i++)
      ret[i] = model.getRGB (pixels[i]);

    return ret;
  }

  static int[]
  convertPixels (byte[] pixels, ColorModel model)
  {
    if (pixels == null || model == null)
    {
      return null;
    }

    int ret[] = new int[pixels.length];

    for (int i = 0; i < pixels.length; i++)
      ret[i] = model.getRGB (pixels[i]);

    return ret;
  }

  native void
  drawPixels (GdkGraphics gc, int bg_red, int bg_green, int bg_blue, 
	      int x, int y, int width, int height, int[] pixels, int offset, 
	      int scansize, double affine[]);

 
  public void 
  setPixels (int x, int y, int width, int height, ColorModel model,
	     int[] pixels, int offset, int scansize)
  {
    if (clip != null)
      {
	Rectangle r;
	r = clip.intersection (new Rectangle (x, y, width, height));
	if (r.width == 0 && r.height == 0)
	  return;

	offset += r.y * scansize + r.x;

	width = r.width;
	height = r.height;
	x = r.x;
	y = r.y;
      }

    drawPixels (gc, redBG, greenBG, blueBG,
	        startX + x, startY + y,
		width, height, convertPixels (pixels, model), offset,
		scansize, affine);
  }

  public void 
  setPixels (int x, int y, int width, int height, ColorModel model, 
	     byte[] pixels, int offset, int scansize)
  {
    setPixels (x, y, width, height, ColorModel.getRGBdefault(),
	       convertPixels (pixels, model), offset, scansize);
  }

  public void 
  setDimensions (int width, int height)
  {
    if (!flipX && !flipY && 
	((this.width == -1 && this.height == -1)
	 || (this.width == width && this.height == height)))
      return;

    affine = new double[6];
    affine[1] = affine[2] = affine[4] = affine[5] = 0;
    
    if (clip != null)
      {
	affine[0] = this.width / (double) s_width;
	affine[3] = this.height / (double) s_height;
      }
    else
      {
	affine[0] = this.width / (double) width;
	affine[3] = this.height / (double) height;
      }

    if (flipX)
      {
	affine[0] = -affine[0];
	affine[4] = this.width;
      }

    if (flipY)
      {
	affine[3] = -affine[3];
	affine[5] = this.height;
      }

    if (affine[0] == 1 && affine[3] == 1)
      affine = null;
  }

  public void 
  setProperties (Hashtable props)
  {
  }

  public void 
  setColorModel (ColorModel model)
  {
  }

  public void 
  setHints (int flags)
  {
  }

  public void 
  imageComplete (int status)
  {
    image.imageComplete(status);

    if (observer != null)
      {
	if (status == ImageConsumer.IMAGEERROR)
	  observer.imageUpdate (null,
				ImageObserver.ERROR,
				-1, -1, -1, -1);
	else
	  observer.imageUpdate (null,
				ImageObserver.ALLBITS,
				-1, -1, -1, -1);
      }
  }
}
