/* GtkVolatileImage.java -- wraps an X pixmap
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import java.awt.ImageCapabilities;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.VolatileImage;

public class GtkVolatileImage extends VolatileImage
{
  int width, height;
  private ImageCapabilities caps;

  /**
   * Don't touch, accessed from native code.
   */
  long nativePointer;

  native long init(GtkComponentPeer component, int width, int height);

  native void destroy();

  native int[] getPixels();

  native void copyArea( int x, int y, int w, int h, int dx, int dy );

  native void drawVolatile( long ptr, int x, int y, int w, int h );
  
  public GtkVolatileImage(GtkComponentPeer component, 
			  int width, int height, ImageCapabilities caps)
  {
    this.width = width;
    this.height = height;
    this.caps = caps;
    nativePointer = init( component, width, height );
  }

  public GtkVolatileImage(int width, int height, ImageCapabilities caps)
  {
    this(null, width, height, caps);
  }

  public GtkVolatileImage(int width, int height)
  {
    this(null, width, height, null);
  }

  public void finalize()
  {
    dispose();
  }

  public void dispose()
  {
    destroy();
  }

  public BufferedImage getSnapshot()
  {
    CairoSurface cs = new CairoSurface( width, height );
    cs.setPixels( getPixels() );
    return CairoSurface.getBufferedImage( cs );
  }

  public Graphics getGraphics()
  {
    return createGraphics();
  }

  public Graphics2D createGraphics()
  {
    return new VolatileImageGraphics( this );
  }

  public int validate(GraphicsConfiguration gc)
  {
    return VolatileImage.IMAGE_OK;
  }

  public boolean contentsLost()
  {
    return false;
  }

  public ImageCapabilities getCapabilities()
  {
    return caps;
  }

  public int getWidth()
  {
    return width;
  }

  public int getHeight()
  {
    return height;
  }

  public int getWidth(java.awt.image.ImageObserver observer)
  {
    return width;
  }
  
  public int getHeight(java.awt.image.ImageObserver observer)
  {
    return height;
  }

  public Object getProperty(String name, ImageObserver observer)
  {
    return null;
  }
}
