/* QtImageGraphics.java --
   Copyright (C)  2005, 2006  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.Color;
import java.awt.GraphicsConfiguration;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.util.Stack;

/**
 * QtComponentPainter is a Graphics2D context for painting to QtImage and
 * QtVolatileImages.
 */
public class QtImageGraphics extends QtGraphics
{
  Image parentImage;
  Stack owners;
  QtImageGraphics topParent;

  public QtImageGraphics(Image image)
  {
    if(!( image instanceof QtVolatileImage || image instanceof QtImage))
      throw new IllegalArgumentException("Cannot create QtImageGraphics for a non-QImage context.");

    owners = new Stack();
    owners.push(this);
    topParent = null;
    int w, h;
    if(image instanceof QtImage)
      {
	w = ((QtImage)image).width; 
	h = ((QtImage)image).height; 
	initImage((QtImage) image );
	((QtImage)image).putPainter( this );
      } 
    else
      {
	w = ((QtVolatileImage)image).width; 
	h = ((QtVolatileImage)image).height; 
	initVolatileImage((QtVolatileImage) image );
	((QtVolatileImage)image).putPainter( this );
      }

    parentImage = image;
    initialClip = new Rectangle( 0, 0, w, h );
    setClip( initialClip );
    setBackground(Color.white); // fixme
    currentAlpha = 1.0;
    setColor(Color.black);
    setup();
  }

  /**
   * Copying constructor
   */
  QtImageGraphics( QtImageGraphics g )
  {
    super( g ); 
    parentImage = g.parentImage;
    if(parentImage instanceof QtImage)
      ((QtImage)parentImage).putPainter( this );
    else
      ((QtVolatileImage)parentImage).putPainter( this );
  }

  public void dispose()
  {
    delete();
    if( parentImage instanceof QtImage )
      ((QtImage)parentImage).removePainter( this );
    else
      ((QtVolatileImage)parentImage).removePainter( this );
  }

  /**
   * Create a copy of this context.
   */
  public Graphics create()
  {
    return new QtImageGraphics( this );
  }

  /**
   * Copy an area.
   */ 
  public void copyArea(int x, int y, int width, int height, 
		       int dx, int dy)
  {
    if(parentImage instanceof QtImage)
      ((QtImage)parentImage).copyArea(x, y, width, height, dx, dy);
    else
      ((QtVolatileImage)parentImage).copyArea(x, y, width, height, dx, dy);
  }

  /**
   * Returns the GraphicsConfiguration of the context component.
   */
  public GraphicsConfiguration getDeviceConfiguration()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }
} 


