/* QtImageDirectGraphics.java --
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
import java.awt.Image;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.image.ImageObserver;

/**
 * A QtImagePainter that does an update after every drawing op.
 */
public class QtImageDirectGraphics extends QtImageGraphics
{
  private QtComponentPeer peer;
  private boolean modified;

  public QtImageDirectGraphics(QtImage image, QtComponentPeer peer)
  {
    super( image );
    this.peer = peer;
    modified = false;
  }

  public QtImageDirectGraphics(QtImageGraphics g)
  {
    super( g );
  }

  private void scheduleUpdate()
  {
  }

  public void dispose()
  {
    super.dispose();
    peer.toolkit.sync();
    peer.QtUpdate();
  }

  public void draw(Shape s)
  {
    super.draw(s);
    scheduleUpdate();
  }

  public void fill(Shape s)
  {
    super.fill(s);
    scheduleUpdate();
  }

  public void drawString(String string, int x, int y)
  {
    super.drawString( string, x, y );
    scheduleUpdate();
  }

  public void drawString(String string, float x, float y)
  {
    super.drawString( string, x, y );
    scheduleUpdate();
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    super.drawLine(x1, y1, x2, y2);
    scheduleUpdate();
  }

  public boolean drawImage(Image image,
                           AffineTransform Tx,
                           ImageObserver obs)
  {
    boolean r = super.drawImage(image, Tx, obs);
    scheduleUpdate();
    return r;
  }

  public boolean drawImage(Image image, int x, int y, Color bgcolor,
                           ImageObserver observer)
  {
    boolean r = super.drawImage(image, x, y, bgcolor, observer);
    scheduleUpdate();
    return r;
  }

  public boolean drawImage(Image image,
                           int dx1, int dy1, int dx2, int dy2,
                           int sx1, int sy1, int sx2, int sy2,
                           Color bgcolor, ImageObserver observer)
  {
    boolean r = super.drawImage( image, dx1,  dy1,  dx2,  dy2,
                                 sx1,  sy1,  sx2,  sy2,
                                 bgcolor, observer);
    scheduleUpdate();
    return r;
  }

  public boolean drawImage(Image image, int x, int y,
                           int width, int height, Color bgcolor,
                           ImageObserver observer)
  {
    boolean r = super.drawImage(image, x, y, width, height, bgcolor,
                                observer);
    scheduleUpdate();
    return r;
  }
}
