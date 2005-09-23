/* QtComponentGraphics.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Paint;

/**
 * QtComponentPainter is a Graphics2D context for painting directly to AWT 
 * components. They require an existing QPainter object (the one passed into 
 * the native paint method), and are created there (ONLY).
 *
 * Since this context does direct on-screen drawing it is NOT thread-safe,
 * and should NOT be used outside the thread in which it was created.
 *
 * In other words,
 * this is intended for use by QtComponentPeer.paintEvent() only.
 *
 */
public class QtComponentGraphics extends QtGraphics
{
  private QtComponentPeer peer;

  /**
   * Creates a new ComponentGraphics from an *existing* QPainter object.
   *
   * @param ptr the pointer to the QPainter object.
   */
  public QtComponentGraphics(long ptr, QtComponentPeer component, 
			     int x, int y, int w, int h)
  {
    nativeObject = ptr;
    peer = component;

    Rectangle r = new Rectangle(x, y, w, h);
    initialClip = r;
    
    setAlpha( 1.0 );
    Color c = component.owner.getBackground();
    if(c == null)
      setBackground(Color.white);
    else
      setBackground( c );

    c = component.owner.getForeground();
    if(c == null)
      setColor( Color.black );
    else
      setColor( c );
    setup();
    setClip( initialClip );
  }

  /**
   * Copying constructor
   */
  QtComponentGraphics( QtComponentGraphics g )
  {
    super( g ); // Slalom is fun
  }

  public Graphics create()
  {
    return new QtComponentGraphics( this );
  }

  /**
   * This is a tricky one
   */ 
  public void copyArea(int x, int y, int width, int height, 
		       int dx, int dy)
  {
    // FIXME 
  }

  /**
   * Returns the GraphicsConfiguration of the context component.
   */
  public GraphicsConfiguration getDeviceConfiguration()
  {
    return peer.getGraphicsConfiguration();
  }
} 


