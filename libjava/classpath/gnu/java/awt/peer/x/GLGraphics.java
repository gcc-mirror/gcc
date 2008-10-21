/* GLGraphics.java -- Graphics2D impl on top of GLX
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package gnu.java.awt.peer.x;

import java.awt.Color;
import java.awt.GraphicsConfiguration;
import java.awt.Rectangle;
import java.awt.image.ColorModel;
import java.util.Map;

import gnu.java.awt.java2d.AbstractGraphics2D;
import gnu.x11.extension.glx.GL;

/**
 * An implementation of Graphics2D on top of the GLX extension of X.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class GLGraphics extends AbstractGraphics2D
{

  /**
   * The rendering context.
   */
  private GL gl;

  /**
   * Creates a new GLGraphics that paints on the specified GL context.
   *
   * @param g the GL context to paint to
   */
  GLGraphics(GL g)
  {
    gl = g;
  }

  public void setBackground(Color b)
  {
    super.setBackground(b);
    
    gl.clearColor(b.getRed() / 255.F, b.getGreen() / 255.F,
                   b.getBlue() / 255.F, b.getAlpha() / 255.F);
  }

  public void clearRect(int x, int y, int w, int h)
  {
    // TODO: Maybe use fillRect().
    gl.clear(GL.COLOR_BUFFER_BIT);
  }

  public void drawLine(int x1, int y1, int x2, int y2)
  {
    gl.begin(GL.LINES);
    gl.vertex2i(x1, y1);
    gl.vertex2i(x2, y2);
    gl.end();
    // TODO: Maybe do:
    // gl.flush();
  }

  public void drawRect(int x, int y, int w, int h)
  {
    gl.polygon_mode(GL.FRONT_AND_BACK, GL.LINE);
    gl.begin(GL.POLYGON);
    gl.recti(x, y, x + w, y + h);
    gl.end();
    // TODO: Maybe do:
    // gl.flush();
  }

  public void fillRect(int x, int y, int w, int h)
  {
    gl.polygon_mode(GL.FRONT_AND_BACK, GL.FILL);
    gl.recti(x, y, x + w, y + h);
    // TODO: Maybe do:
    // gl.flush();
  }

  protected ColorModel getColorModel()
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  @Override
  protected Rectangle getDeviceBounds()
  {
    // FIXME: not sure it's correct
    return new Rectangle(0, 0,
                         gl.display.default_screen.width, 
                         gl.display.default_screen.height);
  }
}
