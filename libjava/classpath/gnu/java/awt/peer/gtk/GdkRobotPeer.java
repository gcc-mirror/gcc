/* GdkRobot.java -- an XTest implementation of RobotPeer
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.AWTException;
import java.awt.GraphicsDevice;
import java.awt.Rectangle;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.peer.RobotPeer;

/**
 * Implements the RobotPeer interface using the XTest extension.
 *
 * @author Thomas Fitzsimmons
 */
public class GdkRobotPeer implements RobotPeer
{
  // gdk-pixbuf provides data in RGBA format
  static final ColorModel cm = new DirectColorModel (32, 0xff000000,
						     0x00ff0000,
						     0x0000ff00,
						     0x000000ff);

  public GdkRobotPeer (GraphicsDevice screen) throws AWTException
  {
    // FIXME: make use of screen parameter when GraphicsDevice is
    // implemented.
    if (!initXTest ())
      throw new AWTException ("XTest extension not supported");
  }

  native boolean initXTest ();

  // RobotPeer methods
  public native void mouseMove (int x, int y);
  public native void mousePress (int buttons);
  public native void mouseRelease (int buttons);
  public native void mouseWheel (int wheelAmt);
  public native void keyPress (int keycode);
  public native void keyRelease (int keycode);
  native int[] nativeGetRGBPixels (int x, int y, int width, int height);

  public int getRGBPixel (int x, int y)
  {
    return cm.getRGB (nativeGetRGBPixels (x, y, 1, 1)[0]);
  }

  public int[] getRGBPixels (Rectangle r)
  {
    int[] gdk_pixels = nativeGetRGBPixels (r.x, r.y, r.width, r.height);
    int[] pixels = new int[r.width * r.height];

    for (int i = 0; i < r.width * r.height; i++)
      pixels[i] = cm.getRGB (gdk_pixels[i]);

    return pixels;
  }

  public void dispose()
  {
    // Nothing to do here yet.
  }
}
