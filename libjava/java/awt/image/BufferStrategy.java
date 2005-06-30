/* BufferStrategy.java -- describes image buffering resources
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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


package java.awt.image;

import java.awt.BufferCapabilities;
import java.awt.Graphics;

/**
 * This class describes a strategy for managing image buffering
 * resources on a Canvas or Window.  A given buffer strategy may make
 * use of hardware acceleration or take advantage of features of the
 * native graphics system.  Examples of buffering strategies are
 * double or triple buffering using either flipping or blitting.  For
 * the details of these algorithms see BufferCapabilities.
 *
 * To use a buffer strategy, you retrieve it from either the current
 * GraphicsConfiguration or from the Component on which you'd like to
 * draw.  Then you can query the strategy's capabilities to make sure
 * they're suitable.
 *
 * If the strategy's capabilities are suitable, you can obtain a
 * graphics object and use it to draw with this strategy.  Drawing
 * with a buffer strategy requires extra care, however.  You'll need
 * to manually cause the next buffer to be shown on the output device.
 * And since buffer strategies are usually implemented with a
 * VolatileImage, you must frequently check that the contents of the
 * buffer are valid and that the buffer still exists.
 *
 * A buffer strategy is usually implemented using a VolatileImage.
 *
 * @see VolatileImage
 * @since 1.4
 */
public abstract class BufferStrategy
{
  /**
   * Creates a new buffer strategy.
   */
  public BufferStrategy()
  {
  }

  /**
   * Retrieves the capabilities of this buffer strategy.
   *
   * @return this buffer strategy's capabilities
   */
  public abstract BufferCapabilities getCapabilities();

  /**
   * Retrieves a graphics object that can be used to draw using this
   * buffer strategy.  This method may not be synchronized so be
   * careful when calling it from multiple threads.  You also must
   * manually dispose of this graphics object.
   *
   * @return a graphics object that can be used to draw using this
   * buffer strategy
   */
  public abstract Graphics getDrawGraphics();

  /**
   * Returns whether or not the buffer's resources have been reclaimed
   * by the native graphics system.  If the buffer resources have been
   * lost then you'll need to obtain new resources before drawing
   * again.  For details, see the documentation for VolatileImage.
   *
   * @return true if the contents were lost, false otherwise
   */
  public abstract boolean contentsLost();

  /**
   * Returns whether or not the buffer's resources were re-created and
   * cleared to the default background color.  If the buffer's
   * resources have recently been re-created and initialized then the
   * buffer's image may need to be re-rendered.  For details, see the
   * documentation for VolatileImage.
   *
   * @return true if the contents were restored, false otherwise
   */
  public abstract boolean contentsRestored();

  /**
   * Applies this buffer strategy.  In other words, this method brings
   * the contents of the back or intermediate buffers to the front
   * buffer.
   */
  public abstract void show();
}
