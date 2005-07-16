/* BufferCapabilities.java -- double-buffering capabilities descriptor
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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


package java.awt;

import java.awt.image.BufferStrategy;

/**
 * A double-buffering capability descriptor.  This class exposes
 * details about the double-buffering algorithms used by image
 * buffers.
 *
 * BufferCapabilities represents algorithms that involve at least two
 * buffers but it can also specify so-called "multi-buffer" schemes
 * involving more than two buffers.  This class describes the
 * capabilities of the front and back buffers as well as the results
 * of "flipping" -- that is, what happens when an image is transferred
 * from the back buffer to the front buffer.
 *
 * Flipping may or may not be supported or may be supported only in
 * fullscreen mode.  If it is not supported then "blitting" is implied
 * -- that is, the contents of the back buffer are copied using a fast
 * block transfer operation from the back buffer to the front buffer.
 *
 * The front buffer is the one that is displayed.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 *
 * @see BufferStrategy#getCapabilities()
 * @see GraphicsConfiguration#getBufferCapabilities()
 *
 * @since 1.4
 */
public class BufferCapabilities implements Cloneable
{
  /**
   * A type-safe enumeration of buffer flipping results.
   *
   * @see AttributeValue
   */
  public static final class FlipContents extends AttributeValue
  {
    /**
     * The names of the different flipping results.
     */
    private static final String[] NAMES
      = { "undefined", "background", "prior", "copied" };

    /**
     * The contents of the back buffer are undefined after flipping.
     */
    public static final FlipContents UNDEFINED = new FlipContents(0);

    /**
     * The back buffer is cleared with the background color after
     * flipping.
     */
    public static final FlipContents BACKGROUND = new FlipContents(1);

    /**
     * The back buffer contains the pre-flipping contents of the front
     * buffer after flipping.  In other words a true "flip" has been
     * performed.
     */
    public static final FlipContents PRIOR = new FlipContents(2);

    /**
     * The back buffer has the same contents as the front buffer after
     * flipping.
     */
    public static final FlipContents COPIED = new FlipContents(3);

    /**
     * Create a new flipping result descriptor.
     *
     * @param value the enumeration value
     */
    private FlipContents(int value)
    {
      super(value, NAMES);
    }
  }

  /**
   * Front buffer capabilities descriptor.
   */
  private final ImageCapabilities front;

  /**
   * Back buffer capabilities descriptor.
   */
  private final ImageCapabilities back;

  /**
   * Describes the results of a "flip" operation.
   */
  private final FlipContents flip;

  /**
   * Creates a buffer capabilities object.
   *
   * @param frontCaps front buffer capabilities descriptor
   * @param backCaps back buffer capabilities descriptor
   * @param flip the results of a flip operation or null if
   * flipping is not supported
   *
   * @exception IllegalArgumentException if frontCaps or backCaps is
   * null
   */
  public BufferCapabilities(ImageCapabilities frontCaps,
			    ImageCapabilities backCaps,
                            FlipContents flip)
  {
    if (frontCaps ==  null || backCaps == null)
      throw new IllegalArgumentException();
    this.front = frontCaps;
    this.back = backCaps;
    this.flip = flip;
  }

  /**
   * Retrieve the front buffer's image capabilities.
   *
   * @return the front buffer's image capabilities
   */
  public ImageCapabilities getFrontBufferCapabilities()
  {
    return front;
  }

  /**
   * Retrieve the back buffer's image capabilities.
   *
   * @return the back buffer's image capabilities
   */
  public ImageCapabilities getBackBufferCapabilities()
  {
    return back;
  }

  /**
   * Return whether or not flipping is supported.
   *
   * @return true if flipping is supported, false otherwise
   */
  public boolean isPageFlipping()
  {
    return flip != null;
  }

  /**
   * Retrieve the result of a flipping operation.  If this method
   * returns null then flipping is not supported.  This implies that
   * "blitting", a fast block transfer, is used to copy the contents
   * of the back buffer to the front buffer.  Other possible return
   * values are:
   * <ul>
   *   <li><code>FlipContents.UNDEFINED</code> the contents of the
   *   back buffer are undefined after flipping.</li>
   *   <li><code>FlipContents.BACKGROUND</code> the contents of the
   *   back buffer are cleared to the background color after
   *   flipping.</li>
   *   <li><code>FlipContents.PRIOR</code> the back buffer contains
   *   the pre-flipping contents of the front * buffer after
   *   flipping.</li>
   *   <li><code>FlipContents.COPIED</code> the back buffer has the
   *   same contents as the front buffer after flipping.</li>
   * </ul>
   *
   * @return the result of a flipping operation or null if flipping is
   * not supported
   */
  public FlipContents getFlipContents()
  {
    return flip;
  }

  /**
   * Returns true if flipping is only supported in fullscreen mode.
   *
   * @return true if flipping is only supported in fullscreen mode,
   * false otherwise
   */
  public boolean isFullScreenRequired()
  {
    return true;
  }

  /**
   * Returns true if flipping can involve more than two buffers.  One
   * or more intermediate buffers may be available in addition to the
   * front and back buffers.
   *
   * @return true if there are more than two buffers available for
   * flipping, false otherwise
   */
  public boolean isMultiBufferAvailable()
  {
    return false;
  }

  /**
   * Clone this buffering capability descriptor.
   *
   * @return a clone of this buffer capability descriptor
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e);
      }
  }
}
