/* Robot.java -- a native input event generator
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


package java.awt;

import gnu.java.awt.ClasspathToolkit;

import java.lang.reflect.InvocationTargetException;
import java.awt.event.InputEvent;
import java.awt.image.BufferedImage;
import java.awt.peer.RobotPeer;

/**
 * The Robot class is used to simulate user interaction with graphical
 * programs.  It can generate native windowing system input events and
 * retrieve image data from the current screen.  Robot is used to test
 * the AWT and Swing library implementations; it can also be used to
 * create self-running demo programs.
 *
 * Since Robot generates native windowing system events, rather than
 * simply inserting {@link AWTEvent}s on the AWT event queue, its use
 * is not restricted to Java programs.  It can be used to
 * programatically drive any graphical application.
 *
 * This implementation requires an X server that supports the XTest
 * extension.
 *
 * @author Thomas Fitzsimmons (fitzsim@redhat.com)
 *
 * @since 1.3
 */
public class Robot
{
  private boolean waitForIdle;
  private int autoDelay;
  private RobotPeer peer;

  /**
   * Construct a Robot object that operates on the default screen.
   *
   * @exception AWTException if GraphicsEnvironment.isHeadless()
   * returns true or if the X server does not support the XTest
   * extension
   * @exception SecurityException if createRobot permission is not
   * granted
   */
  public Robot () throws AWTException
  {
    if (GraphicsEnvironment.isHeadless ())
      throw new AWTException ("Robot: headless graphics environment");

    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkPermission (new AWTPermission ("createRobot"));

    ClasspathToolkit tk = (ClasspathToolkit) Toolkit.getDefaultToolkit ();

    // createRobot will throw AWTException if XTest is not supported.
    peer = tk.createRobot (GraphicsEnvironment.getLocalGraphicsEnvironment ()
			   .getDefaultScreenDevice ());
  }

  /**
   * Construct a Robot object that operates on the specified screen.
   *
   * @exception AWTException if GraphicsEnvironment.isHeadless()
   * returns true or if the X server does not support the XTest
   * extension
   * @exception IllegalArgumentException if screen is not a screen
   * GraphicsDevice
   * @exception SecurityException if createRobot permission is not
   * granted
   */
  public Robot (GraphicsDevice screen) throws AWTException
  {
    if (GraphicsEnvironment.isHeadless ())
      throw new AWTException ("Robot: headless graphics environment");

    if (screen.getType () != GraphicsDevice.TYPE_RASTER_SCREEN)
      throw new IllegalArgumentException ("Robot: graphics"
					  + " device is not a screen");

    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkPermission (new AWTPermission ("createRobot"));

    ClasspathToolkit tk = (ClasspathToolkit) Toolkit.getDefaultToolkit ();

    // createRobot will throw AWTException if XTest is not supported.
    peer = tk.createRobot (screen);
  }

  /**
   * Move the mouse pointer to absolute coordinates (x, y).
   *
   * @param x the destination x coordinate
   * @param y the destination y coordinate
   */
  public void mouseMove(int x, int y)
  {
    peer.mouseMove (x, y);

    if (waitForIdle)
      waitForIdle ();

    if (autoDelay > 0)
      delay (autoDelay);
  }

  /**
   * Press one or more mouse buttons.
   *
   * @param buttons the buttons to press; a bitmask of one or more of
   * these {@link InputEvent} fields:
   *
   * <ul>
   *   <li>BUTTON1_MASK</li>
   *   <li>BUTTON2_MASK</li>
   *   <li>BUTTON3_MASK</li>
   * </ul>
   *
   * @exception IllegalArgumentException if the button mask is invalid
   */
  public void mousePress (int buttons)
  {
    if ((buttons & InputEvent.BUTTON1_MASK) == 0
	&& (buttons & InputEvent.BUTTON2_MASK) == 0
	&& (buttons & InputEvent.BUTTON3_MASK) == 0)
      throw new IllegalArgumentException ("Robot: mousePress:"
					  + " invalid button mask");

    peer.mousePress (buttons);

    if (waitForIdle)
      waitForIdle ();

    if (autoDelay > 0)
      delay (autoDelay);
  }

  /**
   * Release one or more mouse buttons.
   *
   * @param buttons the buttons to release; a bitmask of one or more
   * of these {@link InputEvent} fields:
   *
   * <ul>
   *   <li>BUTTON1_MASK</li>
   *   <li>BUTTON2_MASK</li>
   *   <li>BUTTON3_MASK</li>
   * </ul>
   *
   * @exception IllegalArgumentException if the button mask is invalid
   */
  public void mouseRelease(int buttons)
  {
    if ((buttons & InputEvent.BUTTON1_MASK) == 0
	&& (buttons & InputEvent.BUTTON2_MASK) == 0
	&& (buttons & InputEvent.BUTTON3_MASK) == 0)
      throw new IllegalArgumentException ("Robot: mouseRelease:"
					  + " invalid button mask");

    peer.mouseRelease (buttons);

    if (waitForIdle)
      waitForIdle ();

    if (autoDelay > 0)
      delay (autoDelay);
  }

  /**
   * Rotate the mouse scroll wheel.
   *
   * @param wheelAmt number of steps to rotate mouse wheel.  negative
   * to rotate wheel up (away from the user), positive to rotate wheel
   * down (toward the user).
   *
   * @since 1.4
   */
  public void mouseWheel (int wheelAmt)
  {
    peer.mouseWheel (wheelAmt);

    if (waitForIdle)
      waitForIdle ();

    if (autoDelay > 0)
      delay (autoDelay);
  }

  /**
   * Press a key.
   *
   * @param keycode key to press, a {@link java.awt.event.KeyEvent} VK_ constant
   *
   * @exception IllegalArgumentException if keycode is not a valid key
   */
  public void keyPress (int keycode)
  {
    peer.keyPress (keycode);

    if (waitForIdle)
      waitForIdle ();

    if (autoDelay > 0)
      delay (autoDelay);
  }

  /**
   * Release a key.
   *
   * @param keycode key to release, a {@link java.awt.event.KeyEvent} VK_ 
   *                constant
   *
   * @exception IllegalArgumentException if keycode is not a valid key
   */
  public void keyRelease (int keycode)
  {
    peer.keyRelease (keycode);

    if (waitForIdle)
      waitForIdle ();

    if (autoDelay > 0)
      delay (autoDelay);
  }

  /**
   * Return the color of the pixel at the given screen coordinates.
   *
   * @param x the x coordinate of the pixel
   * @param y the y coordinate of the pixel
   *
   * @return the Color of the pixel at screen coodinates <code>(x, y)</code>
   */
  public Color getPixelColor (int x, int y)
  {
    return new Color (peer.getRGBPixel (x, y));
  }

  /**
   * Create an image containing pixels read from the screen.  The
   * image does not include the mouse pointer.
   *
   * @param screenRect the rectangle of pixels to capture, in screen
   * coordinates
   *
   * @return a BufferedImage containing the requested pixels
   *
   * @exception IllegalArgumentException if requested width and height
   * are not both greater than zero
   * @exception SecurityException if readDisplayPixels permission is
   * not granted
   */
  public BufferedImage createScreenCapture (Rectangle screenRect)
  {
    if (screenRect.width <= 0)
      throw new IllegalArgumentException ("Robot: capture width is <= 0");

    if (screenRect.height <= 0)
      throw new IllegalArgumentException ("Robot: capture height is <= 0");

    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkPermission (new AWTPermission ("readDisplayPixels"));

    int[] pixels = peer.getRGBPixels (screenRect);

    BufferedImage bufferedImage =
      new BufferedImage (screenRect.width, screenRect.height,
			 BufferedImage.TYPE_INT_ARGB);

    bufferedImage.setRGB (0, 0, screenRect.width, screenRect.height,
			  pixels, 0, screenRect.width);

    return bufferedImage;
  }

  /**
   * Check if this Robot automatically calls {@link #waitForIdle()} after
   * generating an event.
   *
   * @return true if waitForIdle is automatically called
   */
  public boolean isAutoWaitForIdle ()
  {
    return waitForIdle;
  }

  /**
   * Set whether or not this Robot automatically calls {@link
   * #waitForIdle()} after generating an event.
   *
   * @param isOn true if waitForIdle should be called automatically
   */
  public void setAutoWaitForIdle (boolean isOn)
  {
    waitForIdle = isOn;
  }

  /**
   * Retrieve the length of time this Robot sleeps after generating an
   * event.
   *
   * @return the length of time in milliseconds
   */
  public int getAutoDelay ()
  {
    return autoDelay;
  }

  /**
   * Set the length of time this Robot sleeps after generating an
   * event.
   *
   * @param ms the length of time in milliseconds
   *
   * @exception IllegalArgumentException if ms is not between 0 and
   * 60,000 milliseconds inclusive
   */
  public void setAutoDelay (int ms)
  {
    if (ms <= 0 || ms >= 60000)
      throw new IllegalArgumentException ("Robot: delay length out-of-bounds");

    autoDelay = ms;
  }

  /**
   * Sleep for a specified length of time.
   *
   * @param ms the length of time in milliseconds
   *
   * @exception IllegalArgumentException if ms is not between 0 and
   * 60,000 milliseconds inclusive
   */
  public void delay (int ms)
  {
    if (ms < 0 || ms > 60000)
      throw new IllegalArgumentException ("Robot: delay length out-of-bounds");

    try
      {
	Thread.sleep (ms);
      }
    catch (InterruptedException e)
      {
	System.err.println ("Robot: delay interrupted");
      }
  }

  /**
   * Wait until all events currently on the event queue have been
   * dispatched.
   */
  public void waitForIdle ()
  {
    if (EventQueue.isDispatchThread ())
      throw new IllegalThreadStateException ("Robot: waitForIdle called from "
					     + "the event dispatch thread");

    try
      {
	EventQueue.invokeAndWait (new Runnable () { public void run () { } });
      }
    catch (InterruptedException e)
      {
	System.err.println ("Robot: waitForIdle interrupted");
      }
    catch (InvocationTargetException e)
      {
	System.err.println ("Robot: waitForIdle cannot invoke target");
      }
  }

  /**
   * Return a string representation of this Robot.
   *
   * @return a string representation
   */
  public String toString ()
  {
    return getClass ().getName ()
	+ "[ autoDelay = " + autoDelay + ", autoWaitForIdle = "
	+ waitForIdle + " ]";
  }
}
