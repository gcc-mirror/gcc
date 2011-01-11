/* GraphicsDevice.java -- information about a graphics device
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

import java.awt.image.VolatileImage;

/**
 * This describes a graphics device available to the given environment. This
 * includes screen and printer devices, and the different configurations for
 * each device. Also, this allows you to create virtual devices which operate
 * over a multi-screen environment.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see GraphicsEnvironment
 * @see GraphicsConfiguration
 * @since 1.3
 * @status updated to 1.4
 */
public abstract class GraphicsDevice
{
  /** Device is a raster screen. */
  public static final int TYPE_RASTER_SCREEN = 0;

  /** Device is a printer. */
  public static final int TYPE_PRINTER = 1;

  /** Device is an image buffer not visible to the user. */
  public static final int TYPE_IMAGE_BUFFER = 2;

  /** The current full-screen window, or null if there is none. */
  private Window full_screen;

  /**
   * The bounds of the fullscreen window before it has been switched to full
   * screen.
   */
  private Rectangle fullScreenOldBounds;

  /** The current display mode, or null if unknown. */
  private DisplayMode mode;

  /**
   * The default constructor.
   *
   * @see GraphicsEnvironment#getScreenDevices()
   * @see GraphicsEnvironment#getDefaultScreenDevice()
   * @see GraphicsConfiguration#getDevice()
   */
  protected GraphicsDevice()
  {
  }

  /**
   * Returns the type of the device.
   *
   * @return the device type
   * @see #TYPE_RASTER_SCREEN
   * @see #TYPE_PRINTER
   * @see #TYPE_IMAGE_BUFFER
   */
  public abstract int getType();

  /**
   * Returns an identification string for the device. This can be
   * vendor-specific, and may be useful for debugging.
   *
   * @return the identification
   */
  public abstract String getIDstring();

  /**
   * Return all configurations valid for this device.
   *
   * @return an array of configurations
   */
  public abstract GraphicsConfiguration[] getConfigurations();

  /**
   * Return the default configuration for this device.
   *
   * @return the default configuration
   */
  public abstract GraphicsConfiguration getDefaultConfiguration();

  /**
   * Return the best configuration, according to the criteria in the given
   * template.
   *
   * @param template the template to adjust by
   * @return the best configuration
   * @throws NullPointerException if template is null
   */
  public GraphicsConfiguration getBestConfiguration
    (GraphicsConfigTemplate template)
  {
    return template.getBestConfiguration(getConfigurations());
  }

  /**
   * Returns true if the device supports full-screen exclusive mode. The
   * default implementation returns true; subclass it if this is not the case.
   *
   * @return true if full screen support is available
   * @since 1.4
   */
  public boolean isFullScreenSupported()
  {
    return true;
  }

  /**
   * Toggle the given window between full screen and normal mode. The previous
   * full-screen window, if different, is restored; if the given window is
   * null, no window will be full screen. If
   * <code>isFullScreenSupported()</code> returns true, full screen mode is
   * considered to be exclusive, which implies:<ul>
   * <li>Windows cannot overlap the full-screen window. All other application
   *     windows will always appear beneath the full-screen window in the
   *     Z-order.</li>
   * <li>Input method windows are disabled. It is advisable to call
   *     <code>Component.enableInputMethods(false)</code> to make a component
   *     a non-client of the input method framework.</li>
   * </ul><br>
   * If <code>isFullScreenSupported()</code> returns false, full-screen
   * exclusive mode is simulated by resizing the window to the size of the
   * screen and positioning it at (0,0). This is also what this method does.
   * If a device supports real fullscreen mode then it should override this
   * method as well as #isFullScreenSupported and #getFullScreenWindow.
   *
   * @param w the window to toggle
   * @see #isFullScreenSupported()
   * @see #getFullScreenWindow()
   * @see #setDisplayMode(DisplayMode)
   * @see Component#enableInputMethods(boolean)
   * @since 1.4
   */
  public synchronized void setFullScreenWindow(Window w)
  {
    // Restore the previous window to normal mode and release the reference.
    if (full_screen != null)
      {
        full_screen.setBounds(fullScreenOldBounds);
      }

    full_screen = null;

    // If w != null, make it full-screen.
    if (w != null)
      {
        fullScreenOldBounds = w.getBounds();
        full_screen = w;
        DisplayMode dMode = getDisplayMode();
        full_screen.setBounds(0, 0, dMode.getWidth(), dMode.getHeight());
        full_screen.requestFocus();
        full_screen.setLocationRelativeTo(null);
      }
  }

  /**
   * Returns the current full-screen window of the device, or null if no
   * window is full-screen.
   *
   * @return the full-screen window
   * @see #setFullScreenWindow(Window)
   * @since 1.4
   */
  public Window getFullScreenWindow()
  {
    return full_screen;
  }

  /**
   * Returns whether this device supports low-level display changes. This may
   * depend on whether full-screen exclusive mode is available.
   *
   * XXX The default implementation returns false for now.
   *
   * @return true if display changes are supported
   * @see #setDisplayMode(DisplayMode)
   * @since 1.4
   */
  public boolean isDisplayChangeSupported()
  {
    return false;
  }

  /**
   * Sets the display mode. This may be dependent on the availability of
   * full-screen exclusive mode.
   *
   * @param mode the new mode
   * @throws IllegalArgumentException if the new mode is not in getDisplayModes
   * @throws UnsupportedOperationException if ! isDisplayChangeSupported()
   * @see #getDisplayMode()
   * @see #getDisplayModes()
   * @see #isDisplayChangeSupported()
   * @since 1.4
   */
  public void setDisplayMode(DisplayMode mode)
  {
    DisplayMode[] array = getDisplayModes();
    if (! isDisplayChangeSupported())
      throw new UnsupportedOperationException();
    int i = array == null ? 0 : array.length;
    while (--i >= 0)
      if (array[i].equals(mode))
        break;
    if (i < 0)
      throw new IllegalArgumentException();
    this.mode = mode;
  }

  /**
   * Returns the current display mode of this device, or null if unknown.
   *
   * @return the current display mode
   * @see #setDisplayMode(DisplayMode)
   * @see #getDisplayModes()
   * @since 1.4
   */
  public DisplayMode getDisplayMode()
  {
    return mode;
  }

  /**
   * Return an array of all available display modes. This implementation
   * returns a 0-length array, so subclasses must override this.
   *
   * @return the array of available modes
   * @since 1.4
   */
  public DisplayMode[] getDisplayModes()
  {
    return new DisplayMode[0];
  }

  /**
   * Return the number of bytes available in accelerated memory on this
   * device. The device may support creation or caching on a first-come,
   * first-served basis, depending on the operating system and driver.
   * Memory may be a finite resource, and because of multi-threading, you
   * are not guaranteed that the result of this method ensures your image
   * will successfully be put in accelerated memory. A negative result means
   * the memory is unlimited. The default implementation assumes no special
   * memory is available, and returns 0.
   *
   * @return the size of accelerated memory available
   * @see VolatileImage#flush()
   * @see ImageCapabilities#isAccelerated()
   */
  public int getAvailableAcceleratedMemory()
  {
    return 0;
  }
} // class GraphicsDevice
