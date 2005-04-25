/* GraphicsEnvironment.java -- information about the graphics environment
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
import gnu.classpath.SystemProperties;
import java.awt.image.BufferedImage;
import java.util.Locale;

/**
 * This descibes the collection of GraphicsDevice and Font objects available
 * on a given platform. The resources might be local or remote, and specify
 * the valid configurations for displaying graphics.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see GraphicsDevice
 * @see GraphicsConfiguration
 * @since 1.4
 * @status updated to 1.4
 */
public abstract class GraphicsEnvironment
{
  private static GraphicsEnvironment localGraphicsEnvironment;

  /**
   * The environment must be obtained from a factory or query method, hence
   * this constructor is protected.
   */
  protected GraphicsEnvironment()
  {
  }

  /**
   * Returns the local graphics environment. If the java.awt.graphicsenv
   * system property is set, it instantiates the specified class,
   * otherwise it assume that the awt toolkit is a ClasspathToolkit
   * and delegates to it to create the instance.
   *
   * @return the local environment
   */
  public static GraphicsEnvironment getLocalGraphicsEnvironment()
  {
    if (localGraphicsEnvironment != null)
      return localGraphicsEnvironment;

    String graphicsenv = SystemProperties.getProperty("java.awt.graphicsenv",
                                                      null);
    if (graphicsenv != null)
      {
        try
          {
            // We intentionally use the bootstrap class loader.
            localGraphicsEnvironment = (GraphicsEnvironment)
                Class.forName(graphicsenv).newInstance();
            return localGraphicsEnvironment;
          }
        catch (Exception x)
          {
            throw (InternalError)
                new InternalError("Unable to instantiate java.awt.graphicsenv")
                    .initCause(x);
          }
      }
    else
      {
        ClasspathToolkit tk;
        tk = ((ClasspathToolkit) Toolkit.getDefaultToolkit());
        localGraphicsEnvironment = tk.getLocalGraphicsEnvironment();
        return localGraphicsEnvironment;
      }
  }

  /**
   * Check if the local environment is headless, meaning that it does not
   * support a display, keyboard, or mouse. Many methods in the Abstract
   * Windows Toolkit (java.awt) throw a {@link HeadlessException} if this
   * returns true.
   *
   * This method returns true if the java.awt.headless property is set
   * to "true".
   *
   * @return true if the environment is headless, meaning that graphics are
   *         unsupported
   * @since 1.4
   */
  public static boolean isHeadless()
  {
    String headless = SystemProperties.getProperty("java.awt.headless", null);
    return "true".equalsIgnoreCase(headless);
  }

  /**
   * Check if the given environment is headless, meaning that it does not
   * support a display, keyboard, or mouse. Many methods in the Abstract
   * Windows Toolkit (java.awt) throw a {@link HeadlessException} if this
   * returns true. This default implementation returns isHeadless(), so
   * subclasses need only override it if they differ.
   *
   * @return true if the environment is headless, meaning that graphics are
   *         unsupported
   * @since 1.4
   */
  public boolean isHeadlessInstance()
  {
    return isHeadless();
  }

  /**
   * Get an array of all the GraphicsDevice objects.
   *
   * @return the available graphics devices, may be 0 length
   * @throws HeadlessException if the environment is headless
   */
  public abstract GraphicsDevice[] getScreenDevices();

  /**
   * Get the default screen GraphicsDevice object.
   *
   * @return the default screen device
   * @throws HeadlessException if the environment is headless
   */
  public abstract GraphicsDevice getDefaultScreenDevice();

  /**
   * Return a Graphics2D object which will render into the specified image.
   *
   * @param image the image to render into
   * @return the object that renders into the image
   */
  public abstract Graphics2D createGraphics(BufferedImage image);

  /**
   * Returns an array of the one-point size fonts available in this
   * environment. From there, the user can select the font and derive the
   * correct one of proper size and attributes, using <code>deriveFont</code>.
   * Only one master version of each font appears in this array; if a font
   * can be derived from another, it must be created in that way.
   *
   * @return the array of available fonts
   * @see #getAvailableFontFamilyNames()
   * @see Font#deriveFont(int, float)
   * @since 1.2
   */
  public abstract Font[] getAllFonts();

  /**
   * Returns an array of the font family names available in this environment.
   * This allows flexibility in choosing the style of font, while still letting
   * the Font class decide its best match.
   *
   * @return the array of available font families
   * @see #getAllFonts()
   * @see Font#getFamily()
   * @since 1.2
   */
  public abstract String[] getAvailableFontFamilyNames();

  /**
   * Returns an array of the font family names available in this environment,
   * localized to the current Locale if l is non-null. This allows
   * flexibility in choosing the style of font, while still letting the Font
   * class decide its best match.
   *
   * @param l the locale to use
   * @return the array of available font families, localized
   * @see #getAllFonts()
   * @see Font#getFamily()
   * @since 1.2
   */
  public abstract String[] getAvailableFontFamilyNames(Locale l);

  /**
   * Returns the point where a window should be centered. You should probably
   * also check that the window fits within the screen bounds. The default
   * simply returns the center of the maximum window bounds; subclasses should
   * override this if native objects (like scrollbars) make that off-centered.
   *
   * @return the centering point
   * @throws HeadlessException if the environment is headless
   * @see #getMaximumWindowBounds()
   * @since 1.4
   */
  public Point getCenterPoint()
  {
    Rectangle r = getMaximumWindowBounds();
    return new Point(r.x + r.width / 2, r.y + r.height / 2);
  }

  /**
   * Returns the maximum bounds for a centered window object. The default
   * implementation simply returns the bounds of the default configuration
   * of the default screen; subclasses should override this to if native
   * objects (like scrollbars) reduce what is truly available. Also,
   * subclasses should override this if the window should be centered across
   * a multi-screen display.
   *
   * @return the maximum window bounds
   * @throws HeadlessException if the environment is headless
   * @see #getCenterPoint()
   * @see GraphicsConfiguration#getBounds()
   * @see Toolkit#getScreenInsets(GraphicsConfiguration)
   * @since 1.4
   */
  public Rectangle getMaximumWindowBounds()
  {
    return getDefaultScreenDevice().getDefaultConfiguration().getBounds();
  }
} // class GraphicsEnvironment
