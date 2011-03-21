/* ClasspathToolkit.java -- Abstract superclass for Classpath toolkits.
   Copyright (C) 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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


package gnu.java.awt;

import gnu.java.awt.peer.ClasspathDesktopPeer;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.java.awt.peer.EmbeddedWindowPeer;
import gnu.java.security.action.SetAccessibleAction;

import java.awt.AWTException;
import java.awt.Desktop;
import java.awt.Font;
import java.awt.FontFormatException;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.peer.DesktopPeer;
import java.awt.peer.RobotPeer;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.util.Map;

import javax.imageio.spi.IIORegistry;

/**
 * An abstract superclass for Classpath toolkits.
 *
 * <p>There exist some parts of AWT and Java2D that are specific to
 * the underlying platform, but for which the {@link Toolkit} class
 * does not provide suitable abstractions. Examples include some
 * methods of {@link Font} or {@link GraphicsEnvironment}. Those
 * methods use ClasspathToolkit as a central place for obtaining
 * platform-specific functionality.
 *
 * <p>In addition, ClasspathToolkit implements some abstract methods
 * of {@link java.awt.Toolkit} that are not really platform-specific,
 * such as the maintenance of a cache of loaded images.
 *
 * <p><b>Thread Safety:</b> The methods of this class may safely be
 * called without external synchronization. This also hold for any
 * inherited {@link Toolkit} methods. Subclasses are responsible for
 * the necessary synchronization.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class ClasspathToolkit
  extends Toolkit
{
  /**
   * Returns a shared instance of the local, platform-specific
   * graphics environment.
   *
   * <p>This method is specific to GNU Classpath. It gets called by
   * the Classpath implementation of {@link
   * GraphicsEnvironment.getLocalGraphcisEnvironment()}.
   */
  public abstract GraphicsEnvironment getLocalGraphicsEnvironment();

  /**
   * Acquires an appropriate {@link ClasspathFontPeer}, for use in
   * classpath's implementation of {@link java.awt.Font}.
   *
   * @param name The logical name of the font. This may be either a face
   * name or a logical font name, or may even be null. A default
   * implementation of name decoding is provided in
   * {@link ClasspathFontPeer}, but may be overridden in other toolkits.
   *
   * @param attrs Any extra {@link java.awt.font.TextAttribute} attributes
   * this font peer should have, such as size, weight, family name, or
   * transformation.
   */
  public abstract ClasspathFontPeer getClasspathFontPeer (String name,
                                                          Map<?,?> attrs);

  /**
   * Creates a {@link Font}, in a platform-specific manner.
   *
   * The default implementation simply constructs a {@link Font}, but some
   * toolkits may wish to override this, to return {@link Font} subclasses
   * which implement {@link java.awt.font.OpenType} or
   * {@link java.awt.font.MultipleMaster}.
   */
  public Font getFont (String name, Map attrs)
  {
    Font f = null;

    // Circumvent the package-privateness of the
    // java.awt.Font.Font(String,Map) constructor.
    try
      {
        Constructor fontConstructor = Font.class.getDeclaredConstructor
                                      (new Class[] { String.class, Map.class });
        AccessController.doPrivileged(new SetAccessibleAction(fontConstructor));
        f = (Font) fontConstructor.newInstance(new Object[] { name, attrs });
      }
    catch (IllegalAccessException e)
      {
        throw new AssertionError(e);
      }
    catch (NoSuchMethodException e)
      {
        throw new AssertionError(e);
      }
    catch (InstantiationException e)
      {
        throw new AssertionError(e);
      }
    catch (InvocationTargetException e)
      {
        throw new AssertionError(e);
      }
    return f;
  }

  /**
   * Creates a font, reading the glyph definitions from a stream.
   *
   * <p>This method provides the platform-specific implementation for
   * the static factory method {@link Font#createFont(int,
   * java.io.InputStream)}.
   *
   * @param format the format of the font data, such as {@link
   * Font#TRUETYPE_FONT}. An implementation may ignore this argument
   * if it is able to automatically recognize the font format from the
   * provided data.
   *
   * @param stream an input stream from where the font data is read
   * in. The stream will be advanced to the position after the font
   * data, but not closed.
   *
   * @throws IllegalArgumentException if <code>format</code> is
   * not supported.
   *
   * @throws FontFormatException if <code>stream</code> does not
   * contain data in the expected format, or if required tables are
   * missing from a font.
   *
   * @throws IOException if a problem occurs while reading in the
   * contents of <code>stream</code>.
   */
  public abstract Font createFont(int format, InputStream stream);

  /**
   * Creates a RobotPeer on a given GraphicsDevice.
   */
  public abstract RobotPeer createRobot (GraphicsDevice screen)
    throws AWTException;

  /**
   * Creates an embedded window peer, and associates it with an
   * EmbeddedWindow object.
   *
   * @param w The embedded window with which to associate a peer.
   */
  public abstract EmbeddedWindowPeer createEmbeddedWindow (EmbeddedWindow w);

  /**
   * Used to register ImageIO SPIs provided by the toolkit.
   *
   * Our default implementation does nothing.
   */
   public void registerImageIOSpis(IIORegistry reg)
   {
   }

   /**
    * Returns the number of mouse buttons.
    * (used by java.awt.MouseInfo).
    *
    * This dummy implementation returns -1 (no mouse).
    * toolkit implementors should overload this method if possible.
    * @since 1.5
    */
   public int getMouseNumberOfButtons()
   {
     return -1;
   }

   /* (non-Javadoc)
    * @see java.awt.Toolkit#createDesktopPeer(java.awt.Desktop)
    */
   protected DesktopPeer createDesktopPeer(Desktop target)
     throws HeadlessException
   {
     if (GraphicsEnvironment.isHeadless())
       throw new HeadlessException();

     return ClasspathDesktopPeer.getDesktop();
   }

}
