/* ClasspathToolkit.java -- Abstract superclass for Classpath toolkits.
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package gnu.java.awt;

import java.awt.Image;
import java.awt.Dimension;
import java.awt.DisplayMode;
import java.awt.Font;
import java.awt.FontFormatException;
import java.awt.FontMetrics;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.image.ColorModel;
import java.awt.image.ImageProducer;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import gnu.java.awt.peer.ClasspathFontPeer;


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
   * A map from URLs to previously loaded images, used by {@link
   * #getImage(java.net.URL)}. For images that were loaded via a path
   * to an image file, the map contains a key with a file URL.
   */
  private Map imageCache;


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
   * Determines the current size of the default, primary screen.
   *
   * @throws HeadlessException if the local graphics environment is
   * headless, which means that no screen is attached and no user
   * interaction is allowed.
   */
  public Dimension getScreenSize()
  {
    DisplayMode mode;

    // getDefaultScreenDevice throws HeadlessException if the
    // local graphics environment is headless.
    mode = GraphicsEnvironment.getLocalGraphicsEnvironment()
      .getDefaultScreenDevice().getDisplayMode();

    return new Dimension(mode.getWidth(), mode.getHeight());
  }


  /**
   * Determines the current color model of the default, primary
   * screen.
   *
   * @see GraphicsEnvironment#getDefaultScreenDevice()
   * @see java.awt.GraphicsDevice#getDefaultConfiguration()
   * @see java.awt.GraphicsConfiguration#getColorModel()
   *
   * @throws HeadlessException if the local graphics environment is
   * headless, which means that no screen is attached and no user
   * interaction is allowed.
   */
  public ColorModel getColorModel()
  {
    // getDefaultScreenDevice throws HeadlessException if the
    // local graphics environment is headless.
    return GraphicsEnvironment.getLocalGraphicsEnvironment()
      .getDefaultScreenDevice().getDefaultConfiguration()
      .getColorModel();
  }

  /**
   * Retrieves the metrics for rendering a font on the screen.
   *
   * @param font the font whose metrics are requested.
   */
  public FontMetrics getFontMetrics(Font font)
  {
    return ((ClasspathFontPeer) font.getPeer ()).getFontMetrics (font);
  }


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

  public abstract ClasspathFontPeer getClasspathFontPeer (String name, Map attrs); 


  /** 
   * Creates a {@link Font}, in a platform-specific manner.
   * 
   * The default implementation simply constructs a {@link Font}, but some
   * toolkits may wish to override this, to return {@link Font} subclasses which
   * implement {@link java.awt.font.OpenType} or
   * {@link java.awt.font.MultipleMaster}.
   */

  public Font getFont (String name, Map attrs) 
  {
    return new Font (name, attrs);
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
   * Returns an image from the specified file, which must be in a
   * recognized format. The set of recognized image formats may vary
   * from toolkit to toolkit.
   *
   * <p>This method maintains a cache for images. If an image has been
   * loaded from the same path before, the cached copy will be
   * returned. The implementation may hold cached copies for an
   * indefinite time, which can consume substantial resources with
   * large images. Users are therefore advised to use {@link
   * #createImage(java.lang.String)} instead.
   *
   * <p>The default implementation creates a file URL for the
   * specified path and invokes {@link #getImage(URL)}.
   *
   * @param path A path to the image file.
   *
   * @return IllegalArgumentException if <code>path</code> does not
   * designate a valid path.
   */
  public Image getImage(String path)
  {
    try
    {
      return getImage(new File(path).toURL());
    }
    catch (MalformedURLException muex)
    {
      throw (IllegalArgumentException) new IllegalArgumentException(path)
        .initCause(muex);
    }
  }


  /**
   * Loads an image from the specified URL. The image data must be in
   * a recognized format. The set of recognized image formats may vary
   * from toolkit to toolkit.
   *
   * <p>This method maintains a cache for images. If an image has been
   * loaded from the same URL before, the cached copy will be
   * returned. The implementation may hold cached copies for an
   * indefinite time, which can consume substantial resources with
   * large images. Users are therefore advised to use {@link
   * #createImage(java.net.URL)} instead.
   *
   * @param url the URL from where the image is read.
   */
  public Image getImage(URL url)
  {
    Image result;

    synchronized (this)
    {
      // Many applications never call getImage. Therefore, we lazily
      // create the image cache when it is actually needed.
      if (imageCache == null)
        imageCache = new HashMap();
      else
      {
        result = (Image) imageCache.get(url);
        if (result != null)
          return result;
      }

      // The createImage(URL) method, which is specified by
      // java.awt.Toolkit, is not implemented by this abstract class
      // because it is platform-dependent. Once Classpath has support
      // for the javax.imageio package, it might be worth considering
      // that toolkits provide native stream readers. Then, the class
      // ClasspathToolkit could provide a general implementation that
      // delegates the image format parsing to javax.imageio.
      result = createImage(url);

      // It is not clear whether it would be a good idea to use weak
      // references here. The advantage would be reduced memory
      // consumption, since loaded images would not be kept
      // forever. But on VMs that frequently perform garbage
      // collection (which includes VMs with a parallel or incremental
      // collector), the image might frequently need to be re-loaded,
      // possibly over a slow network connection.
      imageCache.put(url, result);

      return result;
    }
  }


  /**
   * Returns an image from the specified file, which must be in a
   * recognized format.  The set of recognized image formats may vary
   * from toolkit to toolkit.
   *
   * <p>A new image is created every time this method gets called,
   * even if the same path has been passed before.
   *
   * <p>The default implementation creates a file URL for the
   * specified path and invokes {@link #createImage(URL)}.
   *
   * @param path A path to the file to be read in.
   */
  public Image createImage(String path)
  {
    try
    {
      // The abstract method createImage(URL) is defined by
      // java.awt.Toolkit, but intentionally not implemented by
      // ClasspathToolkit because it is platform specific.
      return createImage(new File(path).toURL());
    }
    catch (MalformedURLException muex)
    {
      throw (IllegalArgumentException) new IllegalArgumentException(path)
        .initCause(muex);
    }
  }
  
  /**
   * Creates an ImageProducer from the specified URL. The image is assumed
   * to be in a recognised format. If the toolkit does not implement the
   * image format or the image format is not recognised, null is returned.
   * This default implementation is overriden by the Toolkit implementations.
   *
   * @param url URL to read image data from.
   */
  public ImageProducer createImageProducer(URL url)
  {
    return null;
  }
}
