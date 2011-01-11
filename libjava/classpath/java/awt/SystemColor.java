/* SystemColor.java -- access dynamic system color values
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;
import java.io.Serializable;

/**
 * This class contains the various "system colors" in use by the native
 * windowing system. The <code>getRGB()</code> method is dynamic on systems
 * which support dynamic system color changes, and most methods in the
 * superclass are written to use this dynamic value when reporting colors.
 * However, the <code>equals()</code> method is not dynamic, and does not
 * track the actual color of instances in this class. This means that equals
 * may give surprising results; you are better off relying on getRGB.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.1
 * @status updated to 1.4
 */
public final class SystemColor extends Color implements Serializable
{
  // Implementation note: To be serial compatible with JDK, this class must
  // violate the semantic meaning of super.value to be one of the
  // NUM_COLORS constants instead of the actual RGB value. Hence there are
  // a lot of ugly workarounds in Color and in this class. I would have
  // designed it MUCH differently, making a separate id field in this class.

  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 4503142729533789064L;

  /**
   * Array index of the desktop color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #desktop
   */
  public static final int DESKTOP = 0;

  /**
   * Array index of the active caption color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #activeCaption
   */
  public static final int ACTIVE_CAPTION = 1;

  /**
   * Array index of the active caption text color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #activeCaptionText
   */
  public static final int ACTIVE_CAPTION_TEXT = 2;

  /**
   * Array index of the active caption border color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #activeCaptionBorder
   */
  public static final int ACTIVE_CAPTION_BORDER = 3;

  /**
   * Array index of the inactive caption color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #inactiveCaption
   */
  public static final int INACTIVE_CAPTION = 4;

  /**
   * Array index of the inactive caption text color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #inactiveCaptionText
   */
  public static final int INACTIVE_CAPTION_TEXT = 5;

  /**
   * Array index of the inactive caption border color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #inactiveCaptionBorder
   */
  public static final int INACTIVE_CAPTION_BORDER = 6;

  /**
   * Array index of the window background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #window
   */
  public static final int WINDOW = 7;

  /**
   * Array index of the window border color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #windowBorder
   */
  public static final int WINDOW_BORDER = 8;

  /**
   * Array index of the window text color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #windowText
   */
  public static final int WINDOW_TEXT = 9;

  /**
   * Array index of the menu background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #menu
   */
  public static final int MENU = 10;

  /**
   * Array index of the menu text color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #menuText
   */
  public static final int MENU_TEXT = 11;

  /**
   * Array index of the text background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #text
   */
  public static final int TEXT = 12;

  /**
   * Array index of the text foreground color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #textText
  */
  public static final int TEXT_TEXT = 13;

  /**
   * Array index of the highlighted text background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #textHighlight
   */
  public static final int TEXT_HIGHLIGHT = 14;

  /**
   * Array index of the highlighted text foreground color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #textHighlightText
   */
  public static final int TEXT_HIGHLIGHT_TEXT = 15;

  /**
   * Array index of the inactive text foreground color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #textInactiveText
   */
  public static final int TEXT_INACTIVE_TEXT = 16;

  /**
   * Array index of the control background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #control
   */
  public static final int CONTROL = 17;

  /**
   * Array index of the control text color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #controlText
   */
  public static final int CONTROL_TEXT = 18;

  /**
   * Array index of the highlighted control background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #controlHighlight
   */
  public static final int CONTROL_HIGHLIGHT = 19;

  /**
   * Array index of the lightly highlighted control background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #controlLtHighlight
   */
  public static final int CONTROL_LT_HIGHLIGHT = 20;

  /**
   * Array index of the shadowed control background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #controlShadow
   */
  public static final int CONTROL_SHADOW = 21;

  /**
   * Array index of the darkly shadowed control background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #controlDkShadow
   */
  public static final int CONTROL_DK_SHADOW = 22;

  /**
   * Array index of the scrollbar background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #scrollbar
   */
  public static final int SCROLLBAR = 23;

  /**
   * Array index of the info background color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #info
   */
  public static final int INFO = 24;

  /**
   * Array index of the info text color.  Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   *
   * @see #infoText
   */
  public static final int INFO_TEXT = 25;

  /**
   * The number of system colors. Used by
   * {@link Toolkit#loadSystemColors(int[])}.
   */
  public static final int NUM_COLORS = 26;

  /**
   * The internal array used to dynamically update <code>getRGB()</code>.
   */
  private static final int[] colors = new int[NUM_COLORS];

  /** The desktop color. */
  public static final SystemColor desktop
    = new SystemColor(DESKTOP);

  /** The active caption background color. */
  public static final SystemColor activeCaption
    = new SystemColor(ACTIVE_CAPTION);

  /** The active caption text color. */
  public static final SystemColor activeCaptionText
    = new SystemColor(ACTIVE_CAPTION_TEXT);

  /** The active caption border color. */
  public static final SystemColor activeCaptionBorder
    = new SystemColor(ACTIVE_CAPTION_BORDER);

  /** The inactive caption background color. */
  public static final SystemColor inactiveCaption
    = new SystemColor(INACTIVE_CAPTION);

  /** The inactive caption text color. */
  public static final SystemColor inactiveCaptionText
    = new SystemColor(INACTIVE_CAPTION_TEXT);

  /** The inactive caption border color. */
  public static final SystemColor inactiveCaptionBorder
    = new SystemColor(INACTIVE_CAPTION_BORDER);

  /** The window background color. */
  public static final SystemColor window
    = new SystemColor(WINDOW);

  /** The window border color. */
  public static final SystemColor windowBorder
    = new SystemColor(WINDOW_BORDER);

  /** The window text color. */
  public static final SystemColor windowText
    = new SystemColor(WINDOW_TEXT);

  /** The menu background color. */
  public static final SystemColor menu
    = new SystemColor(MENU);

  /** The menu text color. */
  public static final SystemColor menuText
    = new SystemColor(MENU_TEXT);

  /** The text background color. */
  public static final SystemColor text
    = new SystemColor(TEXT);

  /** The text foreground color. */
  public static final SystemColor textText
    = new SystemColor(TEXT_TEXT);

  /** The highlighted text background color. */
  public static final SystemColor textHighlight
    = new SystemColor(TEXT_HIGHLIGHT);

  /** The highlighted text foreground color. */
  public static final SystemColor textHighlightText
    = new SystemColor(TEXT_HIGHLIGHT_TEXT);

  /** The inactive text color. */
  public static final SystemColor textInactiveText
    = new SystemColor(TEXT_INACTIVE_TEXT);

  /** The control background color. */
  public static final SystemColor control
    = new SystemColor(CONTROL);

  /** The control text color. */
  public static final SystemColor controlText
    = new SystemColor(CONTROL_TEXT);

  /** The control highlight color. */
  public static final SystemColor controlHighlight
    = new SystemColor(CONTROL_HIGHLIGHT);

  /** The control light highlight color. */
  public static final SystemColor controlLtHighlight
    = new SystemColor(CONTROL_LT_HIGHLIGHT);

  /** The control shadow color. */
  public static final SystemColor controlShadow
    = new SystemColor(CONTROL_SHADOW);

  /** The control dark shadow color. */
  public static final SystemColor controlDkShadow
    = new SystemColor(CONTROL_DK_SHADOW);

  /** The scrollbar color. */
  public static final SystemColor scrollbar
    = new SystemColor(SCROLLBAR);

  /** The info text background color. */
  public static final SystemColor info
    = new SystemColor(INFO);

  /** The info text foreground color. */
  public static final SystemColor infoText
    = new SystemColor(INFO_TEXT);

  /**
   * Construct a system color which is dynamically updated.
   *
   * @param id the color id
   */
  private SystemColor(int id)
  {
    // Note: See Color#Color(int, boolean) to explain why we use this
    // particular constructor.
    super(id, true);
  }

  /**
   * Returns the RGB value for this color, in the sRGB color space. The blue
   * value will be in bits 0-7, green in 8-15, red in 6-23, and the alpha
   * value (bits 24-31) is 0xff. This is dynamically updated, so it may not
   * match the results of <code>getRed()</code>, <code>getGreen()</code>, or
   * <code>getBlue()</code>.
   *
   * @return the current RGB value
   */
  public int getRGB()
  {
    Toolkit.getDefaultToolkit().loadSystemColors(colors);
    return colors[value] | ALPHA_MASK;
  }

  /**
   * Returns a paint context, used for filling areas of a raster scan with
   * the current value of this system color. Since the system colors may be
   * dynamically updated, the returned value may not always be the same; but
   * as the system color is solid, the context does not need any of the
   * passed parameters to do its job.
   *
   * @param cm the requested color model
   * @param deviceBounds the bounding box in device coordinates, ignored
   * @param userBounds the bounding box in user coordinates, ignored
   * @param xform the bounds transformation, ignored
   * @param hints any rendering hints, ignored
   * @return a context for painting this solid color
   */
  public PaintContext createContext(ColorModel cm, Rectangle deviceBounds,
                                    Rectangle2D userBounds,
                                    AffineTransform xform,
                                    RenderingHints hints)
  {
    Toolkit.getDefaultToolkit().loadSystemColors(colors);
    int color = colors[value] | ALPHA_MASK;
    if (context == null || color != context.color || !context.getColorModel().equals(cm))
      context = new ColorPaintContext(cm,color);
    return context;
  }

  /**
   * Returns a string describing this color. This is in the format
   * "java.awt.SystemColor[i=" + index + ']', where index is one of the
   * integer constants of this class. Unfortunately, this description
   * does not describe the current value of the color; for that you should
   * use <code>new Color(syscolor.getRGB()).toString()</code>.
   *
   * @return a string describing this color
   */
  public String toString()
  {
    return "java.awt.SystemColor[i=" + value + ']';
  }
} // class SystemColor
