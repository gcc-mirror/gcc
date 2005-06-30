/* ComponentOrientation.java -- describes a component's orientation
   Copyright (C) 2000, 2001, 2002 Free Software Foundation

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

import java.io.Serializable;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * This class is used to differentiate different orientations for text layout.
 * It controls whether text flows left-to-right or right-to-left, and whether
 * lines are horizontal or vertical, as in this table:<br>
 * <pre>
 * LT      RT      TL      TR
 * A B C   C B A   A D G   G D A
 * D E F   F E D   B E H   H E B
 * G H I   I H G   C F I   I F C
 * </pre>
 * <b>LT</b> languages are most common (left-to-right lines, top-to-bottom).
 * This includes Western European languages, and optionally includes Japanese,
 * Chinese, and Korean. <b>RT</b> languages (right-to-left lines,
 * top-to-bottom) are mainly middle eastern, such as Hebrew and Arabic.
 * <b>TR</b> languages flow top-to-bottom in a line, right-to-left, and are
 * the basis of Japanese, Chinese, and Korean. Finally, <b>TL</b> languages
 * flow top-to-bottom in a line, left-to-right, as in Mongolian.
 *
 * <p>This is a pretty poor excuse for a type-safe enum, since it is not
 * guaranteed that orientation objects are unique (thanks to serialization),
 * yet there is no equals() method. You would be wise to compare the output
 * of isHorizontal() and isLeftToRight() rather than comparing objects with
 * ==, especially since more constants may be added in the future.
 *
 * @author Bryce McKinlay (bryce@albatross.co.nz)
 * @since 1.0
 * @status updated to 1.4
 */
public final class ComponentOrientation implements Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -4113291392143563828L;

  /** Constant for unknown orientation. */
  private static final int UNKNOWN_ID = 1;

  /** Constant for horizontal line orientation. */
  private static final int HORIZONTAL_ID = 2;

  /** Constant for left-to-right orientation. */
  private static final int LEFT_TO_RIGHT_ID = 4;

  /**
   * Items run left to right, and lines flow top to bottom. Examples: English,
   * French.
   */
  public static final ComponentOrientation LEFT_TO_RIGHT
    = new ComponentOrientation(HORIZONTAL_ID | LEFT_TO_RIGHT_ID);

  /**
   * Items run right to left, and lines flow top to bottom. Examples: Arabic,
   * Hebrew.
   */
  public static final ComponentOrientation RIGHT_TO_LEFT
    = new ComponentOrientation(HORIZONTAL_ID);

  /**
   * The orientation is unknown for the locale. For backwards compatibility,
   * this behaves like LEFT_TO_RIGHT in the instance methods.
   */
  public static final ComponentOrientation UNKNOWN
    = new ComponentOrientation(UNKNOWN_ID | HORIZONTAL_ID | LEFT_TO_RIGHT_ID);

  /**
   * The orientation of this object; bitwise-or of unknown (1), horizontal (2),
   * and left-to-right (4).
   *
   * @serial the orientation
   */
  private final int orientation;

  /**
   * Construct a given orientation.
   *
   * @param orientation the orientation
   */
  private ComponentOrientation(int orientation)
  {
    this.orientation = orientation;
  }

  /**
   * Returns true if the lines are horizontal, in which case lines flow
   * top-to-bottom. For example, English, Hebrew. Counterexamples: Japanese,
   * Chinese, Korean, Mongolian.
   *
   * @return true if this orientation has horizontal lines
   */
  public boolean isHorizontal()
  {
    return (orientation & HORIZONTAL_ID) != 0;
  }

  /**
   * If isHorizontal() returns true, then this determines whether items in
   * the line flow left-to-right. If isHorizontal() returns false, items in
   * a line flow top-to-bottom, and this determines if lines flow
   * left-to-right.
   *
   * @return true if this orientation flows left-to-right
   */
  public boolean isLeftToRight()
  {
    return (orientation & LEFT_TO_RIGHT_ID) != 0;
  }

  /**
   * Gets an orientation appropriate for the locale.
   *
   * @param locale the locale
   * @return the orientation for that locale
   * @throws NullPointerException if locale is null
   */
  public static ComponentOrientation getOrientation(Locale locale)
  {
    // Based on iterating over all languages defined in JDK 1.4, this behavior
    // matches Sun's. However, it makes me wonder if any non-horizontal
    // orientations even exist, as it sure contradicts their documentation.
    String language = locale.getLanguage();
    if ("ar".equals(language) || "fa".equals(language) || "iw".equals(language)
        || "ur".equals(language))
      return RIGHT_TO_LEFT;
    return LEFT_TO_RIGHT;
  }

  /**
   * Gets an orientation from a resource bundle. This tries the following:
   *
   * <ul>
   * <li>Use the key "Orientation" to find an instance of ComponentOrientation
   * in the bundle.</li>
   * <li>Get the locale of the resource bundle, and get the orientation of
   * that locale.</li>
   * <li>Give up and get the orientation of the default locale.</li>
   * </ul>
   *
   * @param bdl the bundle to use
   * @return the orientation
   * @throws NullPointerException if bdl is null
   * @deprecated use {@link #getOrientation(Locale)} instead
   */
  public static ComponentOrientation getOrientation(ResourceBundle bdl)
  {
    ComponentOrientation r;
    try
      {
        r = (ComponentOrientation) bdl.getObject("Orientation");
        if (r != null)
          return r;
      }
    catch (MissingResourceException ignored)
      {
      }
    catch (ClassCastException ignored)
      {
      }
    try
      {
        r = getOrientation(bdl.getLocale());
        if (r != null)
          return r;
      }
    catch (Exception ignored)
      {
      }
    return getOrientation(Locale.getDefault());
  }
} // class ComponentOrientation
