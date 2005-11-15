/* FontUIResource.java
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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


package javax.swing.plaf;

import java.awt.Font;


/**
 * A font that is marked as <code>UIResource</code>, which
 * indicates that it has been installed by a pluggable
 * LookAndFeel. Such dimensions are replaced when the LookAndFeel
 * changes.
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class FontUIResource extends Font implements UIResource
{
  /**
   * Constructs a new <code>FontUIResource</code> given
   * the name, style and size of the font.
   *
   * @param name the name of the font. A number of
   *        &#x201c;logical&#x201d; names are supported by any Java
   *        implementation. These are
   *        <code>&#x201c;Dialog&#x201d;</code>,
   *        <code>&#x201c;DialogInput&#x201d;</code>,
   *        <code>&#x201c;Monospaced&#x201d;</code>,
   *        <code>&#x201c;Serif&#x201d;</code>, and
   *        <code>&#x201c;SansSerif&#x201d;</code>.
   *
   * @param style the style of the font, for instance {@link
   *        java.awt.Font#BOLD} or {@link java.awt.Font#PLAIN}.
   *
   * @param size the size of the font in typographic points, for
   *        instance 10, 12 or 13. Designers of LookAndFeels should be
   *        aware that some languages (like Japanese and Chinese) have
   *        glyphs that are too complex to be legible at small point
   *        sizes.
   */
  public FontUIResource(String name, int style, int size)
  {
    super(name, style, size);
  }


  /**
   * Constructs a new <code>FontUIResource</code> given
   * an existing font.
   *
   * @param f the font that serves as a template.
   */
  public FontUIResource(Font f)
  {
    /* This implementation will get rid of many font properties,
     * such as skewing, values of multiple master design axes,
     * etc.,  unless they get encoded into the name.  It probably
     * is not a problem for LookAndFeels because user interfaces
     * are usually not very advanced with respect to typography.
     */
    super(f.getName(), f.getStyle(), f.getSize());
  }
}
