/* FontFactory.java -- Factory for font delegates.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.font;

import java.nio.ByteBuffer;

import java.awt.FontFormatException;
import gnu.java.awt.font.opentype.OpenTypeFontFactory;


/**
 * A factory for creating font delegate objects.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public final class FontFactory
{
  /**
   * The constructor is private so nobody can construct an instance
   */
  private FontFactory()
  {
  }

  
  /**
   * Creates FontDelegate objects for the fonts in the specified buffer.
   * The following font formats are currently recognized:
   * recognized font formats are:
   *
   * <p><ul>
   * <li>OpenType (*.otf);</li>
   * <li>TrueType (*.ttf);</li>
   * <li>TrueType Collections (*.ttc);</li>
   * <li>Apple MacOS X data-fork font (*.dfont).</li></ul>
   *
   * <p>Some formats may contain more than a single font, for example
   * *.ttc and *.dfont files. This is the reason why this function
   * returns an array.
   *
   * <p>The implementation reads data from the buffer only when
   * needed. Therefore, it greatly increases efficiency if
   * <code>buf</code> has been obtained through mapping a file into
   * the virtual address space.
   *
   * @throws FontFormatException if the font data is not in one of the
   * known formats.
   */
  public static FontDelegate[] createFonts(ByteBuffer buf)
    throws FontFormatException
  {
    return OpenTypeFontFactory.createFonts(buf);
  }
}
