/* OpenTypeFontFactory.java -- Creates OpenType and TrueType fonts.
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

package gnu.java.awt.font.opentype;

import gnu.java.awt.font.FontDelegate;
import java.awt.FontFormatException;
import java.awt.font.OpenType;
import java.nio.ByteBuffer;


/**
 * A factory for creating fonts that are stored in an
 * <i>sfnt</i>-housed format, for example OpenType or TrueType.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public final class OpenTypeFontFactory
{
  /**
   * The constructor is private so nobody can construct an instance
   * of this class.
   */
  private OpenTypeFontFactory()
  {
  }


  /**
   * Creates FontDelegate objects for the fonts in the specified
   * buffer.  The following font formats are currently recognized:
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
    OpenTypeFont[] fonts;
    int version;

    version = buf.getInt(0);
    switch (version)
    {
    case 0x00010000:            // Microsoft Windows TrueType
    case OpenType.TAG_TYP1:     // Apple MacOS PostScript ('typ1')
    case OpenTypeFont.TAG_SFNT: // Apple MacOS TrueType ('sfnt')
    case OpenTypeFont.TAG_TRUE: // Apple MacOS TrueType ('true')
    case OpenTypeFont.TAG_OTTO: // OpenType
      return new OpenTypeFont[] { new OpenTypeFont(buf, 0) };
    }


    /* TrueType Collection, see "TrueType Collections" in
     * http://partners.adobe.com/asn/tech/type/opentype/otff.html
     */
    if (version == OpenTypeFont.TAG_TTCF)
    {
      // This code has never been tested.
      fonts = new OpenTypeFont[buf.getInt(8)];
      for (int i = 0; i < fonts.length; i++)
        fonts[i] = new OpenTypeFont(buf, buf.getInt(16 + 4 * i));
      return fonts;
    }


    /* The MacOS X .dfont format is a Macintosh resource fork in
     * a normal file, contaning one or several 'sfnt' resources.
     * Unfortunately, MacOS resource forks have no magic code
     * that could be used for identification. Instead, we just try
     * to extract at least one 'sfnt'.
     */
    try
    {
      MacResourceFork fork = new MacResourceFork(buf);
      MacResourceFork.Resource[] rsrc;

      rsrc = fork.getResources(OpenTypeFont.TAG_SFNT);
      fonts = new OpenTypeFont[rsrc.length];
      for (int i = 0; i < fonts.length; i++)      
        fonts[i] = new OpenTypeFont(rsrc[i].getContent(), 0);

      return fonts;
    }
    catch (Exception ex)
    {
    }

    throw new FontFormatException("not in OpenType or TrueType format");
  }
}
