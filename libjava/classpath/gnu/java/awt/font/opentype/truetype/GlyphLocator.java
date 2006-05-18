/* GlyphLocator.java -- Locates outlines of TrueType glyphs.
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

package gnu.java.awt.font.opentype.truetype;

import java.awt.FontFormatException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.IntBuffer;


/**
 * Locates glyph outlines in a TrueType or OpenType <code>glyf</code>
 * table.
 *
 * @see <a href=
 * "http://partners.adobe.com/asn/tech/type/opentype/loca.html"
 * >Adobe&#x2019;s specification of the OpenType &#x2018;loca&#x2019;
 * table</a>
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
abstract class GlyphLocator
{
  /**
   * The actual glyph data of the font, which is contained in the
   * 'glyf' table.
   */
  protected ByteBuffer glyfTable;


  /**
   * Creates a new GlyphLocator for a <code>loca</code> table.
   *
   * @param format the format of the <code>loca</code> table.  The
   * value must be 0 for two-byte offsets, or 1 for four-byte
   * offsets. TrueType and OpenType fonts indicate the format in the
   * <code>indexToLoc</code> field of the <a href=
   * "http://partners.adobe.com/asn/tech/type/opentype/head.html"
   * >font header</a>.
   *
   * @param loca the <code>loca</code> table of the font, which
   * contains the position of each glyph in the <code>glyf</code>
   * table.
   *
   * @param glyf the <code>glyf</code> table of the font, which
   * contains the outline data of each glyph.
   *
   * @throws FontFormatException if <code>format</code> is neither 0
   * nor 1.
   */
  public static GlyphLocator forTable(int format, ByteBuffer loca,
                                      ByteBuffer glyf)
    throws FontFormatException
  {
    switch (format)
    {
    case 0:
      return new GlyphLocator.TwoByte(loca, glyf);

    case 1:
      return new GlyphLocator.FourByte(loca, glyf);
    
    default:
      throw new FontFormatException("unsupported loca format");
    }
  }


  /**
   * Locates the outline data for a glyph.
   *
   * <p>For efficiency, the glyph locator does not create a new buffer
   * for each invocation. Instead, this method always returns the same
   * buffer object. Therefore, the data of a glyph must have been read
   * completely before another glyph of the same font gets requested
   * through this method.
   *
   * @param glyph the number of the glyph whose outlines are to be
   * retrieved.
   *
   * @return a buffer whose position is set to the first byte of glyph
   * data, and whose limit is set to disallow accessing any data that
   * does not belong to the glyph. If there is no outline data for the
   * requested glyph, as would be the case for the space glyph, the
   * result will be <code>null</code>.
   */
  public abstract ByteBuffer getGlyphData(int glyph);


  /**
   * A GlyphLocator that locates glyphs using two-byte offsets,
   * interpreting <code>loca</code> tables of format 0.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private final static class TwoByte
    extends GlyphLocator
  {
    final CharBuffer indexToLoc;

    TwoByte(ByteBuffer loca, ByteBuffer glyf)
    {
      this.glyfTable = glyf;
      indexToLoc = loca.asCharBuffer();
    }


    public ByteBuffer getGlyphData(int glyph)
    {
      int offset, limit;
      offset = ((int) indexToLoc.get(glyph)) << 1;
      limit = ((int) indexToLoc.get(glyph + 1)) << 1;
      if (offset >= limit)
        return null;

      glyfTable.limit(limit).position(offset);
      return glyfTable;
    }
  }


  /**
   * A GlyphLocator that locates glyphs using four-byte offsets,
   * interpreting <code>loca</code> tables of format 1.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private final static class FourByte
    extends GlyphLocator
  {
    final IntBuffer indexToLoc;

    FourByte(ByteBuffer loca, ByteBuffer glyf)
    {
      this.glyfTable = glyf;
      indexToLoc = loca.asIntBuffer();
    }


    public ByteBuffer getGlyphData(int glyph)
    {
      int offset, limit;
      offset = indexToLoc.get(glyph);
      limit = indexToLoc.get(glyph + 1);
      if (offset >= limit)
        return null;

      glyfTable.limit(limit).position(offset);
      return glyfTable;
    }
  }
}
