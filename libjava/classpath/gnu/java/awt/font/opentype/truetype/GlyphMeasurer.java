/* GlyphMeasurer.java -- Helper for measuring TrueType glyphs.
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
import java.nio.ShortBuffer;


/**
 * A class for measuring TrueType and OpenType glyphs.
 *
 * <p><b>Lack of Thread Safety:</b> Glyph measurers are intentionally
 * <i>not</i> safe to access from multiple concurrent
 * threads. Synchronization needs to be performed externally. Usually,
 * the font has already obtained a lock before calling the scaler,
 * which in turn calls the GlyphMeasurer. It would thus be wasteful to
 * acquire additional locks for the GlyphMeasurer.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
final class GlyphMeasurer
{
  /**
   * A view buffer that allows accessing the contents of the
   * font&#x2019;s <code>hmtx</code> table as shorts.
   */
  private final ShortBuffer horizontalGlyphMetrics;


  /**
   * A view buffer that allows accessing the contents of the
   * font&#x2019;s <code>vmtx</code> table as shorts.
   */
  private final ShortBuffer verticalGlyphMetrics;


  private final int numLongHorizontalMetricsEntries;
  private final int numLongVerticalMetricsEntries;

  private final int horizontalAscent;
  private final int verticalAscent;

  private final int horizontalDescent;
  private final int verticalDescent;

  private final int horizontalLineGap;


  /**
   * Constructs a GlyphMeasurer from TrueType/OpenType font tables.
   *
   * @param hhea the <code>hhea</code> table, which contains
   * information about horizontal metrics that is common to all
   * glyphs.
   *
   * @param hmtx the <code>hmtx</code> table, which contains
   * glyph-specific information about horizontal metrics.
   *
   * @param vhea the <code>vhea</code> table, which contains
   * information about vertical metrics that is common to all
   * glyphs. If a font does not provide such a table, pass
   * <code>null</code>.
   *
   * @param vmtx the <code>vmtx</code> table, which contains
   * glyph-specific information about vertical metrics.  If a font
   * does not provide such a table, pass <code>null</code>.
   */
  GlyphMeasurer(ByteBuffer hhea, ByteBuffer hmtx,
                ByteBuffer vhea, ByteBuffer vmtx)
    throws FontFormatException
  {
    if ((hhea.getInt(0) != 0x00010000) || (hhea.getInt(30) != 0))
      throw new FontFormatException("unsupported hhea format");

    horizontalAscent = hhea.getShort(4);
    horizontalDescent = hhea.getShort(6);
    horizontalLineGap = hhea.getShort(8);

    numLongHorizontalMetricsEntries = hhea.getChar(34);
    horizontalGlyphMetrics = hmtx.asShortBuffer();

    if (vhea != null)
    {
      verticalAscent = vhea.getShort(4);
      verticalDescent = vhea.getShort(6);
      numLongVerticalMetricsEntries = vhea.getChar(34);
      verticalGlyphMetrics = vmtx.asShortBuffer();
    }
    else
    {
      verticalAscent = /* advanceWidthMax */ hhea.getChar(10) / 2;
      verticalDescent = -verticalAscent;
      numLongVerticalMetricsEntries = 0;
      verticalGlyphMetrics = null;
    }
  }


  /**
   * Returns the distance from the baseline to the highest ascender.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @return the maximal ascent, in font units.
   */
  public int getAscent(boolean horizontal)
  {
    return horizontal ? horizontalAscent : verticalAscent;
  }


  /**
   * Returns the distance from the baseline to the lowest descender.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @return the maximal descent, in font units.
   */
  public int getDescent(boolean horizontal)
  {
    return horizontal ? horizontalDescent : verticalDescent;
  }


  /**
   * Returns the typographic line gap.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @return the line gap, in font units.
   */
  public int getLineGap(boolean horizontal)
  {
    return horizontalLineGap;
  }


  /**
   * Determines the advance width of a glyph, without considering
   * hinting.
   *
   * @param glyphIndex the index of the glyph whose advance width is
   * to be determined.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @return the advance width, in font units.
   */
  public int getAdvanceWidth(int glyphIndex, boolean horizontal)
  {
    if (!horizontal)
      return 0;

    glyphIndex = Math.min(glyphIndex,
                          numLongHorizontalMetricsEntries - 1);
    return horizontalGlyphMetrics.get(glyphIndex << 1);
  }


  /**
   * Determines the advance width of a glyph, without considering
   * hinting.
   *
   * @param glyphIndex the index of the glyph whose advance width is
   * to be determined.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @return the advance width, in font units.
   */
  public int getAdvanceHeight(int glyphIndex, boolean horizontal)
  {
    if (horizontal)
      return 0;

    /* If a font does not provide vertical glyph metrics, advance
     * by the height of one horizontal line.
     */
    if (verticalGlyphMetrics == null)
      return horizontalAscent - horizontalDescent + horizontalLineGap;

    glyphIndex = Math.min(glyphIndex,
                          numLongVerticalMetricsEntries - 1);
    return verticalGlyphMetrics.get(glyphIndex << 1);
  }
}
