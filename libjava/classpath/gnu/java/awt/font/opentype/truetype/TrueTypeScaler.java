/* TrueTypeScaler.java -- Font scaler for TrueType outlines.
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

import gnu.java.awt.font.opentype.Hinter;
import gnu.java.awt.font.opentype.Scaler;

import java.awt.FontFormatException;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.nio.ByteBuffer;


/**
 * A scaler for fonts whose outlines are described in the TrueType
 * format.
 *
 * <p><b>Lack of Thread Safety:</b> Font scalers are intentionally
 * <i>not</i> safe to access from multiple concurrent threads.
 * Synchronization needs to be performed externally. Usually, the font
 * that uses this scaler already has obtained a lock before calling
 * the scaler.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public final class TrueTypeScaler
  extends Scaler
{
  /**
   * The TrueType or OpenType table that contains the glyph outlines.
   */
  private ByteBuffer glyfTable;


  /**
   * A helper object for loading glyph outlines.
   */
  private GlyphLoader glyphLoader;


  /**
   * A helper object for measuring the advance width and height of a
   * glyph.
   */
  private final GlyphMeasurer glyphMeasurer;

  private final Zone glyphZone;


  /**
   * The number of units per em. A typical value is 2048, but some
   * font use other numbers as well.
   */
  private int unitsPerEm;


  /**
   * Constructs a new TrueTypeScaler.
   *
   * @param unitsPerEm the number of font units per em. This value can
   * be retrieved from the font&#x2019;s <code>head</code> table.
   *
   * @param maxp the <code>maxp</code> table of the font, which
   * contains various constants needed for setting up the virtual
   * machine that interprets TrueType bytecodes.
   *
   * @param controlValueTable the <code>cvt</code> table of the font,
   * which contains the initial values of the control value table.
   *
   * @param fpgm the <code>fpgm</code> table of the font, which
   * contains a font program that is executed exactly once.  The
   * purpose of the font program is to define functions and to patch
   * the interpreter.
   *
   * @param locaFormat the format of the <code>loca</code> table.  The
   * value must be 0 for two-byte offsets, or 1 for four-byte
   * offsets. TrueType and OpenType fonts indicate the format in the
   * <code>indexToLoc</code> field of the <a href=
   * "http://partners.adobe.com/asn/tech/type/opentype/head.html"
   * >font header</a>.
   *
   * @param loca the <code>loca</code> table of the font, which
   * contains for each glyph the offset of its outline data
   * in <code>glyf</code>.
   *
   * @param glyf the <code>glyf</code> table of the font, which
   * contains the outline data for all glyphs in the font.
   *
   * @param preProgram the <code>prep</code> table of the font, which
   * contains a program that is executed whenever the point size or
   * the device transform have changed.  This program is called
   * pre-program because it gets executed before the instructions of
   * the individual glyphs.  If the font does not contain a
   * pre-program, pass <code>null</code>.
   *
   * @throws FontFormatException if <code>format</code> is neither 0
   * nor 1.
   */
  public TrueTypeScaler(int unitsPerEm,
                        ByteBuffer hhea,
                        ByteBuffer htmx,
                        ByteBuffer vhea,
                        ByteBuffer vtmx,
                        ByteBuffer maxp,                        
                        ByteBuffer controlValueTable,
                        ByteBuffer fpgm,
                        int locaFormat, ByteBuffer loca,
                        ByteBuffer glyf,
                        ByteBuffer preProgram)
    throws FontFormatException
  {
    int maxContours, maxPoints;
    VirtualMachine vm;

    maxContours = Math.max(/* maxContours */ (int) maxp.getChar(8),
                           /* maxCompositeContours */ (int) maxp.getChar(12))
      + /* fix for some broken fonts */ 8;
    maxPoints = Math.max(/* maxPoints */ (int) maxp.getChar(6),
                         /* maxCompositePoints */ (int) maxp.getChar(10))
      + /* fix for some broken fonts */ 12;


    glyphZone = new Zone(maxPoints + /* four phantom points */ 4);
    this.glyfTable = glyf;
    vm = new VirtualMachine(unitsPerEm, maxp,
                            controlValueTable, fpgm,
                            preProgram);

    GlyphLocator locator = GlyphLocator.forTable(locaFormat, loca, glyf);
    glyphMeasurer = new GlyphMeasurer(hhea, htmx, vhea, vtmx);
    glyphLoader = new GlyphLoader(locator, vm, unitsPerEm,
                                  maxContours, maxPoints,
                                  glyphMeasurer);

    this.unitsPerEm = unitsPerEm;
  }


  /**
   * Retrieves the scaled outline of a glyph, adjusting control points
   * to the raster grid if necessary.
   *
   * @param glyphIndex the glyph number whose outline is retrieved.
   *
   * @param pointSize the point size for the glyph.
   *
   * @param deviceTransform an affine transformation for the device.
   *
   * @param antialias whether or not the rasterizer will perform
   * anti-aliasing on the returned path.
   *
   * @param fractionalMetrics <code>false</code> for adjusting glyph
   * positions to the raster grid of device space.
   */
  public GeneralPath getOutline(int glyphIndex,
                                float pointSize,
                                AffineTransform deviceTransform,
                                boolean antialias,
                                boolean fractionalMetrics, Hinter hinter,
                                int type)
  {
    glyphLoader.loadGlyph(glyphIndex, pointSize, deviceTransform,
                          antialias, glyphZone, hinter);
    return glyphZone.getPath(type);
  }

  public Zone getRawOutline(int glyphIndex, AffineTransform transform)
  {
    Zone zone = new Zone(glyphZone.getCapacity());
    glyphLoader.loadGlyph(glyphIndex, transform, zone, null);
    return zone;
  }

  /**
   * Determines the advance width and height for a glyph.
   *
   * @param glyphIndex the glyph whose advance width and height is to
   * be determined.
   *
   * @param pointSize the point size of the font.
   *
   * @param transform a transform that is applied in addition to
   * scaling to the specified point size. This is often used for
   * scaling according to the device resolution. Those who lack any
   * aesthetic sense may also use the transform to slant or stretch
   * glyphs.
   *
   * @param antialias <code>true</code> for anti-aliased rendering,
   * <code>false</code> for normal rendering. For hinted fonts,
   * this parameter may indeed affect the result.
   *
   * @param fractionalMetrics <code>true</code> for fractional metrics,
   * <code>false</code> for rounding the result to a pixel boundary.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @param advance a point whose <code>x</code> and <code>y</code>
   * fields will hold the advance in each direction. It is possible
   * that both values are non-zero, for example if
   * <code>transform</code> is a rotation, or in the case of Urdu
   * fonts.
   */
  public void getAdvance(int glyphIndex,
                         float pointSize,
                         AffineTransform transform,
                         boolean antialias,
                         boolean fractionalMetrics,
                         boolean horizontal,
                         Point2D advance)
  {
    double x, y;
    double scaleFactor = (double) pointSize / unitsPerEm;

    /* FIXME: Should grid-fit if needed. Also, use cache if present
     * in the font.
     */
    advance.setLocation(
      scaleFactor * glyphMeasurer.getAdvanceWidth(glyphIndex, horizontal),
      scaleFactor * glyphMeasurer.getAdvanceHeight(glyphIndex, horizontal));
    
    transform.transform(advance, advance);
  }


  /**
   * Scales a value from font units to pixels, given the point size
   * and the transform.
   *
   * @param pointSize the point size of the font.
   *
   * @param transform a transform that is applied in addition to
   * scaling to the specified point size. This is often used for
   * scaling according to the device resolution.
   *
   * @param fractionalMetrics <code>true</code> for fractional
   * metrics, <code>false</code> for rounding the result to a pixel
   * boundary.
   *
   * @param horizontal <code>true</code> if the <code>funits</code>
   * value is along the x axis, <code>false</code> if it is along the
   * y axis.
   */
  private float scaleFromFUnits(int funits,
                                float pointSize,
                                AffineTransform transform,
                                boolean fractionalMetrics,
                                boolean horizontal)
  {
    double s;

    s = (double) pointSize / unitsPerEm;
    if (transform != null)
      s *= horizontal ? transform.getScaleY() : transform.getScaleX();
    s *= funits;
    if (!fractionalMetrics)
      s = Math.round(s);
    return (float) s;
  }


  /**
   * Determines the distance between the base line and the highest
   * ascender.
   *
   * @param pointSize the point size of the font.
   *
   * @param transform a transform that is applied in addition to
   * scaling to the specified point size. This is often used for
   * scaling according to the device resolution. Those who lack any
   * aesthetic sense may also use the transform to slant or stretch
   * glyphs.
   *
   * @param antialias <code>true</code> for anti-aliased rendering,
   * <code>false</code> for normal rendering. For hinted fonts,
   * this parameter may indeed affect the result.
   *
   * @param fractionalMetrics <code>true</code> for fractional metrics,
   * <code>false</code> for rounding the result to a pixel boundary.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @return the ascent, which usually is a positive number.
   */
  public float getAscent(float pointSize,
                         AffineTransform transform,
                         boolean antialias,
                         boolean fractionalMetrics,
                         boolean horizontal)
  {
    /* Note that the ascent is orthogonal to the direction of line
     * layout: If the line direction is horizontal, the measurement of
     * ascent is along the vertical axis, and vice versa.
     */
    return scaleFromFUnits(glyphMeasurer.getAscent(horizontal),
                           pointSize,
                           transform,
                           fractionalMetrics,
                           /* reverse */ !horizontal);
  }


  /**
   * Determines the distance between the base line and the lowest
   * descender.
   *
   * @param pointSize the point size of the font.
   *
   * @param transform a transform that is applied in addition to
   * scaling to the specified point size. This is often used for
   * scaling according to the device resolution. Those who lack any
   * aesthetic sense may also use the transform to slant or stretch
   * glyphs.
   *
   * @param antialiased <code>true</code> for anti-aliased rendering,
   * <code>false</code> for normal rendering. For hinted fonts,
   * this parameter may indeed affect the result.
   *
   * @param fractionalMetrics <code>true</code> for fractional metrics,
   * <code>false</code> for rounding the result to a pixel boundary.
   *
   * @param horizontal <code>true</code> for horizontal line layout,
   * <code>false</code> for vertical line layout.
   *
   * @return the descent, which usually is a nagative number.
   */
  public float getDescent(float pointSize,
                          AffineTransform transform,
                          boolean antialiased,
                          boolean fractionalMetrics,
                          boolean horizontal)
  {
    /* Note that the descent is orthogonal to the direction of line
     * layout: If the line direction is horizontal, the measurement of
     * descent is along the vertical axis, and vice versa.
     */
    return scaleFromFUnits(glyphMeasurer.getDescent(horizontal),
                           pointSize,
                           transform,
                           fractionalMetrics,
                           /* reverse */ !horizontal);
  }
}
