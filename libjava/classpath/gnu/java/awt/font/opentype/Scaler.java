/* Scaler.java -- Common superclass for font scalers.
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

import gnu.java.awt.font.opentype.truetype.Zone;

import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;


/**
 * An common superclass for all font scalers. The main task of font
 * scaler is to retrieve a scaled and hinted outline for a glyph.
 *
 * <p>To make text more legible, high-quality fonts contain
 * instructions (sometimes also called &#x201c;hints&#x201d;) for
 * moving the scaled control points towards the coordinate grid of the
 * display device.
 *
 * <p><b>Lack of Thread Safety:</b> Font scalers are intentionally
 * <i>not</i> safe to access from multiple concurrent
 * threads. Synchronization needs to be performed externally. Usually,
 * the font that uses this scaler already has obtained a lock before
 * calling the scaler.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class Scaler
{
  /**
   * Retrieves the scaled outline of a glyph, adjusting control points
   * to the raster grid if necessary.
   *
   * @param glyph the glyph number whose outline is retrieved.
   *
   * @param pointSize the point size of the font.
   *
   * @param transform a transform that is applied in addition to
   * scaling to the specified point size. This is often used for
   * scaling according to the device resolution. Those who lack any
   * aesthetic sense may also use the transform to slant or stretch
   * glyphs.
   *
   * @param antialias whether or not the rasterizer will perform
   * anti-aliasing on the returned path.
   *
   * @param fractionalMetrics <code>false</code> for adjusting glyph
   * positions to the raster grid of device space.
   *
   * @return the scaled and grid-fitted outline of the specified
   * glyph, or <code>null</code> for bitmap fonts.
   */
  public abstract GeneralPath getOutline(int glyph,
                                         float pointSize,
                                         AffineTransform transform,
                                         boolean antialias,
                                         boolean fractionalMetrics,
                                         Hinter hinter, int type);


  /**
   * Determines the advance width and height for a glyph.
   *
   * @param glyphIndex the glyph whose advance width is to be
   * determined.
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
   * fields will hold the advance in each direction. It is well
   * possible that both values are non-zero, for example for rotated
   * text or for Urdu fonts.
   */
  public abstract void getAdvance(int glyphIndex,
                                  float pointSize,
                                  AffineTransform transform,
                                  boolean antialias,
                                  boolean fractionalMetrics,
                                  boolean horizontal,
                                  Point2D advance);


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
  public abstract float getAscent(float pointSize,
                                  AffineTransform transform,
                                  boolean antialias,
                                  boolean fractionalMetrics,
                                  boolean horizontal);


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
  public abstract float getDescent(float pointSize,
                                   AffineTransform transform,
                                   boolean antialiased,
                                   boolean fractionalMetrics,
                                   boolean horizontal);

  /**
   * Returns the raw outline data. This is used for the autofitter atm.
   *
   * @param glyph the glyph index
   * @param transform the transform to apply
   *
   * @return the raw glyph outline
   */
  public abstract Zone getRawOutline(int glyph, AffineTransform transform);
}
