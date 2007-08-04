/* FontDelegate.java -- Interface implemented by all font delegates.
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

import java.awt.Font;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.text.CharacterIterator;
import java.util.Locale;


/**
 * The interface that all font delegate objects implement,
 * irrespective of where they get their information from.
 *
 * <p><b>Thread Safety:</b> All classes that implement the
 * <code>FontDelegate</code> interface must allow calling these
 * methods from multiple concurrent threads. The delegates are
 * responsible for performing the necessary synchronization.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public interface FontDelegate
{
  public static final int FLAG_FITTED = 1 << 0;
  public static final int FLAG_NO_HINT_HORIZONTAL = 1 << 1;
  public static final int FLAG_NO_HINT_VERTICAL = 1 << 2;
  public static final int FLAG_NO_HINT_EDGE_POINTS = 1 << 3;
  public static final int FLAG_NO_HINT_STRONG_POINTS = 1 << 4;
  public static final int FLAG_NO_HINT_WEAK_POINTS = 1 << 5;

  /**
   * Returns the full name of this font face in the specified
   * locale, for example <i>&#x201c;Univers Light&#x201d;</i>.
   *
   * @param locale the locale for which to localize the name.
   *
   * @return the face name.
   */
  public String getFullName(Locale locale);
  
  
  /**
   * Returns the name of the family to which this font face belongs,
   * for example <i>&#x201c;Univers&#x201d;</i>.
   *
   * @param locale the locale for which to localize the name.
   *
   * @return the family name.
   */
  public String getFamilyName(Locale locale);


  /**
   * Returns the name of this font face inside the family, for example
   * <i>&#x201c;Light&#x201d;</i>.
   *
   * @param locale the locale for which to localize the name.
   *
   * @return the name of the face inside its family.
   */
  public String getSubFamilyName(Locale locale);
  
  
  /**
   * Returns the PostScript name of this font face, for example
   * <i>&#x201c;Helvetica-Bold&#x201d;</i>.
   *
   * @return the PostScript name, or <code>null</code> if the font
   * does not provide a PostScript name.
   */
  public String getPostScriptName();


  /**
   * Returns the number of glyphs in this font face.
   */
  public int getNumGlyphs();

  /**
   * Returns the glyph code for the specified character.
   *
   * @param c the character to map
   *
   * @return the glyph code
   */
  public int getGlyphIndex(int c);

  /**
   * Returns the index of the glyph which gets displayed if the font
   * cannot map a Unicode code point to a glyph. Many fonts show this
   * glyph as an empty box.
   */
  public int getMissingGlyphCode();
  

  /**
   * Creates a GlyphVector by mapping each character in a
   * CharacterIterator to the corresponding glyph.
   *
   * <p>The mapping takes only the font&#x2019;s <code>cmap</code>
   * tables into consideration. No other operations (such as glyph
   * re-ordering, composition, or ligature substitution) are
   * performed. This means that the resulting GlyphVector will not be
   * correct for text in languages that have complex
   * character-to-glyph mappings, such as Arabic, Hebrew, Hindi, or
   * Thai.
   *
   * @param font the font object that the created GlyphVector
   * will return when it gets asked for its font. This argument is
   * needed because the public API works with java.awt.Font,
   * not with some private delegate like OpenTypeFont.
   *
   * @param frc the font rendering parameters that are used for
   * measuring glyphs. The exact placement of text slightly depends on
   * device-specific characteristics, for instance the device
   * resolution or anti-aliasing. For this reason, any measurements
   * will only be accurate if the passed
   * <code>FontRenderContext</code> correctly reflects the relevant
   * parameters. Hence, <code>frc</code> should be obtained from the
   * same <code>Graphics2D</code> that will be used for drawing, and
   * any rendering hints should be set to the desired values before
   * obtaining <code>frc</code>.
   *
   * @param ci a CharacterIterator for iterating over the
   * characters to be displayed.
   */
  public GlyphVector createGlyphVector(Font font,
                                       FontRenderContext frc,
                                       CharacterIterator ci);


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
  public void getAdvance(int glyphIndex,
                         float pointSize,
                         AffineTransform transform,
                         boolean antialias,
                         boolean fractionalMetrics,
                         boolean horizontal,
                         Point2D advance);
  

  /**
   * Returns the shape of a glyph.
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
   * <code>false</code> for normal rendering. For hinted fonts, this
   * parameter may indeed affect the result.
   *
   * @param fractionalMetrics <code>true</code> for fractional
   * metrics, <code>false</code> for rounding the result to a pixel
   * boundary.
   *
   * @return the scaled and grid-fitted outline of the specified
   * glyph, or <code>null</code> for bitmap fonts.
   */
  public GeneralPath getGlyphOutline(int glyphIndex,
                                     float pointSize,
                                     AffineTransform transform,
                                     boolean antialias,
                                     boolean fractionalMetrics,
                                     int type);


  /**
   * Returns a name for the specified glyph. This is useful for
   * generating PostScript or PDF files that embed some glyphs of a
   * font.
   *
   * <p><b>Names are not unique:</b> Under some rare circumstances,
   * the same name can be returned for different glyphs. It is
   * therefore recommended that printer drivers check whether the same
   * name has already been returned for antoher glyph, and make the
   * name unique by adding the string ".alt" followed by the glyph
   * index.</p>
   *
   * <p>This situation would occur for an OpenType or TrueType font
   * that has a <code>post</code> table of format 3 and provides a
   * mapping from glyph IDs to Unicode sequences through a
   * <code>Zapf</code> table. If the same sequence of Unicode
   * codepoints leads to different glyphs (depending on contextual
   * position, for example, or on typographic sophistication level),
   * the same name would get synthesized for those glyphs.
   *
   * @param glyphIndex the glyph whose name the caller wants to
   * retrieve.
   */
  public String getGlyphName(int glyphIndex);


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
   * @return the ascent, which usually is a positive number.
   */
  public float getAscent(float pointSize,
                         AffineTransform transform,
                         boolean antialiased,
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
  public float getDescent(float pointSize,
                          AffineTransform transform,
                          boolean antialiased,
                          boolean fractionalMetrics,
                          boolean horizontal);
}
