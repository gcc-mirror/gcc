/* OpenTypeFont.java -- Manages OpenType and TrueType fonts.
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

import java.awt.Font;
import java.awt.FontFormatException;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.OpenType;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.nio.ByteBuffer;
import java.text.CharacterIterator;
import java.util.Locale;

import gnu.java.awt.font.FontDelegate;
import gnu.java.awt.font.GNUGlyphVector;
import gnu.java.awt.font.autofit.AutoHinter;
import gnu.java.awt.font.opentype.truetype.TrueTypeScaler;
import gnu.java.awt.font.opentype.truetype.Zone;


/**
 * A font that takes its data from OpenType or TrueType font tables.
 *
 * <p>OpenType is an extension of the TrueType font format. In addition
 * to tables for names, kerning or layout, it also stores the shapes
 * of individual glyphs. Three formats are recognized for glyphs:
 * Quadratic splines (classic TrueType), cubic splines (PostScript),
 * and bitmaps.
 *
 * @see <a
 * href="http://partners.adobe.com/asn/tech/type/opentype/">Adobe&#x2019;s
 * OpenType specification</a>
 *
 * @see <a
 * href="http://developer.apple.com/fonts/TTRefMan/">Apple&#x2019;s</code>
 * TrueType specification</a>
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public final class OpenTypeFont
  implements FontDelegate
{
  static final int TAG_OTTO = 0x4f54544f; // 'OTTO'
  static final int TAG_SFNT = 0x73666e74; // 'sfnt'
  static final int TAG_TRUE = 0x74727565; // 'true'
  static final int TAG_TTCF = 0x74746366; // 'ttcf'
  static final int TAG_ZAPF = 0x5a617066; // 'Zapf'


  /**
   * A buffer containing the font data. Note that this may well be an
   * instance of the subclass MappedByteBuffer, in which case the
   * virtual memory subsystem can more efficiently handle requests for
   * font data. This is especially recommended for large font files
   * that contain many glyphs that are rarely accessed.
   */
  ByteBuffer buf;


  /**
   * The number of glyphs in this font.
   */
  final int numGlyphs;

  int[] tableTag, tableStart, tableLength;


  /**
   * The version of the font in 16.16 fixed-point encoding, for
   * example 0x00010000 for version 1.0. There are also two special
   * version IDs used by fonts for Apple Macintosh, namely 'true'
   * (0x74727565) and 'typ1'. OpenType fonts sometimes have 'OTTO' as
   * their version.
   */
  private int version;


  /**
   * The number of font units per em. For fonts with TrueType
   * outlines, this is usually a power of two (such as 2048). For
   * OpenType fonts with PostScript outlines, other values are
   * acceptable (such as 1000).
   */
  public int unitsPerEm;


  /**
   * A factor to convert font units into ems. This value is <code>1 /
   * unitsPerEm</code>.
   */
  private float emsPerUnit;


  /**
   * The scaler to which the actual scaling work is delegated.
   */
  private Scaler scaler;


  /**
   * A delegate object for mapping Unicode UCS-4 codepoints to glyph
   * IDs.
   */
  private CharGlyphMap cmap;


  /**
   * A delegate object for providing a name for each glyph.
   */
  private GlyphNamer glyphNamer;

  private Hinter hinter;

  /**
   * Constructs an OpenType or TrueType font.
   *
   * @param buf a buffer with the contents of the font file. It is
   * recommended to use a <code>MappedByteBuffer</code> for very
   * large font files.
   *
   * @param offsetTablePosition the position of the OpenType offset
   * table in the font file. The offset table of most OpenType and
   * TrueType fonts starts at position 0.  However, so-called TrueType
   * Collections support multiple OpenType fonts in a single file,
   * which allows sharing some glyphs between fonts. If many glyphs
   * are shared (for example all the Kanji glyphs between multiple
   * Japanese fonts), the space savings can be considerable. In that
   * case, the offset table of each individual font would start at its
   * own position.
   *
   * @throws java.awt.FontFormatException if the font data is
   * not in OpenType or TrueType format.
   */
  OpenTypeFont(ByteBuffer buf, int offsetTablePosition)
    throws FontFormatException
  {
    int numTables, searchRange, entrySelector, rangeShift;

    //buf = buf.duplicate();
    this.buf = buf;
    buf.limit(buf.capacity());
    buf.position(offsetTablePosition);

    /* Check that the font data is in a supported format. */
    version = buf.getInt();
    switch (version)
    {
    case 0x00010000:        // Microsoft TrueType
    case OpenType.TAG_TYP1: // Adobe PostScript embeded in Apple SFNT ('typ1')
    case TAG_SFNT:          // Apple TrueType
    case TAG_TRUE:          // Apple TrueType
    case TAG_OTTO:          // OpenType
      break;

    default:
      throw new FontFormatException("not in OpenType or TrueType format");
    }

    numTables = buf.getShort();
    searchRange = buf.getShort();
    entrySelector = buf.getShort();
    rangeShift = buf.getShort();

    tableTag = new int[numTables];
    tableStart = new int[numTables];
    tableLength = new int[numTables];
    int lastTag = 0;
    for (int i = 0; i < numTables; i++)
    {
      tableTag[i] = buf.getInt();
      if (lastTag >= tableTag[i])
        throw new FontFormatException("unordered OpenType table");

      buf.getInt(); // ignore checksum
      tableStart[i] = buf.getInt();
      tableLength[i] = buf.getInt();

      //System.out.println(tagToString(tableTag[i]) + ", " + tableLength[i]);
    }

    ByteBuffer head = getFontTable(OpenType.TAG_HEAD);
    if ((head.getInt(0) != 0x00010000)
        || (head.getInt(12) != 0x5f0f3cf5))
        throw new FontFormatException("unsupported head version");

    unitsPerEm = head.getChar(18);
    emsPerUnit = 1.0f / (float) unitsPerEm;


    ByteBuffer maxp = getFontTable(OpenType.TAG_MAXP);
    int maxpVersion = maxp.getInt(0);
    switch (maxpVersion)
    {
    case 0x00005000: /* version 0.5, with wrong fractional part */
      numGlyphs = maxp.getChar(4);
      break;

    case 0x00010000: /* version 1.0 */
      numGlyphs = maxp.getChar(4);
      scaler = new TrueTypeScaler(unitsPerEm,
                                  getFontTable(OpenType.TAG_HHEA),
                                  getFontTable(OpenType.TAG_HMTX),
                                  getFontTable(OpenType.TAG_VHEA),
                                  getFontTable(OpenType.TAG_VMTX),
                                  maxp,
                                  getFontTable(OpenType.TAG_CVT),
                                  getFontTable(OpenType.TAG_FPGM),
                                  /* loca format */ head.getShort(50),
                                  getFontTable(OpenType.TAG_LOCA),
                                  getFontTable(OpenType.TAG_GLYF),
                                  getFontTable(OpenType.TAG_PREP));
      break;

    default:
      throw new FontFormatException("unsupported maxp version");
    }
  }


  /**
   * Determines the index of a table into the offset table.  The
   * result can be used to find the offset and length of a table, as
   * in <code>tableStart[getTableIndex(TAG_NAME)]</code>.
   *
   * @param tag the table identifier, for instance
   * <code>OpenType.TAG_NAME</code>.
   *
   * @return the index of that table into the offset table, or
   * -1 if the font does not contain the table specified by
   * <code>tag</code>.
   */
  private int getTableIndex(int tag)
  {
    /* FIXME: Since the font specification requires tableTag[] to be
     * ordered, one should do binary search here.
     */
    for (int i = 0; i < tableTag.length; i++)
      if (tableTag[i] == tag)
        return i;
    return -1;
  }



  /**
   * Returns the name of the family to which this font face belongs,
   * for example <i>&#x201c;Univers&#x201d;</i>.
   *
   * @param locale the locale for which to localize the name.
   *
   * @return the family name.
   */
  public synchronized String getFamilyName(Locale locale)
  {
    String name;

    if (locale == null)
      locale = Locale.getDefault();

    name = getName(NameDecoder.NAME_FAMILY, locale);
    if (name == null)
      name = getName(NameDecoder.NAME_FAMILY, Locale.ENGLISH);
    if (name == null)
      name = getName(NameDecoder.NAME_FAMILY, /* any language */ null);
    if (name == null)
      name = getName(NameDecoder.NAME_FULL, locale);
    if (name == null)
      name = getName(NameDecoder.NAME_FULL, /* any language */ null);
    return name;
  }


  /**
   * Returns the name of this font face inside the family, for example
   * <i>&#x201c;Light&#x201d;</i>.
   *
   * @param locale the locale for which to localize the name.
   *
   * @return the name of the face inside its family.
   */
  public synchronized String getSubFamilyName(Locale locale)
  {
    String name;

    if (locale == null)
      locale = Locale.getDefault();

    name = getName(NameDecoder.NAME_SUBFAMILY, locale);
    if (name == null)
    {
      name = getName(NameDecoder.NAME_SUBFAMILY, Locale.ENGLISH);
      if ("Regular".equals(name))
        name = null;
    }

    if (name == null)
    {
      String lang = locale.getLanguage();
      if ("de".equals(lang))
        name = "Standard";
      else if ("fr".equals(lang))
        name = "Standard";
      else if ("it".equals(lang))
        name = "Normale";
      else if ("nl".equals(lang))
        name = "Normaal";
      else if ("fi".equals(lang))
        name = "Normaali";
      else if ("sv".equals(lang))
        name = "Normal";
      else
        name = "Regular";
    }

    return name;
  }



  /**
   * Returns the full name of this font face, for example
   * <i>&#x201c;Univers Light&#x201d;</i>.
   *
   * @param locale the locale for which to localize the name.
   *
   * @return the face name.
   */
  public synchronized String getFullName(Locale locale)
  {
    String name;

    if (locale == null)
      locale = Locale.getDefault();

    name = getName(NameDecoder.NAME_FULL, locale);
    if (name == null)
      name = getName(NameDecoder.NAME_FULL, Locale.ENGLISH);
    if (name == null)
      name = getName(NameDecoder.NAME_FULL, /* any language */ null);

    return name;
  }


  /**
   * Returns the PostScript name of this font face, for example
   * <i>&#x201c;Univers-Light&#x201d;</i>.
   *
   * @return the PostScript name, or <code>null</code> if the font
   * does not provide a PostScript name.
   */
  public synchronized String getPostScriptName()
  {
    return getName(NameDecoder.NAME_POSTSCRIPT, /* any language */ null);
  }


  /**
   * Returns the number of glyphs in this font face.
   */
  public int getNumGlyphs()
  {
    /* No synchronization is needed because the number of glyphs is
     * set in the constructor, and it cannot change during the
     * lifetime of the object.
     */
    return numGlyphs;
  }


  /**
   * Returns the index of the glyph which gets displayed if the font
   * cannot map a Unicode code point to a glyph. Many fonts show this
   * glyph as an empty box.
   */
  public int getMissingGlyphCode()
  {
    /* No synchronization is needed because the result is constant. */
    return 0;
  }


  /**
   * The font&#x2019;s name table, or <code>null</code> if this
   * table has not yet been accessed.
   */
  private ByteBuffer nameTable;


  /**
   * Extracts a String from the font&#x2019;s name table.
   *
   * @param name the numeric TrueType or OpenType name ID.
   *
   * @param locale the locale for which names shall be localized, or
   * <code>null</code> if the locale does mot matter because the name
   * is known to be language-independent (for example, because it is
   * the PostScript name).
   */
  private String getName(int name, Locale locale)
  {
    if (nameTable == null)
      nameTable = getFontTable(OpenType.TAG_NAME);
    return NameDecoder.getName(nameTable, name, locale);
  }


  /**
   * Returns the version of the font.
   *
   * @see java.awt.font.OpenType#getVersion
   *
   * @return the version in 16.16 fixed-point encoding, for example
   * 0x00010000 for version 1.0.
   */
  public int getVersion()
  {
    /* No synchronization is needed because the version is set in the
     * constructor, and it cannot change during the lifetime of the
     * object.
     */
    return version;
  }


  /**
   * Creates a view buffer for an OpenType table. The caller can
   * access the returned buffer without needing to synchronize access
   * from multiple threads.
   *
   * @param tag the table identifier, for example
   * <code>OpenType.GLYF</code>.
   *
   * @return a slice of the underlying buffer containing the table, or
   * <code>null</code> if the font does not contain the requested
   * table.
   */
  public synchronized ByteBuffer getFontTable(int tag)
  {
    int index, start, len;
    ByteBuffer result;

    index = getTableIndex(tag);
    if (index < 0)
      return null;

    start = tableStart[index];
    len = tableLength[index];
    buf.limit(start + len).position(start);
    result = buf.slice();
    result.limit(len);
    return result;
  }


  /**
   * Returns the size of one of the tables in the font,
   * or -1 if the table does not exist.
   */
  public int getFontTableSize(int tag)
  {
    int index = getTableIndex(tag);
    if (index == -1)
      return index;
    return tableLength[index];
  }


  private CharGlyphMap getCharGlyphMap()
  {
    if (cmap != null)
      return cmap;

    synchronized (this)
    {
      if (cmap == null)
      {
        int index = getTableIndex(OpenType.TAG_CMAP);
        int start = tableStart[index];
        buf.limit(start + tableLength[index]).position(start);
        cmap = CharGlyphMap.forTable(buf);
      }
      return cmap;
    }
  }



  /**
   * Looks up a glyph in the font&#x2019;s <code>cmap</code> tables,
   * without performing any glyph substitution or reordering. Because
   * of this limitation, this method cannot be used for script systems
   * that need advanced glyph mapping, such as Arabic, Korean, or even
   * Latin with exotic accents.
   *
   * <p>It is safe to call this method from any thread.
   *
   * @param ucs4 the Unicode codepoint in the 32-bit Unicode character
   * set UCS-4. Because UTF-16 surrogates do not correspond to a single
   * glyph, it does not make sense to pass them here.
   *
   * @return the glyph index, or zero if the font does not contain
   * a glyph for the specified codepoint.
   */
  public int getGlyph(int ucs4)
  {
    return getCharGlyphMap().getGlyph(ucs4);
  }


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
  public synchronized GlyphVector createGlyphVector(Font font,
                                                    FontRenderContext frc,
                                                    CharacterIterator ci)
  {
    // Initialize hinter if necessary.
    checkHinter(FontDelegate.FLAG_FITTED);

    CharGlyphMap cmap;
    int numGlyphs;
    int[] glyphs;
    int glyph;
    int c;

    cmap = getCharGlyphMap();
    numGlyphs = ci.getEndIndex() - ci.getBeginIndex();
    glyphs = new int[numGlyphs];
    glyph = 0;
    for (c = ci.first(); c != CharacterIterator.DONE; c = ci.next())
    {
      /* handle surrogate pairs */
      if (c >> 10 == 0x36) // U+D800 .. U+DBFF: High surrogate
        c = (((c & 0x3ff) << 10) | (ci.next() & 0x3ff)) + 0x10000;
      glyphs[glyph] = cmap.getGlyph(c);
      glyph += 1;
    }

    /* If we had surrogates, the allocated array is too large.
     * Because this will occur very rarely, it seems acceptable to
     * re-allocate a shorter array and copy the contents around.
     */
    if (glyph != numGlyphs)
    {
      int[] newGlyphs = new int[glyph];
      System.arraycopy(glyphs, 0, newGlyphs, 0, glyph);
      glyphs = newGlyphs;
    }

    return new GNUGlyphVector(this, font, frc, glyphs);
  }

  /**
   * Returns the glyph code for the specified character.
   *
   * @param c the character to map
   *
   * @return the glyph code
   */
  public int getGlyphIndex(int c)
  {
    return getCharGlyphMap().getGlyph(c);
  }

  /**
   * Determines the advance width for a glyph.
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
   * fields will hold the advance in each direction. It is possible
   * that both values are non-zero, for example if
   * <code>transform</code> is a rotation, or in the case of Urdu
   * fonts.
   */
  public synchronized void getAdvance(int glyphIndex,
                                      float pointSize,
                                      AffineTransform transform,
                                      boolean antialias,
                                      boolean fractionalMetrics,
                                      boolean horizontal,
                                      Point2D advance)
  {
    /* Delegate the measurement to the scaler.  The synchronization is
     * needed because the scaler is not synchronized.
     */
    scaler.getAdvance(glyphIndex, pointSize, transform,
                      antialias, fractionalMetrics, horizontal,
                      advance);
  }


  /**
   * Returns the shape of a glyph.
   *
   * @param glyph the glyph whose advance width is to be determined
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
  public synchronized GeneralPath getGlyphOutline(int glyph,
                                                  float pointSize,
                                                  AffineTransform transform,
                                                  boolean antialias,
                                                  boolean fractionalMetrics,
                                                  int flags)
  {
    /* The synchronization is needed because the scaler is not
     * synchronized.
     */
    checkHinter(flags);
    return scaler.getOutline(glyph, pointSize, transform,
                             antialias, fractionalMetrics, hinter, flags);
  }

  /**
   * Fetches the raw glyph outline for the specified glyph index. This is used
   * for the autofitter only ATM and is otherwise not usable for outside code.
   *
   * @param glyph the glyph index to fetch
   * @param transform the transform to apply
   *
   * @return the raw outline of that glyph
   */
  public synchronized Zone getRawGlyphOutline(int glyph,
                                              AffineTransform transform)
  {
    return scaler.getRawOutline(glyph, transform);
  }

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
  public synchronized String getGlyphName(int glyphIndex)
  {
    if (glyphNamer == null)
      glyphNamer = GlyphNamer.forTables(numGlyphs,
                                        getFontTable(OpenType.TAG_POST),
                                        getFontTable(TAG_ZAPF));

    return glyphNamer.getGlyphName(glyphIndex);
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
  public synchronized float getAscent(float pointSize,
                                      AffineTransform transform,
                                      boolean antialiased,
                                      boolean fractionalMetrics,
                                      boolean horizontal)
  {
    return scaler.getAscent(pointSize, transform,
                            antialiased, fractionalMetrics,
                            horizontal);
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
  public synchronized float getDescent(float pointSize,
                                       AffineTransform transform,
                                       boolean antialiased,
                                       boolean fractionalMetrics,
                                       boolean horizontal)
  {
    return scaler.getDescent(pointSize, transform,
                             antialiased, fractionalMetrics,
                             horizontal);
  }


  /**
   * Converts a four-byte tag identifier into a String that can be
   * displayed when debugging this class.
   *
   * @param tag the tag as an <code>int</code>.
   *
   * @return the tag in human-readable form, for example
   * <code>name</code> or <code>glyf</code>.
   */
  static String tagToString(int tag)
  {
    char[] c = new char[4];
    c[0] = (char) ((tag >> 24) & 0xff);
    c[1] = (char) ((tag >> 16) & 0xff);
    c[2] = (char) ((tag >> 8) & 0xff);
    c[3] = (char) (tag & 0xff);
    return new String(c);
  }

  /**
   * Checks if a hinter is installed and installs one when not.
   */
  private void checkHinter(int flags)
  {
    // When another hinting impl gets added (maybe a true TrueType hinter)
    // then add some options here. The Hinter interface might need to be
    // tweaked.
    if (hinter == null)
      {
        try
          {
            hinter = new AutoHinter();
            hinter.init(this);
          }
        catch (Exception ex)
          {
            // Protect from problems inside hinter.
            hinter = null;
            ex.printStackTrace();
          }
      }
    hinter.setFlags(flags);
  }
}
