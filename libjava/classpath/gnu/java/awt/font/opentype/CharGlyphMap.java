/* CharGlyphMap.java -- Manages the 'cmap' table of TrueType fonts
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

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.ShortBuffer;
import java.nio.IntBuffer;


/**
 * A mapping from Unicode codepoints to glyphs. This mapping
 * does not perform any re-ordering or decomposition, so it
 * is not everything that is needed to support Unicode.
 *
 * <p>This class manages the <code>cmap</code> table of
 * OpenType and TrueType fonts.
 *
 * @see <a href="http://partners.adobe.com/asn/tech/type/opentype/cmap.jsp">
 *      the <code>cmap</code> part of Adobe&#x2019; OpenType Specification</a>
 *
 * @see <a href="http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html">
 *      the <code>cmap</code> section of Apple&#x2019;s TrueType Reference
 *      Manual</a>
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class CharGlyphMap
{
  private static final int PLATFORM_UNICODE = 0;
  private static final int PLATFORM_MACINTOSH = 1;
  private static final int PLATFORM_MICROSOFT = 3;


  /**
   * Determines the glyph index for a given Unicode codepoint.  Users
   * should be aware that the character-to-glyph mapping not not
   * everything that is needed for full Unicode support.  For example,
   * the <code>cmap</code> table is not able to synthesize accented
   * glyphs from the canonical decomposition sequence, even if the
   * font would contain a glyph for the composed form.
   *
   * @param ucs4 the Unicode codepoint in UCS-4 encoding. Surrogates
   * (U+D800 to U+DFFF) cannot be passed, they must be mapped to
   * UCS-4 first.
   *
   * @return the glyph index, or 0 if the font does not contain
   * a glyph for this codepoint.
   */
  public abstract int getGlyph(int ucs4);


  /**
   * Reads a CharGlyphMap from an OpenType or TrueType <code>cmap</code>
   * table. The current implementation works as follows:
   * 
   * <p><ol><li>If the font has a type 4 cmap for the Unicode platform
   * (encoding 0, 1, 2, 3 or 4), or a type 4 cmap for the Microsoft
   * platform (encodings 1 or 10), that table is used to map Unicode
   * codepoints to glyphs.  Most recent fonts, both for Macintosh and
   * Windows, should provide such a table.</li>
   *
   * <li>Otherwise, if the font has any type 0 cmap for the Macintosh
   * platform, a Unicode-to-glyph mapping is synthesized from certain
   * type 0 cmaps. The current implementation collects mappings from
   * Roman, Icelandic, Turkish, Croatian, Romanian, Eastern European,
   * Cyrillic, Greek, Hebrew, Arabic and Farsi cmaps.</li>.</ol>
   *
   * @param buf a buffer whose position is right at the start
   * of the entire <code>cmap</code> table, and whose limit
   * is at its end.
   *
   * @return a concrete subclass of <code>CharGlyphMap</code>
   * that performs the mapping.
   *
   * @see <a href=
   * "http://partners.adobe.com/asn/tech/type/opentype/cmap.jsp"
   * >the <code>cmap</code> part of Adobe&#x2019; OpenType Specification</a>
   *
   * @see <a href=
   * "http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html"
   * >the <code>cmap</code> section of Apple&#x2019;s TrueType Reference
   * Manual</a>
   */
  public static CharGlyphMap forTable(ByteBuffer buf)
  {
    boolean hasType0 = false;
    int start4 = -1, platform4 = 0, encoding4 = 0;
    int start12 = -1, platform12 = 0, encoding12 = 0;
    int version;
    int numTables;
    int tableStart = buf.position();
    int limit = buf.limit();
    int format, platform, language, encoding, length, offset;

    version = buf.getChar();
    if (version != 0)
      return null;
    
    numTables = buf.getChar();
    for (int i = 0; i < numTables; i++)
    {
      buf.limit(limit).position(tableStart + 4 + i * 8);
      platform = buf.getChar();
      encoding = buf.getChar();
      offset = tableStart + buf.getInt();

      buf.position(offset);
      format = buf.getChar();

      switch (format)
      {
      case 0:
        hasType0 = true;
        break;

      case 4:
        length = buf.getChar();
        language = buf.getChar();
        if ((start4 == -1)
            && Type4.isSupported(platform, language, encoding))
        {
          start4 = offset;
          platform4 = platform;
          encoding4 = encoding;
        }
        break;

      case 12:
        if ((start12 == -1) && Type12.isSupported(platform, encoding))
        {
          start12 = offset;
          platform12 = platform;
          encoding12 = encoding;
        }
        break;
      }
    }


    if (start12 >= 0)
    {
      try
      {
        buf.limit(limit).position(start12);
        return new Type12(buf, platform12, encoding12);
      }
      catch (Exception ex)
      {
        ex.printStackTrace();
      }
    }

    if (start4 >= 0)
    {
      try
      {
        buf.limit(limit).position(start4);
        return Type4.readTable(buf, platform4, encoding4);
      }
      catch (Exception ex)
      {
      }
    }

    if (hasType0)
    {
      try
      {
        buf.limit(limit).position(tableStart);
        return new Type0(buf);
      }
      catch (Exception ex)
      {
      }
    }

    return new Dummy();
  }


  /**
   * A dummy mapping that maps anything to the undefined glyph.
   * Used if no other cmap is understood in a font.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static final class Dummy
    extends CharGlyphMap
  {
    public int getGlyph(int ucs4)
    {
      return 0;
    }
  }
  
  
  /**
   * A mapping from Unicode code points to glyph IDs through CMAP Type
   * 0 tables. These tables have serious limitations: Only the first
   * 256 glyphs can be addressed, and the source of the mapping is not
   * Unicode, but an encoding used on the Macintosh.
   *
   * <p>However, some fonts have only a Type 0 cmap. In this case, we
   * process all the Type 0 tables we understand, and establish
   * a reversed glyph-to-Unicode mapping. When a glyph is requested
   * for a given Unicode character, we perform a linear search on the
   * reversed table to find the glyph which maps to the requested
   * character. While not blazingly fast, this gives a reasonable
   * fallback for old fonts.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static final class Type0
    extends CharGlyphMap
  {
    /**
     * An array whose <code>i</code>-th element indicates the
     * Unicode code point of glyph <code>i</code> in the font.
     */
    private char[] glyphToUCS2 = new char[256];
    
    
    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Arabic encoding.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/ARABIC.TXT"
     * >the Unicode mapping table for the MacOS Arabic encoding</a>
     */
    private static final String UPPER_ARABIC
      = "\u007e\u0000\u00c4\u00a0\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u06ba\u00ab\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u2026\u00ee\u00ef\u00f1\u00f3\u00bb\u00f4\u00f6\u00f7"
      + "\u00fa\u00f9\u00fb\u00fc\u0020\u0021\"\u0023\u0024\u066a"
      + "\u0026\u0027\u0028\u0029\u002a\u002b\u060c\u002d\u002e\u002f"
      + "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669"
      + "\u003a\u061b\u003c\u003d\u003e\u061f\u274a\u0621\u0622\u0623"
      + "\u0624\u0625\u0626\u0627\u0628\u0629\u062a\u062b\u062c\u062d"
      + "\u062e\u062f\u0630\u0631\u0632\u0633\u0634\u0635\u0636\u0637"
      + "\u0638\u0639\u063a\u005b\\\u005d\u005e\u005f\u0640\u0641"
      + "\u0642\u0643\u0644\u0645\u0646\u0647\u0648\u0649\u064a\u064b"
      + "\u064c\u064d\u064e\u064f\u0650\u0651\u0652\u067e\u0679\u0686"
      + "\u06d5\u06a4\u06af\u0688\u0691\u007b\u007c\u007d\u0698\u06d2";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS East European Roman encoding.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/CENTEURO.TXT"
     * >the Unicode mapping table for the MacOS Central European
     * encoding</a>
     */
    private static final String UPPER_EAST_EUROPEAN_ROMAN
      = "\u007e\u0000\u00c4\u0100\u0101\u00c9\u0104\u00d6\u00dc\u00e1"
      + "\u0105\u010c\u00e4\u010d\u0106\u0107\u00e9\u0179\u017a\u010e"
      + "\u00ed\u010f\u0112\u0113\u0116\u00f3\u0117\u00f4\u00f6\u00f5"
      + "\u00fa\u011a\u011b\u00fc\u2020\u00b0\u0118\u00a3\u00a7\u2022"
      + "\u00b6\u00df\u00ae\u00a9\u2122\u0119\u00a8\u2260\u0123\u012e"
      + "\u012f\u012a\u2264\u2265\u012b\u0136\u2202\u2211\u0142\u013b"
      + "\u013c\u013d\u013e\u0139\u013a\u0145\u0146\u0143\u00ac\u221a"
      + "\u0144\u0147\u2206\u00ab\u00bb\u2026\u00a0\u0148\u0150\u00d5"
      + "\u0151\u014c\u2013\u2014\u201c\u201d\u2018\u2019\u00f7\u25ca"
      + "\u014d\u0154\u0155\u0158\u2039\u203a\u0159\u0156\u0157\u0160"
      + "\u201a\u201e\u0161\u015a\u015b\u00c1\u0164\u0165\u00cd\u017d"
      + "\u017e\u016a\u00d3\u00d4\u016b\u016e\u00da\u016f\u0170\u0171"
      + "\u0172\u0173\u00dd\u00fd\u0137\u017b\u0141\u017c\u0122\u02c7";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Roman encoding for the Croatian language.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/CROATIAN.TXT"
     * >the Unicode mapping table for the MacOS Croatian encoding</a>
     */
    private static final String UPPER_CROATIAN
      = "\u007e\u0000\u00c4\u00c5\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u00e3\u00e5\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u00ec\u00ee\u00ef\u00f1\u00f3\u00f2\u00f4\u00f6\u00f5"
      + "\u00fa\u00f9\u00fb\u00fc\u2020\u00b0\u00a2\u00a3\u00a7\u2022"
      + "\u00b6\u00df\u00ae\u0160\u2122\u00b4\u00a8\u2260\u017d\u00d8"
      + "\u221e\u00b1\u2264\u2265\u2206\u00b5\u2202\u2211\u220f\u0161"
      + "\u222b\u00aa\u00ba\u03a9\u017e\u00f8\u00bf\u00a1\u00ac\u221a"
      + "\u0192\u2248\u0106\u00ab\u010c\u2026\u00a0\u00c0\u00c3\u00d5"
      + "\u0152\u0153\u0110\u2014\u201c\u201d\u2018\u2019\u00f7\u25ca"
      + "\uf8ff\u00a9\u2044\u20ac\u2039\u203a\u00c6\u00bb\u2013\u00b7"
      + "\u201a\u201e\u2030\u00c2\u0107\u00c1\u010d\u00c8\u00cd\u00ce"
      + "\u00cf\u00cc\u00d3\u00d4\u0111\u00d2\u00da\u00db\u00d9\u0131"
      + "\u02c6\u02dc\u00af\u03c0\u00cb\u02da\u00b8\u00ca\u00e6\u02c7";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Cyrillic encoding.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/CYRILLIC.TXT"
     * >the Unicode mapping table for the MacOS Cyrillic encoding</a>
     */
    private static final String UPPER_CYRILLIC
      = "\u007e\u0000\u0410\u0411\u0412\u0413\u0414\u0415\u0416\u0417"
      + "\u0418\u0419\u041a\u041b\u041c\u041d\u041e\u041f\u0420\u0421"
      + "\u0422\u0423\u0424\u0425\u0426\u0427\u0428\u0429\u042a\u042b"
      + "\u042c\u042d\u042e\u042f\u2020\u00b0\u0490\u00a3\u00a7\u2022"
      + "\u00b6\u0406\u00ae\u00a9\u2122\u0402\u0452\u2260\u0403\u0453"
      + "\u221e\u00b1\u2264\u2265\u0456\u00b5\u0491\u0408\u0404\u0454"
      + "\u0407\u0457\u0409\u0459\u040a\u045a\u0458\u0405\u00ac\u221a"
      + "\u0192\u2248\u2206\u00ab\u00bb\u2026\u00a0\u040b\u045b\u040c"
      + "\u045c\u0455\u2013\u2014\u201c\u201d\u2018\u2019\u00f7\u201e"
      + "\u040e\u045e\u040f\u045f\u2116\u0401\u0451\u044f\u0430\u0431"
      + "\u0432\u0433\u0434\u0435\u0436\u0437\u0438\u0439\u043a\u043b"
      + "\u043c\u043d\u043e\u043f\u0440\u0441\u0442\u0443\u0444\u0445"
      + "\u0446\u0447\u0448\u0449\u044a\u044b\u044c\u044d\u044e\u20ac";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Arabic encoding with the Farsi language.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/FARSI.TXT"
     * >the Unicode mapping table for the MacOS Farsi encoding</a>
     */
    private static final String UPPER_FARSI
      = "\u007e\u0000\u00c4\u00a0\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u06ba\u00ab\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u2026\u00ee\u00ef\u00f1\u00f3\u00bb\u00f4\u00f6\u00f7"
      + "\u00fa\u00f9\u00fb\u00fc\u0020\u0021\"\u0023\u0024\u066a"
      + "\u0026\u0027\u0028\u0029\u002a\u002b\u060c\u002d\u002e\u002f"
      + "\u06f0\u06f1\u06f2\u06f3\u06f4\u06f5\u06f6\u06f7\u06f8\u06f9"
      + "\u003a\u061b\u003c\u003d\u003e\u061f\u274a\u0621\u0622\u0623"
      + "\u0624\u0625\u0626\u0627\u0628\u0629\u062a\u062b\u062c\u062d"
      + "\u062e\u062f\u0630\u0631\u0632\u0633\u0634\u0635\u0636\u0637"
      + "\u0638\u0639\u063a\u005b\\\u005d\u005e\u005f\u0640\u0641"
      + "\u0642\u0643\u0644\u0645\u0646\u0647\u0648\u0649\u064a\u064b"
      + "\u064c\u064d\u064e\u064f\u0650\u0651\u0652\u067e\u0679\u0686"
      + "\u06d5\u06a4\u06af\u0688\u0691\u007b\u007c\u007d\u0698\u06d2";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Greek encoding.
     *
     * @see <a
     * href="http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/GREEK.TXT"
     * >the Unicode mapping table for the MacOS Greek encoding</a>
     */
    private static final String UPPER_GREEK
      = "\u007e\u0000\u00c4\u00b9\u00b2\u00c9\u00b3\u00d6\u00dc\u0385"
      + "\u00e0\u00e2\u00e4\u0384\u00a8\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00a3\u2122\u00ee\u00ef\u2022\u00bd\u2030\u00f4\u00f6\u00a6"
      + "\u20ac\u00f9\u00fb\u00fc\u2020\u0393\u0394\u0398\u039b\u039e"
      + "\u03a0\u00df\u00ae\u00a9\u03a3\u03aa\u00a7\u2260\u00b0\u00b7"
      + "\u0391\u00b1\u2264\u2265\u00a5\u0392\u0395\u0396\u0397\u0399"
      + "\u039a\u039c\u03a6\u03ab\u03a8\u03a9\u03ac\u039d\u00ac\u039f"
      + "\u03a1\u2248\u03a4\u00ab\u00bb\u2026\u00a0\u03a5\u03a7\u0386"
      + "\u0388\u0153\u2013\u2015\u201c\u201d\u2018\u2019\u00f7\u0389"
      + "\u038a\u038c\u038e\u03ad\u03ae\u03af\u03cc\u038f\u03cd\u03b1"
      + "\u03b2\u03c8\u03b4\u03b5\u03c6\u03b3\u03b7\u03b9\u03be\u03ba"
      + "\u03bb\u03bc\u03bd\u03bf\u03c0\u03ce\u03c1\u03c3\u03c4\u03b8"
      + "\u03c9\u03c2\u03c7\u03c5\u03b6\u03ca\u03cb\u0390\u03b0\u00ad";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Hebrew encoding.
     *
     * <p>The codepoint 0x81 (HEBREW LIGATURE YIDDISH YOD YOD PATAH)
     * has no composed Unicode equivalent, but is expressed as the
     * sequence U+05F2 U+05B7 in Unicode. A similar situation exists
     * with the codepoint 0xC0 (HEBREW LIGATURE LAMED HOLAM), which
     * MacOS converts to U+F86A U+05DC U+05B9. To correctly deal
     * with these sequences, we probably should synthesize a ligature
     * table if a Hebrew font only provides a Type 0 CMAP.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/HEBREW.TXT"
     * >the Unicode mapping table for the MacOS Hebrew encoding</a>
     */
    private static final String UPPER_HEBREW
      = "\u007e\u0000\u00c4\u0000\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u00e3\u00e5\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u00ec\u00ee\u00ef\u00f1\u00f3\u00f2\u00f4\u00f6\u00f5"
      + "\u00fa\u00f9\u00fb\u00fc\u0020\u0021\"\u0023\u0024\u0025"
      + "\u20aa\u0027\u0029\u0028\u002a\u002b\u002c\u002d\u002e\u002f"
      + "\u0030\u0031\u0032\u0033\u0034\u0035\u0036\u0037\u0038\u0039"
      + "\u003a\u003b\u003c\u003d\u003e\u003f\u0000\u201e\uf89b\uf89c"
      + "\uf89d\uf89e\u05bc\ufb4b\ufb35\u2026\u00a0\u05b8\u05b7\u05b5"
      + "\u05b6\u05b4\u2013\u2014\u201c\u201d\u2018\u2019\ufb2a\ufb2b"
      + "\u05bf\u05b0\u05b2\u05b1\u05bb\u05b9\u0000\u05b3\u05d0\u05d1"
      + "\u05d2\u05d3\u05d4\u05d5\u05d6\u05d7\u05d8\u05d9\u05da\u05db"
      + "\u05dc\u05dd\u05de\u05df\u05e0\u05e1\u05e2\u05e3\u05e4\u05e5"
      + "\u05e6\u05e7\u05e8\u05e9\u05ea\u007d\u005d\u007b\u005b\u007c";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Roman encoding with the Icelandic language.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/ICELAND.TXT"
     * >the Unicode mapping table for the MacOS Icelandic encoding</a>
     */
    private static final String UPPER_ICELANDIC
      = "\u007e\u0000\u00c4\u00c5\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u00e3\u00e5\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u00ec\u00ee\u00ef\u00f1\u00f3\u00f2\u00f4\u00f6\u00f5"
      + "\u00fa\u00f9\u00fb\u00fc\u00dd\u00b0\u00a2\u00a3\u00a7\u2022"
      + "\u00b6\u00df\u00ae\u00a9\u2122\u00b4\u00a8\u2260\u00c6\u00d8"
      + "\u221e\u00b1\u2264\u2265\u00a5\u00b5\u2202\u2211\u220f\u03c0"
      + "\u222b\u00aa\u00ba\u03a9\u00e6\u00f8\u00bf\u00a1\u00ac\u221a"
      + "\u0192\u2248\u2206\u00ab\u00bb\u2026\u00a0\u00c0\u00c3\u00d5"
      + "\u0152\u0153\u2013\u2014\u201c\u201d\u2018\u2019\u00f7\u25ca"
      + "\u00ff\u0178\u2044\u20ac\u00d0\u00f0\u00de\u00fe\u00fd\u00b7"
      + "\u201a\u201e\u2030\u00c2\u00ca\u00c1\u00cb\u00c8\u00cd\u00ce"
      + "\u00cf\u00cc\u00d3\u00d4\uf8ff\u00d2\u00da\u00db\u00d9\u0131"
      + "\u02c6\u02dc\u00af\u02d8\u02d9\u02da\u00b8\u02dd\u02db\u02c7";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Roman encoding for most languages. Exceptions include
     * Croatian, Icelandic, Romanian, and Turkish.
     *
     * @see <a
     * href="http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/ROMAN.TXT"
     * >the Unicode mapping table for the MacOS Roman encoding</a>
     */
    private static final String UPPER_ROMAN
      = "\u007e\u0000\u00c4\u00c5\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u00e3\u00e5\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u00ec\u00ee\u00ef\u00f1\u00f3\u00f2\u00f4\u00f6\u00f5"
      + "\u00fa\u00f9\u00fb\u00fc\u2020\u00b0\u00a2\u00a3\u00a7\u2022"
      + "\u00b6\u00df\u00ae\u00a9\u2122\u00b4\u00a8\u2260\u00c6\u00d8"
      + "\u221e\u00b1\u2264\u2265\u00a5\u00b5\u2202\u2211\u220f\u03c0"
      + "\u222b\u00aa\u00ba\u03a9\u00e6\u00f8\u00bf\u00a1\u00ac\u221a"
      + "\u0192\u2248\u2206\u00ab\u00bb\u2026\u00a0\u00c0\u00c3\u00d5"
      + "\u0152\u0153\u2013\u2014\u201c\u201d\u2018\u2019\u00f7\u25ca"
      + "\u00ff\u0178\u2044\u20ac\u2039\u203a\ufb01\ufb02\u2021\u00b7"
      + "\u201a\u201e\u2030\u00c2\u00ca\u00c1\u00cb\u00c8\u00cd\u00ce"
      + "\u00cf\u00cc\u00d3\u00d4\uf8ff\u00d2\u00da\u00db\u00d9\u0131"
      + "\u02c6\u02dc\u00af\u02d8\u02d9\u02da\u00b8\u02dd\u02db\u02c7";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Roman encoding with the Romanian language.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/ROMANIAN.TXT"
     * >the Unicode mapping table for the MacOS Romanian encoding</a>
     */
    private static final String UPPER_ROMANIAN
      = "\u007e\u0000\u00c4\u00c5\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u00e3\u00e5\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u00ec\u00ee\u00ef\u00f1\u00f3\u00f2\u00f4\u00f6\u00f5"
      + "\u00fa\u00f9\u00fb\u00fc\u2020\u00b0\u00a2\u00a3\u00a7\u2022"
      + "\u00b6\u00df\u00ae\u00a9\u2122\u00b4\u00a8\u2260\u0102\u0218"
      + "\u221e\u00b1\u2264\u2265\u00a5\u00b5\u2202\u2211\u220f\u03c0"
      + "\u222b\u00aa\u00ba\u03a9\u0103\u0219\u00bf\u00a1\u00ac\u221a"
      + "\u0192\u2248\u2206\u00ab\u00bb\u2026\u00a0\u00c0\u00c3\u00d5"
      + "\u0152\u0153\u2013\u2014\u201c\u201d\u2018\u2019\u00f7\u25ca"
      + "\u00ff\u0178\u2044\u20ac\u2039\u203a\u021a\u021b\u2021\u00b7"
      + "\u201a\u201e\u2030\u00c2\u00ca\u00c1\u00cb\u00c8\u00cd\u00ce"
      + "\u00cf\u00cc\u00d3\u00d4\uf8ff\u00d2\u00da\u00db\u00d9\u0131"
      + "\u02c6\u02dc\u00af\u02d8\u02d9\u02da\u00b8\u02dd\u02db\u02c7";


    /**
     * A String whose <code>charAt(i)</code> is the Unicode character
     * that corresponds to the codepoint <code>i + 127</code> in the
     * MacOS Roman encoding with the Turkish language.
     *
     * @see <a href=
     * "http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/TURKISH.TXT"
     * >the Unicode mapping table for the MacOS Turkish encoding</a>
     */
    private static final String UPPER_TURKISH
      = "\u007e\u0000\u00c4\u00c5\u00c7\u00c9\u00d1\u00d6\u00dc\u00e1"
      + "\u00e0\u00e2\u00e4\u00e3\u00e5\u00e7\u00e9\u00e8\u00ea\u00eb"
      + "\u00ed\u00ec\u00ee\u00ef\u00f1\u00f3\u00f2\u00f4\u00f6\u00f5"
      + "\u00fa\u00f9\u00fb\u00fc\u2020\u00b0\u00a2\u00a3\u00a7\u2022"
      + "\u00b6\u00df\u00ae\u00a9\u2122\u00b4\u00a8\u2260\u00c6\u00d8"
      + "\u221e\u00b1\u2264\u2265\u00a5\u00b5\u2202\u2211\u220f\u03c0"
      + "\u222b\u00aa\u00ba\u03a9\u00e6\u00f8\u00bf\u00a1\u00ac\u221a"
      + "\u0192\u2248\u2206\u00ab\u00bb\u2026\u00a0\u00c0\u00c3\u00d5"
      + "\u0152\u0153\u2013\u2014\u201c\u201d\u2018\u2019\u00f7\u25ca"
      + "\u00ff\u0178\u011e\u011f\u0130\u0131\u015e\u015f\u2021\u00b7"
      + "\u201a\u201e\u2030\u00c2\u00ca\u00c1\u00cb\u00c8\u00cd\u00ce"
      + "\u00cf\u00cc\u00d3\u00d4\uf8ff\u00d2\u00da\u00db\u00d9\uf8a0"
      + "\u02c6\u02dc\u00af\u02d8\u02d9\u02da\u00b8\u02dd\u02db\u02c7";


    /**
     * Constructs a CharGlyphMap.Type0 from all type 0 cmaps provided
     * by the font. The implementation is able to fuse multiple type
     * 0 cmaps, such as the MacRoman, Turkish, Icelandic and Croatian
     * encoding, into a single map from Unicode characters to glyph
     * indices.
     *
     * @param buf a ByteBuffer whose position is right at the
     * beginning of the entire cmap table of the font (<i>not</i>
     * at some subtable).
     */
    public Type0(ByteBuffer buf)
    {
      int numTables;
      int tableStart = buf.position();
      int limit = buf.limit();

      /* The CMAP version must be 0. */
      if (buf.getChar() != 0)
        throw new IllegalStateException();
    
      numTables = buf.getChar();
      for (int i = 0; i < numTables; i++)
      {
        buf.limit(limit).position(tableStart + 4 + i * 8);
        int platform = buf.getChar();
        int encoding = buf.getChar();
        int offset = tableStart + buf.getInt();

        buf.position(offset);
        int format = buf.getChar();
        int length = buf.getChar();
        buf.limit(offset + length);
        int language = buf.getChar();

        if (format == 0)
          readSingleTable(buf, platform, language, encoding);
      }
    }


    /**
     * Processes a CMAP Type 0 table whose platform, encoding and
     * language are already known.
     *
     * @param buf the buffer to read the table from, positioned
     *        right after the language tag.
     */
    private void readSingleTable(ByteBuffer buf,
                                 int platform, int language,
                                 int encoding)
    {
      String upper = getUpper129(platform, encoding, language);
      if (upper == null)
        return;

      /* Skip the MacOS codepoints [0 .. 31] because they do not
       * correspond to any Unicode codepoint.
       */
      buf.position(buf.position() + 32);

      /* Irrespective of script and language, the MacOS codepoints
       * [32 .. 126] correspond to the same Unicode codepoint.
       */
      for (int i = 32; i < 126; i++)
        glyphToUCS2[buf.get() & 0xff] = (char) i;

      for (int i = 127; i < 256; i++)
        glyphToUCS2[buf.get() & 0xff] = upper.charAt(i - 127);

      /* Glyph 0 is always the undefined character, which has
       * no codepoint in Unicode.
       */
      glyphToUCS2[0] = 0;
    }


    /**
     * Determines the glyph index for a given Unicode codepoint.
     *
     * @param ucs4 the Unicode codepoint in UCS-4 encoding.
     *
     * @return the glyph index, or 0 if the font does not contain
     * a glyph for this codepoint.
     */
    public int getGlyph(int ucs4)
    {
      /* This linear search is not exactly super fast. However,
       * only really ancient fonts have only a type 0 cmap,
       * so it should not hurt in very many cases. If it shows
       * to be a performance problem, one could do a binary search
       * on a 256-entry table sorted by Unicode codepoint. The
       * matching index of that table could then be used to look
       * up the glyph ID at that position.
       */
      for (int i = 0; i < 256; i++)
        if (glyphToUCS2[i] == ucs4)
          return i;
      return 0;
    }


    /**
     * Returns a String whose <code>charAt(i)</code> is the Unicode
     * character that corresponds to the codepoint <code>i +
     * 127</code> in the encoding specified by the platform, script
     * and language tag of a Type 0 CMAP.
     *
     * @param language the language tag in the cmap subtable.  For the
     * Macintosh platform, this is 0 to indicate language-neutral
     * encoding, or the MacOS language code <i>plus one.</i> The
     * Apple documentation does not mention that one needs to be
     * added, but the Adobe OpenType specification does.
     *
     * @return a String for mapping the top 129 characters to
     * UCS-2. If <code>platform</code> is not <code>1</code>
     * (indicating Macintosh), or if the combination of
     * <code>script</code> and <code>language</code> is not
     * recognized, <code>null</code> will be returned.
     */
    private static String getUpper129(int platform, int script, int language)
    {
      if (platform != PLATFORM_MACINTOSH)
        return null;

      switch (script)
      {
      case 0: /* smRoman */
        if (language == /* langIcelandic+1 */ 16)
          return UPPER_ICELANDIC;
        else if (language == /* langTurkish+1 */ 18)
          return UPPER_TURKISH;
        else if (language == /* langCroatian+1 */ 19)
          return UPPER_CROATIAN;
        else if (language == /* langRomanian+1 */ 38)
          return UPPER_ROMANIAN;
        else if (language == /* language-neutral */ 0)
          return UPPER_ROMAN;
        else
          return null;

      case 4: /* smArabic */
        if (language == /* langFarsi+1 */ 32)
          return UPPER_FARSI;
        else
          return UPPER_ARABIC;

      case 5: /* smHebrew */
        return UPPER_HEBREW;

      case 6: /* smGreek */
        return UPPER_GREEK;

      case 7: /* smCyrillic */
        return UPPER_CYRILLIC;

      case 29: /* smSlavic == smEastEurRoman */
        return UPPER_EAST_EUROPEAN_ROMAN;
      }

      return null;
    }
  }
  

  /**
   * A mapping from Unicode code points to glyph IDs through CMAP Type
   * 4 tables. These tables are able to map two-byte encoded text
   * to glyph IDs, such as Unicode Basic Multilingual Plane which
   * contains U+0000 .. U+FFFE without surrogates.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static final class Type4
    extends CharGlyphMap
  {
    /**
     * Determines whether this implementation supports a combination
     * of platform, language and encoding is supported for a type 4
     * <code>cmap</code> table.
     *
     * <p>Currently, we support the following combinations:
     *
     * <ul><li>the Unicode platform in encodings 0, 1, 2, 3 and
     * 4;</li>
     *
     * <li>the Microsoft platform in encodings 1 (Basic Multilingual
     * Plane) and 10 (full Unicode).</li></ul>
     *
     * <p>Most recent Macintosh fonts provide a type 4
     * <code>cmap</code> for Unicode. Microsoft recommends providing a
     * type 4 <code>cmap</code> for encoding 1 of the Microsoft
     * platform. The implementation of GNU Classpath supports both
     * variants.
     *
     * <p>Not supported are ShiftJIS, Big5, Wansung, Johab, and other
     * non-Unicode encodings. Text can easily be converted to Unicode
     * using the java.nio.charset package.
     */
    static boolean isSupported(int platform, int language, int encoding)
    {
      switch (platform)
      {
      case PLATFORM_UNICODE:
        return (encoding >= 0) && (encoding <= 4);

      case PLATFORM_MICROSOFT:
        return (encoding == /* Basic Multilingual Plane */ 1)
          || (encoding == /* Full Unicode */ 10);
      }

      return false;
    }


    /**
     * Processes a CMAP Type 4 table whose platform, encoding and
     * language are already known. We understand the Unicode platform
     * with encodings 0, 1, 2, 3 and 4, and the Microsoft platform
     * with encodings 1 (Unicode BMP) and 10 (UCS-4).
     *
     * @param buf the buffer to read the table from, positioned at
     * its beginning.
     *
     * @return a Type4 table, or <code>null</code> if the combination
     * of platform and encoding is not understood.
     */
    static Type4 readTable(ByteBuffer buf,
                           int platform, int encoding)
    {
      int tableStart = buf.position();
      char format = buf.getChar();
      int length = buf.getChar();
      int language = buf.getChar();

      if ((format != 4) || !isSupported(platform, language, encoding))
        throw new IllegalArgumentException();

      buf.limit(tableStart + length);

      int segCountX2 = buf.getChar();
      int segCount = segCountX2 / 2;
      int searchRange = buf.getChar();
      int entrySelector = buf.getChar();
      int rangeShift = buf.getChar();

      CharBuffer endCode, startCode, idRangeOffset_glyphID;
      ShortBuffer idDelta;

      int pos = buf.position();
      endCode = buf.asCharBuffer();
      pos += segCountX2 + /* reservedPad */ 2;

      buf.position(pos);
      startCode = buf.asCharBuffer();
      pos += segCountX2;

      buf.position(pos);
      idDelta = buf.asShortBuffer();
      pos += segCountX2;

      buf.position(pos);
      idRangeOffset_glyphID = buf.asCharBuffer();
      
      endCode.limit(segCount);
      startCode.limit(segCount);
      idDelta.limit(segCount);
      idRangeOffset_glyphID.limit((buf.limit() - pos) / 2);

      return new Type4(segCount,
                       endCode, startCode, idDelta,
                       idRangeOffset_glyphID);
    }


    private CharBuffer lastChar;
    private CharBuffer firstChar;
    private ShortBuffer idDelta;
    private CharBuffer rangeID;
    private int numSegments;

    private Type4(int numSegments,
                  CharBuffer lastChar, CharBuffer firstChar,
                  ShortBuffer idDelta, CharBuffer rangeID)
    {
      this.numSegments = numSegments;
      this.lastChar = lastChar;
      this.firstChar = firstChar;
      this.idDelta = idDelta;
      this.rangeID = rangeID;
    }


    /**
     * Determines the glyph index for a given Unicode codepoint.
     *
     * @param ucs4 the Unicode codepoint in UCS-4 encoding.
     *
     * @return the glyph index, or 0 if the font does not contain
     * a glyph for this codepoint.
     */
    public int getGlyph(int ucs4)
    {
      char c, segStart;
      int segment, idRangeOffset;

      if (ucs4 > 0xffff)
        return 0;

      c = (char) ucs4;
      segment = find(c);
      segStart = firstChar.get(segment);
      if ((c < segStart) || (c > lastChar.get(segment)))
        return 0;
      
      /*
       *      System.out.println("seg " + segment
       *                 + ", range=" + (int) rangeID[segment]
       *                 + ", delta=" + delta[segment]);
       */

      idRangeOffset = rangeID.get(segment);
      if (idRangeOffset == 0)
        return (int) (char) (((int) c) + idDelta.get(segment));
      int result = rangeID.get((idRangeOffset >> 1)
                               + (c - segStart) + segment);
      if (result == 0)
        return 0;
      return (int) (char) (result + idDelta.get(segment));
    }


    private int find(char c)
    {
      int min, max, mid;

      min = 0;
      max = numSegments - 1;
      mid = max >> 1;

      while (min < max)
      {
        // System.out.println("(" + min + "," + max + ") " + mid);
        char val = lastChar.get(mid);
        if (val == c)
          break;
        else if (val < c)
          min = mid + 1;
        else if (val > c)
          max = mid;
        mid = (min + max) >> 1;
      }

      return mid;
    }
  }


  /**
   * A mapping from Unicode code points to glyph IDs through CMAP Type
   * 12 tables. These tables are able to map four-byte encoded text
   * to glyph IDs, such as Unicode UCS-4.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static final class Type12
    extends CharGlyphMap
  {
    int numGroups;
    IntBuffer data;
    
    
    /**
     * Determines whether this implementation supports a combination
     * of platform and encoding for a type 12 <code>cmap</code> table.
     *
     * <p>Currently, we support the following combinations:
     *
     * <ul><li>the Unicode platform in encodings 0, 1, 2, 3 and
     * 4;</li>
     *
     * <li>the Microsoft platform in encodings 1 (Basic Multilingual
     * Plane) and 10 (full Unicode).</li></ul>
     */
    static boolean isSupported(int platform, int encoding)
    {
      switch (platform)
      {
      case PLATFORM_UNICODE:
        return (encoding >= 0) && (encoding <= 4);

      case PLATFORM_MICROSOFT:
        return (encoding == /* Basic Multilingual Plane */ 1)
          || (encoding == /* Full Unicode */ 10);
      }

      return false;
    }


    /**
     * Constructs a <code>cmap</code> type 12 table whose platform and
     * encoding are already known. We understand the Unicode platform
     * with encodings 0, 1, 2, 3 and 4, and the Microsoft platform
     * with encodings 1 (Unicode BMP) and 10 (UCS-4).
     *
     * @param buf the buffer to read the table from, positioned at
     * its beginning.
     */
    Type12(ByteBuffer buf, int platform, int encoding)
    {
      int tableStart = buf.position();
      int format = buf.getChar();
      if ((format != 12) || !isSupported(platform, encoding))
        throw new IllegalStateException();

      buf.getChar(); // skip reserved field
      buf.limit(tableStart + buf.getInt());
      int language = buf.getInt();
      numGroups = buf.getInt();
      data = buf.asIntBuffer();
    }
    
    
    /**
     * Determines the glyph index for a given Unicode codepoint.  Users
     * should be aware that the character-to-glyph mapping not not
     * everything that is needed for full Unicode support.  For example,
     * the <code>cmap</code> table is not able to synthesize accented
     * glyphs from the canonical decomposition sequence, even if the
     * font would contain a glyph for the composed form.
     *
     * @param ucs4 the Unicode codepoint in UCS-4 encoding. Surrogates
     * (U+D800 to U+DFFF) cannot be passed, they must be mapped to
     * UCS-4 first.
     *
     * @return the glyph index, or 0 if the font does not contain
     * a glyph for this codepoint.
     */
    public int getGlyph(int ucs4)
    {
      int min, max, mid, startCharCode, endCharCode;

      min = 0;
      max = numGroups - 1;
      mid = max >> 1;
      do
      {
        startCharCode = data.get(3 * mid);
        endCharCode = data.get(3 * mid + 1);

        
        /*
        System.out.println("group " + mid + " (U+"
        + Integer.toHexString(startCharCode)
        + " .. U+" + Integer.toHexString(endCharCode)
        + "): glyph " + (int) data.get(mid*3+2));
        */

        if ((startCharCode <= ucs4)  && (ucs4 <= endCharCode))
          return ucs4
            - startCharCode
            + /* startGlyphID */ data.get(mid * 3 + 2);
        
        if (endCharCode < ucs4)
          min = mid + 1;
        else
          max = mid;
        mid = (min + max) >> 1;
      }
      while (min < max);

      startCharCode = data.get(3 * mid);
      endCharCode = data.get(3 * mid + 1);
      if ((startCharCode <= ucs4)  && (ucs4 <= endCharCode))
        return ucs4
          - startCharCode
          + /* startGlyphID */ data.get(mid * 3 + 2);

      return 0;
    }
  }
}
