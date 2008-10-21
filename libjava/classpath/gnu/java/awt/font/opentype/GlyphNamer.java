/* GlyphNamer.java -- Provides glyph names.
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

import gnu.java.lang.CPStringBuilder;

import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.CharBuffer;


/**
 * Provides names for glyphs, which is useful when embedding fonts
 * in PostScript or PDF documents.
 *
 * <p>If the font has a <code>Zapf</code> table, it is used to map
 * glyph IDs back to a sequence of Unicode codepoints, which then
 * makes it possible to look up or synthesize a PostScript glyph name
 * according to Adobe&#x2019;s conventions. This allows to extract the
 * original text from the generated PDF or PostScript file, which is
 * important for indexing, searching and extracting.
 *
 * <p>Otherwise, glyph names are taken from the <a href=
 * "http://developer.apple.com/fonts/TTRefMan/RM06/Chap6post.html"
 * ><code>post</code> table</a>. All known formats (1, 2, 2.5, 3 and
 * 4) are supported.
 *
 * <p><b>Open Tasks:</b> The code could be cleaner structured by
 * having separate sub-classes for each variant of the POST table.
 * Also, the implementation should not read in all glyph names if a
 * font provides them in a POST table of type 2. It would be
 * sufficient to just read in the offsets and delay the String
 * fetching and conversion to the time when the glyph name is actually
 * requested.
 *
 * <p><b>Lack of Thread Safety:</b> The GlyphNamer class is
 * intentionally <i>not</i> safe to access from multiple concurrent
 * threads. Synchronization needs to be performed externally. Usually,
 * the font has already obtained a lock before calling the GlyphNamer.
 * It would thus be wasteful to acquire additional locks for the
 * GlyphNamer.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
final class GlyphNamer
{
  /**
   * The 'post' table of the font.
   */
  private ByteBuffer postTable;


  /**
   * The 'Zapf' table of the font, or null if the font has no
   * such table.
   */
  private ByteBuffer zapfTable;


  /**
   * The offset of each glyph relative to the Zapf table,
   * or null if the font does not have a Zapf table.
   */
  private IntBuffer zapfOffsets;


  /**
   * The offset from the start of the Zapf table to the start
   * of the extra info area.
   */
  private int zapfExtraInfo;


  /**
   * The format of the post table, a Fixed 16.16 number.
   */
  private int postFormat;


  /**
   * An array of glyph names. Used for table formats 1, 2, 2.5.
   */
  private String[] glyphNames;


  /**
   * An array from glyph to character codes. Similar to the
   * workings of a Zapf table, but maps to CID instead of
   * Unicode. Used for table format 4.
   */
  private CharBuffer glyphCharacterCodes;


  /**
   * The PostScript names of the 258 standard Macintosh glyphs.  Note
   * that some of these glyphs are not in the Adobe Standard Glyph
   * List for New Fonts, namely .notdef, .null, nonmarkingreturn,
   * nonbreakingspace, apple, onesuperior, twosuperior, and
   * threesuperior.
   */
  private static final String[] STANDARD_POSTSCRIPT_GLYPH_NAMES =
  {
    ".notdef",               // glyph #0
    ".null",                 // glyph #1
    "nonmarkingreturn",      // glyph #2
    "space",                 // glyph #3
    "exclam",                // glyph #4
    "quotedbl",              // glyph #5
    "numbersign",            // glyph #6
    "dollar",                // glyph #7
    "percent",               // glyph #8
    "ampersand",             // glyph #9
    "quotesingle",           // glyph #10
    "parenleft",             // glyph #11
    "parenright",            // glyph #12
    "asterisk",              // glyph #13
    "plus",                  // glyph #14
    "comma",                 // glyph #15
    "hyphen",                // glyph #16
    "period",                // glyph #17
    "slash",                 // glyph #18
    "zero",                  // glyph #19
    "one",                   // glyph #20
    "two",                   // glyph #21
    "three",                 // glyph #22
    "four",                  // glyph #23
    "five",                  // glyph #24
    "six",                   // glyph #25
    "seven",                 // glyph #26
    "eight",                 // glyph #27
    "nine",                  // glyph #28
    "colon",                 // glyph #29
    "semicolon",             // glyph #30
    "less",                  // glyph #31
    "equal",                 // glyph #32
    "greater",               // glyph #33
    "question",              // glyph #34
    "at",                    // glyph #35
    "A",                     // glyph #36
    "B",                     // glyph #37
    "C",                     // glyph #38
    "D",                     // glyph #39
    "E",                     // glyph #40
    "F",                     // glyph #41
    "G",                     // glyph #42
    "H",                     // glyph #43
    "I",                     // glyph #44
    "J",                     // glyph #45
    "K",                     // glyph #46
    "L",                     // glyph #47
    "M",                     // glyph #48
    "N",                     // glyph #49
    "O",                     // glyph #50
    "P",                     // glyph #51
    "Q",                     // glyph #52
    "R",                     // glyph #53
    "S",                     // glyph #54
    "T",                     // glyph #55
    "U",                     // glyph #56
    "V",                     // glyph #57
    "W",                     // glyph #58
    "X",                     // glyph #59
    "Y",                     // glyph #60
    "Z",                     // glyph #61
    "bracketleft",           // glyph #62
    "backslash",             // glyph #63
    "bracketright",          // glyph #64
    "asciicircum",           // glyph #65
    "underscore",            // glyph #66
    "grave",                 // glyph #67
    "a",                     // glyph #68
    "b",                     // glyph #69
    "c",                     // glyph #70
    "d",                     // glyph #71
    "e",                     // glyph #72
    "f",                     // glyph #73
    "g",                     // glyph #74
    "h",                     // glyph #75
    "i",                     // glyph #76
    "j",                     // glyph #77
    "k",                     // glyph #78
    "l",                     // glyph #79
    "m",                     // glyph #80
    "n",                     // glyph #81
    "o",                     // glyph #82
    "p",                     // glyph #83
    "q",                     // glyph #84
    "r",                     // glyph #85
    "s",                     // glyph #86
    "t",                     // glyph #87
    "u",                     // glyph #88
    "v",                     // glyph #89
    "w",                     // glyph #90
    "x",                     // glyph #91
    "y",                     // glyph #92
    "z",                     // glyph #93
    "braceleft",             // glyph #94
    "bar",                   // glyph #95
    "braceright",            // glyph #96
    "asciitilde",            // glyph #97
    "Adieresis",             // glyph #98
    "Aring",                 // glyph #99
    "Ccedilla",              // glyph #100
    "Eacute",                // glyph #101
    "Ntilde",                // glyph #102
    "Odieresis",             // glyph #103
    "Udieresis",             // glyph #104
    "aacute",                // glyph #105
    "agrave",                // glyph #106
    "acircumflex",           // glyph #107
    "adieresis",             // glyph #108
    "atilde",                // glyph #109
    "aring",                 // glyph #110
    "ccedilla",              // glyph #111
    "eacute",                // glyph #112
    "egrave",                // glyph #113
    "ecircumflex",           // glyph #114
    "edieresis",             // glyph #115
    "iacute",                // glyph #116
    "igrave",                // glyph #117
    "icircumflex",           // glyph #118
    "idieresis",             // glyph #119
    "ntilde",                // glyph #120
    "oacute",                // glyph #121
    "ograve",                // glyph #122
    "ocircumflex",           // glyph #123
    "odieresis",             // glyph #124
    "otilde",                // glyph #125
    "uacute",                // glyph #126
    "ugrave",                // glyph #127
    "ucircumflex",           // glyph #128
    "udieresis",             // glyph #129
    "dagger",                // glyph #130
    "degree",                // glyph #131
    "cent",                  // glyph #132
    "sterling",              // glyph #133
    "section",               // glyph #134
    "bullet",                // glyph #135
    "paragraph",             // glyph #136
    "germandbls",            // glyph #137
    "registered",            // glyph #138
    "copyright",             // glyph #139
    "trademark",             // glyph #140
    "acute",                 // glyph #141
    "dieresis",              // glyph #142
    "notequal",              // glyph #143
    "AE",                    // glyph #144
    "Oslash",                // glyph #145
    "infinity",              // glyph #146
    "plusminus",             // glyph #147
    "lessequal",             // glyph #148
    "greaterequal",          // glyph #149
    "yen",                   // glyph #150
    "mu",                    // glyph #151
    "partialdiff",           // glyph #152
    "summation",             // glyph #153
    "product",               // glyph #154
    "pi",                    // glyph #155
    "integral",              // glyph #156
    "ordfeminine",           // glyph #157
    "ordmasculine",          // glyph #158
    "Omega",                 // glyph #159
    "ae",                    // glyph #160
    "oslash",                // glyph #161
    "questiondown",          // glyph #162
    "exclamdown",            // glyph #163
    "logicalnot",            // glyph #164
    "radical",               // glyph #165
    "florin",                // glyph #166
    "approxequal",           // glyph #167
    "Delta",                 // glyph #168
    "guillemotleft",         // glyph #169
    "guillemotright",        // glyph #170
    "ellipsis",              // glyph #171
    "nonbreakingspace",      // glyph #172
    "Agrave",                // glyph #173
    "Atilde",                // glyph #174
    "Otilde",                // glyph #175
    "OE",                    // glyph #176
    "oe",                    // glyph #177
    "endash",                // glyph #178
    "emdash",                // glyph #179
    "quotedblleft",          // glyph #180
    "quotedblright",         // glyph #181
    "quoteleft",             // glyph #182
    "quoteright",            // glyph #183
    "divide",                // glyph #184
    "lozenge",               // glyph #185
    "ydieresis",             // glyph #186
    "Ydieresis",             // glyph #187
    "fraction",              // glyph #188
    "currency",              // glyph #189
    "guilsinglleft",         // glyph #190
    "guilsinglright",        // glyph #191
    "fi",                    // glyph #192
    "fl",                    // glyph #193
    "daggerdbl",             // glyph #194
    "periodcentered",        // glyph #195
    "quotesinglbase",        // glyph #196
    "quotedblbase",          // glyph #197
    "perthousand",           // glyph #198
    "Acircumflex",           // glyph #199
    "Ecircumflex",           // glyph #200
    "Aacute",                // glyph #201
    "Edieresis",             // glyph #202
    "Egrave",                // glyph #203
    "Iacute",                // glyph #204
    "Icircumflex",           // glyph #205
    "Idieresis",             // glyph #206
    "Igrave",                // glyph #207
    "Oacute",                // glyph #208
    "Ocircumflex",           // glyph #209
    "apple",                 // glyph #210
    "Ograve",                // glyph #211
    "Uacute",                // glyph #212
    "Ucircumflex",           // glyph #213
    "Ugrave",                // glyph #214
    "dotlessi",              // glyph #215
    "circumflex",            // glyph #216
    "tilde",                 // glyph #217
    "macron",                // glyph #218
    "breve",                 // glyph #219
    "dotaccent",             // glyph #220
    "ring",                  // glyph #221
    "cedilla",               // glyph #222
    "hungarumlaut",          // glyph #223
    "ogonek",                // glyph #224
    "caron",                 // glyph #225
    "Lslash",                // glyph #226
    "lslash",                // glyph #227
    "Scaron",                // glyph #228
    "scaron",                // glyph #229
    "Zcaron",                // glyph #230
    "zcaron",                // glyph #231
    "brokenbar",             // glyph #232
    "Eth",                   // glyph #233
    "eth",                   // glyph #234
    "Yacute",                // glyph #235
    "yacute",                // glyph #236
    "Thorn",                 // glyph #237
    "thorn",                 // glyph #238
    "minus",                 // glyph #239
    "multiply",              // glyph #240
    "onesuperior",           // glyph #241
    "twosuperior",           // glyph #242
    "threesuperior",         // glyph #243
    "onehalf",               // glyph #244
    "onequarter",            // glyph #245
    "threequarters",         // glyph #246
    "franc",                 // glyph #247
    "Gbreve",                // glyph #248
    "gbreve",                // glyph #249
    "Idotaccent",            // glyph #250
    "Scedilla",              // glyph #251
    "scedilla",              // glyph #252
    "Cacute",                // glyph #253
    "cacute",                // glyph #254
    "Ccaron",                // glyph #255
    "ccaron",                // glyph #256
    "dcroat"                 // glyph #257
  };


  private GlyphNamer(int numGlyphs,
                     ByteBuffer postTable,
                     ByteBuffer zapfTable)
  {
    this.postTable = postTable;
    this.zapfTable = zapfTable;

    if ((zapfTable != null) && (zapfTable.getInt(0) == 0x00010000))
    {
      readZapf(numGlyphs);
      return;
    }

    readPost();
  }
  

  /**
   * Sets up the information which allows to retrieve the information
   * on demand.
   *
   * @param numGlyphs the number of glyphs in the font. This value
   * comes from the <code>maxp</code> table.
   */
  public static GlyphNamer forTables(int numGlyphs,
                                     ByteBuffer postTable,
                                     ByteBuffer zapfTable)
  {
    return new GlyphNamer(numGlyphs, postTable, zapfTable);
  }


  /**
   * Retrieves or synthesizes a PostScript name for the glyph.
   * Although the code is reasonably fast, it is recommended
   * to cache the results in the printer driver.
   *
   * <p>If a font provides a 'Zapf' table, the reverse mapping
   * from glyph to UTF-16 sequence is performed, and a glyph
   * name is synthesized following the recommendations by Adobe.
   * This allows to extract the original text from the generated
   * PostScript or PDF, which is a requirement for indexing
   * and searching.
   *
   * <p>If a font does not provide a 'Zapf' table, the glyph name
   * is taken from the 'post' table. Note that some fonts have
   * wrong data in their post data, in which case the resulting
   * name will be garbage. Usually, this does not hurt, unless
   * the user wants to extract text from the generated PostScript
   * or PDF file. The GNU implementation understands all known
   * formats of the post table (1, 2, 2.5, 3 and 4).
   *
   * @param glyph the index of the glyph whose name is to be
   * retrieved.
   *
   * @return the glyph name, such as <code>A</code>,
   * <code>gcircumflex</code>, <code>z_uni0302</code>, or
   * <code>u11C42</code>.</li>
   */
  String getGlyphName(int glyph)
  {
    if (zapfOffsets != null)
    {
      zapfTable.position(zapfOffsets.get(glyph) + 8);
      int numChars = zapfTable.getChar();
      char[] chars = new char[numChars];
      for (int i = 0; i < numChars; i++)
        chars[i] = zapfTable.getChar();
      return getGlyphName(chars);
    }


    /* Type 1, Type 2, Type 2.5 */
    if (glyphNames != null)
      return glyphNames[glyph];

    /* Type 4: Synthesized glyph name. */
    if (glyphCharacterCodes != null)
      return "a" + glyphCharacterCodes.get(glyph);

    /* Type 3: Arbitrary, but unique name for the glyph.
     *
     * To find out what a good naming scheme would be, we have printed
     * a document containing the character U+201C in the font
     * "Hiragino Kaku Gothic Pro W3" (by Dainippon Screen Mfg. Co.,
     * Ltd.) on Apple MacOS X 10.1.5.  This font has a type 3 'post'
     * table, and its 'cmap' maps U+201C to glyph #108. The generated
     * PostScript file defined a character whose name was "g108".
     *
     * Therefore, we use 'g' as name prefix. According to the
     * TrueType/OpenType specification, it should not matter what
     * prefix we use. On the other hand, it does not hurt either to be
     * compatible with a good printer driver.
     *
     * Actually, that specific font also contains a 'Zapf' table,
     * which allows to generate glyph names according to Adobe's
     * conventions, so that extracting text from and searching in the
     * generated PostScript or PDF becomes possible. While the Apple
     * PostScript printer driver does not seem to use the 'Zapf' table
     * for this purpose, we do.
     */
    return "g" + glyph;
  }


  /**
   * Sets up some buffers which allow to quickly read information from
   * the Zapf table.
   *
   * @see <a href=
   *      "http://developer.apple.com/fonts/TTRefMan/RM06/Chap6Zapf.html">
   *      Apple&#x2019;s documentation of the <code>Zapf</code> table</a>
   */
  private void readZapf(int numGlyphs)
  {
    zapfExtraInfo = zapfTable.getInt(4);
    zapfTable.position(8);
    zapfOffsets = zapfTable.asIntBuffer();
    zapfOffsets.limit(numGlyphs);
  }


  /**
   * Reads in the PostScript data from a <code>post</code> table of a
   * TrueType or OpenType font. The implementation currently
   * understands the table formats 1, 2, 2.5, 3, and 4.
   */
  private void readPost()
  {
    int numGlyphs, nameIndex, maxNameIndex;
    char[] nameIndices;
    String[] names;
    byte[] pascalName;

    postTable.position(0);
    postFormat = postTable.getInt();
    switch (postFormat)
    {
    case 0x00010000:
      glyphNames = STANDARD_POSTSCRIPT_GLYPH_NAMES;
      return;

    case 0x00020000:
      postTable.position(32);
      numGlyphs = postTable.getChar();
      glyphNames = new String[numGlyphs];
      pascalName = new byte[255];
      nameIndices = new char[numGlyphs];
      maxNameIndex = 0;
      for (int i = 0; i < numGlyphs; i++)
        maxNameIndex = Math.max(maxNameIndex,
                                nameIndices[i] = postTable.getChar());

      names = new String[Math.max(maxNameIndex - 258 + 1, 0)];
      for (int i = 258; i <= maxNameIndex; i++)
      {
        int nameLen = (postTable.get() & 0xff);
        postTable.get(pascalName, 0, nameLen);
        names[i - 258] = new String(pascalName, 0, nameLen);
      }
      for (int i = 0; i < numGlyphs; i++)
      {
        nameIndex = nameIndices[i];
        if (nameIndex < 258)
          glyphNames[i] = STANDARD_POSTSCRIPT_GLYPH_NAMES[nameIndex];
        else
          glyphNames[i] = names[nameIndex - 258];
      }
      return;

    case 0x00025000: // in case some font has a wrong representation of 2.5
    case 0x00028000:
      /* Format 2.5 is a re-ordering of the standard names. It has
       * been deprecated in February 2000, but might still occasionally
       * float around. Since it can be supported with so little code,
       * we do so.
       */
      postTable.position(32);
      numGlyphs = postTable.getChar();
      glyphNames = new String[numGlyphs];
      for (int i = 0; i < numGlyphs; i++)
        glyphNames[i] = STANDARD_POSTSCRIPT_GLYPH_NAMES[i + postTable.get()];
      return;

    case 0x00030000:
      /* Format 3 leaves it to the printer driver to choose whatever
       * name it wants to.
       */
      return;

    case 0x00040000:
      /* Format 4 is used by Apple for composite fonts that have
       * synthetic glyph names. The name of a glyph is "a" plus
       * the integer (in decimal notation) that follows the table
       * after numGlyphs.
       */
      postTable.position(32);
      numGlyphs = postTable.getChar();
      glyphCharacterCodes = postTable.asCharBuffer();
      glyphCharacterCodes.limit(numGlyphs);
      return;
    }
  }



  /* For generating the following tables, a quick-and-dirty Python
   * script was used. It is unlikely that we ever need to run it
   * again, but for information and convenient access, it is included
   * below. Initial '#' characters need to be removed from the generated
   * strings, they are present so that the lines not break in the middle
   * of Java escape sequences (no, this is not very clean).
   *
   * import string
   *
   * javaEscapes = {0x22:'\\"', 0x5c:'\\\\'}
   * def escape(c):
   *     if javaEscapes.has_key(c):
   *         return javaEscapes[c]
   *     elif 0x20 <= c <= 0x7e:
   *         return chr(c)
   *     else:
   *         return '\\u%04x' % c
   *
   * def dump(name, s, stride):
   *     s = ('#' * stride) + s
   *     print "  private static final String %s" % name
   *     for i in range(0, len(s), 60):
   *         print '    + "%s"' % s[i:i+60]
   *     
   * glyphs = {}
   * for line in open('aglfn13.txt', 'r').readlines():
   *     if line[0] == '#': continue
   *     [ucs, glyphName, desc] = line.split(';')
   *     glyph = int('0x' + ucs, 0)
   *     assert (not glyphs.has_key(glyph)) or (glyphs[glyph] == glyphName)
   *     glyphs[glyph] = glyphName
   * del glyphs[0] # arrowvertex
   * k = glyphs.keys()
   * k.sort()
   * numGlyphs = len(k)
   * names = ''
   * pos = []
   * for glyph in k:
   *     pos.append(len(names) + 1)
   *     names = names + '/' + glyphs[glyph]
   * dump('AGLFN_GLYPHS', string.join(map(escape, k), ''), 5)
   * dump('AGLFN_NAME_OFFSET', string.join(map(escape, pos), ''), 4)
   * dump('AGLFN_NAMES', names + '/', 0)
   */


  /**
   * A String that contains the Unicode codepoint for each glyph
   * in the Adobe Glyph List. The characters are in sorted order.
   *
   * Generated from the Adobe Glyph List for New Fonts, version 1.1
   * of 17 April 2003.
   *
   * @see <a href=
   * "http://partners.adobe.com/asn/tech/type/aglfn13.txt" >Adobe
   * Glyph List for New Fonts</a>
   */
  private static final String AGLFN_GLYPHS
    = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTU"
    + "VWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u00a1\u00a2\u00a3"
    + "\u00a4\u00a5\u00a6\u00a7\u00a8\u00a9\u00aa\u00ab\u00ac\u00ae"
    + "\u00af\u00b0\u00b1\u00b4\u00b5\u00b6\u00b7\u00b8\u00ba\u00bb"
    + "\u00bc\u00bd\u00be\u00bf\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5"
    + "\u00c6\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf"
    + "\u00d0\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d7\u00d8\u00d9"
    + "\u00da\u00db\u00dc\u00dd\u00de\u00df\u00e0\u00e1\u00e2\u00e3"
    + "\u00e4\u00e5\u00e6\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed"
    + "\u00ee\u00ef\u00f0\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f7"
    + "\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00fe\u00ff\u0100\u0101"
    + "\u0102\u0103\u0104\u0105\u0106\u0107\u0108\u0109\u010a\u010b"
    + "\u010c\u010d\u010e\u010f\u0110\u0111\u0112\u0113\u0114\u0115"
    + "\u0116\u0117\u0118\u0119\u011a\u011b\u011c\u011d\u011e\u011f"
    + "\u0120\u0121\u0122\u0123\u0124\u0125\u0126\u0127\u0128\u0129"
    + "\u012a\u012b\u012c\u012d\u012e\u012f\u0130\u0131\u0132\u0133"
    + "\u0134\u0135\u0136\u0137\u0138\u0139\u013a\u013b\u013c\u013d"
    + "\u013e\u013f\u0140\u0141\u0142\u0143\u0144\u0145\u0146\u0147"
    + "\u0148\u0149\u014a\u014b\u014c\u014d\u014e\u014f\u0150\u0151"
    + "\u0152\u0153\u0154\u0155\u0156\u0157\u0158\u0159\u015a\u015b"
    + "\u015c\u015d\u015e\u015f\u0160\u0161\u0162\u0163\u0164\u0165"
    + "\u0166\u0167\u0168\u0169\u016a\u016b\u016c\u016d\u016e\u016f"
    + "\u0170\u0171\u0172\u0173\u0174\u0175\u0176\u0177\u0178\u0179"
    + "\u017a\u017b\u017c\u017d\u017e\u017f\u0192\u01a0\u01a1\u01af"
    + "\u01b0\u01e6\u01e7\u01fa\u01fb\u01fc\u01fd\u01fe\u01ff\u0218"
    + "\u0219\u02bc\u02bd\u02c6\u02c7\u02d8\u02d9\u02da\u02db\u02dc"
    + "\u02dd\u0300\u0301\u0303\u0309\u0323\u0384\u0385\u0386\u0387"
    + "\u0388\u0389\u038a\u038c\u038e\u038f\u0390\u0391\u0392\u0393"
    + "\u0395\u0396\u0397\u0398\u0399\u039a\u039b\u039c\u039d\u039e"
    + "\u039f\u03a0\u03a1\u03a3\u03a4\u03a5\u03a6\u03a7\u03a8\u03aa"
    + "\u03ab\u03ac\u03ad\u03ae\u03af\u03b0\u03b1\u03b2\u03b3\u03b4"
    + "\u03b5\u03b6\u03b7\u03b8\u03b9\u03ba\u03bb\u03bd\u03be\u03bf"
    + "\u03c0\u03c1\u03c2\u03c3\u03c4\u03c5\u03c6\u03c7\u03c8\u03c9"
    + "\u03ca\u03cb\u03cc\u03cd\u03ce\u03d1\u03d2\u03d5\u03d6\u0401"
    + "\u0402\u0403\u0404\u0405\u0406\u0407\u0408\u0409\u040a\u040b"
    + "\u040c\u040e\u040f\u0410\u0411\u0412\u0413\u0414\u0415\u0416"
    + "\u0417\u0418\u0419\u041a\u041b\u041c\u041d\u041e\u041f\u0420"
    + "\u0421\u0422\u0423\u0424\u0425\u0426\u0427\u0428\u0429\u042a"
    + "\u042b\u042c\u042d\u042e\u042f\u0430\u0431\u0432\u0433\u0434"
    + "\u0435\u0436\u0437\u0438\u0439\u043a\u043b\u043c\u043d\u043e"
    + "\u043f\u0440\u0441\u0442\u0443\u0444\u0445\u0446\u0447\u0448"
    + "\u0449\u044a\u044b\u044c\u044d\u044e\u044f\u0451\u0452\u0453"
    + "\u0454\u0455\u0456\u0457\u0458\u0459\u045a\u045b\u045c\u045e"
    + "\u045f\u0462\u0463\u0472\u0473\u0474\u0475\u0490\u0491\u04d9"
    + "\u05b0\u05b1\u05b2\u05b3\u05b4\u05b5\u05b6\u05b7\u05b8\u05b9"
    + "\u05bb\u05bc\u05bd\u05be\u05bf\u05c0\u05c1\u05c2\u05c3\u05d0"
    + "\u05d1\u05d2\u05d3\u05d4\u05d5\u05d6\u05d7\u05d8\u05d9\u05da"
    + "\u05db\u05dc\u05dd\u05de\u05df\u05e0\u05e1\u05e2\u05e3\u05e4"
    + "\u05e5\u05e6\u05e7\u05e8\u05e9\u05ea\u05f0\u05f1\u05f2\u060c"
    + "\u061b\u061f\u0621\u0622\u0623\u0624\u0625\u0626\u0627\u0628"
    + "\u0629\u062a\u062b\u062c\u062d\u062e\u062f\u0630\u0631\u0632"
    + "\u0633\u0634\u0635\u0636\u0637\u0638\u0639\u063a\u0640\u0641"
    + "\u0642\u0643\u0644\u0645\u0646\u0647\u0648\u0649\u064a\u064b"
    + "\u064c\u064d\u064e\u064f\u0650\u0651\u0652\u0660\u0661\u0662"
    + "\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u066a\u066d\u0679"
    + "\u067e\u0686\u0688\u0691\u0698\u06a4\u06af\u06ba\u06d2\u06d5"
    + "\u1e80\u1e81\u1e82\u1e83\u1e84\u1e85\u1ef2\u1ef3\u200c\u200d"
    + "\u200e\u200f\u2012\u2013\u2014\u2015\u2017\u2018\u2019\u201a"
    + "\u201b\u201c\u201d\u201e\u2020\u2021\u2022\u2024\u2025\u2026"
    + "\u202c\u202d\u202e\u2030\u2032\u2033\u2039\u203a\u203c\u2044"
    + "\u20a1\u20a3\u20a4\u20a7\u20aa\u20ab\u20ac\u2105\u2111\u2113"
    + "\u2116\u2118\u211c\u211e\u2122\u2126\u212e\u2135\u2153\u2154"
    + "\u215b\u215c\u215d\u215e\u2190\u2191\u2192\u2193\u2194\u2195"
    + "\u21a8\u21b5\u21d0\u21d1\u21d2\u21d3\u21d4\u2200\u2202\u2203"
    + "\u2205\u2206\u2207\u2208\u2209\u220b\u220f\u2211\u2212\u2217"
    + "\u221a\u221d\u221e\u221f\u2220\u2227\u2228\u2229\u222a\u222b"
    + "\u2234\u223c\u2245\u2248\u2260\u2261\u2264\u2265\u2282\u2283"
    + "\u2284\u2286\u2287\u2295\u2297\u22a5\u22c5\u2302\u2310\u2320"
    + "\u2321\u2329\u232a\u2500\u2502\u250c\u2510\u2514\u2518\u251c"
    + "\u2524\u252c\u2534\u253c\u2550\u2551\u2552\u2553\u2554\u2555"
    + "\u2556\u2557\u2558\u2559\u255a\u255b\u255c\u255d\u255e\u255f"
    + "\u2560\u2561\u2562\u2563\u2564\u2565\u2566\u2567\u2568\u2569"
    + "\u256a\u256b\u256c\u2580\u2584\u2588\u258c\u2590\u2591\u2592"
    + "\u2593\u25a0\u25a1\u25aa\u25ab\u25ac\u25b2\u25ba\u25bc\u25c4"
    + "\u25ca\u25cb\u25cf\u25d8\u25d9\u25e6\u263a\u263b\u263c\u2640"
    + "\u2642\u2660\u2663\u2665\u2666\u266a\u266b";


  /**
   * The offset of each glyph name in AGLFN_NAMES.
   *
   * Generated from the Adobe Glyph List for New Fonts, version 1.1
   * of 17 April 2003.
   *
   * @see <a href=
   * "http://partners.adobe.com/asn/tech/type/aglfn13.txt" >Adobe
   * Glyph List for New Fonts</a>
   */
  private static final String AGLFN_NAME_OFFSET
    = "\u0001\u0007\u000e\u0017\")1;GQ\\ejpw~\u0084\u0089\u008d"
    + "\u0091\u0097\u009c\u00a1\u00a5\u00ab\u00b1\u00b6\u00bc\u00c6"
    + "\u00cb\u00d1\u00d9\u00e2\u00e5\u00e7\u00e9\u00eb\u00ed\u00ef"
    + "\u00f1\u00f3\u00f5\u00f7\u00f9\u00fb\u00fd\u00ff\u0101\u0103"
    + "\u0105\u0107\u0109\u010b\u010d\u010f\u0111\u0113\u0115\u0117"
    + "\u0119\u0125\u012f\u013c\u0148\u0153\u0159\u015b\u015d\u015f"
    + "\u0161\u0163\u0165\u0167\u0169\u016b\u016d\u016f\u0171\u0173"
    + "\u0175\u0177\u0179\u017b\u017d\u017f\u0181\u0183\u0185\u0187"
    + "\u0189\u018b\u018d\u0197\u019b\u01a6\u01b1\u01bc\u01c1\u01ca"
    + "\u01d3\u01d7\u01e1\u01e9\u01f2\u01fc\u0208\u0216\u0221\u022c"
    + "\u0233\u023a\u0244\u024a\u024d\u0257\u0266\u026e\u027b\u028a"
    + "\u0295\u029d\u02ab\u02b8\u02bf\u02c6\u02d2\u02d9\u02e3\u02e9"
    + "\u02ec\u02f5\u02fc\u0303\u030f\u0319\u0320\u0327\u0333\u033d"
    + "\u0341\u0348\u034f\u0356\u0362\u0369\u0373\u037c\u0383\u038a"
    + "\u0391\u039d\u03a7\u03ae\u03b4\u03bf\u03c6\u03cd\u03d9\u03e0"
    + "\u03ea\u03f0\u03f3\u03fc\u0403\u040a\u0416\u0420\u0427\u042e"
    + "\u043a\u0444\u0448\u044f\u0456\u045d\u0469\u0470\u047a\u0481"
    + "\u0488\u048f\u0496\u04a2\u04ac\u04b3\u04b9\u04c3\u04cb\u04d3"
    + "\u04da\u04e1\u04e9\u04f1\u04f8\u04ff\u050b\u0517\u0522\u052d"
    + "\u0534\u053b\u0542\u0549\u0550\u0557\u055f\u0567\u056e\u0575"
    + "\u0580\u058b\u0593\u059b\u05a2\u05a9\u05b5\u05c1\u05c8\u05cf"
    + "\u05da\u05e5\u05f2\u05ff\u060b\u0617\u061c\u0621\u0628\u062f"
    + "\u0637\u063f\u0646\u064d\u0655\u065d\u0668\u0671\u0674\u0677"
    + "\u0683\u068f\u069c\u06a9\u06b6\u06bd\u06c4\u06d1\u06de\u06e5"
    + "\u06ec\u06f1\u06f6\u06fd\u0704\u070b\u0712\u071f\u072c\u0733"
    + "\u073a\u0746\u074a\u074e\u0756\u075e\u0765\u076c\u077a\u0788"
    + "\u078b\u078e\u0795\u079c\u07a9\u07b6\u07bd\u07c4\u07cb\u07d2"
    + "\u07de\u07ea\u07f3\u07fc\u0803\u080a\u0817\u0824\u082b\u0832"
    + "\u0837\u083c\u0843\u084a\u0852\u085a\u0861\u0868\u086e\u0874"
    + "\u0882\u0890\u0898\u08a0\u08ac\u08b8\u08c4\u08d0\u08da\u08e1"
    + "\u08e8\u08f3\u08fe\u0905\u090c\u0912\u0919\u091f\u0925\u092b"
    + "\u0931\u0938\u093f\u094a\u0955\u095d\u0965\u0971\u097d\u098a"
    + "\u0997\u09a1\u09ab\u09b6\u09bc\u09c2\u09cc\u09d1\u09d8\u09de"
    + "\u09eb\u09f5\u09ff\u0a09\u0a17\u0a24\u0a2a\u0a38\u0a43\u0a4d"
    + "\u0a5a\u0a63\u0a6d\u0a7a\u0a87\u0a92\u0aa4\u0aaa\u0aaf\u0ab5"
    + "\u0abd\u0ac2\u0ac6\u0acc\u0ad1\u0ad7\u0ade\u0ae1\u0ae4\u0ae7"
    + "\u0aef\u0af2\u0af6\u0afc\u0b00\u0b08\u0b0c\u0b10\u0b14\u0b21"
    + "\u0b31\u0b3c\u0b49\u0b52\u0b5c\u0b71\u0b77\u0b7c\u0b82\u0b88"
    + "\u0b90\u0b95\u0b99\u0b9f\u0ba4\u0baa\u0bb1\u0bb4\u0bb7\u0bbf"
    + "\u0bc2\u0bc6\u0bcd\u0bd3\u0bd7\u0bdf\u0be3\u0be7\u0beb\u0bf1"
    + "\u0bfe\u0c0e\u0c1b\u0c28\u0c33\u0c3a\u0c43\u0c48\u0c4f\u0c59"
    + "\u0c63\u0c6d\u0c77\u0c81\u0c8b\u0c95\u0c9f\u0ca9\u0cb3\u0cbd"
    + "\u0cc7\u0cd1\u0cdb\u0ce5\u0cef\u0cf9\u0d03\u0d0d\u0d17\u0d21"
    + "\u0d2b\u0d35\u0d3f\u0d49\u0d53\u0d5d\u0d67\u0d71\u0d7b\u0d85"
    + "\u0d8f\u0d99\u0da3\u0dad\u0db7\u0dc1\u0dcb\u0dd5\u0ddf\u0de9"
    + "\u0df3\u0dfd\u0e07\u0e11\u0e1b\u0e25\u0e2f\u0e39\u0e43\u0e4d"
    + "\u0e57\u0e61\u0e6b\u0e75\u0e7f\u0e89\u0e93\u0e9d\u0ea7\u0eb1"
    + "\u0ebb\u0ec5\u0ecf\u0ed9\u0ee3\u0eed\u0ef7\u0f01\u0f0b\u0f15"
    + "\u0f1f\u0f29\u0f33\u0f3d\u0f47\u0f51\u0f5b\u0f65\u0f6f\u0f79"
    + "\u0f83\u0f8d\u0f97\u0fa1\u0fab\u0fb5\u0fbf\u0fc9\u0fd3\u0fdd"
    + "\u0fe7\u0ff1\u0ffb\u1005\u100f\u1019\u1023\u102d\u1037\u1041"
    + "\u104b\u1055\u105f\u1069\u1073\u107d\u1087\u1091\u109b\u10a5"
    + "\u10af\u10b9\u10c3\u10cd\u10d7\u10e1\u10eb\u10f5\u10ff\u1109"
    + "\u1113\u111d\u1127\u1131\u113b\u1145\u114f\u1159\u1163\u116d"
    + "\u1177\u1181\u118b\u1195\u119f\u11a9\u11b3\u11bd\u11c7\u11d1"
    + "\u11db\u11e5\u11ef\u11f9\u1203\u120d\u1217\u1221\u122b\u1235"
    + "\u123f\u1249\u1253\u125d\u1267\u1271\u127b\u1285\u128f\u1299"
    + "\u12a3\u12ad\u12b7\u12c1\u12cb\u12d5\u12df\u12e9\u12f3\u12fd"
    + "\u1307\u1311\u131b\u1325\u132f\u1339\u1343\u134d\u1357\u1361"
    + "\u136b\u1375\u137f\u1389\u1393\u139d\u13a7\u13b1\u13bb\u13c5"
    + "\u13cf\u13d9\u13e3\u13ed\u13f7\u1401\u140b\u1415\u141f\u1429"
    + "\u1433\u143d\u1447\u1451\u145b\u1465\u146f\u1479\u1483\u148d"
    + "\u1497\u14a1\u14ab\u14b5\u14bf\u14c9\u14d3\u14dd\u14e7\u14f1"
    + "\u14f8\u14ff\u1506\u150d\u1517\u1521\u1528\u152f\u1539\u1541"
    + "\u1549\u1551\u155c\u1563\u156a\u1574\u1582\u158c\u1597\u15a6"
    + "\u15b4\u15c1\u15cf\u15dc\u15e3\u15ed\u15f4\u1603\u1612\u161b"
    + "\u1625\u162f\u1639\u1645\u164c\u1653\u1661\u1670\u167a\u1683"
    + "\u1691\u1697\u169c\u16a3\u16ad\u16b2\u16b7\u16c1\u16ca\u16d4"
    + "\u16de\u16ea\u16f3\u1700\u170a\u1710\u171a\u1720\u1729\u1733"
    + "\u173d\u174a\u1756\u1763\u176d\u1775\u1780\u178a\u1794\u179e"
    + "\u17ab\u17ba\u17c7\u17d2\u17e0\u17ed\u17fa\u1804\u1810\u181c"
    + "\u1825\u182b\u1834\u183c\u1847\u1850\u1858\u1862\u1868\u1875"
    + "\u187d\u188a\u1893\u189e\u18a4\u18af\u18b9\u18c6\u18cc\u18d5"
    + "\u18df\u18e7\u18f1\u18fd\u1906\u1912\u191c\u1929\u1936\u1945"
    + "\u194f\u195c\u196b\u1976\u1985\u1993\u199b\u19a1\u19af\u19ba"
    + "\u19c5\u19cf\u19da\u19e3\u19ec\u19f5\u19fe\u1a07\u1a10\u1a19"
    + "\u1a22\u1a2b\u1a34\u1a3d\u1a46\u1a4f\u1a58\u1a61\u1a6a\u1a73"
    + "\u1a7c\u1a85\u1a8e\u1a97\u1aa0\u1aa9\u1ab2\u1abb\u1ac4\u1acd"
    + "\u1ad6\u1adf\u1ae8\u1af1\u1afa\u1b03\u1b0c\u1b15\u1b1e\u1b27"
    + "\u1b30\u1b39\u1b42\u1b4a\u1b52\u1b58\u1b60\u1b68\u1b70\u1b76"
    + "\u1b7e\u1b88\u1b8f\u1b96\u1b9d\u1ba8\u1bb0\u1bb8\u1bc0\u1bc8"
    + "\u1bd0\u1bd7\u1bde\u1be8\u1bf2\u1bfd\u1c07\u1c14\u1c18\u1c1f"
    + "\u1c24\u1c2a\u1c2f\u1c35\u1c3d\u1c49";


  /**
   * The name of each glyph in the Adobe Glyph List for New Fonts
   * (AGLFN). The name of the n-th glyph starts at position
   * AGLFN_NAME_OFFSET.charAt(n). It ends before the following
   * slash (slashes cannot be part of a PostScript name, which
   * is why we use it for separation).
   *
   * <p>Generated from the Adobe Glyph List for New Fonts, version 1.1
   * of 17 April 2003.
   *
   * @see <a href=
   * "http://partners.adobe.com/asn/tech/type/aglfn13.txt" >Adobe
   * Glyph List for New Fonts</a>
   */
  private static final String AGLFN_NAMES
    = "/space/exclam/quotedbl/numbersign/dollar/percent/ampersand/q"
    + "uotesingle/parenleft/parenright/asterisk/plus/comma/hyphen/p"
    + "eriod/slash/zero/one/two/three/four/five/six/seven/eight/nin"
    + "e/colon/semicolon/less/equal/greater/question/at/A/B/C/D/E/F"
    + "/G/H/I/J/K/L/M/N/O/P/Q/R/S/T/U/V/W/X/Y/Z/bracketleft/backsla"
    + "sh/bracketright/asciicircum/underscore/grave/a/b/c/d/e/f/g/h"
    + "/i/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z/braceleft/bar/bracerigh"
    + "t/asciitilde/exclamdown/cent/sterling/currency/yen/brokenbar"
    + "/section/dieresis/copyright/ordfeminine/guillemotleft/logica"
    + "lnot/registered/macron/degree/plusminus/acute/mu/paragraph/p"
    + "eriodcentered/cedilla/ordmasculine/guillemotright/onequarter"
    + "/onehalf/threequarters/questiondown/Agrave/Aacute/Acircumfle"
    + "x/Atilde/Adieresis/Aring/AE/Ccedilla/Egrave/Eacute/Ecircumfl"
    + "ex/Edieresis/Igrave/Iacute/Icircumflex/Idieresis/Eth/Ntilde/"
    + "Ograve/Oacute/Ocircumflex/Otilde/Odieresis/multiply/Oslash/U"
    + "grave/Uacute/Ucircumflex/Udieresis/Yacute/Thorn/germandbls/a"
    + "grave/aacute/acircumflex/atilde/adieresis/aring/ae/ccedilla/"
    + "egrave/eacute/ecircumflex/edieresis/igrave/iacute/icircumfle"
    + "x/idieresis/eth/ntilde/ograve/oacute/ocircumflex/otilde/odie"
    + "resis/divide/oslash/ugrave/uacute/ucircumflex/udieresis/yacu"
    + "te/thorn/ydieresis/Amacron/amacron/Abreve/abreve/Aogonek/aog"
    + "onek/Cacute/cacute/Ccircumflex/ccircumflex/Cdotaccent/cdotac"
    + "cent/Ccaron/ccaron/Dcaron/dcaron/Dcroat/dcroat/Emacron/emacr"
    + "on/Ebreve/ebreve/Edotaccent/edotaccent/Eogonek/eogonek/Ecaro"
    + "n/ecaron/Gcircumflex/gcircumflex/Gbreve/gbreve/Gdotaccent/gd"
    + "otaccent/Gcommaaccent/gcommaaccent/Hcircumflex/hcircumflex/H"
    + "bar/hbar/Itilde/itilde/Imacron/imacron/Ibreve/ibreve/Iogonek"
    + "/iogonek/Idotaccent/dotlessi/IJ/ij/Jcircumflex/jcircumflex/K"
    + "commaaccent/kcommaaccent/kgreenlandic/Lacute/lacute/Lcommaac"
    + "cent/lcommaaccent/Lcaron/lcaron/Ldot/ldot/Lslash/lslash/Nacu"
    + "te/nacute/Ncommaaccent/ncommaaccent/Ncaron/ncaron/napostroph"
    + "e/Eng/eng/Omacron/omacron/Obreve/obreve/Ohungarumlaut/ohunga"
    + "rumlaut/OE/oe/Racute/racute/Rcommaaccent/rcommaaccent/Rcaron"
    + "/rcaron/Sacute/sacute/Scircumflex/scircumflex/Scedilla/scedi"
    + "lla/Scaron/scaron/Tcommaaccent/tcommaaccent/Tcaron/tcaron/Tb"
    + "ar/tbar/Utilde/utilde/Umacron/umacron/Ubreve/ubreve/Uring/ur"
    + "ing/Uhungarumlaut/uhungarumlaut/Uogonek/uogonek/Wcircumflex/"
    + "wcircumflex/Ycircumflex/ycircumflex/Ydieresis/Zacute/zacute/"
    + "Zdotaccent/zdotaccent/Zcaron/zcaron/longs/florin/Ohorn/ohorn"
    + "/Uhorn/uhorn/Gcaron/gcaron/Aringacute/aringacute/AEacute/aea"
    + "cute/Oslashacute/oslashacute/Scommaaccent/scommaaccent/afii5"
    + "7929/afii64937/circumflex/caron/breve/dotaccent/ring/ogonek/"
    + "tilde/hungarumlaut/gravecomb/acutecomb/tildecomb/hookaboveco"
    + "mb/dotbelowcomb/tonos/dieresistonos/Alphatonos/anoteleia/Eps"
    + "ilontonos/Etatonos/Iotatonos/Omicrontonos/Upsilontonos/Omega"
    + "tonos/iotadieresistonos/Alpha/Beta/Gamma/Epsilon/Zeta/Eta/Th"
    + "eta/Iota/Kappa/Lambda/Mu/Nu/Xi/Omicron/Pi/Rho/Sigma/Tau/Upsi"
    + "lon/Phi/Chi/Psi/Iotadieresis/Upsilondieresis/alphatonos/epsi"
    + "lontonos/etatonos/iotatonos/upsilondieresistonos/alpha/beta/"
    + "gamma/delta/epsilon/zeta/eta/theta/iota/kappa/lambda/nu/xi/o"
    + "micron/pi/rho/sigma1/sigma/tau/upsilon/phi/chi/psi/omega/iot"
    + "adieresis/upsilondieresis/omicrontonos/upsilontonos/omegaton"
    + "os/theta1/Upsilon1/phi1/omega1/afii10023/afii10051/afii10052"
    + "/afii10053/afii10054/afii10055/afii10056/afii10057/afii10058"
    + "/afii10059/afii10060/afii10061/afii10062/afii10145/afii10017"
    + "/afii10018/afii10019/afii10020/afii10021/afii10022/afii10024"
    + "/afii10025/afii10026/afii10027/afii10028/afii10029/afii10030"
    + "/afii10031/afii10032/afii10033/afii10034/afii10035/afii10036"
    + "/afii10037/afii10038/afii10039/afii10040/afii10041/afii10042"
    + "/afii10043/afii10044/afii10045/afii10046/afii10047/afii10048"
    + "/afii10049/afii10065/afii10066/afii10067/afii10068/afii10069"
    + "/afii10070/afii10072/afii10073/afii10074/afii10075/afii10076"
    + "/afii10077/afii10078/afii10079/afii10080/afii10081/afii10082"
    + "/afii10083/afii10084/afii10085/afii10086/afii10087/afii10088"
    + "/afii10089/afii10090/afii10091/afii10092/afii10093/afii10094"
    + "/afii10095/afii10096/afii10097/afii10071/afii10099/afii10100"
    + "/afii10101/afii10102/afii10103/afii10104/afii10105/afii10106"
    + "/afii10107/afii10108/afii10109/afii10110/afii10193/afii10146"
    + "/afii10194/afii10147/afii10195/afii10148/afii10196/afii10050"
    + "/afii10098/afii10846/afii57799/afii57801/afii57800/afii57802"
    + "/afii57793/afii57794/afii57795/afii57798/afii57797/afii57806"
    + "/afii57796/afii57807/afii57839/afii57645/afii57841/afii57842"
    + "/afii57804/afii57803/afii57658/afii57664/afii57665/afii57666"
    + "/afii57667/afii57668/afii57669/afii57670/afii57671/afii57672"
    + "/afii57673/afii57674/afii57675/afii57676/afii57677/afii57678"
    + "/afii57679/afii57680/afii57681/afii57682/afii57683/afii57684"
    + "/afii57685/afii57686/afii57687/afii57688/afii57689/afii57690"
    + "/afii57716/afii57717/afii57718/afii57388/afii57403/afii57407"
    + "/afii57409/afii57410/afii57411/afii57412/afii57413/afii57414"
    + "/afii57415/afii57416/afii57417/afii57418/afii57419/afii57420"
    + "/afii57421/afii57422/afii57423/afii57424/afii57425/afii57426"
    + "/afii57427/afii57428/afii57429/afii57430/afii57431/afii57432"
    + "/afii57433/afii57434/afii57440/afii57441/afii57442/afii57443"
    + "/afii57444/afii57445/afii57446/afii57470/afii57448/afii57449"
    + "/afii57450/afii57451/afii57452/afii57453/afii57454/afii57455"
    + "/afii57456/afii57457/afii57458/afii57392/afii57393/afii57394"
    + "/afii57395/afii57396/afii57397/afii57398/afii57399/afii57400"
    + "/afii57401/afii57381/afii63167/afii57511/afii57506/afii57507"
    + "/afii57512/afii57513/afii57508/afii57505/afii57509/afii57514"
    + "/afii57519/afii57534/Wgrave/wgrave/Wacute/wacute/Wdieresis/w"
    + "dieresis/Ygrave/ygrave/afii61664/afii301/afii299/afii300/fig"
    + "uredash/endash/emdash/afii00208/underscoredbl/quoteleft/quot"
    + "eright/quotesinglbase/quotereversed/quotedblleft/quotedblrig"
    + "ht/quotedblbase/dagger/daggerdbl/bullet/onedotenleader/twodo"
    + "tenleader/ellipsis/afii61573/afii61574/afii61575/perthousand"
    + "/minute/second/guilsinglleft/guilsinglright/exclamdbl/fracti"
    + "on/colonmonetary/franc/lira/peseta/afii57636/dong/Euro/afii6"
    + "1248/Ifraktur/afii61289/afii61352/weierstrass/Rfraktur/presc"
    + "ription/trademark/Omega/estimated/aleph/onethird/twothirds/o"
    + "neeighth/threeeighths/fiveeighths/seveneighths/arrowleft/arr"
    + "owup/arrowright/arrowdown/arrowboth/arrowupdn/arrowupdnbse/c"
    + "arriagereturn/arrowdblleft/arrowdblup/arrowdblright/arrowdbl"
    + "down/arrowdblboth/universal/partialdiff/existential/emptyset"
    + "/Delta/gradient/element/notelement/suchthat/product/summatio"
    + "n/minus/asteriskmath/radical/proportional/infinity/orthogona"
    + "l/angle/logicaland/logicalor/intersection/union/integral/the"
    + "refore/similar/congruent/approxequal/notequal/equivalence/le"
    + "ssequal/greaterequal/propersubset/propersuperset/notsubset/r"
    + "eflexsubset/reflexsuperset/circleplus/circlemultiply/perpend"
    + "icular/dotmath/house/revlogicalnot/integraltp/integralbt/ang"
    + "leleft/angleright/SF100000/SF110000/SF010000/SF030000/SF0200"
    + "00/SF040000/SF080000/SF090000/SF060000/SF070000/SF050000/SF4"
    + "30000/SF240000/SF510000/SF520000/SF390000/SF220000/SF210000/"
    + "SF250000/SF500000/SF490000/SF380000/SF280000/SF270000/SF2600"
    + "00/SF360000/SF370000/SF420000/SF190000/SF200000/SF230000/SF4"
    + "70000/SF480000/SF410000/SF450000/SF460000/SF400000/SF540000/"
    + "SF530000/SF440000/upblock/dnblock/block/lfblock/rtblock/ltsh"
    + "ade/shade/dkshade/filledbox/H22073/H18543/H18551/filledrect/"
    + "triagup/triagrt/triagdn/triaglf/lozenge/circle/H18533/invbul"
    + "let/invcircle/openbullet/smileface/invsmileface/sun/female/m"
    + "ale/spade/club/heart/diamond/musicalnote/musicalnotedbl/";


  /**
   * Determines the name of a glyph according to the Adobe Glyph List
   * for New Fonts (AGLFN). Because all glyphs in AGLFN correspond to
   * a precomposed Unicode codepoint, the mismatch between characters
   * and glyphs is not an issue here.
   *
   * @param c the Unicode codepoint that corresponds to the glyph, for
   * example <code>0x010a</code> for <code>LATIN CAPITAL LETTER C WITH
   * DOT ABOVE</code>.
   *
   * @return the glyph name, for example <code>Cdotaccent</code>. If
   * the glyph is not in the <i>Adobe Glyph List for New Fonts</i>,
   * <code>null</code> is returned.
   *
   * @see <a href=
   * "http://partners.adobe.com/asn/tech/type/aglfn13.txt" >Adobe
   * Glyph List for New Fonts (AGLFN), version 1.1 of April 17,
   * 2003</a>
   *
   * @see <a href=
   * "http://partners.adobe.com/asn/developer/type/unicodegn.html#6"
   * >Adobe&#x2019;s guidelines related to Unicode</a>
   */
  private static String getAGLFNName(char c)
  {
    int min, max, mid;
    char midChar;

    /* Performs a binary search in the sorted array (actually, a
     * String) of glyphs in the Adobe Glyph List for New Fonts.
     *
     * A good compiler might be able to optimize a call to charAt for
     * a static final String, but this routine is probably not that
     * critical to performance.
     */
    min = 0;
    max = AGLFN_GLYPHS.length() - 1;
    mid = max >> 1;    
    midChar = AGLFN_GLYPHS.charAt(mid);
    do
    {
      if (midChar == c)
        break;
      else if (midChar < c)
        min = mid + 1;
      else
        max = mid;
      mid = (min + max) >> 1;
      midChar = AGLFN_GLYPHS.charAt(mid);
    }
    while (min < max);
    
    if (midChar != c)
      return null;

    int pos = AGLFN_NAME_OFFSET.charAt(mid);
    return AGLFN_NAMES.substring(pos, AGLFN_NAMES.indexOf('/', pos));
  }


  /**
   * Returns the PostScript name of a glyph, given the sequence of
   * Unicode characters that is required to produce the glyph.  The
   * returned name follows Adobe&#x2019;s glyph naming recommendations
   * in order to allow searching and indexing of the produced
   * PostScript and PDF.
   *
   * <p>Some examples:
   * <ul><li><code>U+0041</code> gives <code>A</code>;</li>
   * <li><code>U+011D</code> gives <code>gcircumflex</code>;</li>
   * <li><code>U+007A U+0302</code> gives <code>z_uni0302</code>;</li>
   * <li><code>U+D807 U+DC42</code> (an UTF-16 escape sequence)
   * gives <code>u11C42</code>;</li>
   *  </ul>.
   *
   * <p>The routine does <i>not</i> bring sequences in any canonical
   * form. Therefore, the result for <code>U+0067 U+0302</code> (the
   * decomposition of <code>U+011D</code>) will be
   * <code>g_uni0302</code>, not <code>gcircumflex</code>.
   *
   * @see <a href=
   * "http://partners.adobe.com/asn/tech/type/unicodegn.jsp" >Unicode
   * and Glyph Names</a> and <a href=
   * "http://partners.adobe.com/asn/tech/type/glyphnamelimits.jsp"
   * >Glyph Names and Current Implementations</a>
   */
  private static String getGlyphName(char[] chars)
  {
    char c;
    String name;
    int numChars;
    boolean hasSurrogates = false;

    if ((chars == null) || ((numChars = chars.length) == 0))
      return ".notdef";

    /* The vast majority of cases will be just a single character.
     * Therefore, we have a special code path for this case.
     */
    if (numChars == 1)
    {
      c = chars[0];
      name = getAGLFNName(c);
      if (name != null)
        return name;
    }
    
    CPStringBuilder buf = new CPStringBuilder(numChars * 8);
    for (int i = 0; i < numChars; i++)
    {
      if (i > 0)
        buf.append('_');
      c = chars[i];

      /* handle surrogate pairs */
      if (c >> 10 == 0x36) // U+D800 .. U+DBFF: High surrogate
      {
        /* Adobe recommends using the 'u' prefix only for
         * characters outside the Unicode Basic Multilingual Plane,
         * because Acrobat 4 and 5 understand only the "uni" prefix.
         * The 'u' prefix will be supported by Acrobat 6 and later.
         *
         * For further information, please refer to this page:
         * http://partners.adobe.com/asn/tech/type/glyphnamelimits.jsp#3
         */
        int ucs4 = (((c & 0x3ff) << 10) | (chars[++i] & 0x3ff)) + 0x10000;
        buf.append('u');
        buf.append(Integer.toHexString(ucs4).toUpperCase());
      }
      else
      {
        /* Try the Adobe Glyph List. */
        name = getAGLFNName(c);
        if (name != null)
          buf.append(name);
        else
        {
          char nibble;
          buf.append("uni");
          nibble = (char) (((c >> 12) & 0xf) + 0x30);
          if (nibble > 0x39)
            nibble += 7;
          buf.append(nibble);
          nibble = (char) (((c >> 8) & 0xf) + 0x30);
          if (nibble > 0x39)
            nibble += 7;
          buf.append(nibble);
          nibble = (char) (((c >> 4) & 0xf) + 0x30);
          if (nibble > 0x39)
            nibble += 7;
          buf.append(nibble);
          nibble = (char) (((c >> 0) & 0xf) + 0x30);
          if (nibble > 0x39)
            nibble += 7;
          buf.append(nibble);
        }
      }
    }
    return buf.toString();
  }
}
