/* IconvMetaData.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.java.nio.charset.iconv;

import java.util.HashMap;
import java.util.Vector;

/**
 * This is ugly glue. iconv doesn't have character metadata,
 * so we include it here.
 *
 * TODO: Add more charsets which GNU iconv and the JDK support which aren't
 * included here.
 *
 * @author Sven de Marothy
 */
final class IconvMetaData
{
  /**
   * Map of names (and aliases) to metadata instances
   */
  private static HashMap names;

  /**
   * Vector of MetaData instances
   */
  private static Vector charsets;

  /**
   * Name to use with iconv (may differ from the nio canonical.
   */
  private String iconvName;

  /**
   * Average number of bytes per char.
   */
  private float averageBperC;

  /**
   * Maximum number of bytes per char.
   */
  private float maxBperC;

  /**
   * Average number of chars per byte.
   */
  private float averageCperB;

  /**
   * Maximum number of chars per byte.
   */
  private float maxCperB;

  /**
   * NIO canonical name.
   */
  private String nioCanonical;

  /**
   * Charset aliases.
   */
  private String[] aliases;

  IconvMetaData(String nioCanonical, float averageBperC, float maxBperC,
                float averageCperB, float maxCperB, String[] aliases,
                String iconvName)
  {
    this.nioCanonical = nioCanonical;
    this.iconvName = iconvName;

    this.averageBperC = averageBperC;
    this.maxBperC = maxBperC;
    this.averageCperB = averageCperB;
    this.maxCperB = maxCperB;
    this.aliases = aliases;

    names.put(nioCanonical, this);
    names.put(iconvName, this);
    for (int i = 0; i < aliases.length; i++)
      names.put(aliases[i], this);
    charsets.add(this);
  }

  static Vector charsets()
  {
    return charsets;
  }

  String[] aliases()
  {
    return aliases;
  }

  String nioCanonical()
  {
    return nioCanonical;
  }

  String iconvName()
  {
    return iconvName;
  }

  float maxBytesPerChar()
  {
    return maxBperC;
  }

  float maxCharsPerByte()
  {
    return maxCperB;
  }

  float averageBytesPerChar()
  {
    return averageBperC;
  }

  float averageCharsPerByte()
  {
    return averageCperB;
  }

  static IconvMetaData get(String s)
  {
    return (IconvMetaData) names.get(s);
  }

  static void setup()
  {
    names = new HashMap();
    charsets = new Vector();
    new IconvMetaData("Big5", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[] { "big-5", "csBig5" }, "Big5");

    new IconvMetaData("Big5-HKSCS", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[] { "big5-hkscs", "Big5_HKSCS", "big5hkscs" },
                      "Big5-HKSCS");

    new IconvMetaData("EUC-CN", 2.0f, 2.0f, 0.5f, 1.0f, new String[] {  },
                      "EUC-CN");

    new IconvMetaData("EUC-JP", 3.0f, 3.0f, 0.5f, 1.0f,
                      new String[]
                      {
                        "eucjis", "x-eucjp", "csEUCPkdFmtjapanese", "eucjp",
                        "Extended_UNIX_Code_Packed_Format_for_Japanese",
                        "x-euc-jp", "euc_jp"
                      }, "EUC-JP");

    new IconvMetaData("EUC-KR", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[]
                      {
                        "ksc5601", "5601", "ksc5601_1987", "ksc_5601",
                        "ksc5601-1987", "euc_kr", "ks_c_5601-1987", "euckr",
                        "csEUCKR"
                      }, "EUC-KR");

    new IconvMetaData("EUC-TW", 4.0f, 4.0f, 2.0f, 2.0f,
                      new String[] { "cns11643", "euc_tw", "euctw", }, "EUC-TW");

    new IconvMetaData("GB18030", 4.0f, 4.0f, 1.0f, 2.0f,
                      new String[] { "gb18030-2000", }, "GB18030");

    new IconvMetaData("GBK", 2.0f, 2.0f, 0.5f, 1.0f, new String[] { "GBK" },
                      "GBK");

    new IconvMetaData("ISO-2022-CN-CNS", 4.0f, 4.0f, 2.0f, 2.0f,
                      new String[] { "ISO2022CN_CNS" }, "ISO-2022-CN"); // will this work?

    new IconvMetaData("ISO-2022-CN-GB", 4.0f, 4.0f, 2.0f, 2.0f,
                      new String[] { "ISO2022CN_GB" }, "ISO-2022-CN"); // same here?

    new IconvMetaData("ISO-2022-KR", 4.0f, 4.0f, 1.0f, 1.0f,
                      new String[] { "ISO2022KR", "csISO2022KR" },
                      "ISO-2022-KR");

    new IconvMetaData("ISO-8859-1", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "iso-ir-100", "ISO_8859-1", "latin1", "l1", "IBM819",
                        "CP819", "csISOLatin1", "8859_1", "ISO8859_1",
                        "ISO_8859_1", "ibm-819", "ISO_8859-1:1987", "819"
                      }, "ISO-8859-1");

    new IconvMetaData("ISO-8859-13", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_13", "8859_13", "ibm-921_P100-1995", "ibm-921",
                        "iso_8859_13", "iso8859_13", "iso-8859-13", "8859_13",
                        "cp921", "921"
                      }, "ISO-8859-13");

    new IconvMetaData("ISO-8859-15", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "8859_15", "csISOlatin9", "IBM923", "cp923", "923",
                        "LATIN0", "csISOlatin0", "ISO8859_15_FDIS", "L9",
                        "IBM-923", "ISO8859-15", "LATIN9", "ISO_8859-15",
                        "ISO-8859-15",
                      }, "ISO-8859-15");

    new IconvMetaData("ISO-8859-2", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_2", "8859_2", "ibm-912_P100-1995", "ibm-912",
                        "iso_8859_2", "iso8859_2", "iso-8859-2",
                        "ISO_8859-2:1987", "latin2", "csISOLatin2",
                        "iso-ir-101", "l2", "cp912", "912", "windows-28592"
                      }, "ISO-8859-2");

    new IconvMetaData("ISO-8859-3", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_3", "8859_3", "ibm-913_P100-2000", "ibm-913",
                        "iso_8859_3", "iso8859_3", "iso-8859-3",
                        "ISO_8859-3:1988", "latin3", "csISOLatin3",
                        "iso-ir-109", "l3", "cp913", "913", "windows-28593"
                      }, "ISO-8859-3");

    new IconvMetaData("ISO-8859-4", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_4", "8859_4", "ibm-914_P100-1995", "ibm-914",
                        "iso_8859_4", "iso8859_4", "iso-8859-4", "latin4",
                        "csISOLatin4", "iso-ir-110", "ISO_8859-4:1988", "l4",
                        "cp914", "914", "windows-28594"
                      }, "ISO-8859-4");

    new IconvMetaData("ISO-8859-5", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_5", "8859_5", "ibm-915_P100-1995", "ibm-915",
                        "iso_8859_5", "iso8859_5", "iso-8859-5", "cyrillic",
                        "csISOLatinCyrillic", "iso-ir-144", "ISO_8859-5:1988",
                        "cp915", "915", "windows-28595"
                      }, "ISO-8859-5");

    new IconvMetaData("ISO-8859-6", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "8859_6", "ibm-1089_P100-1995", "ibm-1089",
                        "iso_8859_6", "iso8859_6", "iso-8859-6", "arabic",
                        "csISOLatinArabic", "iso-ir-127", "ISO_8859-6:1987",
                        "ECMA-114", "ASMO-708", "8859_6", "cp1089", "1089",
                        "windows-28596", "ISO-8859-6-I", "ISO-8859-6-E"
                      }, "ISO-8859-6");

    new IconvMetaData("ISO-8859-7", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_7", "8859_7", "ibm-813_P100-1995", "ibm-813",
                        "iso_8859_7", "iso8859_7", "iso-8859-7", "greek",
                        "greek8", "ELOT_928", "ECMA-118", "csISOLatinGreek",
                        "iso-ir-126", "ISO_8859-7:1987", "cp813", "813",
                        "windows-28597"
                      }, "ISO-8859-7");

    new IconvMetaData("ISO-8859-8", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_8", "8859_8", "ibm-916_P100-1995", "ibm-916",
                        "iso_8859_8", "iso8859_8", "iso-8859-8", "hebrew",
                        "csISOLatinHebrew", "iso-ir-138", "ISO_8859-8:1988",
                        "ISO-8859-8-I", "ISO-8859-8-E", "cp916", "916",
                        "windows-28598"
                      }, "ISO-8859-8");

    new IconvMetaData("ISO-8859-9", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "ISO8859_9", "8859_9", "ibm-920_P100-1995", "ibm-920",
                        "iso8859_9", "iso-8859-9", "iso_8859_9", "latin5",
                        "csISOLatin5", "iso-ir-148", "ISO_8859-9:1989", "l5",
                        "cp920", "920", "windows-28599", "ECMA-128"
                      }, "ISO-8859-9");

    new IconvMetaData("Johab", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[] { "ms1361", "ksc5601_1992", "ksc5601-1992", },
                      "Johab");

    new IconvMetaData("KOI8-R", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "KOI8_R", "KOI8", "KOI-8", "KOI_8", "koi8-r", "koi8r",
                        "koi-8-r", "koi"
                      }, "KOI8-R");

    new IconvMetaData("Shift_JIS", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[]
                      {
                        "shift-jis", "x-sjis", "ms_kanji", "shift_jis",
                        "csShiftJIS", "sjis", "pck",
                      }, "Shift_JIS");

    new IconvMetaData("TIS-620", 1.0f, 1.0f, 1.0f, 1.0f, new String[] {  },
                      "TIS-620");

    new IconvMetaData("US-ASCII", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "IBM367", "ISO646-US", "ANSI_X3.4-1986", "cp367",
                        "ASCII", "iso_646.irv:1983", "646", "us", "iso-ir-6",
                        "csASCII", "ANSI_X3.4-1968", "ISO_646.irv:1991",
                      }, "US-ASCII");

    new IconvMetaData("UTF-16", 2.0f, 4.0f, 0.5f, 1.0f,
                      new String[]
                      {
                        "UTF_16", "UTF16", "ISO-10646-UCS-2", "unicode",
                        "csUnicode", "ucs-2", "UnicodeBig"
                      }, "UTF-16");

    new IconvMetaData("UTF-16BE", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[]
                      {
                        "X-UTF-16BE", "UTF_16BE", "UTF16BE", "ISO-10646-UCS-2",
                        "x-utf-16be", "ibm-1200", "ibm-1201", "ibm-5297",
                        "ibm-13488", "ibm-17584", "windows-1201", "cp1200",
                        "cp1201", "UnicodeBigUnmarked"
                      }, "UTF-16BE");

    new IconvMetaData("UTF-16LE", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[]
                      {
                        "UTF_16LE", "UTF16LE", "X-UTF-16LE", "ibm-1202",
                        "ibm-13490", "ibm-17586", "UTF16_LittleEndian",
                        "UnicodeLittleUnmarked"
                      }, "UTF-16LE");

    new IconvMetaData("UTF-8", 1.1f, 4.0f, 1.0f, 2.0f,
                      new String[]
                      {
                        "UTF8", "ibm-1208", "ibm-1209", "ibm-5304", "ibm-5305",
                        "windows-65001", "cp1208"
                      }, "UTF-8");

    new IconvMetaData("windows-1250", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1250", "ibm-5346_P100-1998", "ibm-5346",
                        "cp1250", "cp-1250", "cp_1250", "windows1250",
                        "windows_1250"
                      }, "windows-1250");

    new IconvMetaData("windows-1251", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1251", "cp1251", "cp-1251", "cp_1251",
                        "windows1251", "windows_1251"
                      }, "windows-1251");

    new IconvMetaData("windows-1252", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1252", "ibm-5348_P100-1997", "ibm-5348",
                        "windows-1252", "cp1252", "cp-1252"
                      }, "windows-1252");

    new IconvMetaData("windows-1253", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1253", "cp1253", "cp-1253", "cp_1253",
                        "windows1253", "windows_1253"
                      }, "windows-1253");

    new IconvMetaData("windows-1254", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1254", "cp1254", "cp-1254", "cp_1254",
                        "windows1254", "windows_1254"
                      }, "windows-1254");

    new IconvMetaData("windows-1255", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1255", "cp1255", "cp-1255", "cp_1255",
                        "windows1255", "windows_1255"
                      }, "windows-1255");

    new IconvMetaData("windows-1256", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1255", "cp1256", "cp-1256", "cp_1256",
                        "windows1256", "windows_1256"
                      }, "windows-1256");

    new IconvMetaData("windows-1257", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1255", "cp1257", "cp-1257", "cp_1257",
                        "windows1257", "windows_1257"
                      }, "windows-1257");

    new IconvMetaData("windows-1258", 1.0f, 1.0f, 1.0f, 1.0f,
                      new String[]
                      {
                        "Windows1255", "cp1258", "cp-1258", "cp_1258",
                        "windows1258", "windows_1258"
                      }, "windows-1258");

    new IconvMetaData("windows-936", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[] { "cp936", "ms-936", "ms936", "ms_936" },
                      "windows-936");

    new IconvMetaData("windows-949", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[] { "cp949", "ms-949", "ms_949", "ms949" },
                      "cp949");

    new IconvMetaData("windows-950", 2.0f, 2.0f, 0.5f, 1.0f,
                      new String[] { "cp950", "ms_950", "ms-950", "ms950", },
                      "cp950");
  }
}
