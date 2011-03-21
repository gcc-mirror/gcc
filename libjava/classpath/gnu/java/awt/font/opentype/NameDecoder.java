/* NameDecoder.java -- Decodes names of OpenType and TrueType fonts.
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

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Locale;


/**
 * A utility class that helps with decoding the names of OpenType
 * and TrueType fonts.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class NameDecoder
{
  public static final int NAME_COPYRIGHT = 0;


  /**
   * Specifies the name of the family to which a font belongs, for
   * example &#x201c;Univers&#x201d;.
   */
  public static final int NAME_FAMILY = 1;


  /**
   * Specified the name of the font inside its family, for
   * example &#x201c;Light&#x201d;.
   */
  public static final int NAME_SUBFAMILY = 2;


  public static final int NAME_UNIQUE = 3;


  /**
   * Specifies the full human-readable name of a font, for example
   * &#x201c;Univers Light&#x201d;
   */
  public static final int NAME_FULL = 4;


  public static final int NAME_VERSION = 5;


  /**
   * Specifies the PostScript name of a font, for example
   * &#x201c;Univers-Light&#x201d;.
   */
  public static final int NAME_POSTSCRIPT = 6;


  public static final int NAME_TRADEMARK = 7;
  public static final int NAME_MANUFACTURER = 8;
  public static final int NAME_DESIGNER = 9;
  public static final int NAME_DESCRIPTION = 10;
  public static final int NAME_VENDOR_URL = 11;
  public static final int NAME_DESIGNER_URL = 12;
  public static final int NAME_LICENSE = 13;
  public static final int NAME_LICENSE_URL = 14;
  public static final int NAME_PREFERRED_FAMILY = 16;
  public static final int NAME_PREFERRED_SUBFAMILY = 17;
  public static final int NAME_FULL_MACCOMPATIBLE = 18;
  public static final int NAME_SAMPLE_TEXT = 19;
  public static final int NAME_POSTSCRIPT_CID = 20;


  private static final int PLATFORM_MACINTOSH = 1;
  private static final int PLATFORM_MICROSOFT = 3;


  public static String getName(ByteBuffer nameTable,
                               int name, Locale locale)
  {
    int numRecords;
    int macLanguage, msLanguage;
    int offset;
    int namePlatform, nameEncoding, nameLanguage, nameID, nameLen;
    int nameStart;
    String result;
    boolean match;

    if (nameTable == null)
      return null;

    nameTable.position(0);
    /* We understand only format 0 of the name table. */
    if (nameTable.getShort() != 0)
      return null;

    macLanguage = getMacLanguageCode(locale);
    msLanguage = getMicrosoftLanguageCode(locale);
    numRecords = nameTable.getShort();
    offset = nameTable.getShort();

    for (int i = 0; i < numRecords; i++)
    {
      namePlatform = nameTable.getShort();
      nameEncoding = nameTable.getShort();
      nameLanguage = nameTable.getShort();
      nameID = nameTable.getShort();
      nameLen = nameTable.getShort();
      nameStart = offset + nameTable.getShort();


      if (nameID != name)
        continue;

      // Handle PS seperately as it can be only ASCII, although
      // possibly encoded as UTF-16BE
      if ( name == NAME_POSTSCRIPT )
        {
          if( nameTable.get(nameStart) == 0 ) // Peek at top byte
            result = decodeName("UTF-16BE", nameTable, nameStart, nameLen);
          else
            result = decodeName("ASCII", nameTable, nameStart, nameLen);
          return result;
        }

      match = false;
      switch (namePlatform)
      {
      case PLATFORM_MACINTOSH:
        if ((nameLanguage == macLanguage) || (locale == null))
          match = true;
        else
        {
          switch (macLanguage)
          {
          case 49: /* Azerbaijani/Cyrillic */
            match = (nameLanguage == /* Azerbaijani/Arabic */ 50)
              || (nameLanguage == /* Azerbaijani/Roman */ 150);
            break;

          case 57: /* Mongolian/Mongolian */
            match = (nameLanguage == /* Mongolian/Cyrillic */ 58);
            break;

          case 83: /* Malay/Roman */
            match = (nameLanguage == /* Malay/Arabic */ 84);
            break;
          }
        }
        break;

      case PLATFORM_MICROSOFT:
        if (((nameLanguage & 0xff) == msLanguage) || (locale == null))
          match = true;
        break;
      }


      if (match)
      {
        result = decodeName(namePlatform, nameEncoding, nameLanguage,
                            nameTable, nameStart, nameLen);
        if (result != null)
          return result;
      }
    }

    return null;
  }


  /**
   * The language codes used by the Macintosh operating system.  MacOS
   * defines numeric language identifiers in the range [0 .. 95] and
   * [128 .. 150]. To map this numeric identifier into an ISO 639
   * language code, multiply it by two and take the substring at that
   * position.
   *
   * <p>ISO 639 has revised the code for some languages, namely
   * <code>he</code> for Hebrew (formerly <code>iw</code>),
   * <code>yi</code> (formerly <code>ji</code>), and <code>id</code>
   * for Indonesian (formerly <code>in</code>). In those cases, this
   * table intentionally contains the older, obsolete code.  The
   * reason is that this is the code which
   * java.util.Locale.getLanguage() is specified to return.  The
   * implementation of {@link #getMacLanguageCode} depends on this.
   *
   * @see <a href=
   * "http://www.unicode.org/unicode/onlinedat/languages.html"
   * >Language Codes: ISO 639, Microsoft and Macintosh</a>
   */
  private static final String macLanguageCodes
    // 0                   1                   2
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    = "enfrdeitnlsvesdaptnoiwjaarfielismttrhrzhurhithkoltplhuetlv  "

    // 3                   4                   5
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    + "fofaruzhnlgdsqrocssksljisrmkbgukbeuzkkazazhykamokytgtkmnmnps"

    // 6                   7                   8
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    + "kukssdbonesamrbnasgupaormlkntatesimykmloviintlmsmsamti  sosw"

    // 9                   10                  11
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    + "rwrn  mgeo                                                  "

    // 12                  13                  14
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    + "                cyeucalaqugnayttugtsjwsuglafbriugdgvgatoelkl"

    // 15
    // 0
    + "az";


  /**
   * The primary language IDs used by the Microsoft operating systems.
   *
   * <p>ISO 639 has revised the code for some languages, namely
   * <code>he</code> for Hebrew (formerly <code>iw</code>),
   * <code>yi</code> (formerly <code>ji</code>), and <code>id</code>
   * for Indonesian (formerly <code>in</code>). In those cases, this
   * table intentionally contains the older, obsolete code.  The
   * reason is that this is the code which
   * java.util.Locale.getLanguage() is specified to return.  The
   * implementation of {@link #getMicrosoftLanguageCode} depends on
   * this.
   *
   * @see <a href=
   * "http://www.unicode.org/unicode/onlinedat/languages.html"
   * >Language Codes: ISO 639, Microsoft and Macintosh</a>
   */
  private static final String microsoftLanguageCodes
    // 0                   1                   2
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    = "  arbgcazhcsdadeelenesfifriwhuisitjakonlnoplptrmrorushsksqsv"

    // 3                   4                   5
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    + "thtrurinukbesletlvlttgfavihyazeu  mk  ts    xhzuafkafohimt  "

    // 6                   7                   8
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    + "gajimskkkyswtkuzttbnpaguortateknmlasmrsamnbocykmlomygl    sd"

    // 9                   10                  11
    // 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
    + "  si  iuam  ksnefypstl      ha  yo              omtign  laso";


  /**
   * Maps a Java Locale into a MacOS language code.
   *
   * <p>For languages that are written in several script systems,
   * MacOS defines multiple language codes. Java Locales have a
   * variant which could be used for that purpose, but a small
   * test program revealed that with Sun's JDK 1.4.1_01, only two
   * of 134 available Locales have a variant tag (namely no_NO_NY
   * and th_TH_TH).</p>
   *
   * <p>The following cases are problematic:
   *
   * <ul> <li>Azerbaijani (az): The MacOS language code is 49 if
   * Azerbaijani is written in the Cyrillic script; 50 if written in
   * the Arabic script; 150 if written in the Roman script.  This
   * method will always return 49 for the Azerbaijani locale.</li>
   *
   * <li>Mongolian (mn): The MacOS language code is 57 if Mongolian is
   * written in the Mongolian script; 58 if written in the Cyrillic
   * script. This method will always return 57 for the Mongolian
   * locale.</li>
   *
   * <li>Malay (ms): The MacOS language code is 83 if Malay is written
   * in the Roman script; 84 if written in the Arabic script. This
   * method will always return 83 for the Malay locale.</li> </ul>
   *
   * @return a MacOS language code, or -1 if there is no such code for
   * <code>loc</code>&#x2019;s language.
   */
  private static int getMacLanguageCode(Locale loc)
  {
    int code;

    if (loc == null)
      return -1;

    code = findLanguageCode(loc.getLanguage(), macLanguageCodes);
    switch (code)
    {
    case 19:
      /* Traditional Chinese (MacOS language #19) and and Simplified
       * Chinese (MacOS language #33) both have "zh" as their ISO 639
       * code.
       */
      if (loc.equals(Locale.SIMPLIFIED_CHINESE))
        code = 33;
      break;

      // Other special cases would be 49, 57 and 83, but we do not
      // know what do do about them. See the method documentation for
      // details.
    }

    return code;
  }


  /**
   * Maps a Java Locale into a Microsoft language code.
   */
  private static int getMicrosoftLanguageCode(Locale locale)
  {
    String isoCode;
    int code;

    if (locale == null)
      return -1;

    isoCode = locale.getLanguage();
    code = findLanguageCode(isoCode, microsoftLanguageCodes);
    if (code == -1)
    {
      if (isoCode.equals("hr") || isoCode.equals("sr"))
      {
        /* Microsoft uses code 26 for "sh" (Serbo-Croatian),
         * "hr" (Croatian) and "sr" (Serbian). Our table contains
         * "sh".
         */
        code = 26;
      }
      else if (isoCode.equals("gd"))
      {
        /* Microsoft uses code 60 for "gd" (Scottish Gaelic) and
         * "ga" (Irish Gaelic). Out table contains "ga".
         */
        code = 60;
      }
    }
    return code;
  }


  private static int findLanguageCode(String lang, String langCodes)
  {
    int index;
    if (lang == null)
      return -1;

    if (lang.length() != 2)
      return -1;

    index = 0;
    do
    {
      index = langCodes.indexOf(lang, index);

      /* The index must be even to be considered a match. Otherwise, we
       * could match with the second letter of one language and the
       * first of antoher one.
       */
    }
    while (!((index < 0) || ((index & 1) == 0)));
    if (index < 0)
      return -1;

    index = index / 2;
    return index;
  }


  private static String decodeName(int platform, int encoding, int language,
                                   ByteBuffer buffer, int offset, int len)
  {
    String charsetName = getCharsetName(platform, language, encoding);
    if (charsetName == null)
      return null;

    return decodeName(charsetName, buffer, offset, len);
  }

  private static String decodeName(String charsetName,
                                   ByteBuffer buffer, int offset, int len)
  {
    byte[] byteBuf;
    int oldPosition;

    byteBuf = new byte[len];
    oldPosition = buffer.position();
    try
    {
      buffer.position(offset);
      buffer.get(byteBuf);
      try
      {
        return new String(byteBuf, charsetName);
      }
      catch (UnsupportedEncodingException uex)
      {
      }
    }
    finally
    {
      buffer.position(oldPosition);
    }

    return null;
  }


  /**
   * Maps a MacOS language code into a Java Locale.
   *
   * @param macLanguageCode the MacOS language code for
   * the language whose Java locale is to be retrieved.
   *
   * @return an suitable Locale, or <code>null</code> if
   * the mapping cannot be performed.
   */
  private static Locale getMacLocale(int macLanguageCode)
  {
    String isoCode;

    switch (macLanguageCode)
    {
    case 0: return Locale.ENGLISH;
    case 1: return Locale.FRENCH;
    case 2: return Locale.GERMAN;
    case 3: return Locale.ITALIAN;
    case 11: return Locale.JAPANESE;
    case 23: return Locale.KOREAN;
    case 19: return Locale.TRADITIONAL_CHINESE;
    case 33: return Locale.SIMPLIFIED_CHINESE;
    }

    if ((macLanguageCode < 0) || (macLanguageCode > 150))
      return null;

    isoCode = macLanguageCodes.substring(macLanguageCode << 1,
                                         (macLanguageCode + 1) << 1);
    if (isoCode.charAt(0) == ' ')
      return null;

    return new Locale(isoCode);
  }



  /**
   * Maps a Windows LCID into a Java Locale.
   *
   * @param lcid the Windows language ID whose Java locale
   * is to be retrieved.
   *
   * @return an suitable Locale, or <code>null</code> if
   * the mapping cannot be performed.
   */
  private static Locale getWindowsLocale(int lcid)
  {
    /* FIXME: This is grossly incomplete. */
    switch (lcid)
    {
    case 0x0407: return Locale.GERMAN;
    case 0x0408: return new Locale("el", "GR");
    case 0x0409: return Locale.ENGLISH;
    case 0x040b: return new Locale("fi");
    case 0x040c: return Locale.FRENCH;
    case 0x0416: return new Locale("pt");
    case 0x0807: return new Locale("de", "CH");
    case 0x0809: return new Locale("en", "UK");
    case 0x080c: return new Locale("fr", "BE");
    case 0x0816: return new Locale("pt", "BR");
    case 0x0c07: return new Locale("de", "AT");
    case 0x0c09: return new Locale("en", "AU");
    case 0x0c0c: return new Locale("fr", "CA");
    case 0x1007: return new Locale("de", "LU");
    case 0x1009: return new Locale("en", "CA");
    case 0x100c: return new Locale("fr", "CH");
    case 0x1407: return new Locale("de", "LI");
    case 0x1409: return new Locale("en", "NZ");
    case 0x140c: return new Locale("fr", "LU");
    case 0x1809: return new Locale("en", "IE");

    default:
      return null;
    }
  }


  /**
   * Maps a Macintosh Script Manager code to the name of the
   * corresponding Java Charset.
   *
   * @param macScript a MacOS ScriptCode, for example
   * 6 for <code>smGreek</code>.
   *
   * @return a String that can be used to retrieve a Java
   * CharsetDecorder, for example <code>MacGreek</code>, or
   * <code>null</code> if <code>macScript</code> has an
   * unsupported value.
   */
  private static String getMacCharsetName(int macScript)
  {
    switch (macScript)
    {
    case 0: return "MacRoman";
    case 1: return "MacJapanese";
    case 2: return "MacKorean";
    case 3: return "MacTradChinese";
    case 4: return "MacArabic";
    case 5: return "MacHebrew";
    case 6: return "MacGreek";
    case 7: return "MacCyrillic";
    case 8: return "MacRSymbol";
    case 9: return "MacDevanagari";
    case 10: return "MacGurmukhi";
    case 11: return "MacGujarati";
    case 12: return "MacOriya";
    case 13: return "MacBengali";
    case 14: return "MacTamil";
    case 15: return "MacTelugu";
    case 16: return "MacKannada";
    case 17: return "MacMalayalam";
    case 18: return "MacSinhalese";
    case 19: return "MacBurmese";
    case 20: return "MacKhmer";
    case 21: return "MacThai";
    case 22: return "MacLao";
    case 23: return "MacGeorgian";
    case 24: return "MacArmenian";
    case 25: return "MacSimpChinese";
    case 26: return "MacTibetan";
    case 27: return "MacMongolian";
    case 28: return "MacEthiopic";
    case 29: return "MacCentralEurope";
    case 30: return "MacVietnamese";
    case 31: return "MacExtArabic";

    default: return null;
    }
  }


  /**
   * Maps a Microsoft locale ID (LCID) to the name of the
   * corresponding Java Charset.
   *
   * @param lcid the Microsoft locale ID.
   *
   * @return a String that can be used to retrieve a Java
   * CharsetDecorder, for example <code>windows-1252</code>, or
   * <code>null</code> if <code>lcid</code> has an unsupported value.
   */
  private static String getMicrosoftCharsetName(int lcid)
  {
    int lang;
    char codePage = '?';

    /* Extract the language code from the LCID. */
    lang = lcid & 0x3ff;

    /* In the majority of cases, the language alone determines the
     * codepage.
     */
    if (lang < 100)
      codePage = (" 612D022322225022EC2202201?002A462110777 68 ?2 1  "
                  + "    2 2     2112 ?1           1     2           2 ")
        .charAt(lang);

    /* There are a few exceptions, however, where multiple code pages
     * are used for the same language. */
    if (codePage == '?')
    {
      switch (lcid)
      {
      case 0x041a: // Croatian --> Windows-1250 (Central Europe)
      case 0x081a: // Serbian (Latin) --> Windows-1250 (Central Europe)
        codePage = '0';
        break;

      case 0x42c: // Azeri (Latin) --> Windows-1254 (Turkish)
      case 0x443: // Uzbek (Latin) --> Windows-1254 (Turkish)
        codePage = '4';
        break;

      case 0x82c: // Azeri (Cyrillic) --> Windows-1251 (Cyrillic)
      case 0x843: // Uzbek (Cyrillic) --> Windows-1251 (Cyrillic)
      case 0xc1a: // Serbian (Cyrillic) --> Windows-1251 (Cyrillic)
        codePage = '1';
        break;
      }
    }

    switch (codePage)
    {
    case '0': return "windows-1250"; // Central Europe
    case '1': return "windows-1251"; // Cyrillic
    case '2': return "windows-1252"; // Latin 1
    case '3': return "windows-1253"; // Greek
    case '4': return "windows-1254"; // Turkish
    case '5': return "windows-1255"; // Hebrew
    case '6': return "windows-1256"; // Arabic
    case '7': return "windows-1257"; // Baltic
    case '8': return "windows-1258"; // Vietnam
    case 'A': return "windows-874";  // Thai
    case 'B': return "windows-936";  // Simplified Chinese, GBK
    case 'C': return "windows-949";  // Korean
    case 'D': return "windows-950";  // Traditional Chinese, Big5
    case 'E': return "windows-932";  // Japanese Shift-JIS
    default: return null;
    }
  }


  /**
   * Returns the Locale of an OpenType name.
   *
   * @param platform the OpenType platform ID.
   *
   * @param language the language tag of the OpenType name. If
   * <code>platform</code> is 1, this is the MacOS language code.
   *
   * @param encoding the encoding tag of the OpenType name. If
   * <code>platform</code> is 1, this is the MacOS script code.
   */
  public static Locale getLocale(int platform, int language, int encoding)
  {
    switch (platform)
    {
    case 1: /* Apple Macintosh */
      return getMacLocale(language);

    case 3: /* Microsoft Windows */
      return getWindowsLocale(language);

    default:
      return null;
    }
  }


  /**
   * Determines the name of the charset for an OpenType font name.
   *
   * @param platform the OpenType platform ID.
   *
   * @param language the language tag of the OpenType name. If
   * <code>platform</code> is 1, this is the MacOS language code.
   *
   * @param encoding the encoding tag of the OpenType name. If
   * <code>platform</code> is 1, this is the MacOS script code.
   *
   * @return a charset name such as <code>&quot;MacRoman&quot;</code>,
   * or <code>null</code> if the combination is not known.
   */
  public static String getCharsetName(int platform, int language, int encoding)
  {
    switch (platform)
    {
    case 1: /* Apple Macintosh */
      return getMacCharsetName(encoding);

    case 3: /* Microsoft Windows */
      return getMicrosoftCharsetName(language);

    default:
      return null;
    }
  }
}
