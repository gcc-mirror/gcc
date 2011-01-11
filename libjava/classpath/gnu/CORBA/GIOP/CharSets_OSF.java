/* CharSets_OSF.java --
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


package gnu.CORBA.GIOP;

import java.nio.charset.Charset;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;

/**
 * This class contains the codes, used to identify character sets
 * in CORBA. These codes are defined in Open Software Foundation (OSF)
 * code set registry.
 *
 * The name of this class specially sets "OSF" apart if somebody would start
 * searching Open Software Foundation abbreviation.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class CharSets_OSF
{
  public static final int ASCII = 0x00010020;
  public static final int ISO8859_1 = 0x00010001;
  public static final int ISO8859_2 = 0x00010002;
  public static final int ISO8859_3 = 0x00010003;
  public static final int ISO8859_4 = 0x00010004;
  public static final int ISO8859_5 = 0x00010005;
  public static final int ISO8859_6 = 0x00010006;
  public static final int ISO8859_7 = 0x00010007;
  public static final int ISO8859_8 = 0x00010008;
  public static final int ISO8859_9 = 0x00010009;
  public static final int ISO8859_15_FDIS = 0x0001000F;
  public static final int UTF8 = 0x05010001;
  public static final int UTF16 = 0x00010109;
  public static final int UCS2 = 0x00010100;
  public static final int Cp1047 = 0x10020417;
  public static final int Cp1250 = 0x100204E2;
  public static final int Cp1251 = 0x100204E3;
  public static final int Cp1252 = 0x100204E4;
  public static final int Cp1253 = 0x100204E5;
  public static final int Cp1254 = 0x100204E6;
  public static final int Cp1255 = 0x100204E7;
  public static final int Cp1256 = 0x100204E8;
  public static final int Cp1257 = 0x100204E9;
  public static final int Cp1363 = 0x10020553;
  public static final int Cp1363C = 0x10020553;
  public static final int Cp1381 = 0x10020565;
  public static final int Cp1383 = 0x10020567;
  public static final int Cp1386 = 0x1002056A;
  public static final int Cp33722 = 0x100283BA;
  public static final int Cp33722C = 0x100283BA;
  public static final int Cp930 = 0x100203A2;
  public static final int Cp943 = 0x100203AF;
  public static final int Cp943C = 0x100203AF;
  public static final int Cp949 = 0x100203B5;
  public static final int Cp949C = 0x100203B5;
  public static final int Cp950 = 0x100203B6;
  public static final int Cp964 = 0x100203C4;
  public static final int Cp970 = 0x100203CA;
  public static final int EUC_JP = 0x00030010;
  public static final int EUC_KR = 0x0004000A;
  public static final int EUC_TW = 0x00050010;

  /**
   * The native character set for the narrow character.
   */
  public static final int NATIVE_CHARACTER = ISO8859_1;

  /**
   * The native character set for the wide character.
   */
  public static final int NATIVE_WIDE_CHARACTER = UTF16;

  /**
   * Table to convert from the code to string name.
   */
  private static Hashtable code_to_string;

  /**
   * Table to convert from the string name to code.
   */
  private static Hashtable string_to_code;

  /**
   * Get the charset code from its name.
   *
   * @return the charset code of 0 if not defined.
   */
  public static int getCode(String name)
  {
    if (string_to_code == null)
      makeMap();

    Integer code = (Integer) string_to_code.get(name);
    return code == null ? 0 : code.intValue();
  }

  /**
   * Get the charset name from its code.
   *
   * @return the code set name or nullfor the unknown code set.
   */
  public static String getName(int code)
  {
    if (code_to_string == null)
      makeMap();
    return (String) code_to_string.get(new Integer(code));
  }

  /**
   * Get the list of supported char sets for that the CORBA codes are
   * also known.
   */
  public static int[] getSupportedCharSets()
  {
    Set supported_sets = Charset.availableCharsets().keySet();
    int[] supported = new int[ supported_sets.size() ];
    Iterator iter = supported_sets.iterator();

    int i = 0;
    int code;
    while (iter.hasNext())
      {
        code = getCode(iter.next().toString());
        if (code > 0)
          supported [ i++ ] = code;
      }

    // Truncate the unused part.
    int[] f = new int[ i ];
    System.arraycopy(supported, 0, f, 0, f.length);

    return f;
  }

  /**
   * Create a convertion map.
   */
  private static void makeMap()
  {
    code_to_string = new Hashtable();
    string_to_code = new Hashtable();

    // Put standard char sets.
    put(ASCII, "US-ASCII");
    put(ISO8859_1, "ISO-8859-1");
    put(UTF16, "UTF-16");

    // Put other known char sets.
    put(ISO8859_2, "ISO-8859-2");
    put(ISO8859_3, "ISO-8859-3");
    put(ISO8859_4, "ISO-8859-4");
    put(ISO8859_5, "ISO-8859-5");
    put(ISO8859_6, "ISO-8859-6");
    put(ISO8859_7, "ISO-8859-7");
    put(ISO8859_8, "ISO-8859-8");
    put(ISO8859_9, "ISO-8859-9");

    put(UTF8, "UTF-8");
    put(UCS2, "UCS-2");

    put(ISO8859_15_FDIS, "ISO8859-15-FDIS");

    put(Cp1047, "Cp1047");
    put(Cp1250, "Cp1250");
    put(Cp1251, "Cp1251");
    put(Cp1252, "Cp1252");
    put(Cp1253, "Cp1253");
    put(Cp1254, "Cp1254");
    put(Cp1255, "Cp1255");
    put(Cp1256, "Cp1256");
    put(Cp1257, "Cp1257");
    put(Cp1363, "Cp1363");
    put(Cp1363C, "Cp1363C");
    put(Cp1381, "Cp1381");
    put(Cp1383, "Cp1383");
    put(Cp1386, "Cp1386");
    put(Cp33722, "Cp33722");
    put(Cp33722C, "Cp33722C");
    put(Cp930, "Cp930");
    put(Cp943, "Cp943");
    put(Cp943C, "Cp943C");
    put(Cp949, "Cp949");
    put(Cp949C, "Cp949C");
    put(Cp950, "Cp950");
    put(Cp964, "Cp964");
    put(Cp970, "Cp970");

    put(EUC_JP, "EUC-JP");
    put(EUC_KR, "EUC-KR");
    put(EUC_TW, "EUC-TW");
  }

  private static void put(int code, String name)
  {
    Integer ic = new Integer(code);

    code_to_string.put(ic, name);
    string_to_code.put(name, ic);
  }
}
