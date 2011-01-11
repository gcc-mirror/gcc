/* MediaSizeName.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

package javax.print.attribute.standard;

import javax.print.attribute.EnumSyntax;

/**
 * <code>MediaSizeName</code> is a subclass of the <code>Media</code> printing
 * attribute and provides selection of media to be used by the means of
 * defined size names. The class pre-defines commonly available media sizes.
 * This media type enumeration may be used in alternative to
 * MediaName/MediaTray.
 * <p>
 * <b>IPP Compatibility:</b> MediaSizeName is not an IPP 1.1 attribute on its
 * own. It provides parts of the <code>media</code> attribute type values.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class MediaSizeName extends Media
{
  private static final long serialVersionUID = 2778798329756942747L;

  /**
   * The ISO A0 size: 841 mm by 1189 mm.
   */
  public static final MediaSizeName ISO_A0 = new MediaSizeName(0);

  /**
   * The ISO A1 size: 594 mm by 841 mm.
   */
  public static final MediaSizeName ISO_A1 = new MediaSizeName(1);

  /**
   * The ISO A2 size: 420 mm by 594 mm.
   */
  public static final MediaSizeName ISO_A2 = new MediaSizeName(2);

  /**
   * The ISO A3 size: 297 mm by 420 mm.
   */
  public static final MediaSizeName ISO_A3 = new MediaSizeName(3);

  /**
   * The ISO A4 size: 210 mm by 297 mm.
   */
  public static final MediaSizeName ISO_A4 = new MediaSizeName(4);

  /**
   * The ISO A5 size: 148 mm by 210 mm.
   */
  public static final MediaSizeName ISO_A5 = new MediaSizeName(5);

  /**
   * The ISO A6 size: 105 mm by 148 mm.
   */
  public static final MediaSizeName ISO_A6 = new MediaSizeName(6);

  /**
   * The ISO A7 size: 74 mm by 105 mm.
   */
  public static final MediaSizeName ISO_A7 = new MediaSizeName(7);

  /**
   * The ISO A8 size: 52 mm by 74 mm.
   */
  public static final MediaSizeName ISO_A8 = new MediaSizeName(8);

  /**
   * The ISO A9 size: 37 mm by 52 mm.
   */
  public static final MediaSizeName ISO_A9 = new MediaSizeName(9);

  /**
   * The ISO A10 size: 26 mm by 37 mm.
   */
  public static final MediaSizeName ISO_A10 = new MediaSizeName(10);

  /**
   * The ISO B0 size: 1000 mm by 1414 mm.
   */
  public static final MediaSizeName ISO_B0 = new MediaSizeName(11);

  /**
   * The ISO B1 size: 707 mm by 1000 mm.
   */
  public static final MediaSizeName ISO_B1 = new MediaSizeName(12);

  /**
   * The ISO B2 size: 500 mm by 707 mm.
   */
  public static final MediaSizeName ISO_B2 = new MediaSizeName(13);

  /**
   * The ISO B3 size: 353 mm by 500 mm.
   */
  public static final MediaSizeName ISO_B3 = new MediaSizeName(14);

  /**
   * The ISO B4 size: 250 mm by 353 mm.
   */
  public static final MediaSizeName ISO_B4 = new MediaSizeName(15);

  /**
   * The ISO B5 size: 176 mm by 250 mm.
   */
  public static final MediaSizeName ISO_B5 = new MediaSizeName(16);

  /**
   * The ISO B6 size: 125 mm by 176 mm.
   */
  public static final MediaSizeName ISO_B6 = new MediaSizeName(17);

  /**
   * The ISO B7 size: 88 mm by 125 mm.
   */
  public static final MediaSizeName ISO_B7 = new MediaSizeName(18);

  /**
   * The ISO B8 size: 62 mm by 88 mm.
   */
  public static final MediaSizeName ISO_B8 = new MediaSizeName(19);

  /**
   * The ISO B9 size: 44 mm by 62 mm.
   */
  public static final MediaSizeName ISO_B9 = new MediaSizeName(20);

  /**
   * The ISO B10 size: 31 mm by 44 mm.
   */
  public static final MediaSizeName ISO_B10 = new MediaSizeName(21);

  /**
   * The JIS B0 size: 1030mm x 1456mm.
   */
  public static final MediaSizeName JIS_B0 = new MediaSizeName(22);

  /**
   * The JIS B1 size: 728mm x 1030mm.
   */
  public static final MediaSizeName JIS_B1 = new MediaSizeName(23);

  /**
   * The JIS B2 size: 515mm x 728mm.
   */
  public static final MediaSizeName JIS_B2 = new MediaSizeName(24);

  /**
   * The JIS B3 size: 364mm x 515mm.
   */
  public static final MediaSizeName JIS_B3 = new MediaSizeName(25);

  /**
   * The JIS B4 size: 257mm x 364mm.
   */
  public static final MediaSizeName JIS_B4 = new MediaSizeName(26);

  /**
   * The JIS B5 size: 182mm x 257mm.
   */
  public static final MediaSizeName JIS_B5 = new MediaSizeName(27);

  /**
   * The JIS B6 size: 128mm x 182mm.
   */
  public static final MediaSizeName JIS_B6 = new MediaSizeName(28);

  /**
   * The JIS B7 size: 91mm x 128mm.
   */
  public static final MediaSizeName JIS_B7 = new MediaSizeName(29);

  /**
   * The JIS B8 size: 64mm x 91mm.
   */
  public static final MediaSizeName JIS_B8 = new MediaSizeName(30);

  /**
   * The JIS B9 size: 45mm x 64mm.
   */
  public static final MediaSizeName JIS_B9 = new MediaSizeName(31);

  /**
   * The JIS B10 size: 32mm x 45mm.
   */
  public static final MediaSizeName JIS_B10 = new MediaSizeName(32);

  /**
   * The ISO C0 size: 917 mm by 1297 mm.
   */
  public static final MediaSizeName ISO_C0 = new MediaSizeName(33);

  /**
   * The ISO C1 size: 648 mm by 917 mm.
   */
  public static final MediaSizeName ISO_C1 = new MediaSizeName(34);

  /**
   * The ISO C2 size: 458 mm by 648 mm.
   */
  public static final MediaSizeName ISO_C2 = new MediaSizeName(35);

  /**
   * The ISO C3 size: 324 mm by 458 mm.
   */
  public static final MediaSizeName ISO_C3 = new MediaSizeName(36);

  /**
   * The ISO C4 size: 229 mm by 324 mm.
   */
  public static final MediaSizeName ISO_C4 = new MediaSizeName(37);

  /**
   * The ISO C5 size: 162 mm by 229 mm.
   */
  public static final MediaSizeName ISO_C5 = new MediaSizeName(38);

  /**
   * The ISO C6 size: 114 mm by 162 mm.
   */
  public static final MediaSizeName ISO_C6 = new MediaSizeName(39);

  /**
   * The North American letter size: 8.5 inches by 11 inches.
   */
  public static final MediaSizeName NA_LETTER = new MediaSizeName(40);

  /**
   * The North American legal size: 8.5 inches by 14 inches.
   */
  public static final MediaSizeName NA_LEGAL = new MediaSizeName(41);

  /**
   * The executive size: 7.25 inches by 10.5 inches.
   */
  public static final MediaSizeName EXECUTIVE = new MediaSizeName(42);

  /**
   * The ledger size: 11 inches by 17 inches.
   */
  public static final MediaSizeName LEDGER = new MediaSizeName(43);

  /**
   * The tabloid size: 11 inches by 17 inches.
   */
  public static final MediaSizeName TABLOID = new MediaSizeName(44);

  /**
   * The invoice size: 5.5 inches by 8.5 inches.
   */
  public static final MediaSizeName INVOICE = new MediaSizeName(45);

  /**
   * The folio size: 8.5 inches by 13 inches.
   */
  public static final MediaSizeName FOLIO = new MediaSizeName(46);

  /**
   * The quarto size: 8.5 inches by 10.83 inches.
   */
  public static final MediaSizeName QUARTO = new MediaSizeName(47);

  /**
   * The Japanese postcard size, 100 mm by 148 mm.
   */
  public static final MediaSizeName JAPANESE_POSTCARD = new MediaSizeName(48);

  /**
   * The Japanese Double postcard size: 148 mm by 200 mm.
   */
  public static final MediaSizeName JAPANESE_DOUBLE_POSTCARD =
    new MediaSizeName(49);

  /**
   * The engineering ANSI A size medium: 8.5 inches x 11 inches.
   */
  public static final MediaSizeName A = new MediaSizeName(50);

  /**
   * The engineering ANSI B size medium: 11 inches x 17 inches.
   */
  public static final MediaSizeName B = new MediaSizeName(51);

  /**
   * The engineering ANSI C size medium: 17 inches x 22 inches.
   */
  public static final MediaSizeName C = new MediaSizeName(52);

  /**
   * The engineering ANSI D size medium: 22 inches x 34 inches.
   */
  public static final MediaSizeName D = new MediaSizeName(53);

  /**
   * The engineering ANSI E size medium: 34 inches x 44 inches.
   */
  public static final MediaSizeName E = new MediaSizeName(54);

  /**
   * The ISO Designated Long size: 110 mm by 220 mm.
   */
  public static final MediaSizeName ISO_DESIGNATED_LONG =
    new MediaSizeName(55);

  /**
   * The Italy envelope size: 110 mm by 230 mm.
   */
  public static final MediaSizeName ITALY_ENVELOPE = new MediaSizeName(56);

  /**
   * The Monarch envelope size: 3.87 inch by 7.5 inch.
   */
  public static final MediaSizeName MONARCH_ENVELOPE = new MediaSizeName(57);

  /**
   * The Personal envelope size: 3.625 inch by 6.5 inch.
   */
  public static final MediaSizeName PERSONAL_ENVELOPE = new MediaSizeName(58);

  /**
   * The North American number 9 business envelope size:
   * 3.875 inches by 8.875 inches.
   */
  public static final MediaSizeName NA_NUMBER_9_ENVELOPE =
    new MediaSizeName(59);

  /**
   * The North American number 10 business envelope size:
   * 4.125 inches by 9.5 inches.
   */
  public static final MediaSizeName NA_NUMBER_10_ENVELOPE =
    new MediaSizeName(60);

  /**
   * The North American number 11 business envelope size:
   * 4.5 inches by 10.375 inches.
   */
  public static final MediaSizeName NA_NUMBER_11_ENVELOPE =
    new MediaSizeName(61);

  /**
   * The North American number 12 business envelope size:
   * 4.75 inches by 11 inches.
   */
  public static final MediaSizeName NA_NUMBER_12_ENVELOPE =
    new MediaSizeName(62);

  /**
   * The North American number 14 business envelope size:
   * 5 inches by 11.5 inches.
   */
  public static final MediaSizeName NA_NUMBER_14_ENVELOPE =
    new MediaSizeName(63);

  /**
   * The North American 6x9 inch envelope size.
   */
  public static final MediaSizeName NA_6X9_ENVELOPE = new MediaSizeName(64);

  /**
   * The North American 7x9 inch envelope size.
   */
  public static final MediaSizeName NA_7X9_ENVELOPE = new MediaSizeName(65);

  /**
   * The North American 9x11 inch envelope size.
   */
  public static final MediaSizeName NA_9X11_ENVELOPE = new MediaSizeName(66);

  /**
   * The North American 9x12 inch envelope size.
   */
  public static final MediaSizeName NA_9X12_ENVELOPE = new MediaSizeName(67);

  /**
   * The North American 10x13 inch envelope size.
   */
  public static final MediaSizeName NA_10X13_ENVELOPE = new MediaSizeName(68);

  /**
   * The North American 10x14 inch envelope size.
   */
  public static final MediaSizeName NA_10X14_ENVELOPE = new MediaSizeName(69);

  /**
   * The North American 10x15 inch envelope size.
   */
  public static final MediaSizeName NA_10X15_ENVELOPE = new MediaSizeName(70);

  /**
   * The North American 5 inches by 7 inches.
   */
  public static final MediaSizeName NA_5X7 = new MediaSizeName(71);

  /**
   * The North American 8 inches by 10 inches.
   */
  public static final MediaSizeName NA_8X10 = new MediaSizeName(72);

  private static final String[] stringTable =
    { "iso-a0",  "iso-a1", "iso-a2", "iso-a3", "iso-a4", "iso-a5", "iso-a6",
      "iso-a7", "iso-a8", "iso-a9", "iso-a10", "iso-b0", "iso-b1", "iso-b2",
      "iso-b3", "iso-b4", "iso-b5", "iso-b6", "iso-b7", "iso-b8", "iso-b9",
      "iso-b10", "jis-b0", "jis-b1", "jis-b2", "jis-b3", "jis-b4", "jis-b5",
      "jis-b6", "jis-b7", "jis-b8", "jis-b9", "jis-b10", "iso-c0", "iso-c1",
      "iso-c2", "iso-c3", "iso-c4", "iso-c5", "iso-c6", "na-letter",
      "na-legal", "executive", "ledger", "tabloid", "invoice", "folio",
      "quarto", "japanese-postcard", "oufuko-postcard", "a", "b", "c", "d",
      "e", "iso-designated-long", "italian-envelope", "monarch-envelope",
      "personal-envelope", "na-number-9-envelope", "na-number-10-envelope",
      "na-number-11-envelope", "na-number-12-envelope",
      "na-number-14-envelope", "na-6x9-envelope", "na-7x9-envelope",
      "na-9x11-envelope", "na-9x12-envelope", "na-10x13-envelope",
      "na-10x14-envelope", "na-10x15-envelope", "na-5x7", "na-8x10" };

  private static final MediaSizeName[] enumValueTable =
    { ISO_A0, ISO_A1, ISO_A2, ISO_A3, ISO_A4, ISO_A5, ISO_A6, ISO_A7, ISO_A8,
      ISO_A9, ISO_A10, ISO_B0, ISO_B1, ISO_B2, ISO_B3, ISO_B4, ISO_B5, ISO_B6,
      ISO_B7, ISO_B8, ISO_B9, ISO_B10, JIS_B0, JIS_B1, JIS_B2, JIS_B3, JIS_B4,
      JIS_B5, JIS_B6, JIS_B7, JIS_B8, JIS_B9, JIS_B10, ISO_C0, ISO_C1, ISO_C2,
      ISO_C3, ISO_C4, ISO_C5, ISO_C6, NA_LETTER, NA_LEGAL, EXECUTIVE, LEDGER,
      TABLOID, INVOICE, FOLIO, QUARTO, JAPANESE_POSTCARD,
      JAPANESE_DOUBLE_POSTCARD, A, B, C, D, E, ISO_DESIGNATED_LONG,
      ITALY_ENVELOPE, MONARCH_ENVELOPE, PERSONAL_ENVELOPE,
      NA_NUMBER_9_ENVELOPE, NA_NUMBER_10_ENVELOPE, NA_NUMBER_11_ENVELOPE,
      NA_NUMBER_12_ENVELOPE, NA_NUMBER_14_ENVELOPE, NA_6X9_ENVELOPE,
      NA_7X9_ENVELOPE, NA_9X11_ENVELOPE, NA_9X12_ENVELOPE, NA_10X13_ENVELOPE,
      NA_10X14_ENVELOPE, NA_10X15_ENVELOPE, NA_5X7, NA_8X10 };

  /**
   * Constructs a <code>MediaSizeName</code> object.
   *
   * @param value the enum value.
   */
  protected MediaSizeName(int value)
  {
    super(value);
  }

  /**
   * Returns a table with the enumeration values represented as strings
   * for this object.
   *
   * @return The enumeration values as strings.
   */
  protected String[] getStringTable()
  {
    return stringTable;
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * @return The enumeration values.
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return enumValueTable;
  }
}
