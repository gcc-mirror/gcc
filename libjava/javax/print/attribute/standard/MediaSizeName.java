/* MediaSizeName.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class MediaSizeName extends Media
{
  private static final long serialVersionUID = 2778798329756942747L;

  public static final MediaSizeName ISO_A0 = new MediaSizeName(0);
  public static final MediaSizeName ISO_A1 = new MediaSizeName(1);
  public static final MediaSizeName ISO_A2 = new MediaSizeName(2);
  public static final MediaSizeName ISO_A3 = new MediaSizeName(3);
  public static final MediaSizeName ISO_A4 = new MediaSizeName(4);
  public static final MediaSizeName ISO_A5 = new MediaSizeName(5);
  public static final MediaSizeName ISO_A6 = new MediaSizeName(6);
  public static final MediaSizeName ISO_A7 = new MediaSizeName(7);
  public static final MediaSizeName ISO_A8 = new MediaSizeName(8);
  public static final MediaSizeName ISO_A9 = new MediaSizeName(9);
  public static final MediaSizeName ISO_A10 = new MediaSizeName(10);
  public static final MediaSizeName ISO_B0 = new MediaSizeName(11);
  public static final MediaSizeName ISO_B1 = new MediaSizeName(12);
  public static final MediaSizeName ISO_B2 = new MediaSizeName(13);
  public static final MediaSizeName ISO_B3 = new MediaSizeName(14);
  public static final MediaSizeName ISO_B4 = new MediaSizeName(15);
  public static final MediaSizeName ISO_B5 = new MediaSizeName(16);
  public static final MediaSizeName ISO_B6 = new MediaSizeName(17);
  public static final MediaSizeName ISO_B7 = new MediaSizeName(18);
  public static final MediaSizeName ISO_B8 = new MediaSizeName(19);
  public static final MediaSizeName ISO_B9 = new MediaSizeName(20);
  public static final MediaSizeName ISO_B10 = new MediaSizeName(21);
  public static final MediaSizeName JIS_B0 = new MediaSizeName(22);
  public static final MediaSizeName JIS_B1 = new MediaSizeName(23);
  public static final MediaSizeName JIS_B2 = new MediaSizeName(24);
  public static final MediaSizeName JIS_B3 = new MediaSizeName(25);
  public static final MediaSizeName JIS_B4 = new MediaSizeName(26);
  public static final MediaSizeName JIS_B5 = new MediaSizeName(27);
  public static final MediaSizeName JIS_B6 = new MediaSizeName(28);
  public static final MediaSizeName JIS_B7 = new MediaSizeName(29);
  public static final MediaSizeName JIS_B8 = new MediaSizeName(30);
  public static final MediaSizeName JIS_B9 = new MediaSizeName(31);
  public static final MediaSizeName JIS_B10 = new MediaSizeName(32);
  public static final MediaSizeName ISO_C0 = new MediaSizeName(33);
  public static final MediaSizeName ISO_C1 = new MediaSizeName(34);
  public static final MediaSizeName ISO_C2 = new MediaSizeName(35);
  public static final MediaSizeName ISO_C3 = new MediaSizeName(36);
  public static final MediaSizeName ISO_C4 = new MediaSizeName(37);
  public static final MediaSizeName ISO_C5 = new MediaSizeName(38);
  public static final MediaSizeName ISO_C6 = new MediaSizeName(39);
  public static final MediaSizeName NA_LETTER = new MediaSizeName(40);
  public static final MediaSizeName NA_LEGAL = new MediaSizeName(41);
  public static final MediaSizeName EXECUTIVE = new MediaSizeName(42);
  public static final MediaSizeName LEDGER = new MediaSizeName(43);
  public static final MediaSizeName TABLOID = new MediaSizeName(44);
  public static final MediaSizeName INVOICE = new MediaSizeName(45);
  public static final MediaSizeName FOLIO = new MediaSizeName(46);
  public static final MediaSizeName QUARTO = new MediaSizeName(47);
  public static final MediaSizeName JAPANESE_POSTCARD = new MediaSizeName(48);
  public static final MediaSizeName JAPANESE_DOUBLE_POSTCARD =
    new MediaSizeName(49);
  public static final MediaSizeName A = new MediaSizeName(50);
  public static final MediaSizeName B = new MediaSizeName(51);
  public static final MediaSizeName C = new MediaSizeName(52);
  public static final MediaSizeName D = new MediaSizeName(53);
  public static final MediaSizeName E = new MediaSizeName(54);
  public static final MediaSizeName ISO_DESIGNATED_LONG =
    new MediaSizeName(55);
  public static final MediaSizeName ITALY_ENVELOPE = new MediaSizeName(56);
  public static final MediaSizeName MONARCH_ENVELOPE = new MediaSizeName(57);
  public static final MediaSizeName PERSONAL_ENVELOPE = new MediaSizeName(58);
  public static final MediaSizeName NA_NUMBER_9_ENVELOPE =
    new MediaSizeName(59);
  public static final MediaSizeName NA_NUMBER_10_ENVELOPE =
    new MediaSizeName(60);
  public static final MediaSizeName NA_NUMBER_11_ENVELOPE =
    new MediaSizeName(61);
  public static final MediaSizeName NA_NUMBER_12_ENVELOPE =
    new MediaSizeName(62);
  public static final MediaSizeName NA_NUMBER_14_ENVELOPE =
    new MediaSizeName(63);
  public static final MediaSizeName NA_6X9_ENVELOPE = new MediaSizeName(64);
  public static final MediaSizeName NA_7X9_ENVELOPE = new MediaSizeName(65);
  public static final MediaSizeName NA_9X11_ENVELOPE = new MediaSizeName(66);
  public static final MediaSizeName NA_9X12_ENVELOPE = new MediaSizeName(67);
  public static final MediaSizeName NA_10X13_ENVELOPE = new MediaSizeName(68);
  public static final MediaSizeName NA_10X14_ENVELOPE = new MediaSizeName(69);
  public static final MediaSizeName NA_10X15_ENVELOPE = new MediaSizeName(70);
  public static final MediaSizeName NA_5X7 = new MediaSizeName(71);
  public static final MediaSizeName NA_8X10 = new MediaSizeName(72);

  /**
   * Constructs a <code>MediaSizeName</code> object.
   */
  protected MediaSizeName(int value)
  {
    super(value);
  }
}
