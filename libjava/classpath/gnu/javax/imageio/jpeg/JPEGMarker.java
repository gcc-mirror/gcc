/* JPEGMarker.java --
   Copyright (C)  2006  Free Software Foundation, Inc.

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

package gnu.javax.imageio.jpeg;

public class JPEGMarker
{
  /**
   * JFIF identifiers.
   */
  public final static byte JFIF_J = (byte) 0x4a;
  public final static byte JFIF_F = (byte) 0x46;
  public final static byte JFIF_I = (byte) 0x49;
  public final static byte JFIF_X = (byte) 0x46;

  /**
   * JFIF extension codes.
   */
  public final static byte JFXX_JPEG = (byte) 0x10;
  public final static byte JFXX_ONE_BPP = (byte) 0x11;
  public final static byte JFXX_THREE_BPP = (byte) 0x13;

  /**
   * Marker prefix byte.
   */
  public final static byte XFF = (byte) 0xff;

  /**
   * Marker byte that represents a literal 0xff.
   */
  public final static byte X00 = (byte) 0x00;

  /**
   *  Application Reserved Keyword.
   */
  public final static byte APP0 = (byte) 0xe0;

  public final static byte APP1 = (byte) 0xe1;
  public final static byte APP2 = (byte) 0xe2;
  public final static byte APP3 = (byte) 0xe3;
  public final static byte APP4 = (byte) 0xe4;
  public final static byte APP5 = (byte) 0xe5;
  public final static byte APP6 = (byte) 0xe6;
  public final static byte APP7 = (byte) 0xe7;
  public final static byte APP8 = (byte) 0xe8;
  public final static byte APP9 = (byte) 0xe9;
  public final static byte APP10 = (byte) 0xea;
  public final static byte APP11 = (byte) 0xeb;
  public final static byte APP12 = (byte) 0xec;
  public final static byte APP13 = (byte) 0xed;
  public final static byte APP14 = (byte) 0xee;
  public final static byte APP15 = (byte) 0xef;

  /**
   * Modulo Restart Interval.
   */
  public final static byte RST0 = (byte) 0xd0;

  public final static byte RST1 = (byte) 0xd1;
  public final static byte RST2 = (byte) 0xd2;
  public final static byte RST3 = (byte) 0xd3;
  public final static byte RST4 = (byte) 0xd4;
  public final static byte RST5 = (byte) 0xd5;
  public final static byte RST6 = (byte) 0xd6;
  public final static byte RST7 = (byte) 0xd7;

  /**
   * Nondifferential Huffman-coding frame (baseline dct).
   */
  public final static byte SOF0 = (byte) 0xc0;

  /**
   * Nondifferential Huffman-coding frame (extended dct).
   */
  public final static byte SOF1 = (byte) 0xc1;

  /**
   * Nondifferential Huffman-coding frame (progressive dct).
   */
  public final static byte SOF2 = (byte) 0xc2;

  /**
   * Nondifferential Huffman-coding frame Lossless (Sequential).
   */
  public final static byte SOF3 = (byte) 0xc3;

  /**
   * Differential Huffman-coding frame Sequential DCT.
   */
  public final static byte SOF5 = (byte) 0xc5;

  /**
   * Differential Huffman-coding frame Progressive DCT.
   */
  public final static byte SOF6 = (byte) 0xc6;

  /**
   * Differential Huffman-coding frame lossless.
   */
  public final static byte SOF7 = (byte) 0xc7;

  /**
   * Nondifferential Arithmetic-coding frame (extended dct).
   */
  public final static byte SOF9 = (byte) 0xc9;

  /**
   * Nondifferential Arithmetic-coding frame (progressive dct).
   */
  public final static byte SOF10 = (byte) 0xca;

  /**
   * Nondifferential Arithmetic-coding frame (lossless).
   */
  public final static byte SOF11 = (byte) 0xcb;

  /**
   * Differential Arithmetic-coding frame (sequential dct).
   */
  public final static byte SOF13 = (byte) 0xcd;

  /**
   * Differential Arithmetic-coding frame (progressive dct).
   */
  public final static byte SOF14 = (byte) 0xce;

  /**
   * Differential Arithmetic-coding frame (lossless).
   */
  public final static byte SOF15 = (byte) 0xcf;

  /**
   * Huffman Table.
   */
  public final static byte DHT = (byte) 0xc4;

  /**
   * Quantization Table.
   */
  public final static byte DQT = (byte) 0xdb;
 
  /**
   * Start of Scan.
   */
  public final static byte SOS = (byte) 0xda;

  /**
   * Defined Restart Interval.
   */
  public final static byte DRI = (byte) 0xdd;

  /**
   * Comment in JPEG.
   */
  public final static byte COM = (byte) 0xfe;

  /**
   * Start of Image.
   */
  public final static byte SOI = (byte) 0xd8;

  /**
   * End of Image.
   */
  public final static byte EOI = (byte) 0xd9;

  /**
   * Define Number of Lines.
   */
  public final static byte DNL = (byte) 0xdc;
}
