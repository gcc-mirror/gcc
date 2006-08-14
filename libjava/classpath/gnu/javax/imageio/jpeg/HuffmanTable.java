/* HuffmanTable.java --
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

import java.io.IOException;

import javax.imageio.plugins.jpeg.JPEGHuffmanTable;


/**
 * This Object construct a JPEGHuffmanTable which can be used to encode/decode
 * a scan from a JPEG codec stream. The table must be initalized with either a
 * BITS byte amount and a Huffman Table Value for decoding or a Huffman Size
 * and Huffman Code table for encoding.
 */
public class HuffmanTable
{
  public final static int HUFFMAN_MAX_TABLES = 4;

  private short[]  huffcode = new short[256];
  private short[]  huffsize = new short[256];
  private short[]  EHUFCO;
  private short[]  EHUFSI;
  private short[]  valptr = new short[16];
  private short[]  mincode = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                            -1,-1,-1};
  private short[]  maxcode = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                            -1, -1, -1};
  private short[] huffval;
  private short[] bits;

  static byte JPEG_DC_TABLE = 0;
  static byte JPEG_AC_TABLE = 1;

  private short lastk = 0;

  public HuffmanTable(JPEGHuffmanTable table)
  {
    huffcode = table.getValues();
    bits = table.getLengths();
  }

  /**
   * Generated from FIGURE C.1 - Generation of table of Huffman code sizes on
   * ISO DIS 10918-1. Requirements and Guidelines
   */
  private void generateSizeTable()
  {
    short index=0;
    for(short i=0; i < bits.length ; i++)
      {
        for(short j=0; j < bits[i] ; j++)
          {
            huffsize[index] = (short) (i+1);
            index++;
          }
      }
    lastk = index;
  }

  /**
   * Generated from FIGURE C.2 - Generation of table of Huffman codes on
   * ISO DIS 10918-1. Requirements and Guidelines
   */
  private void generateCodeTable()
  {
    short k=0;
    short si = huffsize[0];
    short code = 0;
    for(short i=0; i < huffsize.length ; i++)
      {
        while(huffsize[k]==si)
          {
            huffcode[k] = code;
            code++;
            k++;
          }
        code <<= 1;
        si++;
      }
  }

  /**
   * Generated from FIGURE F.15 - Generation of decode table generation on
   * ISO DIS 10918-1. Requirements and Guidelines
   */
  private void generateDecoderTables()
  {
    short bitcount = 0;
    for(int i=0; i < 16 ; i++)
      {
        if(bits[i]!=0)
          valptr[i] = bitcount;
        for(int j=0 ; j < bits[i] ; j++)
          {
            if(huffcode[j+bitcount] < mincode[i] || mincode[i] == -1)
              mincode[i] = huffcode[j+bitcount];

            if(huffcode[j+bitcount] > maxcode[i])
              maxcode[i] = huffcode[j+bitcount];
          }
        if(mincode[i]!=-1)
          valptr[i] = (short) (valptr[i] - mincode[i]);
        bitcount += bits[i];
      }
  }

  /**
   * Generated from FIGURE C.3 - Generation of Order Codes and tables EHUFCO
   * and EHUFSI from the ISO DIS 10918-1. Requirements and Guidelines
   */
  public void orderCodes(boolean isDC)
  {
    EHUFCO = new short[isDC ? 15 : 255];
    EHUFSI = new short[isDC ? 15 : 255];

    for (int p=0; p < lastk ; p++)
      {
        int i = huffval[p];
        if(i < 0 || i > EHUFCO.length || EHUFSI[i]!=0)
          System.err.println("Error, bad huffman table.");
        EHUFCO[i] = huffcode[p];
        EHUFSI[i] = huffsize[p];
      }
  }

  /**
   * Generated from FIGURE F.12 - Extending the sign bit of a decoded value in on
   * ISO DIS 10918-1. Requirements and Guidelines<p>
   *
   * @param diff TODO
   * @param t TODO
   * @return TODO
   */
  public static int extend(int diff, int t)
  {
    int Vt = (int)Math.pow(2,(t-1));
    if(diff<Vt)
      {
        Vt=(-1 << t)+1;
        diff=diff+Vt;
      }
    return diff;
  }

  /**
   * Generated from FIGURE F.16 - Procedure for DECODE on
   * ISO DIS 10918-1. Requirements and Guidelines<p>
   *
   * This function takes in a dynamic amount of bits and using the Huffman
   * table returns information on how many bits must be read in to a byte in
   * order to reconstruct said byte.
   *
   * @param JPEGStream the bits of the data stream.
   */
  public int decode(JPEGImageInputStream JPEGStream)
    throws IOException, JPEGException
  {
    int i=0;
    short code = (short) JPEGStream.readBits(1);
    while(code > maxcode[i])
      {
        i++;
        code <<= 1;
        code |= JPEGStream.readBits(1);
      }
    int val = huffval[code+(valptr[i])];
    if(val < 0)
      val = 256 + val;
    return val;
  }
}
