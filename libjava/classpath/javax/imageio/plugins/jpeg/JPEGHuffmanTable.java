/* JPEGHuffmanTable.java --
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


package javax.imageio.plugins.jpeg;

/**
 * The JPEGHuffmanTable class represents a Huffman table read from a
 * JPEG image file.  The standard JPEG AC and DC chrominance and
 * luminance values are provided as static fields.
 */
public class JPEGHuffmanTable
{
  /**
   * Huffman code lengths.
   */
  private short[] lengths;

  /**
   * Huffman values.
   */
  private short[] values;

  // The private constructors are used for these final fields to avoid
  // unnecessary copying.
  /**
   * The standard JPEG AC chrominance Huffman table.
   */
  public static final JPEGHuffmanTable StdACChrominance =
      new JPEGHuffmanTable(new short[] { 0, 2, 1, 2, 4, 4, 3, 4, 7, 5,
                                         4, 4, 0, 1, 2, 0x77 },
                           new short[]  { 0x00, 0x01, 0x02, 0x03, 0x11,
                                          0x04, 0x05, 0x21, 0x31, 0x06,
                                          0x12, 0x41, 0x51, 0x07, 0x61,
                                          0x71, 0x13, 0x22, 0x32, 0x81,
                                          0x08, 0x14, 0x42, 0x91, 0xa1,
                                          0xb1, 0xc1, 0x09, 0x23, 0x33,
                                          0x52, 0xf0, 0x15, 0x62, 0x72,
                                          0xd1, 0x0a, 0x16, 0x24, 0x34,
                                          0xe1, 0x25, 0xf1, 0x17, 0x18,
                                          0x19, 0x1a, 0x26, 0x27, 0x28,
                                          0x29, 0x2a, 0x35, 0x36, 0x37,
                                          0x38, 0x39, 0x3a, 0x43, 0x44,
                                          0x45, 0x46, 0x47, 0x48, 0x49,
                                          0x4a, 0x53, 0x54, 0x55, 0x56,
                                          0x57, 0x58, 0x59, 0x5a, 0x63,
                                          0x64, 0x65, 0x66, 0x67, 0x68,
                                          0x69, 0x6a, 0x73, 0x74, 0x75,
                                          0x76, 0x77, 0x78, 0x79, 0x7a,
                                          0x82, 0x83, 0x84, 0x85, 0x86,
                                          0x87, 0x88, 0x89, 0x8a, 0x92,
                                          0x93, 0x94, 0x95, 0x96, 0x97,
                                          0x98, 0x99, 0x9a, 0xa2, 0xa3,
                                          0xa4, 0xa5, 0xa6, 0xa7, 0xa8,
                                          0xa9, 0xaa, 0xb2, 0xb3, 0xb4,
                                          0xb5, 0xb6, 0xb7, 0xb8, 0xb9,
                                          0xba, 0xc2, 0xc3, 0xc4, 0xc5,
                                          0xc6, 0xc7, 0xc8, 0xc9, 0xca,
                                          0xd2, 0xd3, 0xd4, 0xd5, 0xd6,
                                          0xd7, 0xd8, 0xd9, 0xda, 0xe2,
                                          0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
                                          0xe8, 0xe9, 0xea, 0xf2, 0xf3,
                                          0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
                                          0xf9, 0xfa }, false);

  /**
   * The standard JPEG AC luminance Huffman table.
   */
  public static final JPEGHuffmanTable StdACLuminance =
      new JPEGHuffmanTable(new short[] { 0, 2, 1, 3, 3, 2, 4, 3, 5, 5,
                                         4, 4, 0, 0, 1, 0x7d },
                           new short[] { 0x01, 0x02, 0x03, 0x00, 0x04,
                                         0x11, 0x05, 0x12, 0x21, 0x31,
                                         0x41, 0x06, 0x13, 0x51, 0x61,
                                         0x07, 0x22, 0x71, 0x14, 0x32,
                                         0x81, 0x91, 0xa1, 0x08, 0x23,
                                         0x42, 0xb1, 0xc1, 0x15, 0x52,
                                         0xd1, 0xf0, 0x24, 0x33, 0x62,
                                         0x72, 0x82, 0x09, 0x0a, 0x16,
                                         0x17, 0x18, 0x19, 0x1a, 0x25,
                                         0x26, 0x27, 0x28, 0x29, 0x2a,
                                         0x34, 0x35, 0x36, 0x37, 0x38,
                                         0x39, 0x3a, 0x43, 0x44, 0x45,
                                         0x46, 0x47, 0x48, 0x49, 0x4a,
                                         0x53, 0x54, 0x55, 0x56, 0x57,
                                         0x58, 0x59, 0x5a, 0x63, 0x64,
                                         0x65, 0x66, 0x67, 0x68, 0x69,
                                         0x6a, 0x73, 0x74, 0x75, 0x76,
                                         0x77, 0x78, 0x79, 0x7a, 0x83,
                                         0x84, 0x85, 0x86, 0x87, 0x88,
                                         0x89, 0x8a, 0x92, 0x93, 0x94,
                                         0x95, 0x96, 0x97, 0x98, 0x99,
                                         0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
                                         0xa6, 0xa7, 0xa8, 0xa9, 0xaa,
                                         0xb2, 0xb3, 0xb4, 0xb5, 0xb6,
                                         0xb7, 0xb8, 0xb9, 0xba, 0xc2,
                                         0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
                                         0xc8, 0xc9, 0xca, 0xd2, 0xd3,
                                         0xd4, 0xd5, 0xd6, 0xd7, 0xd8,
                                         0xd9, 0xda, 0xe1, 0xe2, 0xe3,
                                         0xe4, 0xe5, 0xe6, 0xe7, 0xe8,
                                         0xe9, 0xea, 0xf1, 0xf2, 0xf3,
                                         0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
                                         0xf9, 0xfa }, false);

  /**
   * The standard JPEG DC chrominance Huffman table.
   */
  public static final JPEGHuffmanTable StdDCChrominance =
      new JPEGHuffmanTable(new short[] { 0, 3, 1, 1, 1, 1, 1, 1, 1, 1,
                                         1, 0, 0, 0, 0, 0 },
                           new short[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                         10, 11 }, false);

  /**
   * The standard JPEG DC luminance Huffman table.
   */
  public static final JPEGHuffmanTable StdDCLuminance =
      new JPEGHuffmanTable(new short[] { 0, 1, 5, 1, 1, 1, 1, 1, 1, 0,
                                         0, 0, 0, 0, 0, 0 },
                           new short[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                         10, 11 }, false);

  /**
   * Construct and initialize a Huffman table. Copies are created of
   * the array arguments. lengths[index] stores the number of Huffman
   * values with Huffman codes of length index + 1. The values array
   * stores the Huffman values in order of increasing code length.
   * 
   * @param lengths an array of Huffman code lengths
   * @param values a sorted array of Huffman values
   * @throws IllegalArgumentException if either parameter is null, if
   * lengths.length > 16 or values.length > 256, if any value in
   * length or values is negative, or if the parameters do not
   * describe a valid Huffman table
   */
  public JPEGHuffmanTable(short[] lengths, short[] values)
  {
    // Create copies of the lengths and values arguments.
    this(checkLengths(lengths), checkValues(values, lengths), true);
  }

  /**
   * Private constructor that avoids unnecessary copying and argument
   * checking.
   *
   * @param lengths an array of Huffman code lengths
   * @param values a sorted array of Huffman values
   * @param copy true if copies should be created of the given arrays
   */
  private JPEGHuffmanTable(short[] lengths, short[] values, boolean copy)
  {
    this.lengths = copy ? (short[]) lengths.clone() : lengths;
    this.values = copy ? (short[]) values.clone() : values;
  }

  private static short[] checkLengths(short[] lengths)
  {
    if (lengths == null || lengths.length > 16)
      throw new IllegalArgumentException("invalid length array");

    for (int i = 0; i < lengths.length; i++)
      {
        if (lengths[i] < 0)
          throw new IllegalArgumentException("negative length");
      }

    int sum = 0;
    for (int i = 0; i < lengths.length; i++)
      {
        if (lengths[i] > ((1 << (i + 1)) - 1))
          throw new IllegalArgumentException("invalid number of codes"
                                             + " for code length " + (i + 1));
        sum += lengths[i];
      }

    return lengths;
  }

  private static short[] checkValues(short[] values, short[] lengths)
  {
    if (values == null || values.length > 256)
      throw new IllegalArgumentException("invalid values array");

    for (int i = 0; i < values.length; i++)
      {
        if (values[i] < 0)
          throw new IllegalArgumentException("negative value");
      }
    // lengths is known-valid by this point.
    int sum = 0;
    for (int i = 0; i < lengths.length; i++)
      sum += lengths[i];

    if (values.length != sum)
      throw new IllegalArgumentException("invalid number of values"
                                         + " for number of codes");

    return values;
  }

  /**
   * Retrieve a copy of the array of Huffman code lengths.  If the
   * returned array is called lengthcount, there are
   * lengthcount[index] codes of length index + 1.
   *
   * @return a copy of the array of Huffman code lengths
   */
  public short[] getLengths()
  {
    return (short[]) lengths.clone();
  }

  /**
   * Retrieve a copy of the array of Huffman values, sorted in order
   * of increasing code length.
   *
   * @return a copy of the array of Huffman values
   */
  public short[] getValues()
  {
    return (short[]) values.clone();
  }

  /**
   * Create a string representation of this JPEG Huffman table.
   *
   * @return a string representation of this JPEG Huffman table.
   */
  public String toString()
  {
    StringBuffer buffer = new StringBuffer();
    
    buffer.append("JPEGHuffmanTable:\nlengths:");
    
    for (int i = 0; i < lengths.length; i++)
      buffer.append(" " + lengths[i]);
    
    buffer.append("\nvalues:");
    
    for (int i = 0; i < values.length; i++)
      buffer.append(" " + values[i]);
    
    return buffer.toString();
  }
}
