/* JPEGImageReadParam.java --
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

import javax.imageio.ImageReadParam;

/**
 * The JPEGImageReadParam class is only used to set JPEG decoding
 * tables for streams that do not provide their own tables.  If a
 * stream does not provide tables and a custom JPEGImageReadParam is
 * not provided, then the standard JPEG tables are used from the
 * JPEGQTable and JPEGHuffmanTable classes.  If a stream does provide
 * decoding tables then JPEGImageReadParam will be ignored.
 * JPEGImageReadParam cannot be used to retrieve the tables from a
 * stream.  Instead, use IIOMetadata for this purpose.
 *
 * A JPEGImageReadParam instance is retrieved from the built-in JPEG
 * ImageReader using the getDefaultImageReadParam method.
 */
public class JPEGImageReadParam
  extends ImageReadParam
{
  private JPEGQTable[] qTables;
  private JPEGHuffmanTable[] DCHuffmanTables;
  private JPEGHuffmanTable[] ACHuffmanTables;

  /**
   * Construct a JPEGImageReadParam.
   */
  public JPEGImageReadParam()
  {
    super();
  }

  /**
   * Check if the decoding tables are set.
   *
   * @return true if the decoding tables are set, false otherwise
   */
  public boolean areTablesSet()
  {
    // If qTables is not null then all tables are set.
    return (qTables != null);
  }

  /**
   * Set the quantization and Huffman tables that will be used to
   * decode the stream.  Copies are created of the array arguments.
   * The number of Huffman tables must be the same in both Huffman
   * table arrays.  No argument may be null and no array may be longer
   * than four elements.
   *
   * @param qTables JPEG quantization tables
   * @param DCHuffmanTables JPEG DC Huffman tables
   * @param ACHuffmanTables JPEG AC Huffman tables
   *
   * @throws IllegalArgumentException if any argument is null, if any
   * of the arrays are longer than four elements, or if the Huffman
   * table arrays do not have the same number of elements
   */
  public void setDecodeTables(JPEGQTable[] qTables,
                              JPEGHuffmanTable[] DCHuffmanTables,
                              JPEGHuffmanTable[] ACHuffmanTables)
  {
    if (qTables == null || DCHuffmanTables == null || ACHuffmanTables == null)
      throw new IllegalArgumentException("null argument");

    if (qTables.length > 4 || DCHuffmanTables.length > 4
        || ACHuffmanTables.length > 4)
      throw new IllegalArgumentException("argument has too many elements");

    if (DCHuffmanTables.length != ACHuffmanTables.length)
      throw new IllegalArgumentException("Huffman table arrays differ in length");

    // Do a shallow copy.  JPEGQTable's data is not directly
    // modifyable since JPEGQTable.getTable returns a copy.  Therefore
    // it is safe to have multiple references to a single JPEGQTable.
    // Likewise for JPEGHuffmanTable.
    this.qTables = (JPEGQTable[]) qTables.clone();
    this.DCHuffmanTables = (JPEGHuffmanTable[]) DCHuffmanTables.clone();
    this.ACHuffmanTables = (JPEGHuffmanTable[]) ACHuffmanTables.clone();
  }

  /**
   * Clear the quantization and Huffman decoding tables.
   */
  public void unsetDecodeTables()
  {
    qTables = null;
    DCHuffmanTables = null;
    ACHuffmanTables = null;
  }

  /**
   * Retrieve the quantization tables.
   *
   * @returns an array of JPEG quantization tables
   */
  public JPEGQTable[] getQTables()
  {
    return qTables == null ? qTables : (JPEGQTable[]) qTables.clone();
  }

  /**
   * Retrieve the DC Huffman tables.
   *
   * @return an array of JPEG DC Huffman tables
   */
  public JPEGHuffmanTable[] getDCHuffmanTables()
  {
    return DCHuffmanTables == null ? DCHuffmanTables
      : (JPEGHuffmanTable[]) DCHuffmanTables.clone();
  }

  /**
   * Retrieve the AC Huffman tables.
   *
   * @return an array of JPEG AC Huffman tables
   */
  public JPEGHuffmanTable[] getACHuffmanTables()
  {
    return ACHuffmanTables == null ? ACHuffmanTables
      : (JPEGHuffmanTable[]) ACHuffmanTables.clone();
  }
}
