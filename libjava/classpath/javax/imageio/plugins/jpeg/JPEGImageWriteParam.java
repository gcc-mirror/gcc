/* JPEGImageWriteParam.java --
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

import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import javax.imageio.ImageWriteParam;

/**
 * The JPEGImageWriteParam class can be used to specify tables and
 * settings used in the JPEG encoding process.  Encoding tables are
 * taken from the metadata associated with the output stream, and
 * failing that (if the metadata tables are null) from an instance of
 * JPEGImageWriteParam.  The default metadata uses the standard JPEG
 * tables from the JPEGQTable and JPEGHuffmanTable classes.  Non-null
 * metadata tables override JPEGImageWriteParam tables.  Compression
 * settings range from 1.0, best compression, through 0.75, default
 * compression, to 0.0, worst compression.
 *
 * A JPEGImageWriteParam instance is retrieved from the built-in JPEG
 * ImageWriter using the getDefaultImageWriteParam method.
 */
public class JPEGImageWriteParam
  extends ImageWriteParam
{
  private JPEGQTable[] qTables;
  private JPEGHuffmanTable[] DCHuffmanTables;
  private JPEGHuffmanTable[] ACHuffmanTables;
  private boolean optimize;
  private String[] compressionQualityDescriptions;
  private float[] compressionQualityValues;

  /**
   * Localized messages are stored in separate files.
   */
  private ResourceBundle messages;

  /**
   * Construct a JPEGImageWriteParam with the following state: tiling
   * is not supported, progressive mode is supported, initial
   * progressive mode is MODE_DISABLED, compression is supported, one
   * compression type named "JPEG" is supported and the default
   * compression quality is 0.75f.  Compression type names and
   * compression quality descriptions are localized to the given
   * locale.
   *
   * @param locale the locale used for message localization
   */
  public JPEGImageWriteParam(Locale locale)
  {
    super(locale);

    // Get localized compression type and compression quality
    // description strings for the given locale.
    messages = PropertyResourceBundle.getBundle
      ("javax/imageio/plugins/jpeg/MessagesBundle", locale);

    // Initialize inherited ImageWriter fields.
    canWriteTiles = false;
    canWriteProgressive = true;
    progressiveMode = MODE_DISABLED;
    canWriteCompressed = true;
    compressionTypes = new String[]
      {
        messages.getString("compression.types.jpeg")
      };
    compressionType = compressionTypes[0];
    compressionQuality = 0.75f;
  }

  /**
   * Reset the compression quality to 0.75f.
   */
  public void unsetCompression()
  {
    compressionQuality = 0.75f;
  }

  /**
   * Check if compression algorithm is lossless.  JPEGImageWriteParam
   * overrides this ImageWriteParam method to always return false
   * since JPEG compression is inherently lossy.
   *
   * @return false
   */
  public boolean isCompressionLossless()
  {
    return false;
  }

  /**
   * Retrieve an array of compression quality descriptions.  These
   * messages are localized using the locale provided upon
   * construction.  Each compression quality description in the
   * returned array corresponds to the compression quality value at
   * the same index in the array returned by
   * getCompressionQualityValues.
   *
   * @return an array of strings each of which describes a compression
   * quality value
   */
  public String[] getCompressionQualityDescriptions()
  {
    // Make sure exceptions are thrown when this image write param is
    // in the wrong state.
    super.getCompressionQualityDescriptions();

    if (compressionQualityDescriptions == null)
      {
        compressionQualityDescriptions = new String[]
          {
            messages.getString("compression.minimum"),
            messages.getString("compression.default"),
            messages.getString("compression.maximum")
          };
      }

    return compressionQualityDescriptions;
  }

  /**
   * Retrieve an array of compression quality values, ordered from
   * lowest quality to highest quality.
   *
   * @return an array of compressions quality values
   */
  public float[] getCompressionQualityValues()
  {
    // Make sure exceptions are thrown when this image write param is
    // in the wrong state.
    super.getCompressionQualityValues();

    if (compressionQualityValues == null)
      compressionQualityValues = new float[] { 0.05f, 0.75f, 0.95f };

    return compressionQualityValues;
  }

  /**
   * Check if the encoding tables are set.
   *
   * @return true if the encoding tables are set, false otherwise
   */
  public boolean areTablesSet()
  {
    // If qTables is not null then all tables are set.
    return (qTables != null);
  }

  /**
   * Set the quantization and Huffman tables that will be used to
   * encode the stream.  Copies are created of the array arguments.
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
  public void setEncodeTables(JPEGQTable[] qTables,
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
   * Clear the quantization and Huffman encoding tables.
   */
  public void unsetEncodeTables()
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

  /**
   * Specify whether or not Huffman tables written to the output
   * stream should be optimized.  Every image encoded with this flag
   * set will contain a Huffman table, and the generated Huffman
   * tables will override those specified in the metadata.
   *
   * @param optimize true to generate optimized Huffman tables, false
   * otherwise
   */
  public void setOptimizeHuffmanTables(boolean optimize)
  {
    this.optimize = optimize;
  }

  /**
   * Check whether or not Huffman tables written to the output stream
   * will be optimized.  Unless otherwise set using
   * setOptimizeHuffmanTables, this returns false.
   *
   * @return true Huffman tables written to the output stream will be
   * optimized, false otherwise
   */
  public boolean getOptimizeHuffmanTables()
  {
    return optimize;
  }
}
