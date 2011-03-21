/* JPEGComponent.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

import java.util.ArrayList;
import java.io.IOException;
import java.awt.image.WritableRaster;

import javax.imageio.plugins.jpeg.JPEGHuffmanTable;

/**
 * This class holds the methods to decode and write a component information to
 * a raster.
 */
public class JPEGComponent
{
  public byte factorH, factorV, component_id, quant_id;
  public int width = 0, height = 0, maxV = 0, maxH = 0;
  public HuffmanTable ACTable;
  public HuffmanTable DCTable;
  public int[] quantizationTable;
  public double previousDC = 0;
  ArrayList data = new ArrayList();

  /**
   * Initializes the component
   *
   * @param id
   * @param factorHorizontal
   * @param factorVertical
   * @param quantizationID
   */
  public JPEGComponent(byte id, byte factorHorizontal, byte factorVertical,
                       byte quantizationID)
  {
    component_id = id;
    factorH = factorHorizontal;
    factorV = factorVertical;
    quant_id = quantizationID;
  }

  /**
   * If a restart marker is found with too little of an MCU count (i.e. our
   * Restart Interval is 63 and we have 61 we copy the last MCU until it's
   * full)
   *
   * @param index
   * @param length
   */
  public void padMCU(int index, int length)
  {
    double[] src = (double[]) data.get(index - 1);
    for (int i = 0; i < length; i++)
      data.add(index, src);
  }

  /**
   * Reset the interval by setting the previous DC value
   */
  public void resetInterval()
  {
    previousDC = 0;
  }

  /**
   * Run the Quantization backward method on all of the block data.
   */
  public void quantitizeData()
  {
    for (int i = 0; i < data.size(); i++)
      {
        double[] mydata = (double[]) data.get(i);
        for (int j = 0; j < mydata.length; j++)
          mydata[j] *= quantizationTable[j];
      }
  }

  public void setDCTable(JPEGHuffmanTable table)
  {
    DCTable = new HuffmanTable(table);
  }

  public void setACTable(JPEGHuffmanTable table)
  {
    ACTable = new HuffmanTable(table);
  }

  /**
   * Run the Inverse DCT method on all of the block data
   */
  public void idctData(DCT myDCT)
  {
    for (int i = 0; i < data.size(); i++)
      data.add(i,myDCT.fast_idct(ZigZag.decode8x8_map((double[]) data.remove(i))));
  }

  /**
   * This scales up the component size based on the factor size. This
   * calculates everyting up automatically so it's simply ran at the end of
   * the frame to normalize the size of all of the components.
   */
  public void scaleByFactors()
  {
    int factorUpVertical = maxV / factorV;
    int factorUpHorizontal = maxH / factorH;

    if (factorUpVertical > 1)
      {
        for (int i = 0; i < data.size(); i++)
          {
            double[][] src = (double[][]) data.remove(i);
            double[][] dest =
              new double[src.length * factorUpVertical][src[0].length];
            for (int j = 0; j < src.length; j++)
              {
                for (int u = 0; u < factorUpVertical; u++)
                  {
                    dest[j * factorUpVertical + u] = src[j];
                  }
              }
            data.add(i, dest);
          }
      }

    if (factorUpHorizontal > 1)
      {
        for (int i = 0; i < data.size(); i++)
          {
            double[][] src = (double[][]) data.remove(i);
            double[][] dest =
              new double[src.length][src[0].length * factorUpHorizontal];
            for (int j = 0; j < src.length; j++)
              {
                for (int u = 0; u < src[0].length; u++)
                  {
                    for (int v = 0; v < factorUpHorizontal; v++)
                      dest[j][u * factorUpHorizontal + v] = src[j][u];
                  }
              }
            data.add(i, dest);
          }
      }
  }

  /**
   * This write the block of data to the raster throwing out anything that
   * spills over the raster width or height.
   *
   * @param raster
   * @param data
   * @param compIndex
   * @param x
   * @param y
   */
  public void writeBlock(WritableRaster raster, double[][] data,
                         int compIndex, int x, int y)
  {
    for (int yIndex = 0; yIndex < data.length; yIndex++)
      {
        for (int xIndex = 0; xIndex < data[yIndex].length; xIndex++)
          {
            // The if statement is needed because blocks can spill over the
            // frame width because they are padded to make sure we keep the
            // height of the block the same as the width of the block
            if (x + xIndex < raster.getWidth()
                && y + yIndex < raster.getHeight())
              raster.setSample(x + xIndex, y + yIndex, compIndex,
                               data[yIndex][xIndex]);
          }
      }
  }

  /**
   * This writes data to a raster block, so really it's reading not writing
   * but it writes the data to the raster block by factor size in a zig zag
   * fashion. This has the helper function writeBlock which does the actual
   * writing.
   *
   * @param raster
   * @param componentIndex
   */
  public void writeData(WritableRaster raster, int componentIndex)
  {
    int x = 0, y = 0, lastblockheight = 0, incrementblock = 0;

    // Keep looping through all of the blocks until there are no more.
    while(data.size() > 0)
      {
        int blockwidth = 0;
        int blockheight = 0;

        if (x >= raster.getWidth())
          {
            x = 0;
            y += incrementblock;
          }

        // Loop through the horizontal component blocks of the MCU first
        // then for each horizontal line write out all of the vertical
        // components
        for (int factorVIndex = 0; factorVIndex < factorV; factorVIndex++)
          {
            blockwidth = 0;

            for (int factorHIndex = 0; factorHIndex < factorH; factorHIndex++)
              {
                // Captures the width of this block so we can increment the
                // X coordinate
                double[][] blockdata = (double[][]) data.remove(0);

                // Writes the data at the specific X and Y coordinate of
                // this component
                writeBlock(raster, blockdata, componentIndex, x, y);
                blockwidth += blockdata[0].length;
                x += blockdata[0].length;
                blockheight = blockdata.length;
              }
            y += blockheight;
            x -= blockwidth;
            lastblockheight += blockheight;
          }
        y -= lastblockheight;
        incrementblock = lastblockheight;
        lastblockheight = 0;
        x += blockwidth;
      }
  }

  /**
   * Set the quantization table for this component.
   *
   * @param quanttable
   */
  public void setQuantizationTable(int[] quanttable)
  {
    quantizationTable = quanttable;
  }

  /**
   * Read in a partial MCU for this component
   *
   * @param stream TODO
   * @throws JPEGException TODO
   * @throws IOException TODO
   */
  public void readComponentMCU(JPEGImageInputStream stream)
    throws JPEGException, IOException
  {
    for (int i = 0; i < factorH * factorV; i++)
      {
        double dc = decode_dc_coefficient(stream);
        double[] datablock = decode_ac_coefficients(stream);
        datablock[0] = dc;
        data.add(datablock);
      }
  }

  /**
   * Generated from text on F-22, F.2.2.1 - Huffman decoding of DC
   * coefficients on ISO DIS 10918-1. Requirements and Guidelines.
   *
   * @param JPEGStream TODO
   *
   * @return TODO
   * @throws JPEGException TODO
   * @throws IOException TODO
   */
  public double decode_dc_coefficient(JPEGImageInputStream JPEGStream)
        throws JPEGException, IOException
  {
    int t = DCTable.decode(JPEGStream);
    double diff = JPEGStream.readBits(t);
    diff = HuffmanTable.extend((int) diff, t);
    diff = (previousDC + diff);
    previousDC = diff;
    return diff;
  }

  /**
   * Generated from text on F-23, F.13 - Huffman decoded of AC coefficients
   * on ISO DIS 10918-1. Requirements and Guidelines.
   *
   * @param JPEGStream TODO
   * @return TODO
   *
   * @throws JPEGException TODO
   * @throws IOException TODO
   */
  public double[] decode_ac_coefficients(JPEGImageInputStream JPEGStream)
    throws JPEGException, IOException
  {
    double[] zz = new double[64];

    for (int k = 1; k < 64; k++)
      {
        int s = ACTable.decode(JPEGStream);
        int r = s >> 4;
        s &= 15;

        if (s != 0)
          {
            k += r;
            r = (int) JPEGStream.readBits(s);
            s = HuffmanTable.extend(r, s);
            zz[k] = s;
          }
        else
          {
            if (r != 15)
              return (zz);
            k += 15;
          }
      }
    return zz;
  }
}
