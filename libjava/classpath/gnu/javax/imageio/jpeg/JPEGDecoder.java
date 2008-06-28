/* JPEGDecoder.java --
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
import java.nio.ByteOrder;

import javax.imageio.plugins.jpeg.JPEGHuffmanTable;
import javax.imageio.plugins.jpeg.JPEGQTable;
import javax.imageio.stream.ImageInputStream;

import java.util.ArrayList;
import java.util.Hashtable;
import java.awt.Point;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

public class JPEGDecoder
{
  byte majorVersion;
  byte minorVersion;
  byte units;
  short Xdensity;
  short Ydensity;
  byte Xthumbnail;
  byte Ythumbnail;
  byte[] thumbnail;
  BufferedImage image;
  int width;
  int height;
  
  byte marker;

  /**
   * This decoder expects JFIF 1.02 encoding.
   */
  public static final byte MAJOR_VERSION = (byte) 1;
  public static final byte MINOR_VERSION = (byte) 2;

  /**
   * The length of the JFIF field not including thumbnail data.
   */
  public static final short JFIF_FIXED_LENGTH = 16;

  /**
   * The length of the JFIF extension field not including extension
   * data.
   */
  public static final short JFXX_FIXED_LENGTH = 8;

  private JPEGImageInputStream jpegStream;

  ArrayList jpegFrames = new ArrayList();

  JPEGHuffmanTable[] dcTables = new JPEGHuffmanTable[4];
  JPEGHuffmanTable[] acTables = new JPEGHuffmanTable[4];
  JPEGQTable[] qTables = new JPEGQTable[4];

    public int getHeight()
    {
      return height;
    }
    
    public int getWidth()
    {
        return width;
    }
  public JPEGDecoder(ImageInputStream in)
    throws IOException, JPEGException
  {
    jpegStream = new JPEGImageInputStream(in);
    jpegStream.setByteOrder(ByteOrder.LITTLE_ENDIAN);

    if (jpegStream.findNextMarker() != JPEGMarker.SOI)
      throw new JPEGException("Failed to find SOI marker.");

    if (jpegStream.findNextMarker() != JPEGMarker.APP0)
      throw new JPEGException("Failed to find APP0 marker.");

    int length = jpegStream.readShort();
    if (!(length >= JFIF_FIXED_LENGTH))
      throw new JPEGException("Failed to find JFIF field.");

    byte[] identifier = new byte[5];
    jpegStream.read(identifier);
    if (identifier[0] != JPEGMarker.JFIF_J
        || identifier[1] != JPEGMarker.JFIF_F
        || identifier[2] != JPEGMarker.JFIF_I
        || identifier[3] != JPEGMarker.JFIF_F
        || identifier[4] != JPEGMarker.X00)
      throw new JPEGException("Failed to read JFIF identifier.");

    majorVersion = jpegStream.readByte();
    minorVersion = jpegStream.readByte();
    if (majorVersion != MAJOR_VERSION
        || (majorVersion == MAJOR_VERSION
            && minorVersion < MINOR_VERSION))
      throw new JPEGException("Unsupported JFIF version.");

    units = jpegStream.readByte();
    if (units > (byte) 2)
      throw new JPEGException("Units field is out of range.");

    Xdensity = jpegStream.readShort();
    Ydensity = jpegStream.readShort();
    Xthumbnail = jpegStream.readByte();
    Ythumbnail = jpegStream.readByte();

    // 3 * for RGB data
    int thumbnailLength = 3 * Xthumbnail * Ythumbnail;
    if (length > JFIF_FIXED_LENGTH
        && thumbnailLength != length - JFIF_FIXED_LENGTH)
      throw new JPEGException("Invalid length, Xthumbnail"
                              + " or Ythumbnail field.");

    if (thumbnailLength > 0)
      {
        thumbnail = new byte[thumbnailLength];
        if (jpegStream.read(thumbnail) != thumbnailLength)
          throw new IOException("Failed to read thumbnail.");
      }
  }

  public void decode()
    throws IOException
  {
    System.out.println ("DECODE!!!");
    // The frames in this jpeg are loaded into a list. There is
    // usually just one frame except in heirarchial progression where
    // there are multiple frames.
    JPEGFrame frame = null;

    // The restart interval defines how many MCU's we should have
    // between the 8-modulo restart marker. The restart markers allow
    // us to tell whether or not our decoding process is working
    // correctly, also if there is corruption in the image we can
    // recover with these restart intervals. (See RSTm DRI).
    int resetInterval = 0;

    // The JPEGDecoder constructor parses the JFIF field.  At this
    // point jpegStream points to the first byte after the JFIF field.

    // Find the first marker after the JFIF field.
    byte marker = jpegStream.findNextMarker();

    // Check for a JFIF extension field directly following the JFIF
    // header and advance the current marker to the next marker in the
    // stream, if necessary.
    decodeJFIFExtension();

    // Loop through until there are no more markers to read in, at
    // that point everything is loaded into the jpegFrames array and
    // can be processed.
    while (true)
      {
        switch (marker)
          {
            // APPn Application Reserved Information - Just throw this
            // information away because we wont be using it.
          case JPEGMarker.APP0:
          case JPEGMarker.APP1:
          case JPEGMarker.APP2:
          case JPEGMarker.APP3:
          case JPEGMarker.APP4:
          case JPEGMarker.APP5:
          case JPEGMarker.APP6:
          case JPEGMarker.APP7:
          case JPEGMarker.APP8:
          case JPEGMarker.APP9:
          case JPEGMarker.APP10:
          case JPEGMarker.APP11:
          case JPEGMarker.APP12:
          case JPEGMarker.APP13:
          case JPEGMarker.APP14:
          case JPEGMarker.APP15:
            jpegStream.skipBytes(jpegStream.readShort() - 2);
            break;

          case JPEGMarker.SOF0:
            // SOFn Start of Frame Marker, Baseline DCT - This is the start
            // of the frame header that defines certain variables that will
            // be carried out through the rest of the encoding. Multiple
            // frames are used in a heirarchiel system, however most JPEG's
            // only contain a single frame.
            jpegFrames.add(new JPEGFrame());
            frame = (JPEGFrame) jpegFrames.get(jpegFrames.size() - 1);
            // Skip the frame length.
            jpegStream.readShort();
            // Bits percision, either 8 or 12.
            frame.setPrecision(jpegStream.readByte());
            // Scan lines = to the height of the frame.
            frame.setScanLines(jpegStream.readShort());
            // Scan samples per line = to the width of the frame.
            frame.setSamplesPerLine(jpegStream.readShort());
            // Number of Color Components (or channels).
            frame.setComponentCount(jpegStream.readByte());

            // Set the color mode for this frame, so far only 2 color
            // modes are supported.
            if (frame.getComponentCount() == 1)
              frame.setColorMode(JPEGFrame.JPEG_COLOR_GRAY);
            else
              frame.setColorMode(JPEGFrame.JPEG_COLOR_YCbCr);
            // Add all of the necessary components to the frame.
            for (int i = 0; i < frame.getComponentCount(); i++)
              frame.addComponent(jpegStream.readByte(), jpegStream.readByte(),
                                 jpegStream.readByte());
            break;

          case JPEGMarker.SOF2:
            jpegFrames.add(new JPEGFrame());
            frame = (JPEGFrame) jpegFrames.get(jpegFrames.size() - 1);
            // Skip the frame length.
            jpegStream.readShort();
            // Bits percision, either 8 or 12.
            frame.setPrecision(jpegStream.readByte());
            // Scan lines = to the height of the frame.
            frame.setScanLines(jpegStream.readShort());
            // Scan samples per line = to the width of the frame.
            frame.setSamplesPerLine(jpegStream.readShort());
            // Number of Color Components (or channels).
            frame.setComponentCount(jpegStream.readByte());

            // Set the color mode for this frame, so far only 2 color
            // modes are supported.
            if (frame.getComponentCount() == 1)
              frame.setColorMode(JPEGFrame.JPEG_COLOR_GRAY);
            else
              frame.setColorMode(JPEGFrame.JPEG_COLOR_YCbCr);

            // Add all of the necessary components to the frame.
            for (int i = 0; i < frame.getComponentCount(); i++)
              frame.addComponent(jpegStream.readByte(), jpegStream.readByte(),
                                 jpegStream.readByte());
            break;

          case JPEGMarker.DHT:
            // DHT non-SOF Marker - Huffman Table is required for decoding
            // the JPEG stream, when we receive a marker we load in first
            // the table length (16 bits), the table class (4 bits), table
            // identifier (4 bits), then we load in 16 bytes and each byte
            // represents the count of bytes to load in for each of the 16
            // bytes. We load this into an array to use later and move on 4
            // huffman tables can only be used in an image.
            int huffmanLength = (jpegStream.readShort() - 2);

            // Keep looping until we are out of length.
            int index = huffmanLength;

            // Multiple tables may be defined within a DHT marker. This
            // will keep reading until there are no tables left, most
            // of the time there are just one tables.
            while (index > 0)
              {
                // Read the identifier information and class
                // information about the Huffman table, then read the
                // 16 byte codelength in and read in the Huffman values
                // and put it into table info.
                byte huffmanInfo = jpegStream.readByte();
                byte tableClass = (byte) (huffmanInfo >> 4);
                byte huffmanIndex = (byte) (huffmanInfo & 0x0f);
                short[] codeLength = new short[16];
                jpegStream.readFully(codeLength, 0, codeLength.length);
                int huffmanValueLen = 0;
                for (int i = 0; i < 16; i++)
                  huffmanValueLen += codeLength[i];
                index -= (huffmanValueLen + 17);
                short[] huffmanVal = new short[huffmanValueLen];
                for (int i = 0; i < huffmanVal.length; i++)
                  huffmanVal[i] = jpegStream.readByte();
                // Assign DC Huffman Table.
                if (tableClass == HuffmanTable.JPEG_DC_TABLE)
                  dcTables[(int) huffmanIndex] = new JPEGHuffmanTable(codeLength,
                                                                      huffmanVal);
                // Assign AC Huffman Table.
                else if (tableClass == HuffmanTable.JPEG_AC_TABLE)
                  acTables[(int) huffmanIndex] = new JPEGHuffmanTable(codeLength,
                                                                      huffmanVal);
              }
            break;
          case JPEGMarker.DQT:
            // DQT non-SOF Marker - This defines the quantization
            // coeffecients, this allows us to figure out the quality of
            // compression and unencode the data. The data is loaded and
            // then stored in to an array.
            short quantizationLength = (short) (jpegStream.readShort() - 2);
            for (int j = 0; j < quantizationLength / 65; j++)
              {
                byte quantSpecs = jpegStream.readByte();
                int[] quantData = new int[64];
                if ((byte) (quantSpecs >> 4) == 0)
                  // Precision 8 bit.
                  {
                    for (int i = 0; i < 64; i++)
                      quantData[i] = jpegStream.readByte();
                  
                  }
                else if ((byte) (quantSpecs >> 4) == 1)
                  // Precision 16 bit.
                  {
                    for (int i = 0; i < 64; i++)
                      quantData[i] = jpegStream.readShort();
                  }
                qTables[(int) (quantSpecs & 0x0f)] = new JPEGQTable (quantData);
              }
            break;
          case JPEGMarker.SOS:
            // SOS non-SOF Marker - Start Of Scan Marker, this is where the
            // actual data is stored in a interlaced or non-interlaced with
            // from 1-4 components of color data, if three components most
            // likely a YCrCb model, this is a fairly complex process.

            // Read in the scan length.
            jpegStream.readShort();
            // Number of components in the scan.
            byte numberOfComponents = jpegStream.readByte();
            byte[] componentSelector = new byte[numberOfComponents];
            for (int i = 0; i < numberOfComponents; i++)
              {
                // Component ID, packed byte containing the Id for the
                // AC table and DC table.
                byte componentID = jpegStream.readByte();
                byte tableInfo = jpegStream.readByte();
                frame.setHuffmanTables(componentID,
                                       acTables[(byte) (tableInfo >> 4)],
                                       dcTables[(byte) (tableInfo & 0x0f)]);
                componentSelector[i] = componentID;
              }
            byte startSpectralSelection = jpegStream.readByte();
            byte endSpectralSelection = jpegStream.readByte();
            byte successiveApproximation = jpegStream.readByte();

            int mcuIndex = 0; 
            int mcuTotalIndex = 0;
            // This loops through until a MarkerTagFound exception is
            // found, if the marker tag is a RST (Restart Marker) it
            // simply skips it and moves on this system does not handle
            // corrupt data streams very well, it could be improved by
            // handling misplaced restart markers.
            while (true)
              {
                try
                  {
                    // Loop though capturing MCU, instruct each
                    // component to read in its necessary count, for
                    // scaling factors the components automatically
                    // read in how much they need
                    for (int compIndex = 0; compIndex < numberOfComponents; compIndex++)
                      {
                        JPEGComponent comp = frame.components.getComponentByID(componentSelector[compIndex]);
                        comp.readComponentMCU(jpegStream);
                      }
                    mcuIndex++;
                    mcuTotalIndex++;
                  }
                // We've found a marker, see if the marker is a restart
                // marker or just the next marker in the stream. If
                // it's the next marker in the stream break out of the
                // while loop, if it's just a restart marker skip it
                catch (JPEGMarkerFoundException bse)
                  {
                    // Handle JPEG Restart Markers, this is where the
                    // count of MCU's per interval is compared with
                    // the count actually obtained, if it's short then
                    // pad on some MCU's ONLY for components that are
                    // greater than one. Also restart the DC prediction
                    // to zero.
                    if (marker == JPEGMarker.RST0
                        || marker == JPEGMarker.RST1
                        || marker == JPEGMarker.RST2
                        || marker == JPEGMarker.RST3
                        || marker == JPEGMarker.RST4
                        || marker == JPEGMarker.RST5
                        || marker == JPEGMarker.RST6
                        || marker == JPEGMarker.RST7)
                      {
                        for (int compIndex = 0; compIndex < numberOfComponents; compIndex++)
                          {
                            JPEGComponent comp = frame.components.getComponentByID(componentSelector[compIndex]);
                            if (compIndex > 1)
                              comp.padMCU(mcuTotalIndex, resetInterval - mcuIndex);
                            comp.resetInterval();
                          }
                        mcuTotalIndex += (resetInterval - mcuIndex);
                        mcuIndex = 0;
                      }
                    else
                      {
                        // We're at the end of our scan, exit out.
                        break;
                      }
                  }
              }
            break;
          case JPEGMarker.DRI:
            // DRI - This defines the restart interval, if we have a
            // restart interval when we reach our restart modulo calculate
            // whether the count of MCU's specified in the restart
            // interval have been reached, if they havent then pad with
            // whatever MCU was last used, this is supposed to be a form of
            // error recovery but it turns out that some JPEG encoders
            // purposely cause missing MCU's on repeating MCU's to compress
            // data even more (even though it adds an extra layer of
            // complexity.. But since when is JPEG easy?
            jpegStream.skipBytes(2);
            resetInterval = jpegStream.readShort();
            break;
          case JPEGMarker.COM:
            // COM - This is a comment that was inserted into the JPEG, we
            // simply skip over the comment because it's really of no
            // importance, usually contains a verbal description of the
            // application or author who created the JPEG.
            jpegStream.skipBytes(jpegStream.readShort() - 2);
            break;
          case JPEGMarker.DNL:
            // DNL - This sets the height of the image. This is the Define
            // Number Lines for the image, I'm not sure exactly why we need
            // this but, whatever we'll abide.
            frame.setScanLines(jpegStream.readShort());
            break;
          case JPEGMarker.EOI:
            // EOI - End of Image, this processes the frames and turns the
            // frames into a buffered image.

            if (jpegFrames.size() == 0)
              {
                return;
              }
            else if (jpegFrames.size() == 1)
              {
                // Only one frame, JPEG Non-Heirarchial Frame.

                DCT myDCT = new DCT();
                WritableRaster raster =
                  Raster.createInterleavedRaster(DataBuffer.TYPE_BYTE,
                                                 frame.width,
                                                 frame.height,
                                                 frame.getComponentCount(),
                                                 new Point(0, 0));

                // Unencode the data.
                for (int i = 0; i < frame.getComponentCount(); i++)
                  {
                    JPEGComponent comp = frame.components.get(i);
                    comp.setQuantizationTable(qTables[comp.quant_id].getTable());
                    comp.quantitizeData();
                    comp.idctData(myDCT);
                  }
                // Scale the image and write the data to the raster.
                for (int i = 0; i < frame.getComponentCount(); i++)
                  {
                    JPEGComponent comp = frame.components.get(i);
                    comp.scaleByFactors();
                    comp.writeData(raster, i);
                    // Ensure garbage collection.
                    comp = null;
                  }
                // Grayscale Color Image (1 Component).
                if (frame.getComponentCount() == 1)
                  {
                    ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_GRAY);
                    ComponentColorModel ccm =
                      new ComponentColorModel(cs, false, false,
                                              Transparency.OPAQUE,
                                              DataBuffer.TYPE_BYTE);
                    image = new BufferedImage(ccm, raster, false,
                                              new Hashtable());
                  }
                // YCbCr Color Image (3 Components).
                else if (frame.getComponentCount() == 3)
                  {
                    ComponentColorModel ccm =
                      new ComponentColorModel(new YCbCr_ColorSpace(), false,
                                              false, Transparency.OPAQUE,
                                              DataBuffer.TYPE_BYTE);
                    image = new BufferedImage(ccm, raster, false,
                                              new Hashtable());
                  }
                // Possibly CMYK or RGBA ?
                else
                  {
                    throw new JPEGException("Unsupported Color Mode: 4 "
                                            + "Component Color Mode found.");
                  }
                height = frame.height;
                width = frame.width;
              }
            else
              {
                //JPEG Heirarchial Frame (progressive or baseline).
                throw new JPEGException("Unsupported Codec Type:"
                                        + " Hierarchial JPEG");
              }
            break;
          case JPEGMarker.SOF1:
            // ERROR - If we encounter any of the following marker codes
            // error out with a codec exception, progressive, heirarchial,
            // differential, arithmetic, lossless JPEG's are not supported.
            // This is where enhancements can be made for future versions.
            // Thankfully 99% of all JPEG's are baseline DCT.
            throw new JPEGException("Unsupported Codec Type: Extended "
                                    + "Sequential DCT JPEG's Not-Supported");
            //case JPEGMarker.SOF2:
            //	throw new JPEGException("Unsupported Codec Type: Progressive DCT JPEG's Not-Supported");
          case JPEGMarker.SOF3:
            throw new JPEGException("Unsupported Codec Type:"
                                    + " Lossless (sequential)");
          case JPEGMarker.SOF5:
            throw new JPEGException("Unsupported Codec Type:"
                                    + " Differential sequential DCT");
          case JPEGMarker.SOF6:
            throw new JPEGException("Unsupported Codec Type:"
                                    + " Differential progressive DCT");
          case JPEGMarker.SOF7:
            throw new JPEGException("Unsupported Codec Type:"
                                    + " Differential lossless");
          case JPEGMarker.SOF9:
          case JPEGMarker.SOF10:
          case JPEGMarker.SOF11:
          case JPEGMarker.SOF13:
          case JPEGMarker.SOF14:
          case JPEGMarker.SOF15:
            throw new JPEGException("Unsupported Codec Type:"
                                    + " Arithmetic Coding Frame");
          default:
            // Unknown marker found, ignore it.
          }
        marker = jpegStream.findNextMarker();
      }
  }

  // If the current marker is APP0, tries to decode a JFIF extension
  // and advances the current marker to the next marker in the stream.
  private void decodeJFIFExtension() throws IOException
  {
    if (marker == JPEGMarker.APP0)
      {
        int length = jpegStream.readShort();

        if (length >= JFXX_FIXED_LENGTH)
          {
            byte[] identifier = new byte[5];
            jpegStream.read(identifier);
            if (identifier[0] != JPEGMarker.JFIF_J
                || identifier[1] != JPEGMarker.JFIF_F
                || identifier[2] != JPEGMarker.JFIF_X
                || identifier[3] != JPEGMarker.JFIF_X
                || identifier[4] != JPEGMarker.X00)
              // Not a JFXX field.  Ignore it and continue.
              jpegStream.skipBytes(length - 7);
            else
              {
                byte extension_code = jpegStream.readByte();

                switch (extension_code)
                  {
                  case JPEGMarker.JFXX_JPEG:
                    // FIXME: add support for JFIF Extension:
                    // Thumbnail coded using JPEG.
                    jpegStream.skipBytes(length - 8);
                  case JPEGMarker.JFXX_ONE_BPP:
                    // FIXME: add support for JFIF Extension:
                    // Thumbnail stored using 1 byte/pixel.
                    jpegStream.skipBytes(length - 8);
                  case JPEGMarker.JFXX_THREE_BPP:
                    // FIXME: add support for JFIF Extension:
                    // Thumbnail stored using 3 bytes/pixel.
                    jpegStream.skipBytes(length - 8);
                  }
              }
          }
        else
          {
            // Unknown APP0 marker.  Ignore it and continue.
            jpegStream.skipBytes(length - 2);
          }
        marker = jpegStream.findNextMarker();
      }
  }

  public BufferedImage getImage()
  {
    return image;
  }
}
