/* ProfileHeader.java -- Encapsules ICC Profile header data
   Copyright (C) 2004 Free Software Foundation

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

package gnu.java.awt.color;

import java.awt.color.ColorSpace;
import java.awt.color.ICC_Profile;
import java.nio.ByteBuffer;


/**
 * Header, abstracts and validates the header data.
 *
 * @author Sven de Marothy
 */
public class ProfileHeader
{
  /**
   * Magic identifier (ASCII 'acsp')
   */
  private static final int icMagicNumber = 0x61637370;

  /**
   * Mapping from ICC Profile signatures to ColorSpace types
   */
  private static final int[] csTypeMap =
                                         {
                                           ICC_Profile.icSigXYZData,
                                           ColorSpace.TYPE_XYZ,
                                           ICC_Profile.icSigLabData,
                                           ColorSpace.TYPE_Lab,
                                           ICC_Profile.icSigLuvData,
                                           ColorSpace.TYPE_Luv,
                                           ICC_Profile.icSigYCbCrData,
                                           ColorSpace.TYPE_YCbCr,
                                           ICC_Profile.icSigYxyData,
                                           ColorSpace.TYPE_Yxy,
                                           ICC_Profile.icSigRgbData,
                                           ColorSpace.TYPE_RGB,
                                           ICC_Profile.icSigGrayData,
                                           ColorSpace.TYPE_GRAY,
                                           ICC_Profile.icSigHsvData,
                                           ColorSpace.TYPE_HSV,
                                           ICC_Profile.icSigHlsData,
                                           ColorSpace.TYPE_HLS,
                                           ICC_Profile.icSigCmykData,
                                           ColorSpace.TYPE_CMYK,
                                           ICC_Profile.icSigCmyData,
                                           ColorSpace.TYPE_CMY,
                                           ICC_Profile.icSigSpace2CLR,
                                           ColorSpace.TYPE_2CLR,
                                           ICC_Profile.icSigSpace3CLR,
                                           ColorSpace.TYPE_3CLR,
                                           ICC_Profile.icSigSpace4CLR,
                                           ColorSpace.TYPE_4CLR,
                                           ICC_Profile.icSigSpace5CLR,
                                           ColorSpace.TYPE_5CLR,
                                           ICC_Profile.icSigSpace6CLR,
                                           ColorSpace.TYPE_6CLR,
                                           ICC_Profile.icSigSpace7CLR,
                                           ColorSpace.TYPE_7CLR,
                                           ICC_Profile.icSigSpace8CLR,
                                           ColorSpace.TYPE_8CLR,
                                           ICC_Profile.icSigSpace9CLR,
                                           ColorSpace.TYPE_9CLR,
                                           ICC_Profile.icSigSpaceACLR,
                                           ColorSpace.TYPE_ACLR,
                                           ICC_Profile.icSigSpaceBCLR,
                                           ColorSpace.TYPE_BCLR,
                                           ICC_Profile.icSigSpaceCCLR,
                                           ColorSpace.TYPE_CCLR,
                                           ICC_Profile.icSigSpaceDCLR,
                                           ColorSpace.TYPE_DCLR,
                                           ICC_Profile.icSigSpaceECLR,
                                           ColorSpace.TYPE_ECLR,
                                           ICC_Profile.icSigSpaceFCLR,
                                           ColorSpace.TYPE_FCLR
                                         };

  /**
   * Size of an ICC header (128 bytes)
   */
  public static final int HEADERSIZE = 128;

  /**
   * Mapping of ICC class signatures to profile class constants
   */
  private static final int[] classMap =
                                        {
                                          ICC_Profile.icSigInputClass,
                                          ICC_Profile.CLASS_INPUT,
                                          ICC_Profile.icSigDisplayClass,
                                          ICC_Profile.CLASS_DISPLAY,
                                          ICC_Profile.icSigOutputClass,
                                          ICC_Profile.CLASS_OUTPUT,
                                          ICC_Profile.icSigLinkClass,
                                          ICC_Profile.CLASS_DEVICELINK,
                                          ICC_Profile.icSigColorSpaceClass,
                                          ICC_Profile.CLASS_COLORSPACECONVERSION,
                                          ICC_Profile.icSigAbstractClass,
                                          ICC_Profile.CLASS_ABSTRACT,
                                          ICC_Profile.icSigNamedColorClass,
                                          ICC_Profile.CLASS_NAMEDCOLOR
                                        };
  private int size;
  private int cmmId;

  // Major/Minor version, The ICC-1998 spec is major v2
  private int majorVersion;

  // Major/Minor version, The ICC-1998 spec is major v2
  private int minorVersion;
  private int profileClass; // profile device class
  private int colorSpace; // data color space type
  private int profileColorSpace; // profile connection space (PCS) type
  private byte[] timestamp; // original creation timestamp
  private int platform; // platform signature
  private int flags; // flags
  private int magic; // magic number.
  private int manufacturerSig; // manufacturer sig
  private int modelSig; // model sig
  private byte[] attributes; // Attributes
  private int intent; // rendering intent
  private byte[] illuminant; // illuminant info (Coordinates of D50 in the PCS)
  private int creatorSig; // Creator sig (same type as manufacturer)

  /**
   * Creates a 'default' header for use with our predefined profiles.
   * Note the device and profile color spaces are not set.
   */
  public ProfileHeader()
  {
    creatorSig = 0;
    intent = 0;
    modelSig = manufacturerSig = (int) 0x6E6f6E65; // 'none'
    magic = icMagicNumber;
    cmmId = 0;
    platform = 0; // no preferred platform
    timestamp = new byte[8];
    majorVersion = 2;
    minorVersion = 0x10;
    flags = 0;

    // D50 in XYZ format (encoded)
    illuminant = new byte[]
                 {
                   (byte) 0x00, (byte) 0x00, (byte) 0xf6, (byte) 0xd6,
                   (byte) 0x00, (byte) 0x01, (byte) 0x00, (byte) 0x00,
                   (byte) 0x00, (byte) 0x00, (byte) 0xd3, (byte) 0x2d
                 };
    attributes = new byte[8];
    profileClass = ICC_Profile.CLASS_DISPLAY;
  }

  /**
   * Creates a header from profile data. Only the header portion (128 bytes)
   * is read, so the array passed need not be the full profile.
   */
  public ProfileHeader(byte[] data)
  {
    ByteBuffer buf = ByteBuffer.wrap(data);

    // Get size (the sign bit shouldn't matter.
    // A valid profile can never be +2Gb)
    size = buf.getInt(ICC_Profile.icHdrSize);

    // CMM ID
    cmmId = buf.getInt(ICC_Profile.icHdrCmmId);

    // Version number
    majorVersion = (int) (data[ICC_Profile.icHdrVersion]);
    minorVersion = (int) (data[ICC_Profile.icHdrVersion + 1]);

    // Profile/Device class
    int classSig = buf.getInt(ICC_Profile.icHdrDeviceClass);
    profileClass = -1;
    for (int i = 0; i < classMap.length; i += 2)
      if (classMap[i] == classSig)
        {
          profileClass = classMap[i + 1];
          break;
        }

    // get the data color space
    int csSig = buf.getInt(ICC_Profile.icHdrColorSpace);
    colorSpace = -1;
    for (int i = 0; i < csTypeMap.length; i += 2)
      if (csTypeMap[i] == csSig)
        {
          colorSpace = csTypeMap[i + 1];
          break;
        }

    // get the profile color space (PCS), must be xyz or lab except
    // for device-link-class profiles
    int pcsSig = buf.getInt(ICC_Profile.icHdrPcs);
    profileColorSpace = -1;
    if (profileClass != ICC_Profile.CLASS_DEVICELINK)
      {
        if (pcsSig == ICC_Profile.icSigXYZData)
          profileColorSpace = ColorSpace.TYPE_XYZ;
        if (pcsSig == ICC_Profile.icSigLabData)
          profileColorSpace = ColorSpace.TYPE_Lab;
      }
    else
      {
        for (int i = 0; i < csTypeMap.length; i += 2)
          if (csTypeMap[i] == pcsSig)
            {
              profileColorSpace = csTypeMap[i + 1];
              break;
            }
      }

    // creation timestamp
    timestamp = new byte[8];
    System.arraycopy(data, ICC_Profile.icHdrDate, timestamp, 0, 8);

    // magic number
    magic = buf.getInt(ICC_Profile.icHdrMagic);

    //  platform info
    platform = buf.getInt(ICC_Profile.icHdrPlatform);
    // get flags
    flags = buf.getInt(ICC_Profile.icHdrFlags);
    // get manufacturer sign
    manufacturerSig = buf.getInt(ICC_Profile.icHdrManufacturer);
    // get header model
    modelSig = buf.getInt(ICC_Profile.icHdrModel);
    // attributes
    attributes = new byte[8];
    System.arraycopy(data, ICC_Profile.icHdrAttributes, attributes, 0, 8);
    // rendering intent
    intent = buf.getInt(ICC_Profile.icHdrRenderingIntent);
    // illuminant info
    illuminant = new byte[12];
    System.arraycopy(data, ICC_Profile.icHdrIlluminant, illuminant, 0, 12);
    // Creator signature
    creatorSig = buf.getInt(ICC_Profile.icHdrCreator);
    // The rest of the header (Total size: 128 bytes) is unused..
  }

  /**
   * Verify that the header is valid
   * @param size equals the file size if it is to be verified, -1 otherwise
   * @throws IllegalArgumentException if the header is found to be invalid.
   */
  public void verifyHeader(int size) throws IllegalArgumentException
  {
    // verify size
    if (size != -1 && this.size != size)
      throw new IllegalArgumentException("Invalid profile length:" + size);

    // Check version number
    if (majorVersion != 2)
      throw new IllegalArgumentException("Wrong major version number:"
                                         + majorVersion);

    // Profile/Device class
    if (profileClass == -1)
      throw new IllegalArgumentException("Invalid profile/device class");

    // get the data color space
    if (colorSpace == -1)
      throw new IllegalArgumentException("Invalid colorspace");

    // profile color space
    if (profileColorSpace == -1)
      throw new IllegalArgumentException("Invalid PCS.");

    // check magic number
    if (magic != icMagicNumber)
      throw new IllegalArgumentException("Invalid magic number!");
  }

  /**
   * Creates a header, setting the header file size at the same time.
   * @param size the profile file size.
   */
  public byte[] getData(int size)
  {
    byte[] data = new byte[HEADERSIZE];
    ByteBuffer buf = ByteBuffer.wrap(data);
    buf.putInt(ICC_Profile.icHdrSize, size);
    buf.putInt(ICC_Profile.icHdrCmmId, cmmId);
    buf.putShort(ICC_Profile.icHdrVersion,
                 (short) (majorVersion << 8 | minorVersion));
    for (int i = 1; i < classMap.length; i += 2)
      if (profileClass == classMap[i])
        buf.putInt(ICC_Profile.icHdrDeviceClass, classMap[i - 1]);
    for (int i = 1; i < csTypeMap.length; i += 2)
      if (csTypeMap[i] == colorSpace)
        buf.putInt(ICC_Profile.icHdrColorSpace, csTypeMap[i - 1]);
    for (int i = 1; i < csTypeMap.length; i += 2)
      if (csTypeMap[i] == profileColorSpace)
        buf.putInt(ICC_Profile.icHdrPcs, csTypeMap[i - 1]);

    System.arraycopy(timestamp, 0, data, ICC_Profile.icHdrDate,
                     timestamp.length);
    buf.putInt(ICC_Profile.icHdrMagic, icMagicNumber);
    buf.putInt(ICC_Profile.icHdrPlatform, platform);
    buf.putInt(ICC_Profile.icHdrFlags, flags);
    buf.putInt(ICC_Profile.icHdrManufacturer, manufacturerSig);
    buf.putInt(ICC_Profile.icHdrModel, modelSig);
    System.arraycopy(attributes, 0, data, ICC_Profile.icHdrAttributes,
                     attributes.length);
    buf.putInt(ICC_Profile.icHdrRenderingIntent, intent);
    System.arraycopy(illuminant, 0, data, ICC_Profile.icHdrIlluminant,
                     illuminant.length);
    buf.putInt(ICC_Profile.icHdrCreator, creatorSig);
    return buf.array();
  }

  public int getSize()
  {
    return size;
  }

  public void setSize(int s)
  {
    size = s;
  }

  public int getMajorVersion()
  {
    return majorVersion;
  }

  public int getMinorVersion()
  {
    return minorVersion;
  }

  public int getProfileClass()
  {
    return profileClass;
  }

  public void setProfileClass(int pc)
  {
    profileClass = pc;
  }

  public int getColorSpace()
  {
    return colorSpace;
  }

  public int getProfileColorSpace()
  {
    return profileColorSpace;
  }

  public void setColorSpace(int cs)
  {
    colorSpace = cs;
  }

  public void setProfileColorSpace(int pcs)
  {
    profileColorSpace = pcs;
  }

}
