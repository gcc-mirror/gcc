/* ICC_Profile.java -- color space profiling
   Copyright (C) 2000, 2002 Free Software Foundation

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt.color;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.OutputStream;
import java.io.Serializable;

/**
 * STUBBED
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 * @since 1.2
 */
public class ICC_Profile implements Serializable
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = -3938515861990936766L;

  public static final int CLASS_INPUT = 0;
  public static final int CLASS_DISPLAY = 1;
  public static final int CLASS_OUTPUT = 2;
  public static final int CLASS_DEVICELINK = 3;
  public static final int CLASS_COLORSPACECONVERSION = 4;
  public static final int CLASS_ABSTRACT = 5;
  public static final int CLASS_NAMEDCOLOR = 6;

  public static final int icSigXYZData = 1482250784;
  public static final int icSigLabData = 1281450528;
  public static final int icSigLuvData = 1282766368;
  public static final int icSigYCbCrData = 1497588338;
  public static final int icSigYxyData = 1501067552;
  public static final int icSigRgbData = 1380401696;
  public static final int icSigGrayData = 1196573017;
  public static final int icSigHsvData = 1213421088;
  public static final int icSigHlsData = 1212961568;
  public static final int icSigCmykData = 1129142603;
  public static final int icSigCmyData = 1129142560;
  public static final int icSigSpace2CLR = 843271250;
  public static final int icSigSpace3CLR = 860048466;
  public static final int icSigSpace4CLR = 876825682;
  public static final int icSigSpace5CLR = 893602898;
  public static final int icSigSpace6CLR = 910380114;
  public static final int icSigSpace7CLR = 927157330;
  public static final int icSigSpace8CLR = 943934546;
  public static final int icSigSpace9CLR = 960711762;
  public static final int icSigSpaceACLR = 1094929490;
  public static final int icSigSpaceBCLR = 1111706706;
  public static final int icSigSpaceCCLR = 1128483922;
  public static final int icSigSpaceDCLR = 1145261138;
  public static final int icSigSpaceECLR = 1162038354;
  public static final int icSigSpaceFCLR = 1178815570;

  public static final int icSigInputClass = 1935896178;
  public static final int icSigDisplayClass = 1835955314;
  public static final int icSigOutputClass = 1886549106;
  public static final int icSigLinkClass = 1818848875;
  public static final int icSigAbstractClass = 1633842036;
  public static final int icSigColorSpaceClass = 1936744803;
  public static final int icSigNamedColorClass = 1852662636;

  public static final int icPerceptual = 0;
  public static final int icRelativeColorimetric = 1;
  public static final int icSaturation = 2;
  public static final int icAbsoluteColorimetric = 3;

  public static final int icSigHead = 1751474532;
  public static final int icSigAToB0Tag = 1093812784;
  public static final int icSigAToB1Tag = 1093812785;
  public static final int icSigAToB2Tag = 1093812786;
  public static final int icSigBlueColorantTag = 1649957210;
  public static final int icSigBlueTRCTag = 1649693251;
  public static final int icSigBToA0Tag = 1110589744;
  public static final int icSigBToA1Tag = 1110589745;
  public static final int icSigBToA2Tag = 1110589746;
  public static final int icSigCalibrationDateTimeTag = 1667329140;
  public static final int icSigCharTargetTag = 1952543335;
  public static final int icSigCopyrightTag = 1668313716;
  public static final int icSigCrdInfoTag = 1668441193;
  public static final int icSigDeviceMfgDescTag = 1684893284;
  public static final int icSigDeviceModelDescTag = 1684890724;
  public static final int icSigDeviceSettingsTag = 1684371059;
  public static final int icSigGamutTag = 1734438260;
  public static final int icSigGrayTRCTag = 1800688195;
  public static final int icSigGreenColorantTag = 1733843290;
  public static final int icSigGreenTRCTag = 1733579331;
  public static final int icSigLuminanceTag = 1819635049;
  public static final int icSigMeasurementTag = 1835360627;
  public static final int icSigMediaBlackPointTag = 1651208308;
  public static final int icSigMediaWhitePointTag = 2004119668;
  public static final int icSigNamedColor2Tag = 1852009522;
  public static final int icSigOutputResponseTag = 1919251312;
  public static final int icSigPreview0Tag = 1886545200;
  public static final int icSigPreview1Tag = 1886545201;
  public static final int icSigPreview2Tag = 1886545202;
  public static final int icSigProfileDescriptionTag = 1684370275;
  public static final int icSigProfileSequenceDescTag = 1886610801;
  public static final int icSigPs2CRD0Tag = 1886610480;
  public static final int icSigPs2CRD1Tag = 1886610481;
  public static final int icSigPs2CRD2Tag = 1886610482;
  public static final int icSigPs2CRD3Tag = 1886610483;
  public static final int icSigPs2CSATag = 1886597747;
  public static final int icSigPs2RenderingIntentTag = 1886597737;
  public static final int icSigRedColorantTag = 1918392666;
  public static final int icSigRedTRCTag = 1918128707;
  public static final int icSigScreeningDescTag = 1935897188;
  public static final int icSigScreeningTag = 1935897198;
  public static final int icSigTechnologyTag = 1952801640;
  public static final int icSigUcrBgTag = 1650877472;
  public static final int icSigViewingCondDescTag = 1987405156;
  public static final int icSigViewingConditionsTag = 1986618743;
  public static final int icSigChromaticityTag = 1667789421;

  public static final int icHdrSize = 0;
  public static final int icHdrCmmId = 4;
  public static final int icHdrVersion = 8;
  public static final int icHdrDeviceClass = 12;
  public static final int icHdrColorSpace = 16;
  public static final int icHdrPcs = 20;
  public static final int icHdrDate = 24;
  public static final int icHdrMagic = 36;
  public static final int icHdrPlatform = 40;
  public static final int icHdrFlags = 44;
  public static final int icHdrManufacturer = 48;
  public static final int icHdrModel = 52;
  public static final int icHdrAttributes = 56;
  public static final int icHdrRenderingIntent = 64;
  public static final int icHdrIlluminant = 68;
  public static final int icHdrCreator = 80;

  public static final int icTagType = 0;
  public static final int icTagReserved = 4;
  public static final int icCurveCount = 8;
  public static final int icCurveData = 12;

  public static final int icXYZNumberX = 8;

  /**
   * @serial
   */
  final int iccProfileSerializedDataVersion = 1;

  transient int profileID;

  ICC_Profile(int profileID)
  {
    this.profileID = profileID;
  }

  protected void finalize()
  {
    // XXX What resources should we free?
  }

  public static ICC_Profile getInstance(byte[] data)
  {
    throw new Error("not implemented");
  }

  public static ICC_Profile getInstance(int cspace)
  {
    return new ICC_Profile(cspace);
  }

  public static ICC_Profile getInstance(String filename) throws IOException
  {
    return getInstance(new FileInputStream(filename));
  }

  public static ICC_Profile getInstance(InputStream in) throws IOException
  {
    throw new Error("not implemented");
  }

  public int getMajorVersion()
  {
    throw new Error("not implemented");
  }

  public int getMinorVersion()
  {
    throw new Error("not implemented");
  }

  public int getProfileClass()
  {
    throw new Error("not implemented");
  }

  public int getColorSpaceType()
  {
    throw new Error("not implemented");
  }

  public int getPCSType()
  {
    throw new Error("not implemented");
  }

  public void write(String filename) throws IOException
  {
    write(new FileOutputStream(filename));
  }

  public void write(OutputStream out) throws IOException
  {
    throw new Error("not implemented");
  }

  public byte[] getData()
  {
    throw new Error("not implemented");
  }

  public byte[] getData(int tagSignature)
  {
    throw new Error("not implemented");
  }

  public void setData(int tagSignature, byte[] data)
  {
    throw new Error("not implemented");
  }

  public int getNumComponents()
  {
    switch (profileID)
      {
      case ColorSpace.CS_sRGB:
      case ColorSpace.CS_LINEAR_RGB:
      case ColorSpace.CS_CIEXYZ:
	return 3;
      case ColorSpace.CS_GRAY:
	return 1;
      case ColorSpace.CS_PYCC: // have no clue about this one
      default:
	throw new UnsupportedOperationException("profile not implemented");
      }
  }

  protected Object readResolve() throws ObjectStreamException
  {
    throw new Error("not implemented");
  }

  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    throw new Error("not implemented");
  }

  private void writeObject(ObjectOutputStream s) throws IOException
  {
    throw new Error("not implemented");
  }
} // class ICC_Profile
