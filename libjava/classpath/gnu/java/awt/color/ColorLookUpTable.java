/* ColorLookUpTable.java -- ICC v2 CLUT
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
 * ColorLookUpTable handles color lookups through a color lookup table,
 * as defined in the ICC specification.
 * Both 'mft2' and 'mft1' (8 and 16-bit) type CLUTs are handled.
 *
 * This will have to be updated later for ICC 4.0.0
 *
 * @author Sven de Marothy
 */
public class ColorLookUpTable
{
  /**
   * CIE 1931 D50 white point (in Lab coordinates)
   */
  private static float[] D50 = { 0.96422f, 1.00f, 0.82521f };

  /**
   * Number of input/output channels
   */
  int nIn;

  /**
   * Number of input/output channels
   */
  int nOut;
  int nInTableEntries; // Number of input table entries
  int nOutTableEntries; // Number of output table entries
  int gridpoints; // Number of gridpoints
  int nClut; // This is nOut*(gridpoints**nIn)
  double[][] inTable; // 1D input table ([channel][table])
  short[][] outTable; // 1D input table ([channel][table])
  double[] clut; // The color lookup table
  float[][] inMatrix; // input matrix (XYZ only)
  boolean useMatrix; // Whether to use the matrix or not.
  int[] multiplier;
  int[] offsets; // Hypercube offsets 
  boolean inputLab; // Set if the CLUT input CS is Lab
  boolean outputLab; // Set if the CLUT output CS is Lab

  /**
   * Constructor
   * Requires a profile file to get the CLUT from and the tag of the
   * CLUT to create. (icSigXToYZTag where X,Y = [A | B], Z = [0,1,2])
   */
  public ColorLookUpTable(ICC_Profile profile, int tag)
  {
    useMatrix = false;

    switch (tag)
      {
      case ICC_Profile.icSigAToB0Tag:
      case ICC_Profile.icSigAToB1Tag:
      case ICC_Profile.icSigAToB2Tag:
	if (profile.getColorSpaceType() == ColorSpace.TYPE_XYZ)
	  useMatrix = true;
	inputLab = false;
	outputLab = (profile.getPCSType() == ColorSpace.TYPE_Lab);
	break;
      case ICC_Profile.icSigBToA0Tag:
      case ICC_Profile.icSigBToA1Tag:
      case ICC_Profile.icSigBToA2Tag:
	if (profile.getPCSType() == ColorSpace.TYPE_XYZ)
	  useMatrix = true;
	inputLab = (profile.getPCSType() == ColorSpace.TYPE_Lab);
	outputLab = false;
	break;
      default:
	throw new IllegalArgumentException("Not a clut-type tag.");
      }

    byte[] data = profile.getData(tag);
    if (data == null)
      throw new IllegalArgumentException("Unsuitable profile, does not contain a CLUT.");

    // check 'mft'
    if (data[0] != 0x6d || data[1] != 0x66 || data[2] != 0x74)
      throw new IllegalArgumentException("Unsuitable profile, invalid CLUT data.");

    if (data[3] == 0x32)
      readClut16(data);
    else if (data[3] == 0x31)
      readClut8(data);
    else
      throw new IllegalArgumentException("Unknown/invalid CLUT type.");
  }

  /**
   * Loads a 16-bit CLUT into our data structures
   */
  private void readClut16(byte[] data)
  {
    ByteBuffer buf = ByteBuffer.wrap(data);

    nIn = data[8] & (0xFF);
    nOut = data[9] & (0xFF);
    nInTableEntries = buf.getShort(48);
    nOutTableEntries = buf.getShort(50);
    gridpoints = data[10] & (0xFF);

    inMatrix = new float[3][3];
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
	inMatrix[i][j] = ((float) (buf.getInt(12 + (i * 3 + j) * 4))) / 65536.0f;

    inTable = new double[nIn][nInTableEntries];
    for (int channel = 0; channel < nIn; channel++)
      for (int i = 0; i < nInTableEntries; i++)
	inTable[channel][i] = (double) ((int) buf.getShort(52
	                                                   + (channel * nInTableEntries
	                                                   + i) * 2)
	                      & (0xFFFF)) / 65536.0;

    nClut = nOut;
    multiplier = new int[nIn];
    multiplier[nIn - 1] = nOut;
    for (int i = 0; i < nIn; i++)
      {
	nClut *= gridpoints;
	if (i > 0)
	  multiplier[nIn - i - 1] = multiplier[nIn - i] * gridpoints;
      }

    int clutOffset = 52 + nIn * nInTableEntries * 2;
    clut = new double[nClut];
    for (int i = 0; i < nClut; i++)
      clut[i] = (double) ((int) buf.getShort(clutOffset + i * 2) & (0xFFFF)) / 65536.0;

    outTable = new short[nOut][nOutTableEntries];
    for (int channel = 0; channel < nOut; channel++)
      for (int i = 0; i < nOutTableEntries; i++)
	outTable[channel][i] = buf.getShort(clutOffset
	                                    + (nClut
	                                    + channel * nOutTableEntries + i) * 2);

    // calculate the hypercube corner offsets
    offsets = new int[(1 << nIn)];
    offsets[0] = 0;
    for (int j = 0; j < nIn; j++)
      {
	int factor = 1 << j;
	for (int i = 0; i < factor; i++)
	  offsets[factor + i] = offsets[i] + multiplier[j];
      }
  }

  /**
   * Loads a 8-bit CLUT into our data structures.
   */
  private void readClut8(byte[] data)
  {
    ByteBuffer buf = ByteBuffer.wrap(data);

    nIn = (data[8] & (0xFF));
    nOut = (data[9] & (0xFF));
    nInTableEntries = 256; // always 256
    nOutTableEntries = 256; // always 256
    gridpoints = (data[10] & (0xFF));

    inMatrix = new float[3][3];
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
	inMatrix[i][j] = ((float) (buf.getInt(12 + (i * 3 + j) * 4))) / 65536.0f;

    inTable = new double[nIn][nInTableEntries];
    for (int channel = 0; channel < nIn; channel++)
      for (int i = 0; i < nInTableEntries; i++)
	inTable[channel][i] = (double) ((int) buf.get(48
	                                              + (channel * nInTableEntries
	                                              + i)) & (0xFF)) / 255.0;

    nClut = nOut;
    multiplier = new int[nIn];
    multiplier[nIn - 1] = nOut;
    for (int i = 0; i < nIn; i++)
      {
	nClut *= gridpoints;
	if (i > 0)
	  multiplier[nIn - i - 1] = multiplier[nIn - i] * gridpoints;
      }

    int clutOffset = 48 + nIn * nInTableEntries;
    clut = new double[nClut];
    for (int i = 0; i < nClut; i++)
      clut[i] = (double) ((int) buf.get(clutOffset + i) & (0xFF)) / 255.0;

    outTable = new short[nOut][nOutTableEntries];
    for (int channel = 0; channel < nOut; channel++)
      for (int i = 0; i < nOutTableEntries; i++)
	outTable[channel][i] = (short) (buf.get(clutOffset + nClut
	                                        + channel * nOutTableEntries
	                                        + i) * 257);

    // calculate the hypercube corner offsets
    offsets = new int[(1 << nIn)];
    offsets[0] = 0;
    for (int j = 0; j < nIn; j++)
      {
	int factor = 1 << j;
	for (int i = 0; i < factor; i++)
	  offsets[factor + i] = offsets[i] + multiplier[j];
      }
  }

  /**
   * Performs a lookup through the Color LookUp Table.
   * If the CLUT tag type is AtoB the conversion will be from the device
   * color space to the PCS, BtoA type goes in the opposite direction.
   *
   * For convenience, the PCS values for input or output will always be
   * CIE XYZ (D50), if the actual PCS is Lab, the values will be converted.
   *
   * N-dimensional linear interpolation is used.
   */
  float[] lookup(float[] in)
  {
    float[] in2 = new float[in.length];
    if (useMatrix)
      {
	for (int i = 0; i < 3; i++)
	  in2[i] = in[0] * inMatrix[i][0] + in[1] * inMatrix[i][1]
	           + in[2] * inMatrix[i][2];
      }
    else if (inputLab)
      in2 = XYZtoLab(in);
    else
      System.arraycopy(in, 0, in2, 0, in.length);

    // input table 
    for (int i = 0; i < nIn; i++)
      {
	int index = (int) Math.floor(in2[i] * (double) (nInTableEntries - 1)); // floor in

	// clip values.
	if (index >= nInTableEntries - 1)
	  in2[i] = (float) inTable[i][nInTableEntries - 1];
	else if (index < 0)
	  in2[i] = (float) inTable[i][0];
	else
	  {
	    // linear interpolation
	    double alpha = in2[i] * ((double) nInTableEntries - 1.0) - index;
	    in2[i] = (float) (inTable[i][index] * (1 - alpha)
	             + inTable[i][index + 1] * alpha);
	  }
      }

    // CLUT lookup
    double[] output2 = new double[nOut];
    double[] weights = new double[(1 << nIn)];
    double[] clutalpha = new double[nIn]; // interpolation values
    int offset = 0; // = gp
    for (int i = 0; i < nIn; i++)
      {
	int index = (int) Math.floor(in2[i] * ((double) gridpoints - 1.0));
	double alpha = in2[i] * ((double) gridpoints - 1.0) - (double) index;

	// clip values.
	if (index >= gridpoints - 1)
	  {
	    index = gridpoints - 1;
	    alpha = 1.0;
	  }
	else if (index < 0)
	  index = 0;
	clutalpha[i] = alpha;
	offset += index * multiplier[i];
      }

    // Calculate interpolation weights
    weights[0] = 1.0;
    for (int j = 0; j < nIn; j++)
      {
	int factor = 1 << j;
	for (int i = 0; i < factor; i++)
	  {
	    weights[factor + i] = weights[i] * clutalpha[j];
	    weights[i] *= (1.0 - clutalpha[j]);
	  }
      }

    for (int i = 0; i < nOut; i++)
      output2[i] = weights[0] * clut[offset + i];

    for (int i = 1; i < (1 << nIn); i++)
      {
	int offset2 = offset + offsets[i];
	for (int f = 0; f < nOut; f++)
	  output2[f] += weights[i] * clut[offset2 + f];
      }

    // output table 
    float[] output = new float[nOut];
    for (int i = 0; i < nOut; i++)
      {
	int index = (int) Math.floor(output2[i] * ((double) nOutTableEntries
	                             - 1.0));

	// clip values.
	if (index >= nOutTableEntries - 1)
	  output[i] = outTable[i][nOutTableEntries - 1];
	else if (index < 0)
	  output[i] = outTable[i][0];
	else
	  {
	    // linear interpolation
	    double a = output2[i] * ((double) nOutTableEntries - 1.0)
	               - (double) index;
	    output[i] = (float) ((double) ((int) outTable[i][index] & (0xFFFF)) * (1
	                - a)
	                + (double) ((int) outTable[i][index + 1] & (0xFFFF)) * a) / 65536f;
	  }
      }

    if (outputLab)
      return LabtoXYZ(output);
    return output;
  }

  /**
   * Converts CIE Lab coordinates to (D50) XYZ ones.
   */
  private float[] LabtoXYZ(float[] in)
  {
    // Convert from byte-packed format to a 
    // more convenient one (actual Lab values)
    // (See ICC spec for details)
    // factor is 100 * 65536 / 65280
    in[0] = (float) (100.392156862745 * in[0]);
    in[1] = (in[1] * 256.0f) - 128.0f;
    in[2] = (in[2] * 256.0f) - 128.0f;

    float[] out = new float[3];

    out[1] = (in[0] + 16.0f) / 116.0f;
    out[0] = in[1] / 500.0f + out[1];
    out[2] = out[1] - in[2] / 200.0f;

    for (int i = 0; i < 3; i++)
      {
	double exp = out[i] * out[i] * out[i];
	if (exp <= 0.008856)
	  out[i] = (out[i] - 16.0f / 116.0f) / 7.787f;
	else
	  out[i] = (float) exp;
	out[i] = D50[i] * out[i];
      }
    return out;
  }

  /**
   * Converts CIE XYZ coordinates to Lab ones.
   */
  private float[] XYZtoLab(float[] in)
  {
    float[] temp = new float[3];

    for (int i = 0; i < 3; i++)
      {
	temp[i] = in[i] / D50[i];

	if (temp[i] <= 0.008856f)
	  temp[i] = (7.7870689f * temp[i]) + (16f / 116.0f);
	else
	  temp[i] = (float) Math.exp((1.0 / 3.0) * Math.log(temp[i]));
      }

    float[] out = new float[3];
    out[0] = (116.0f * temp[1]) - 16f;
    out[1] = 500.0f * (temp[0] - temp[1]);
    out[2] = 200.0f * (temp[1] - temp[2]);

    // Normalize to packed format
    out[0] = (float) (out[0] / 100.392156862745);
    out[1] = (out[1] + 128f) / 256f;
    out[2] = (out[2] + 128f) / 256f;
    for (int i = 0; i < 3; i++)
      {
	if (out[i] < 0f)
	  out[i] = 0f;
	if (out[i] > 1f)
	  out[i] = 1f;
      }
    return out;
  }
}
