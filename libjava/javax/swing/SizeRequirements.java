/* SizeRequirements.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package javax.swing;

// Imports
import java.io.*;

/**
 * SizeRequirements
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class SizeRequirements implements Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * minimum
	 */
	public int minimum;

	/**
	 * preferred
	 */
	public int preferred;

	/**
	 * maximum
	 */
	public int maximum;

	/**
	 * alignment
	 */
	public float alignment;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor SizeRequirements
	 */
	public SizeRequirements() {
		// TODO
	} // SizeRequirements()

	/**
	 * Constructor SizeRequirements
	 * @param min TODO
	 * @param pref TODO
	 * @param max TODO
	 * @param align TODO
	 */
	public SizeRequirements(int min, int pref, int max, float align) {
		// TODO
	} // SizeRequirements()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * toString
	 * @returns String
	 */
	public String toString() {
		return null; // TODO
	} // toString()

	/**
	 * getTiledSizeRequirements
	 * @param children TODO
	 * @returns SizeRequirements
	 */
	public static SizeRequirements getTiledSizeRequirements(
			SizeRequirements[] children) {
		return null; // TODO
	} // getTiledSizeRequirements()

	/**
	 * getAlignedSizeRequirements
	 * @param children TODO
	 * @returns SizeRequirements
	 */
	public static SizeRequirements getAlignedSizeRequirements(
			SizeRequirements[] children) {
		return null; // TODO
	} // getAlignedSizeRequirements()

	/**
	 * calculateTiledPositions
	 * @param allocated TODO
	 * @param total TODO
	 * @param children TODO
	 * @param offset TODO
	 * @param spans TODO
	 */
	public static void calculateTiledPositions(int allocated,
			SizeRequirements total, SizeRequirements[] children,
			int[] offset, int[] spans) {
		// TODO
	} // calculateTiledPositions()

	/**
	 * calculateAlignedPositions
	 * @param allocated TODO
	 * @param total TODO
	 * @param children TODO
	 * @param offset TODO
	 * @param spans TODO
	 */
	public static void calculateAlignedPositions(int allocated,
			SizeRequirements total, SizeRequirements[] children,
			int[] offset, int[] spans) {
		// TODO
	} // calculateAlignedPositions()

	/**
	 * adjustSizes
	 * @param delta TODO
	 * @param children TODO
	 * @returns int[]
	 */
	public static int[] adjustSizes(int delta, SizeRequirements[] children) {
		return null; // TODO
	} // adjustSizes()


} // SizeRequirements
