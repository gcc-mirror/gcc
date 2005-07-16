/* SizeSequence.java --
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

package javax.swing;

/**
 * SizeSequence
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class SizeSequence {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * sizes
	 */
	private int[] sizes = new int[0];


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor SizeSequence
	 */
	public SizeSequence() {
		sizes = new int[0];
	} // SizeSequence()

	/**
	 * Constructor SizeSequence
	 * @param numEntries TODO
	 */
	public SizeSequence(int numEntries) {
		this(numEntries, 0);
	} // SizeSequence()

	/**
	 * Constructor SizeSequence
	 * @param numEntries TODO
	 * @param value TODO
	 */
	public SizeSequence(int numEntries, int value) {
		insertEntries(0, numEntries, value);
	} // SizeSequence()

	/**
	 * Constructor SizeSequence
	 * @param sizes TODO
	 */
	public SizeSequence(int[] sizes) {
		setSizes(sizes);
	} // SizeSequence()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * setSize
	 * @param index TODO
	 * @param size TODO
	 */
	public void setSize(int index, int size) {
		sizes[index] = size;
	} // setSize()

	/**
	 * getIndex
	 * @param position TODO
	 * @returns int
	 */
	public int getIndex(int position) {
		return 0; // TODO
	} // getIndex()

	/**
	 * getSize
	 * @param index TODO
	 * @returns int
	 */
	public int getSize(int index) {
		return sizes[index];
	} // getSize()

	/**
	 * setSizes
	 * @param sizes TODO
	 */
	public void setSizes(int[] sizes) {
	
		// Variables
		int		index;
		
		// Initialize Sizes
		this.sizes = new int[sizes.length];
		for (index = 0; index < sizes.length; index++) {
			this.sizes[index] = sizes[index];
		} // for

	} // setSizes()

	/**
	 * getSizes
	 * @returns int[]
	 */
	public int[] getSizes() {
	
		// Variables
		int[]	array;
		int		index;

		// Create New Array
		array = new int[sizes.length];
		for (index = 0; index < sizes.length; index++) {
			array[index] = sizes[index];
		} // for

		// Return Newly created array
		return array;

	} // getSizes()

	/**
	 * getPosition
	 * @param index TODO
	 * @returns int
	 */
	public int getPosition(int index) {
	
		// Variables
		int		position;
		int		loop;
		
		// Process Sizes
		position = 0;
		for (loop = 0; loop < index; loop++) {
			position += sizes[loop];
		} // for

		// Return Position
		return position;

	} // getPosition()

	/**
	 * insertEntries
	 * @param start TODO
	 * @param length TODO
	 * @param value TODO
	 */
	public void insertEntries(int start, int length, int value) {

		// Variables
		int[]	array;
		int		index;
		int		arrayIndex;
		int		loop;

		// Create New Array
		array = new int[sizes.length + length];
		arrayIndex = 0;
		for (index = 0; index < sizes.length; index++) {
			if (index == start) {
				for (loop = 0; loop < length; loop++) {
					array[arrayIndex] = value;
					arrayIndex++;
				} // for
			} else {
				array[arrayIndex] = sizes[index];
				arrayIndex++;
			} // if
		} // for

	} // insertEntries()

	/**
	 * removeEntries
	 * @param start TODO
	 * @param length TODO
	 */
	public void removeEntries(int start, int length) {

		// Variables
		int[]	array;
		int		index;
		int		arrayIndex;

		// Sanity Check
		if ((start + length) > sizes.length) {
			throw new IllegalArgumentException("Specified start/length that " +
				"is greater than available sizes");
		} // if

		// Create New Array
		array = new int[sizes.length - length];
		arrayIndex = 0;
		for (index = 0; index < sizes.length; index++) {
			if (index == start) {
				index += length - 1;
			} else {
				array[arrayIndex] = sizes[index];
				arrayIndex++;
			} // if
		} // for

	} // removeEntries()


} // SizeSequence
