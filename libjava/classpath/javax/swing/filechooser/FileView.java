/* FileView.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


package javax.swing.filechooser;

import java.io.File;

import javax.swing.Icon;

/**
 * FileView
 * @author	Andrew Selkirk
 * @version	1.0
 */
public abstract class FileView {

	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor FileView
	 */
	public FileView() {
		// TODO
	} // FileView()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getName
	 * @param file TODO
	 * @returns String
	 */
	public String getName(File file) {
		return null; // TODO
	} // getName()

	/**
	 * getDescription
	 * @param value0 TODO
	 * @returns String
	 */
	public String getDescription(File value0) {
		return null; // TODO
	} // getDescription()

	/**
	 * getTypeDescription
	 * @param value0 TODO
	 * @returns String
	 */
	public String getTypeDescription(File value0) {
		return null; // TODO
	} // getTypeDescription()

	/**
	 * getIcon
	 * @param value0 TODO
	 * @returns Icon
	 */
	public Icon getIcon(File value0) {
		return null; // TODO
	} // getIcon()

	/**
	 * isTraversable
	 * @param value0 TODO
	 * @returns Boolean
	 */
	public Boolean isTraversable(File value0) {
		return null; // TODO
	} // isTraversable()


} // FileView
