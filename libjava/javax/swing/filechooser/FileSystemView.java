/* FileSystemView.java --
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

package javax.swing.filechooser;

// Imports
import java.io.*;

/**
 * FileSystemView
 * @author	Andrew Selkirk
 * @version	1.0
 */
public abstract class FileSystemView {

	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor FileSystemView
	 */
	public FileSystemView() {
		// TODO
	} // FileSystemView()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getFileSystemView
	 * @returns FileSystemView
	 */
	public static FileSystemView getFileSystemView() {
		return null; // TODO
	} // getFileSystemView()

	/**
	 * isRoot
	 * @param file TODO
	 * @returns boolean
	 */
	public abstract boolean isRoot(File file);

	/**
	 * createNewFolder
	 * @param file TODO
	 * @exception IOException TODO
	 * @returns File
	 */
	public abstract File createNewFolder(File file) throws IOException;

	/**
	 * isHiddenFile
	 * @param file TODO
	 * @returns boolean
	 */
	public abstract boolean isHiddenFile(File file);

	/**
	 * getRoots
	 * @returns File[]
	 */
	public abstract File[] getRoots();

	/**
	 * getHomeDirectory
	 * @returns File
	 */
	public File getHomeDirectory() {
		return null; // TODO
	} // getHomeDirectory()

	/**
	 * createFileObject
	 * @param directory TODO
	 * @param filename TODO
	 * @returns File
	 */
	public File createFileObject(File directory, String filename) {
		return null; // TODO
	} // createFileObject()

	/**
	 * createFileObject
	 * @param path TODO
	 * @returns File
	 */
	public File createFileObject(String path) {
		return null; // TODO
	} // createFileObject()

	/**
	 * getFiles
	 * @param directory TODO
	 * @param fileHiding TODO
	 * @returns File[]
	 */
	public File[] getFiles(File directory, boolean fileHiding) {
		return null; // TODO
	} // getFiles()

	/**
	 * getParentDirectory
	 * @param directory TODO
	 * @returns File
	 */
	public File getParentDirectory(File directory) {
		return null; // TODO
	} // getParentDirectory()


} // FileSystemView
