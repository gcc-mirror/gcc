/* TreePath.java --
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


package javax.swing.tree;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * TreePath
 * @author Andrew Selkirk
 */
public class TreePath implements Serializable
{
  static final long serialVersionUID = 4380036194768077479L;

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * path
	 */
	private Object[]	path	= null;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor TreePath
	 * @param path TODO
	 */
	public TreePath(Object[] path) {

		// Create Path
		this.path = new Object[path.length];
		System.arraycopy(path, 0, this.path, 0, path.length);

	} // TreePath()

	/**
	 * Constructor TreePath
	 * @param element TODO
	 */
	public TreePath(Object element) {

		// Create Path
		path = new Object[1];
		path[0] = element;

	} // TreePath()

	/**
	 * Constructor TreePath
	 * @param path TODO
	 * @param element TODO
	 */
	protected TreePath(TreePath path, Object element) {

		// Variables
		Object[]	treepath;

		// Get Tree Path
		treepath = path.getPath();

		// Create Tree Path
		this.path = new Object[treepath.length + 1];
		System.arraycopy(treepath, 0, this.path, 0, treepath.length);
		this.path[treepath.length] = element;

	} // TreePath()

	/**
	 * Constructor TreePath
	 * @param path TODO
	 * @param length TODO
	 */
	protected TreePath(Object[] path, int length) {

		// Create Path
		this.path = new Object[length];
		System.arraycopy(path, 0, this.path, 0, length);

	} // TreePath()

	/**
	 * Constructor TreePath
	 */
	protected TreePath() {
		path = new Object[0];
	} // TreePath()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * hashCode
	 * @returns int
	 */
	public int hashCode() {
		return getLastPathComponent().hashCode();
	} // hashCode()

	/**
	 * equals
	 * @param object TODO
	 * @returns boolean
	 */
	public boolean equals(Object object) {

		// Variables
		Object[]	treepath;
		int			index;

		// Check for TreePath
		if (object instanceof TreePath) {

			// Get Path Elements
			treepath = ((TreePath) object).getPath();

			// Check length
			if (treepath.length != path.length) {
				return false;
			} // if

			// Check Elements
			for (index = 0; index < path.length; index++) {
				if (treepath[index] != path[index]) {
					return false;
				} // if
			} // for

			// Tree Path's are equals
			return true;

		} // if

		// Unequal
		return false;

	} // equals()

	/**
	 * toString
	 * @returns String
	 */
	public String toString() {
		return null; // TODO
	} // toString()

	/**
	 * writeObject
	 * @param value0 TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream value0) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * readObject
	 * @param value0 TODO
	 * @exception IOException TODO
	 * @exception ClassNotFoundException TODO
	 */
	private void readObject(ObjectInputStream value0) throws IOException, ClassNotFoundException {
		// TODO
	} // readObject()

	/**
	 * getPath
	 * @returns Object[]
	 */
	public Object[] getPath() {
		return path;
	} // getPath()

	/**
	 * getLastPathComponent
	 * @returns Object
	 */
	public Object getLastPathComponent() {
		return path[path.length - 1];
	} // getLastPathComponent()

	/**
	 * getPathCount
	 * @returns int
	 */
	public int getPathCount() {
		return path.length;
	} // getPathCount()

	/**
	 * getPathComponent
	 * @param position TODO
	 * @returns Object
	 */
	public Object getPathComponent(int position) {
		return path[position];
	} // getPathComponent()

	/**
	 * isDescendant
	 * @param path TODO
	 * @returns boolean
	 */
	public boolean isDescendant(TreePath path) {

		// Variables
		Object[]	treepath;
		int			index;
		int			index2;

		// Get Descendant path
		treepath = path.getPath();

		// Locate Start Index
		index = 0;
		index2 = 0;
		while (treepath[index] != this.path[index2]) {
			index++;
		} // while

		// Verify Paths
		while (treepath[index] == this.path[index2]) {
			index++;
			index2++;
		} // while

		// Check for descendant
		if (index2 != this.path.length) {
			return false;
		} // if

		// Is Descendant
		return true;

	} // isDescendant()

	/**
	 * pathByAddingChild
	 * @param element TODO
	 * @returns TreePath
	 */
	public TreePath pathByAddingChild(Object element) {
		return new TreePath(this, element);
	} // pathByAddingChild()

	/**
	 * getParentPath
	 * @returns TreePath
	 */
	public TreePath getParentPath() {
		return new TreePath(this.getPath(), path.length - 1);
	} // getParentPath()


} // TreePath
