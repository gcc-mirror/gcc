/* TreeModelEvent.java --
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


package javax.swing.event;

import java.util.EventObject;

import javax.swing.tree.TreePath;

/**
 * TreeModelEvent
 * @author Andrew Selkirk
 */
public class TreeModelEvent extends EventObject {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------
	
	/**
	 * childIndices
	 */
	protected int[]		childIndices	= null;

	/**
	 * children
	 */
	protected Object[]	children		= null;

	/**
	 * path
	 */
	protected TreePath	path			= null;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------
	
	/**
	 * Constructor TreeModelEvent
	 * @param source Source object
	 * @param path
	 */
	public TreeModelEvent(Object source, Object[] path) {
		super(source);
		this.path = new TreePath(path);
	} // TreeModelEvent()

	/**
	 * Constructor TreeModelEvent
	 * @param source Source object
	 * @param path path
	 * @param childIndices Child indices
	 * @param children Children
	 */
	public TreeModelEvent(Object source, Object[] path,
						int[] childIndices, Object[] children) {
		super(source);
		this.path		 	= new TreePath(path);
		this.childIndices	= childIndices;
		this.children		= children;
	} // TreeModelEvent()

	/**
	 * Constructor TreeModelEvent
	 * @param source Source object
	 * @param path Path
	 */
	public TreeModelEvent(Object source, TreePath path) {
		super(source);
		this.path = path;
	} // TreeModelEvent()

	/**
	 * Constructor TreeModelEvent
	 * @param source Source object
	 * @param path Path
	 * @param childIndices Child indices
	 * @param children Children
	 */
	public TreeModelEvent(Object source, TreePath path,
						int[] childIndices, Object[] children) {
		super(source);
		this.path		 	= path;
		this.childIndices	= childIndices;
		this.children		= children;
	} // TreeModelEvent()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getChildIndices
	 * @returns child indices
	 */
	public int[] getChildIndices() {
		return childIndices;
	} // getChildIndices()

	/**
	 * getChildren
	 * @returns children
	 */
	public Object[] getChildren() {
		return children;
	} // getChildren()

	/**
	 * getPath
	 * @returns path
	 */
	public Object[] getPath() {
		return path.getPath();
	} // getPath()

	/**
	 * getTreePath
	 * @returns TreePath
	 */
	public TreePath getTreePath() {
		return path;
	} // getTreePath()

	/**
	 * String representation
	 * @returns String representation
	 */
	public String toString() {
		return getClass() + " [Source: " + getSource() + ", TreePath: " + getTreePath() +
        ", Child Indicies: " + getChildIndices() + ", Children: " + getChildren() + 
        ", Path: " + getPath() +"]";
	} // toString()


} // TreeModelEvent
