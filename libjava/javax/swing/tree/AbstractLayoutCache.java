/* AbstractLayoutCache.java --
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

import java.awt.Rectangle;
import java.util.Enumeration;
import javax.swing.event.TreeModelEvent;

/**
 * AbstractLayoutCache
 * @author Andrew Selkirk
 */
public abstract class AbstractLayoutCache implements RowMapper
{

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * NodeDimensions
	 */
	public abstract static class NodeDimensions {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor NodeDimensions
		 */
		public NodeDimensions() {
			// TODO
		} // NodeDimensions()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getNodeDimensions
		 * @param value0 TODO
		 * @param value1 TODO
		 * @param value2 TODO
		 * @param value3 TODO
		 * @param value4 TODO
		 * @returns Rectangle
		 */
		public abstract Rectangle getNodeDimensions(Object value0, int value1, int value2, boolean value3, Rectangle value4);


	} // NodeDimensions


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * nodeDimensions
	 */
	protected NodeDimensions nodeDimensions;

	/**
	 * treeModel
	 */
	protected TreeModel treeModel;

	/**
	 * treeSelectionModel
	 */
	protected TreeSelectionModel treeSelectionModel;

	/**
	 * rootVisible
	 */
	protected boolean rootVisible;

	/**
	 * rowHeight
	 */
	protected int rowHeight;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor AbstractLayoutCache
	 */
	public AbstractLayoutCache() {
		// TODO
	} // AbstractLayoutCache()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * setNodeDimensions
	 * @param value0 TODO
	 */
	public void setNodeDimensions(NodeDimensions value0) {
		// TODO
	} // setNodeDimensions()

	/**
	 * getNodeDimensions
	 * @returns NodeDimensions
	 */
	public NodeDimensions getNodeDimensions() {
		return null; // TODO
	} // getNodeDimensions()

	/**
	 * getNodeDimensions
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 * @param value4 TODO
	 * @returns Rectangle
	 */
	protected Rectangle getNodeDimensions(Object value0, int value1, int value2, boolean value3, Rectangle value4) {
		return null; // TODO
	} // getNodeDimensions()

	/**
	 * setModel
	 * @param value0 TODO
	 */
	public void setModel(TreeModel value0) {
		// TODO
	} // setModel()

	/**
	 * getModel
	 * @returns TreeModel
	 */
	public TreeModel getModel() {
		return null; // TODO
	} // getModel()

	/**
	 * setRootVisible
	 * @param value0 TODO
	 */
	public void setRootVisible(boolean value0) {
		// TODO
	} // setRootVisible()

	/**
	 * isRootVisible
	 * @returns boolean
	 */
	public boolean isRootVisible() {
		return false; // TODO
	} // isRootVisible()

	/**
	 * setRowHeight
	 * @param value0 TODO
	 */
	public void setRowHeight(int value0) {
		// TODO
	} // setRowHeight()

	/**
	 * getRowHeight
	 * @returns int
	 */
	public int getRowHeight() {
		return 0; // TODO
	} // getRowHeight()

	/**
	 * setSelectionModel
	 * @param value0 TODO
	 */
	public void setSelectionModel(TreeSelectionModel value0) {
		// TODO
	} // setSelectionModel()

	/**
	 * getSelectionModel
	 * @returns TreeSelectionModel
	 */
	public TreeSelectionModel getSelectionModel() {
		return null; // TODO
	} // getSelectionModel()

	/**
	 * getPreferredHeight
	 * @returns int
	 */
	public int getPreferredHeight() {
		return 0; // TODO
	} // getPreferredHeight()

	/**
	 * getPreferredWidth
	 * @param value0 TODO
	 * @returns int
	 */
	public int getPreferredWidth(Rectangle value0) {
		return 0; // TODO
	} // getPreferredWidth()

	/**
	 * isExpanded
	 * @param value0 TODO
	 * @returns boolean
	 */
	public abstract boolean isExpanded(TreePath value0);

	/**
	 * getBounds
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns Rectangle
	 */
	public abstract Rectangle getBounds(TreePath value0, Rectangle value1);

	/**
	 * getPathForRow
	 * @param value0 TODO
	 * @returns TreePath
	 */
	public abstract TreePath getPathForRow(int value0);

	/**
	 * getRowForPath
	 * @param value0 TODO
	 * @returns int
	 */
	public abstract int getRowForPath(TreePath value0);

	/**
	 * getPathClosestTo
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns TreePath
	 */
	public abstract TreePath getPathClosestTo(int value0, int value1);

	/**
	 * getVisiblePathsFrom
	 * @param value0 TODO
	 * @returns Enumeration
	 */
	public abstract Enumeration getVisiblePathsFrom(TreePath value0);

	/**
	 * getVisibleChildCount
	 * @param value0 TODO
	 * @returns int
	 */
	public abstract int getVisibleChildCount(TreePath value0);

	/**
	 * setExpandedState
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public abstract void setExpandedState(TreePath value0, boolean value1);

	/**
	 * getExpandedState
	 * @param value0 TODO
	 * @returns boolean
	 */
	public abstract boolean getExpandedState(TreePath value0);

	/**
	 * getRowCount
	 * @returns int
	 */
	public abstract int getRowCount();

	/**
	 * invalidateSizes
	 */
	public abstract void invalidateSizes();

	/**
	 * invalidatePathBounds
	 * @param value0 TODO
	 */
	public abstract void invalidatePathBounds(TreePath value0);

	/**
	 * treeNodesChanged
	 * @param value0 TODO
	 */
	public abstract void treeNodesChanged(TreeModelEvent value0);

	/**
	 * treeNodesInserted
	 * @param value0 TODO
	 */
	public abstract void treeNodesInserted(TreeModelEvent value0);

	/**
	 * treeNodesRemoved
	 * @param value0 TODO
	 */
	public abstract void treeNodesRemoved(TreeModelEvent value0);

	/**
	 * treeStructureChanged
	 * @param value0 TODO
	 */
	public abstract void treeStructureChanged(TreeModelEvent value0);

	/**
	 * getRowsForPaths
	 * @param value0 TODO
	 * @returns int[]
	 */
	public int[] getRowsForPaths(TreePath[] value0) {
		return null; // TODO
	} // getRowsForPaths()

	/**
	 * isFixedRowHeight
	 * @returns boolean
	 */
	protected boolean isFixedRowHeight() {
		return false; // TODO
	} // isFixedRowHeight()


} // AbstractLayoutCache
