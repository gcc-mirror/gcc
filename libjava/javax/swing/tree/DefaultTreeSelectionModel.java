/* DefaultTreeSelectionModel.java --
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

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.EventListener;
import java.util.Vector;
import javax.swing.DefaultListSelectionModel;
import javax.swing.event.SwingPropertyChangeSupport;
import javax.swing.event.EventListenerList;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

/**
 * DefaultTreeSelectionModel
 * @author Andrew Selkirk
 */
public class DefaultTreeSelectionModel
  implements Cloneable, Serializable, TreeSelectionModel
{
  static final long serialVersionUID = 3288129636638950196L;

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * SELECTION_MODE_PROPERTY
	 */
	public static final String SELECTION_MODE_PROPERTY = "selectionMode";

	/**
	 * changeSupport
	 */
	protected SwingPropertyChangeSupport changeSupport;

	/**
	 * selection
	 */
	protected TreePath[] selection;

	/**
	 * listenerList
	 */
	protected EventListenerList listenerList;

	/**
	 * rowMapper
	 */
	protected transient RowMapper rowMapper;

	/**
	 * listSelectionModel
	 */
	protected DefaultListSelectionModel listSelectionModel;

	/**
	 * selectionMode
	 */
	protected int selectionMode;

	/**
	 * leadPath
	 */
	protected TreePath leadPath;

	/**
	 * leadIndex
	 */
	protected int leadIndex;

	/**
	 * leadRow
	 */
	protected int leadRow;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultTreeSelectionModel
	 */
	public DefaultTreeSelectionModel() {
		// TODO
	} // DefaultTreeSelectionModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * clone
	 * @exception CloneNotSupportedException TODO
	 * @returns Object
	 */
	public Object clone() throws CloneNotSupportedException {
		return null; // TODO
	} // clone()

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
	 * setRowMapper
	 * @param value0 TODO
	 */
	public void setRowMapper(RowMapper value0) {
		// TODO
	} // setRowMapper()

	/**
	 * getRowMapper
	 * @returns RowMapper
	 */
	public RowMapper getRowMapper() {
		return null; // TODO
	} // getRowMapper()

	/**
	 * setSelectionMode
	 * @param value0 TODO
	 */
	public void setSelectionMode(int value0) {
		// TODO
	} // setSelectionMode()

	/**
	 * getSelectionMode
	 * @returns int
	 */
	public int getSelectionMode() {
		return 0; // TODO
	} // getSelectionMode()

	/**
	 * setSelectionPath
	 * @param value0 TODO
	 */
	public void setSelectionPath(TreePath value0) {
		// TODO
	} // setSelectionPath()

	/**
	 * setSelectionPaths
	 * @param value0 TODO
	 */
	public void setSelectionPaths(TreePath[] value0) {
		// TODO
	} // setSelectionPaths()

	/**
	 * addSelectionPath
	 * @param value0 TODO
	 */
	public void addSelectionPath(TreePath value0) {
		// TODO
	} // addSelectionPath()

	/**
	 * addSelectionPaths
	 * @param value0 TODO
	 */
	public void addSelectionPaths(TreePath[] value0) {
		// TODO
	} // addSelectionPaths()

	/**
	 * removeSelectionPath
	 * @param value0 TODO
	 */
	public void removeSelectionPath(TreePath value0) {
		// TODO
	} // removeSelectionPath()

	/**
	 * removeSelectionPaths
	 * @param value0 TODO
	 */
	public void removeSelectionPaths(TreePath[] value0) {
		// TODO
	} // removeSelectionPaths()

	/**
	 * getSelectionPath
	 * @returns TreePath
	 */
	public TreePath getSelectionPath() {
		return null; // TODO
	} // getSelectionPath()

	/**
	 * getSelectionPaths
	 * @returns TreePath[]
	 */
	public TreePath[] getSelectionPaths() {
		return null; // TODO
	} // getSelectionPaths()

	/**
	 * getSelectionCount
	 * @returns int
	 */
	public int getSelectionCount() {
		return 0; // TODO
	} // getSelectionCount()

	/**
	 * isPathSelected
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isPathSelected(TreePath value0) {
		return false; // TODO
	} // isPathSelected()

	/**
	 * isSelectionEmpty
	 * @returns boolean
	 */
	public boolean isSelectionEmpty() {
		return false; // TODO
	} // isSelectionEmpty()

	/**
	 * clearSelection
	 */
	public void clearSelection() {
		// TODO
	} // clearSelection()

	/**
	 * addTreeSelectionListener
	 * @param value0 TODO
	 */
	public void addTreeSelectionListener(TreeSelectionListener value0) {
		// TODO
	} // addTreeSelectionListener()

	/**
	 * removeTreeSelectionListener
	 * @param value0 TODO
	 */
	public void removeTreeSelectionListener(TreeSelectionListener value0) {
		// TODO
	} // removeTreeSelectionListener()

	/**
	 * fireValueChanged
	 * @param value0 TODO
	 */
	protected void fireValueChanged(TreeSelectionEvent value0) {
		// TODO
	} // fireValueChanged()

	/**
	 * getListeners
	 * @param value0 TODO
	 * @returns EventListener[]
	 */
	public EventListener[] getListeners(Class value0) {
		return null; // TODO
	} // getListeners()

	/**
	 * getSelectionRows
	 * @returns int[]
	 */
	public int[] getSelectionRows() {
		return null; // TODO
	} // getSelectionRows()

	/**
	 * getMinSelectionRow
	 * @returns int
	 */
	public int getMinSelectionRow() {
		return 0; // TODO
	} // getMinSelectionRow()

	/**
	 * getMaxSelectionRow
	 * @returns int
	 */
	public int getMaxSelectionRow() {
		return 0; // TODO
	} // getMaxSelectionRow()

	/**
	 * isRowSelected
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isRowSelected(int value0) {
		return false; // TODO
	} // isRowSelected()

	/**
	 * resetRowSelection
	 */
	public void resetRowSelection() {
		// TODO
	} // resetRowSelection()

	/**
	 * getLeadSelectionRow
	 * @returns int
	 */
	public int getLeadSelectionRow() {
		return 0; // TODO
	} // getLeadSelectionRow()

	/**
	 * getLeadSelectionPath
	 * @returns TreePath
	 */
	public TreePath getLeadSelectionPath() {
		return null; // TODO
	} // getLeadSelectionPath()

	/**
	 * addPropertyChangeListener
	 * @param value0 TODO
	 */
	public synchronized void addPropertyChangeListener(PropertyChangeListener value0) {
		// TODO
	} // addPropertyChangeListener()

	/**
	 * removePropertyChangeListener
	 * @param value0 TODO
	 */
	public synchronized void removePropertyChangeListener(PropertyChangeListener value0) {
		// TODO
	} // removePropertyChangeListener()

	/**
	 * insureRowContinuity
	 */
	protected void insureRowContinuity() {
		// TODO
	} // insureRowContinuity()

	/**
	 * arePathsContiguous
	 * @param value0 TODO
	 * @returns boolean
	 */
	protected boolean arePathsContiguous(TreePath[] value0) {
		return false; // TODO
	} // arePathsContiguous()

	/**
	 * canPathsBeAdded
	 * @param value0 TODO
	 * @returns boolean
	 */
	protected boolean canPathsBeAdded(TreePath[] value0) {
		return false; // TODO
	} // canPathsBeAdded()

	/**
	 * canPathsBeRemoved
	 * @param value0 TODO
	 * @returns boolean
	 */
	protected boolean canPathsBeRemoved(TreePath[] value0) {
		return false; // TODO
	} // canPathsBeRemoved()

	/**
	 * notifyPathChange
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	protected void notifyPathChange(Vector value0, TreePath value1) {
		// TODO
	} // notifyPathChange()

	/**
	 * updateLeadIndex
	 */
	protected void updateLeadIndex() {
		// TODO
	} // updateLeadIndex()

	/**
	 * insureUniqueness
	 */
	protected void insureUniqueness() {
		// TODO
	} // insureUniqueness()


} // DefaultTreeSelectionModel
