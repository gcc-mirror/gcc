/* TreeSelectionEvent.java --
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

package javax.swing.event;

// Imports
import java.util.*;
import javax.swing.tree.*;

/**
 * TreeSelectionEvent
 * @author Andrew Selkirk
 * @version	1.0
 */
public class TreeSelectionEvent extends EventObject {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * paths
	 */
	protected TreePath[] paths;

	/**
	 * areNew
	 */
	protected boolean[] areNew;

	/**
	 * oldLeadSelectionPath
	 */
	protected TreePath oldLeadSelectionPath;

	/**
	 * newLeadSelectionPath
	 */
	protected TreePath newLeadSelectionPath;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor TreeSelectionEvent
	 * @param source TODO
	 * @param paths TODO
	 * @param areNew TODO
	 * @param oldLeadSelectionPath TODO
	 * @param newLeadSelectionPath TODO
	 */
	public TreeSelectionEvent(Object source, TreePath[] paths,
				boolean[] areNew, TreePath oldLeadSelectionPath,
				TreePath newLeadSelectionPath) {
		super(source);
		this.paths					= paths;
		this.areNew					= areNew;
		this.oldLeadSelectionPath	= oldLeadSelectionPath;
		this.newLeadSelectionPath	= newLeadSelectionPath;
	} // TreeSelectionEvent()

	/**
	 * Constructor TreeSelectionEvent
	 * @param source TODO
	 * @param paths TODO
	 * @param areNew TODO
	 * @param oldLeadSelectionPath TODO
	 * @param newLeadSelectionPath TODO
	 */
	public TreeSelectionEvent(Object source, TreePath path,
				boolean isNew, TreePath oldLeadSelectionPath,
				TreePath newLeadSelectionPath) {
		super(source);
//TODO		this.paths					= new TreePath[1]{path};
//TODO		this.areNew					= new boolean[1]{isNew};
		this.oldLeadSelectionPath	= oldLeadSelectionPath;
		this.newLeadSelectionPath	= newLeadSelectionPath;
	} // TreeSelectionEvent()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getPath
	 * @returns TreePath
	 */
	public TreePath getPath() {
		return paths[0];
	} // getPath()

	/**
	 * getPaths
	 * @returns TreePath[]
	 */
	public TreePath[] getPaths() {
		return paths;
	} // getPaths()

	/**
	 * isAddedPath
	 * @returns boolean
	 */
	public boolean isAddedPath() {
		return false; // TODO
	} // isAddedPath()

	/**
	 * isAddedPath
	 * @param path TODO
	 * @returns boolean
	 */
	public boolean isAddedPath(TreePath path) {
		return false; // TODO
	} // isAddedPath()

	/**
	 * isAddedPath
	 * @param index TODO
	 * @returns boolean
	 */
	public boolean isAddedPath(int index) {
		return false; // TODO
	} // isAddedPath()

	/**
	 * getOldLeadSelectionPath
	 * @returns TreePath
	 */
	public TreePath getOldLeadSelectionPath() {
		return oldLeadSelectionPath;
	} // getOldLeadSelectionPath()

	/**
	 * getNewLeadSelectionPath
	 * @returns TreePath
	 */
	public TreePath getNewLeadSelectionPath() {
		return newLeadSelectionPath;
	} // getNewLeadSelectionPath()

	/**
	 * cloneWithSource
	 * @param source TODO
	 * @returns Object
	 */
	public Object cloneWithSource(Object source) {
		return null; // TODO
	} // cloneWithSource()


} // TreeSelectionEvent
