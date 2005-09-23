/* TreeSelectionEvent.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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
			    TreePath newLeadSelectionPath)
  {
    super(source);
    this.paths					= paths;
    this.areNew					= areNew;
    this.oldLeadSelectionPath	= oldLeadSelectionPath;
    this.newLeadSelectionPath	= newLeadSelectionPath;
  } // TreeSelectionEvent()

  /**
   * Constructor TreeSelectionEvent
   * @param source TODO
   * @param path TODO
   * @param isNew TODO
   * @param oldLeadSelectionPath TODO
   * @param newLeadSelectionPath TODO
   */
  public TreeSelectionEvent(Object source, TreePath path,
			    boolean isNew, TreePath oldLeadSelectionPath,
			    TreePath newLeadSelectionPath)
  {
    super(source);
    this.paths = new TreePath[]{path};
    this.areNew = new boolean[]{isNew};
    this.oldLeadSelectionPath	= oldLeadSelectionPath;
    this.newLeadSelectionPath	= newLeadSelectionPath;
  } // TreeSelectionEvent()


  //-------------------------------------------------------------
  // Methods ----------------------------------------------------
  //-------------------------------------------------------------

  /**
   * @returns the first path element
   */
  public TreePath getPath()
  {
    return paths[0];
  } // getPath()

  /**
   * 
   * @returns the paths with selection changed
   */
  public TreePath[] getPaths()
  {
    return (TreePath[]) paths.clone();
  } // getPaths()

  /**
   * @return true if the first path is added to the selection, false otherwise
   */
  public boolean isAddedPath()
  {
    return areNew[0];
  } // isAddedPath()

  /**
   * @param path the path to check
   * @return true if the path is added to the selection, false otherwise
   */
  public boolean isAddedPath(TreePath path)
  {
    for (int i = paths.length - 1; i >= 0; i--)
      if (paths[i].equals(path))
	return areNew[i];

    return false;
  } // isAddedPath()

  /**
   * @param index the index'th path
   * @return true if the path is added to the selection, false otherwise
   */
  public boolean isAddedPath(int index)
  {
    return areNew[index];
  } // isAddedPath()

  /**
   * @return the previous lead selection path
   */
  public TreePath getOldLeadSelectionPath()
  {
    return oldLeadSelectionPath;
  } // getOldLeadSelectionPath()

  /**
   * @returns the current lead selection path
   */
  public TreePath getNewLeadSelectionPath()
  {
    return newLeadSelectionPath;
  } // getNewLeadSelectionPath()

  /**
   * @param source the new event source
   * @return a cloned event with another event source
   */
  public Object cloneWithSource(Object source)
  {
    return new TreeSelectionEvent (source, paths, areNew,
				   oldLeadSelectionPath,
				   newLeadSelectionPath);
  } // cloneWithSource()


} // TreeSelectionEvent
