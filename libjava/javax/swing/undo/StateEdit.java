/* StateEdit.java --
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

package javax.swing.undo;

// Imports
import java.util.*;

/**
 * StateEdit
 * @author	Andrew Selkirk
 */
public class StateEdit extends AbstractUndoableEdit
{

  //-------------------------------------------------------------
  // Variables --------------------------------------------------
  //-------------------------------------------------------------

  /**
   * RCSID
   */
  protected static final String RCSID = ""; // TODO

  /**
   * object
   */
  protected StateEditable object;

  /**
   * preState
   */
  protected Hashtable preState;

  /**
   * postState
   */
  protected Hashtable postState;

  /**
   * undoRedoName
   */
  protected String undoRedoName;


  //-------------------------------------------------------------
  // Initialization ---------------------------------------------
  //-------------------------------------------------------------

  /**
   * Constructor StateEdit
   * @param obj Object to edit
   */
  public StateEdit(StateEditable obj)
  {
    init(obj, null);
  }

  /**
   * Constructor StateEdit
   * @param obj Object to edit
   * @param name Presentation name
   */
  public StateEdit(StateEditable obj, String name)
  {
    init(obj, name);
  }


  //-------------------------------------------------------------
  // Methods ----------------------------------------------------
  //-------------------------------------------------------------

  /**
   * Initialize this object.
   * @param obj Object to edit
   * @param name Presentation name
   */
  protected void init(StateEditable obj, String name)
  {
    object = obj;
    undoRedoName = name;
    preState = new Hashtable();
    postState = new Hashtable();
    obj.storeState(preState);
  }

  /**
   * Indicate that all edits are finished, and update this object
   * with final state.
   */
  public void end()
  {
    object.storeState(postState);
    removeRedundantState();
  }

  /**
   * Undo this edit by applying the initial state to the edited object.
   */
  public void undo()
  {
    object.restoreState(preState);
  }

  /**
   * Undo this edit by applying the final state to the edited object.
   */
  public void redo()
  {
    object.restoreState(postState);
  }

  /**
   * Return the presentation name of this object.
   * @returns The name, or null if not set
   */
  public String getPresentationName()
  {
    return undoRedoName;
  }

  /**
   * removeRedundantState
   */
  protected void removeRedundantState()
  {
    Iterator i = preState.keySet().iterator();
    while (i.hasNext())
      {
	Object key = i.next();
	if (postState.containsKey(key))
	  {
	    if (preState.get(key).equals(postState.get(key)))
	      {
		i.remove();
		postState.remove(key);
	      }
	  }
      }
  }
}
