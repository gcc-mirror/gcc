/* StateEdit.java -- UndoableEdit for StateEditable implementations.
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

import java.util.Hashtable;
import java.util.Iterator;

/**
 * A helper class, making it easy to support undo and redo.
 *
 * <p>The following example shows how to use this class.</p>
 *
 * <pre>
 * Foo foo; // class Foo implements {@link StateEditable}
 * StateEdit edit;
 *
 * edit = new StateEdit(foo, "Name Change");
 * foo.setName("Jane Doe");
 * edit.end();
 * undoManager.addEdit(edit);
 * </pre>
 *
 * <p>If <code>Foo</code>&#x2019;s implementation of {@link
 * StateEditable} considers the name as part of the editable state,
 * the user can now choose &#x201c;Undo Name Change&#x201d; or
 * &#x201c;Redo Name Change&#x201d; from the respective menu. No
 * further undo support is needed from the application.</p>
 *
 * <p>The following explains what happens in the example.</p>
 *
 * <ol>
 * <li>When a <code>StateEdit</code> is created, the associated
 *     {@link StateEditable} gets asked to store its state into a hash
 *     table, {@link #preState}.</li>
 * <li>The application will now perform some changes to the edited
 *     object. This typically happens by invoking methods on the edited
 *     object.</li>
 * <li>The editing phase is terminated by invoking the {@link #end()}
 *     method of the <code>StateEdit</code>. The <code>end()</code> method
 *     does two things.
 *
 *     <ul>
 *     <li>The edited object receives a second request for storing
 *         its state.  This time, it will use a different hash table, {@link
 *         #postState}.</li>
 *     <li>To increase efficiency, the <code>StateEdit</code> now removes
 *         any entries from {@link #preState} and {@link #postState} that have
 *         the same key, and whose values are equal. Equality is determined
 *         by invoking the <code>equals</code> method inherited from
 *         {@link java.lang.Object}.</li>
 *     </ul></li>
 * <li>When the user later chooses to undo the <code>StateEdit</code>,
 * the edited object is asked to {@linkplain StateEditable#restoreState
 * restore its state} from the {@link #preState} table.  Similarly,
 * when the user chooses to <i>redo</i> the <code>StateEdit</code>,
 * the edited object gets asked to restore its state from the {@link
 * #postState}.</li>
 * </ol>
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class StateEdit
  extends AbstractUndoableEdit
{
  /**
   * The ID of the Java source file in Sun&#x2019;s Revision Control
   * System (RCS).  This certainly should not be part of the API
   * specification. But in order to be API-compatible with
   * Sun&#x2019;s reference implementation, GNU Classpath also has to
   * provide this field. However, we do not try to match its value.
   */
  protected static final String RCSID = "";


  /**
   * The object which is being edited by this <code>StateEdit</code>.
   */
  protected StateEditable object;


  /**
   * The state of <code>object</code> at the time of constructing
   * this <code>StateEdit</code>.
   */
  protected Hashtable preState;


  /**
   * The state of <code>object</code> at the time when {@link #end()}
   * was called.
   */
  protected Hashtable postState;


  /**
   * A human-readable name for this edit action.
   */
  protected String undoRedoName;


  /**
   * Constructs a <code>StateEdit</code>, specifying the object whose
   * state is being edited.
   *
   * @param obj the object whose state is being edited by this
   * <code>StateEdit</code>.
   */
  public StateEdit(StateEditable obj)
  {
    init(obj, null);
  }


  /**
   * Constructs a <code>StateEdit</code>, specifying the object whose
   * state is being edited.
   *
   * @param obj the object whose state is being edited by this
   * <code>StateEdit</code>.
   *
   * @param name the human-readable name of the editing action.
   */
  public StateEdit(StateEditable obj, String name)
  {
    init(obj, name);
  }


  /**
   * Initializes this <code>StateEdit</code>. The edited object will
   * be asked to store its current state into {@link #preState}.
   *
   * @param obj the object being edited.
   *
   * @param name the human-readable name of the editing action.
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
   * Informs this <code>StateEdit</code> that all edits are finished.
   * The edited object will be asked to store its state into {@link
   * #postState}, and any redundant entries will get removed from
   * {@link #preState} and {@link #postState}.
   */
  public void end()
  {
    object.storeState(postState);
    removeRedundantState();
  }


  /**
   * Undoes this edit operation. The edited object will be asked to
   * {@linkplain StateEditable#restoreState restore its state} from
   * {@link #preState}.
   *
   * @throws CannotUndoException if {@link #canUndo()} returns
   * <code>false</code>, for example because this action has already
   * been undone.
   */
  public void undo()
  {
    super.undo();
    object.restoreState(preState);
  }


  /**
   * Redoes this edit operation. The edited object will be asked to
   * {@linkplain StateEditable#restoreState restore its state} from
   * {@link #postState}.
   *
   * @throws CannotRedoException if {@link #canRedo()} returns
   * <code>false</code>, for example because this action has not yet
   * been undone.
   */
  public void redo()
  {
    super.redo();
    object.restoreState(postState);
  }


  /**
   * Returns a human-readable, localized name that describes this
   * editing action and can be displayed to the user.
   *
   * @return the name, or <code>null</code> if no presentation
   * name is available.
   */
  public String getPresentationName()
  {
    return undoRedoName;
  }


  /**
   * Removes all redundant entries from the pre- and post-edit state
   * hash tables. An entry is considered redundant if it is present
   * both before and after the edit, and if the two values are equal.
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
