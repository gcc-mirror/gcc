/* StateEditable.java -- Interface for collaborating with StateEdit.
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


/**
 * The interface for objects whose state can be undone or redone by a
 * {@link StateEdit} action.
 *
 * <p>The following example shows how to write a class that implements
 * this interface.
 *
 * <pre> class Foo
 *   implements StateEditable
 * {
 *   private String name;
 *
 *   public void setName(String n) { name = n; }
 *
 *   public void restoreState(Hashtable h)
 *   {
 *     if (h.containsKey("name"))
 *       setName((String) h.get("name"));
 *   }
 *
 *   public void storeState(Hashtable s)
 *   {
 *     s.put("name", name);
 *   }
 * }</pre>
 *
 * @see StateEdit
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public interface StateEditable
{
  /**
   * The ID of the Java source file in Sun&#x2019;s Revision Control
   * System (RCS).  This certainly should not be part of the API
   * specification. But in order to be API-compatible with
   * Sun&#x2019;s reference implementation, GNU Classpath also has to
   * provide this field. However, we do not try to match its value.
   */
  String RCSID = "";


  /**
   * Performs an edit action, taking any editable state information
   * from the specified hash table.
   *
   * <p><b>Note to implementors of this interface:</b> To increase
   * efficiency, the <code>StateEdit</code> class {@linkplan
   * StateEdit#removeRedundantState() removes redundant state
   * information}. Therefore, implementations of this interface must be
   * prepared for the case where certain keys were stored into the
   * table by {@link #storeState}, but are not present anymore
   * when the <code>restoreState</code> method gets called.
   *
   * @param state a hash table containing the relevant state
   * information.
   */
  void restoreState(Hashtable state);


  /**
   * Stores any editable state information into the specified hash
   * table.
   *
   * @param state a hash table for storing relevant state
   * information.
   */
  void storeState(Hashtable state);
}
