/* ActionMap.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


/**
 * @author Andrew Selkirk
 * @author Michael Koch
 */
public class ActionMap
  implements Serializable
{
  private static final long serialVersionUID = -6277518704513986346L;

  /**
   * actionMap
   */
  private Map actionMap = new HashMap();

  /**
   * parent
   */
  private ActionMap parent;

  /**
   * Creates a new <code>ActionMap</code> instance.
   */
  public ActionMap()
  {
  }

  /**
   * Returns an action associated with an object.
   *
   * @param key the key of the enty
   *
   * @return the action associated with key, may be null
   */
  public Action get(Object key)
  {
    Object result = actionMap.get(key);

    if (result == null && parent != null)
      result = parent.get(key);

    return (Action) result;
  }

  /**
   * Puts a new <code>Action</code> into the <code>ActionMap</code>.
   * If action is null an existing entry will be removed.
   *
   * @param key the key for the entry
   * @param action the action.
   */
  public void put(Object key, Action action)
  {
    if (action == null)
      actionMap.remove(key);
    else
      actionMap.put(key, action);
  }

  /**
   * Remove an entry from the <code>ActionMap</code>.
   *
   * @param key the key of the entry to remove
   */
  public void remove(Object key)
  {
    actionMap.remove(key);
  }

  /**
   * Returns the parent of this <code>ActionMap</code>.
   *
   * @return the parent, may be null.
   */
  public ActionMap getParent()
  {
    return parent;
  }

  /**
   * Sets a parent for this <code>ActionMap</code>.
   *
   * @param parentMap the new parent
   */
  public void setParent(ActionMap parentMap)
  {
    parent = parentMap;
  }

  /**
   * Returns the number of entries in this <code>ActionMap</code>.
   *
   * @return the number of entries
   */
  public int size()
  {
    return actionMap.size();
  }

  /**
   * Clears the <code>ActionMap</code>.
   */
  public void clear()
  {
    actionMap.clear();
  }

  /**
   * Returns all keys of entries in this <code>ActionMap</code>.
   *
   * @return an array of keys
   */
  public Object[] keys()
  {
    return actionMap.keySet().toArray();
  }

  /**
   * Returns all keys of entries in this <code>ActionMap</code>
   * and all its parents.
   *
   * @return an array of keys
   */
  public Object[] allKeys()
  {
    Set set = new HashSet();

    if (parent != null)
      set.addAll(Arrays.asList(parent.allKeys()));

    set.addAll(actionMap.keySet());
    return set.toArray();
  }

  /**
   * writeObject
   *
   * @param stream the stream to write to
   *
   * @exception IOException If an error occurs
   */
  private void writeObject(ObjectOutputStream stream)
    throws IOException
  {
    // TODO
  }

  /**
   * readObject
   *
   * @param stream the stream to read from
   *
   * @exception ClassNotFoundException If the serialized class cannot be found
   * @exception IOException If an error occurs
   */
  private void readObject(ObjectInputStream stream)
    throws ClassNotFoundException, IOException
  {
    // TODO
  }
}
