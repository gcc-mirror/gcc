/* InputMap.java --
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
 *
 * @since 1.3
 */
public class InputMap
  implements Serializable
{
  private static final long serialVersionUID = -5429059542008604257L;

  /**
   * inputMap
   */
  private Map inputMap = new HashMap();

  /**
   * parent
   */
  private InputMap parent;

  /**
   * Creates a new <code>InputMap</code> instance.
   */
  public InputMap()
  {
    // TODO
  }

  /**
   * Returns the binding for keystroke.
   *
   * @param key the key of the enty
   *
   * @return the binding associated with keystroke may be null
   */
  public Object get(KeyStroke keystroke)
  {
    Object result = inputMap.get(keystroke);

    if (result == null && parent != null)
      result = parent.get(keystroke);
    return result;
  }

  /**
   * Puts a new entry into the <code>InputMap</code>.
   * If actionMapKey is null an existing entry will be removed.
   *
   * @param keystroke the keystroke for the entry
   * @param actionMapKey the action.
   */
  public void put(KeyStroke keystroke, Object actionMapKey)
  {
    if (actionMapKey == null)
      inputMap.remove(keystroke);
    else
      inputMap.put(keystroke, actionMapKey);
  }

  /**
   * Remove an entry from the <code>InputMap</code>.
   *
   * @param key the key of the entry to remove
   */
  public void remove(KeyStroke keystroke)
  {
    inputMap.remove(keystroke);
  }

  /**
   * Returns the parent of this <code>InputMap</code>.
   *
   * @return the parent, may be null.
   */
  public InputMap getParent()
  {
    return parent;
  }

  /**
   * Sets a parent for this <code>InputMap</code>.
   *
   * @param parentMap the new parent
   */
  public void setParent(InputMap parentMap)
  {
    parent = parentMap;
  }

  /**
   * Returns the number of entries in this <code>InputMap</code>.
   *
   * @return the number of entries
   */
  public int size()
  {
    return inputMap.size();
  }

  /**
   * Clears the <code>InputMap</code>.
   */
  public void clear()
  {
    inputMap.clear();
  }

  /**
   * Returns all keys of entries in this <code>InputMap</code>.
   *
   * @return an array of keys
   */
  public KeyStroke[] keys()
  {
    KeyStroke[] array = new KeyStroke[size()];
    return (KeyStroke[]) inputMap.keySet().toArray(array);
  }

  /**
   * Returns all keys of entries in this <code>InputMap</code>
   * and all its parents.
   *
   * @return an array of keys
   */
  public KeyStroke[] allKeys()
  {
    Set set = new HashSet();

    if (parent != null)
      set.addAll(Arrays.asList(parent.allKeys()));

    set.addAll(inputMap.keySet());
    KeyStroke[] array = new KeyStroke[size()];
    return (KeyStroke[]) set.toArray(array);
  }

  /**
   * writeObject
   *
   * @param stream the stream to write to
   *
   * @exception IOException If an error occurs
   */
  private void writeObject(ObjectOutputStream stream) throws IOException
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
