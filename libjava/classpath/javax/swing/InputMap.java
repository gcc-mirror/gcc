/* InputMap.java --
   Copyright (C) 2002, 2004, 2006, Free Software Foundation, Inc.

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

package javax.swing;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Maps {@link KeyStroke}s to arbitrary objects, usually Strings. This
 * is used in combination with {@link ActionMap}s.
 *
 * If a component receives an input event, this is looked up in
 * the component's <code>InputMap</code>. The result is an object which
 * serves as a key to the component's <code>ActionMap</code>. Finally
 * the <code>Action</code> that is stored is executed.
 *
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
   * Storage for the KeyStroke --> Object mappings.
   */
  private Map inputMap;

  /**
   * An optional parent map.
   */
  private InputMap parent;

  /**
   * Creates a new <code>InputMap</code> instance.  This default instance
   * contains no mappings and has no parent.
   */
  public InputMap()
  {
    // nothing to do
  }

  /**
   * Returns the binding for the specified keystroke, if there is one.
   *
   * @param keystroke the key of the entry (<code>null</code> is ignored).
   *
   * @return The binding associated with the specified keystroke (or 
   *     <code>null</code>).
   */
  public Object get(KeyStroke keystroke)
  {
    Object result = null;
    if (inputMap != null)
      result = inputMap.get(keystroke);

    if (result == null && parent != null)
      result = parent.get(keystroke);
    return result;
  }

  /**
   * Puts a new entry into the <code>InputMap</code>.  If 
   * <code>actionMapKey</code> is <code>null</code> any existing entry will be 
   * removed.
   *
   * @param keystroke the keystroke for the entry (<code>null</code> is 
   *     ignored).
   * @param actionMapKey the action (<code>null</code> permitted).
   */
  public void put(KeyStroke keystroke, Object actionMapKey)
  {
    if (keystroke == null)
      return;
    if (inputMap == null)
      inputMap = new HashMap();
    if (actionMapKey == null)
      inputMap.remove(keystroke);
    else
      inputMap.put(keystroke, actionMapKey);
  }

  /**
   * Removes an entry from this <code>InputMap</code>.  Note that this will
   * not remove any entry from the parent map, if there is one.
   *
   * @param keystroke the key of the entry to remove (<code>null</code> is 
   *     ignored).
   */
  public void remove(KeyStroke keystroke)
  {
    if (inputMap != null)
      inputMap.remove(keystroke);
  }

  /**
   * Returns the parent of this <code>InputMap</code>.  The default value
   * is <code>null</code>.
   *
   * @return The parent map (possibly <code>null</code>).
   * 
   * @see #setParent(InputMap)
   */
  public InputMap getParent()
  {
    return parent;
  }

  /**
   * Sets a parent for this <code>InputMap</code>.  If a parent is specified,
   * the {@link #get(KeyStroke)} method will look in the parent if it cannot
   * find an entry in this map.
   *
   * @param parentMap the new parent (<code>null</code> permitted).
   * 
   * @see #getParent()
   */
  public void setParent(InputMap parentMap)
  {
    parent = parentMap;
  }

  /**
   * Returns the number of entries in this <code>InputMap</code>.  This count 
   * does not include any entries from the parent map, if there is one.
   *
   * @return The number of entries.
   */
  public int size()
  {
    int result = 0;
    if (inputMap != null)
      result = inputMap.size();
    return result;
  }

  /**
   * Clears the entries from this <code>InputMap</code>.  The parent map, if
   * there is one, is not cleared.
   */
  public void clear()
  {
    if (inputMap != null)
      inputMap.clear();
  }

  /**
   * Returns all keys of entries in this <code>InputMap</code>.  This does not
   * include keys defined in the parent, if there is one (use the 
   * {@link #allKeys()} method for that case).
   * <br><br>
   * Following the behaviour of the reference implementation, this method will
   * return <code>null</code> when no entries have been added to the map, 
   * and a zero length array if entries have been added but subsequently 
   * removed (or cleared) from the map.
   *
   * @return An array of keys (may be <code>null</code> or have zero length).
   */
  public KeyStroke[] keys()
  {
    if (inputMap != null)
      {
        KeyStroke[] array = new KeyStroke[size()];
        return (KeyStroke[]) inputMap.keySet().toArray(array);
      }
    return null;
  }

  /**
   * Returns all keys of entries in this <code>InputMap</code> and all its 
   * parents.
   *
   * @return An array of keys (may be <code>null</code> or have zero length).
   */
  public KeyStroke[] allKeys()
  {
    Set set = new HashSet();

    if (parent != null)
      {
        Object[] parentKeys = parent.allKeys();
        if (parentKeys != null)
          set.addAll(Arrays.asList(parentKeys));
      }
    if (inputMap != null)
      set.addAll(inputMap.keySet());
    if (set.size() == 0)
      return null;    
    KeyStroke[] array = new KeyStroke[set.size()];
    return (KeyStroke[]) set.toArray(array);
  }

}
