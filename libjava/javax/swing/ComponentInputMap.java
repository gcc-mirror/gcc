/* ComponentInputMap.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


/**
 * @author Andrew Selkirk
 * @author Michael Koch
 */
public class ComponentInputMap extends InputMap
{
  /**
   * The component to notify.
   */
  private JComponent component;

  /**
   * Creates <code>ComponentInputMap</code> object that notifies the given
   * component about changes to it.
   *
   * @param comp the component to notify
   *
   * @exception IllegalArgumentException if comp is null
   */
  public ComponentInputMap(JComponent comp)
  {
    if (comp == null)
      throw new IllegalArgumentException();
    
    this.component = comp;
  }

  /**
   * Puts a new entry into the <code>InputMap</code>.
   * If actionMapKey is null an existing entry will be removed.
   *
   * @param keystroke the keystroke for the entry
   * @param actionMapKey the action.
   */
  public void put(KeyStroke keystroke, Object value)
  {
    super.put(keystroke, value);
    // FIXME: Notify component.
  }

  /**
   * Clears the <code>InputMap</code>.
   */
  public void clear()
  {
    super.clear();
    // FIXME: Notify component.
  }

  /**
   * Remove an entry from the <code>InputMap</code>.
   *
   * @param key the key of the entry to remove
   */
  public void remove(KeyStroke keystroke)
  {
    super.remove(keystroke);
    // FIXME: Notify component.
  }

  /**
   * Sets a parent for this <code>ComponentInputMap</code>.
   *
   * @param parentMap the new parent
   *
   * @exception IllegalArgument if parentMap is not a
   * <code>ComponentInputMap</code> or not associated with the same component
   */
  public void setParent(InputMap parentMap)
  {
    if (! (parentMap instanceof ComponentInputMap))
      throw new IllegalArgumentException();

    if (((ComponentInputMap) parentMap).getComponent() != component)
      throw new IllegalArgumentException();
   
    super.setParent(parentMap);
    // FIXME: Notify component.
  }

  /**
   * Returns the component to notify about changes.
   *
   * @return a <code>JComponent</code> object
   */
  public JComponent getComponent()
  {
    return component;
  }
}
