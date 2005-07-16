/* TextAction.java --
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


package javax.swing.text;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashSet;

import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * TextAction
 * @author Andrew Selkirk
 */
public abstract class TextAction extends AbstractAction
{
  /**
   * Constructor TextAction
   * @param name TODO
   */
  public TextAction(String name)
  {
    super(name);
  }

  /**
   * Returns the <code>JTextComponent</code> object associated with the given
   * <code>ActionEvent</code>. If the source of the event is not a
   * <code>JTextComponent</code> the currently focused text component is returned.
   * 
   * @param event the action event
   * 
   * @return the <code>JTextComponent</code>
   */
  protected final JTextComponent getTextComponent(ActionEvent event)
  {
    if (event.getSource() instanceof JTextComponent)
      return (JTextComponent) event.getSource();

    return getFocusedComponent();
  }

  /**
   * Creates a new array of <code>Action</code> containing both given arrays.
   * 
   * @param list1 the first action array
   * @param list2 the second action array
   *
   * @return the augmented array of actions
   */
  public static final Action[] augmentList(Action[] list1, Action[] list2)
  {
    HashSet actionSet = new HashSet();

    for (int i = 0; i < list1.length; ++i)
      actionSet.add(list1[i]);

    for (int i = 0; i < list2.length; ++i)
      actionSet.add(list2[i]);

    ArrayList list = new ArrayList(actionSet);
    return (Action[]) list.toArray(new Action[actionSet.size()]);
  }

  /**
   * Returns the current focused <code>JTextComponent</code> object.
   * 
   * @return the <code>JTextComponent</code>
   */
  protected final JTextComponent getFocusedComponent()
  {
    return null; // TODO
  }
}
