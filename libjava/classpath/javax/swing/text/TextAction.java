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

import java.awt.Component;
import java.awt.KeyboardFocusManager;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.util.HashMap;
import java.util.Iterator;

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
    JTextComponent target = null;
    if (event != null)
      {
        Object source = event.getSource();
        if (source instanceof JTextComponent)
          target = (JTextComponent) source;
      }
    if (target == null)
      target = getFocusedComponent();
    return target;
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
    HashMap<Object,Action> actions = new HashMap<Object,Action>();

    for (int i = 0; i < list1.length; ++i)
      {
        Action a = list1[i];
        Object name = a.getValue(Action.NAME);
        actions.put(name != null ? name : "", a);
      }

    for (int i = 0; i < list2.length; ++i)
      {
        Action a = list2[i];
        Object name = a.getValue(Action.NAME);
        actions.put(name != null ? name : "", a);
      }
    Action[] augmented = new Action[actions.size()];
    
    int i = 0;
    for (Iterator<Action> it = actions.values().iterator(); it.hasNext(); i++)
      augmented[i] = it.next();
    return augmented;

  }

  /**
   * Returns the current focused <code>JTextComponent</code> object.
   * 
   * @return the <code>JTextComponent</code>
   */
  protected final JTextComponent getFocusedComponent()
  {
    KeyboardFocusManager kfm =
      KeyboardFocusManager.getCurrentKeyboardFocusManager();
    Component focused = kfm.getPermanentFocusOwner();
    JTextComponent textComp = null;
    if (focused instanceof JTextComponent)
      textComp = (JTextComponent) focused;
    return textComp;
  }
  
  /** Abstract helper class which implements everything needed for an
   * Action implementation in <code>DefaultEditorKit</code> which
   * does horizontal movement (and selection).  
   */
  abstract static class HorizontalMovementAction extends TextAction
  {
    int dir;
    
    HorizontalMovementAction(String name, int direction)
    {
      super(name);
      dir = direction;
    }
    
    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      try
      {
        if (t != null)
          {
            int offs
              = Utilities.getNextVisualPositionFrom(t,
                                                    t.getCaretPosition(),
                                                    dir);
              
            Caret c = t.getCaret();
            
            actionPerformedImpl(c, offs);
            
            c.setMagicCaretPosition(t.modelToView(offs).getLocation());
          }
      }
    catch(BadLocationException ble)
      {
        throw 
          (InternalError) new InternalError("Illegal offset").initCause(ble);
      }
    
    }
    
    protected abstract void actionPerformedImpl(Caret c, int offs)
      throws BadLocationException;
  }
  
  /** Abstract helper class which implements everything needed for an
   * Action implementation in <code>DefaultEditorKit</code> which
   * does vertical movement (and selection).
   */  
  abstract static class VerticalMovementAction extends TextAction
  {
    int dir;
    
    VerticalMovementAction(String name, int direction)
    {
      super(name);
      dir = direction;
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      try
        {
          if (t != null)
            {
              Caret c = t.getCaret();
              // The magic caret position may be null when the caret
              // has not moved yet.
              Point mcp = c.getMagicCaretPosition();

              int pos;
              if (mcp != null)
                {
                  mcp.y = t.modelToView(c.getDot()).y;
                  pos = t.viewToModel(mcp);
                }
              else
                pos = c.getDot();
        
              pos = Utilities.getNextVisualPositionFrom(t,
                                                        t.getCaretPosition(),
                                                        dir);
        
              if (pos > -1)
                actionPerformedImpl(c, pos);
            }
        }
      catch(BadLocationException ble) 
      {
        throw 
          (InternalError) new InternalError("Illegal offset").initCause(ble);
      }
    }
    
    protected abstract void actionPerformedImpl(Caret c, int offs)
    throws BadLocationException;
    
  }
  
  
}
