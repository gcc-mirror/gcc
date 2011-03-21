/* GtkCheckboxPeer.java -- Implements CheckboxPeer with GTK
   Copyright (C) 1998, 1999, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.event.ItemEvent;
import java.awt.peer.CheckboxPeer;
import java.util.WeakHashMap;

/**
 * This class wraps either a GtkCheckButton or a GtkOptionButton
 * depending on if this peer's owner belongs to a CheckboxGroup.
 */
public class GtkCheckboxPeer extends GtkComponentPeer
  implements CheckboxPeer
{
  // The CheckboxGroup to which this GtkCheckboxPeer's owner belongs.
  public CheckboxGroup current_group;
  // The current state of the GTK checkbox.
  private boolean currentState;

  // A map from CheckboxGroup to GSList* GTK option group pointer.
  private static WeakHashMap<CheckboxGroup,Long> groupMap
    = new WeakHashMap<CheckboxGroup,Long>();

  public native void createCheckButton ();
  public native void createRadioButton (long groupPointer);

  public native void addToGroup (long groupPointer);
  public native void removeFromGroup ();
  public native void switchToGroup (long groupPointer);

  public native void connectSignals ();

  /**
   * Overridden to set Font of label inside button.
   */
  protected native void gtkWidgetModifyFont(String name, int style, int size);
  native void gtkButtonSetLabel (String label);
  native void gtkToggleButtonSetActive (boolean is_active);

  public GtkCheckboxPeer (Checkbox c)
  {
    super (c);
  }

  public void create ()
  {
    Checkbox checkbox = (Checkbox) awtComponent;
    current_group = checkbox.getCheckboxGroup ();
    if (current_group == null)
      {
        // Initially we're not part of a group so we're backed by a
        // GtkCheckButton.
        createCheckButton();
      }
    else
      {
        // Initially we're part of a group.

        // See if this group is already stored in our map.
        Long groupPointer = null;
        synchronized (groupMap)
        {
          groupPointer = groupMap.get(current_group);
        }

        if (groupPointer == null)
          {
            // We don't know about this group.  Create a new native
            // group pointer for this group and store it in our map.
           createRadioButton(0);
          }
        else
          {
            // We already know about this group.  Pass the
            // corresponding native group pointer value to the native
            // create method.
            createRadioButton(groupPointer.longValue());
          }
      }
    currentState = checkbox.getState();
    gtkToggleButtonSetActive(currentState);

    String label = checkbox.getLabel();
    if (label != null)
      gtkButtonSetLabel(label);
  }

  /**
   * Sets native GtkCheckButton is state is different from current
   * state.  Will set currentState to state to prevent posting an
   * event since events should only be posted for user initiated
   * clicks on the GtkCheckButton.
   */
  public synchronized void setState (boolean state)
  {
    if (currentState != state)
      {
        currentState = state;
        gtkToggleButtonSetActive(state);
      }
  }

  public void setLabel (String label)
  {
    gtkButtonSetLabel (label);
  }

  public void setCheckboxGroup (CheckboxGroup group)
  {
    if (current_group == null && group != null)
      {
        // This peer's owner is currently not in a group, and now
        // we're adding it to a group.  This means that the backing
        // GtkWidget will change from a GtkCheckButton to a
        // GtkRadioButton.

        current_group = group;

        // See if the new group is already stored in our map.
        Long groupPointer = null;
        synchronized (groupMap)
        {
          groupPointer = groupMap.get(current_group);
        }

        if (groupPointer == null)
          {
            // We don't know about this group.  Create a new native
            // group pointer for this group and store it in our map.
            addToGroup(0);
          }
        else
          {
            // We already know about this group.  Pass the
            // corresponding native group pointer value to the native
            // create method.
            addToGroup(groupPointer.longValue());
          }
      }
    else if (current_group != null && group == null)
      {
        // This peer's owner is currently in a group, and now we're
        // removing it from a group.  This means that the backing
        // GtkWidget will change from a GtkRadioButton to a
        // GtkCheckButton.
        removeFromGroup();
        current_group = null;
      }
    else if (current_group == null && group == null)
      {
        // This peer's owner is currently not in a group, and we're
        // not adding it to a group, so simply return.
        return;
      }
    else if (current_group != group)
      {
        // This peer's owner is currently in a group, and now we're
        // putting it in another group.  This means that we must
        // remove the backing GtkRadioButton from one group and add it
        // to the other group.

        current_group = group;

        // See if the new group is already stored in our map.
        Long groupPointer = null;
        synchronized (groupMap)
        {
          groupPointer = groupMap.get(current_group);
        }

        if (groupPointer == null)
          {
            // We don't know about this group.  Create a new native
            // group pointer for this group and store it in our map.
            switchToGroup(0);
          }
        else
          {
            // We already know about this group.  Pass the
            // corresponding native group pointer value to the native
            // create method.
            switchToGroup(groupPointer.longValue());
          }
      }
  }

  // Override the superclass postItemEvent so that the peer doesn't
  // need information that we have.
  // called back by native side: item_toggled_cb
  public synchronized void postItemEvent(Object item, boolean state)
  {
    // Only fire event is state actually changed.
    if (currentState != state)
      {
        currentState = state;
        super.postItemEvent(awtComponent,
                            state ? ItemEvent.SELECTED : ItemEvent.DESELECTED);
      }
  }

  public void addToGroupMap(long groupPointer)
  {
    synchronized (groupMap)
    {
      groupMap.put(current_group, new Long (groupPointer));
    }
  }

  public void dispose ()
  {
    groupMap.clear();
    current_group = null;
    currentState = false;
    super.dispose ();
  }
}
