/* GtkCheckboxPeer.java -- Implements CheckboxPeer with GTK
   Copyright (C) 1998, 1999, 2002, 2003 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.Component;
import java.awt.Font;
import java.awt.peer.CheckboxPeer;

public class GtkCheckboxPeer extends GtkComponentPeer
  implements CheckboxPeer
{
  // Group from last time it was set.
  public GtkCheckboxGroupPeer old_group;
  // The current state of the GTK checkbox.
  private boolean currentState;  

  public native void create (GtkCheckboxGroupPeer group);
  public native void nativeSetCheckboxGroup (GtkCheckboxGroupPeer group);
  public native void connectSignals ();
  native void gtkSetFont (String name, int style, int size);
  native void gtkButtonSetLabel (String label);
  native void gtkToggleButtonSetActive (boolean is_active);

  public GtkCheckboxPeer (Checkbox c)
  {
    super (c);
  }

  // FIXME: we must be able to switch between a checkbutton and a
  // radiobutton dynamically.
  public void create ()
  {
    Checkbox checkbox = (Checkbox) awtComponent;
    CheckboxGroup g = checkbox.getCheckboxGroup ();
    old_group = GtkCheckboxGroupPeer.getCheckboxGroupPeer (g);
    create (old_group);
    gtkToggleButtonSetActive (checkbox.getState ());
    gtkButtonSetLabel (checkbox.getLabel ());
  }

  public void setState (boolean state)
  {
    if (currentState != state)
      gtkToggleButtonSetActive (state);
  }

  public void setLabel (String label)
  {
    gtkButtonSetLabel (label);
  }

  public void setCheckboxGroup (CheckboxGroup group)
  {
    GtkCheckboxGroupPeer gp
      = GtkCheckboxGroupPeer.getCheckboxGroupPeer (group);
    if (gp != old_group)
      {
	if (old_group != null)
	  old_group.remove (this);
	nativeSetCheckboxGroup (gp);
	old_group = gp;
      }
  }

  // Override the superclass postItemEvent so that the peer doesn't
  // need information that we have.
  public void postItemEvent (Object item, int stateChange)
  {
    Checkbox currentCheckBox = ((Checkbox)awtComponent);
    // A firing of the event is only desired if the state has changed due to a 
    // button press. The currentCheckBox's state must be different from the 
    // one that the stateChange is changing to. 
    // stateChange = 1 if it goes from false -> true
    // stateChange = 2 if it goes from true -> false
    if (( !currentCheckBox.getState() && stateChange == 1)
        || (currentCheckBox.getState() && stateChange == 2))
    {
      super.postItemEvent (awtComponent, stateChange);
      currentState = !currentCheckBox.getState();
      currentCheckBox.setState(currentState);
    }
  }

  public void dispose ()
  {
    // Notify the group so that the native state can be cleaned up
    // appropriately.
    if (old_group != null)
      old_group.remove (this);
    super.dispose ();
  }
}
