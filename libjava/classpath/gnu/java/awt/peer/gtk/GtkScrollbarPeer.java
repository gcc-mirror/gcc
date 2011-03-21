/* GtkScrollbarPeer.java -- Implements ScrollbarPeer with GTK+
   Copyright (C) 1998, 1999, 2005, 2006  Free Software Foundation, Inc.

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

import java.awt.Scrollbar;
import java.awt.event.AdjustmentEvent;
import java.awt.peer.ScrollbarPeer;

public class GtkScrollbarPeer extends GtkComponentPeer
    implements ScrollbarPeer
{
  void create ()
  {
    Scrollbar sb = (Scrollbar) awtComponent;

    create (sb.getOrientation (), sb.getValue (),
            sb.getMinimum (), sb.getMaximum (),
            sb.getUnitIncrement (), sb.getBlockIncrement (),
            sb.getVisibleAmount ());
  }

  native void create (int orientation, int value,
                      int min, int max, int stepIncr, int pageIncr,
                      int visibleAmount);

  native void connectSignals ();

  public GtkScrollbarPeer (Scrollbar s)
  {
    super (s);
  }

  public native void setLineIncrement(int amount);
  public native void setPageIncrement(int amount);

  public void setValues(int value, int visible, int min, int max)
  {
    Scrollbar sb = (Scrollbar) awtComponent;
    if (!sb.getValueIsAdjusting())
      setBarValues(value, visible, min, max);
  }

  private native void setBarValues(int value, int visible, int min, int max);

  /**
   * Called from the native site when the scrollbar changed.
   * Posts a "user generated" AdjustmentEvent to the queue.
   */
  protected void postAdjustmentEvent (int type, int value)
  {
    Scrollbar bar = (Scrollbar) awtComponent;
    q().postEvent(new AdjustmentEvent(bar,
                                      AdjustmentEvent.ADJUSTMENT_VALUE_CHANGED,
                                      type, value, true));
  }
}
