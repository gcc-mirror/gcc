/* GtkGenericPeer.java - Has a hashcode.  Yuck.
   Copyright (C) 1998, 1999, 2002, 2006 Free Software Foundation, Inc.

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

import java.awt.EventQueue;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;

import gnu.classpath.Pointer;

public class GtkGenericPeer
{
  // Used by Native State Association (NSA) functions to map
  // gtk_widget to peer object.
  final int native_state = getUniqueInteger ();

  // Next native state value we will assign.
  private static int next_native_state = 0;

  // The widget or other java-side object we wrap.
  protected final Object awtWidget;

  /**
   * The pointer to the native GTK widget.
   *
   * This field is manipulated by native code. Don't change or remove
   * without adjusting the native code.
   */
  private Pointer widget;

  /**
   * The pointer to the global reference to this object. The native
   * code creates a JNI global reference of the peer object to be able
   * to pass it to the event callbacks. It gets stored here, so that
   * we can later delete it in the dispose() method.
   *
   * This field is manipulated by native code. Don't change or remove
   * without adjusting the native code.
   */
  private Pointer globalRef;

  /**
   * We initialize the field IDs that are used by native code here because
   * these remain valid until a class gets unloaded.
   */
  static
  {
    GtkToolkit.initializeGlobalIDs();
    initIDs();
  }

  /**
   * Initializes the field IDs that are used by the native code.
   */
  private static native void initIDs();

  /**
   * Dispose of our native state.  Calls gtk_widget_destroy on the
   * native widget and removes the awtWidget from the native state
   * tables. Should be overridden by subclasses if this is not (all)
   * that needs to be done.
   */
  public native void dispose ();

  static EventQueue q ()
  {
    return Toolkit.getDefaultToolkit ().getSystemEventQueue ();
  }

  protected GtkGenericPeer (Object awtWidget)
  {
    this.awtWidget = awtWidget;
  }

  protected void postActionEvent (String command, int mods)
  {
    q().postEvent (new ActionEvent (awtWidget, ActionEvent.ACTION_PERFORMED,
                                  command, mods));
  }

  // Return a unique integer for use in the native state mapping
  // code.  We can't use a hash code since that is not guaranteed to
  // be unique.
  static synchronized int getUniqueInteger ()
  {
    // Let's assume this will never wrap.
    return next_native_state++;
  }

  /**
   * Helper method to set Font for Gtk Widget.
   */
  protected void gtkWidgetModifyFont(Font f)
  {
    gtkWidgetModifyFont(f.getName(), f.getStyle(), f.getSize());
  }

  /**
   * Sets font for this Gtk Widget. Should be overridden by peers which
   * are composed of different widgets or are contained in bins.
   */
  protected native void gtkWidgetModifyFont(String name, int style, int size);

  static void printCurrentThread ()
  {
    System.out.println ("gtkgenericpeer, thread: " + Thread.currentThread ());
  }
}
