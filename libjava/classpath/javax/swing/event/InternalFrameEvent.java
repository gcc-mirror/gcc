/* InternalFrameEvent.java --
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


package javax.swing.event;

import java.awt.AWTEvent;

import javax.swing.JInternalFrame;

/**
 * @author Andrew Selkirk
 */
public class InternalFrameEvent extends AWTEvent
{
  private static final long serialVersionUID = -5204823611874873183L;

  /**
   * Internal frame activated event
   */
  public static final int INTERNAL_FRAME_ACTIVATED = 25554;

  /**
   * Internal frame closed event
   */
  public static final int INTERNAL_FRAME_CLOSED = 25551;

  /**
   * Internal frame closing event
   */
  public static final int INTERNAL_FRAME_CLOSING = 25550;

  /**
   * Internal frame deactivated event
   */
  public static final int INTERNAL_FRAME_DEACTIVATED = 25555;

  /**
   * Internal frame deiconifed event
   */
  public static final int INTERNAL_FRAME_DEICONIFIED = 25553;

  /**
   * Internal frame frame first event
   */
  public static final int INTERNAL_FRAME_FIRST = 25549;

  /**
   * Internal frame iconified event
   */
  public static final int INTERNAL_FRAME_ICONIFIED = 25552;

  /**
   * Internal frame last event
   */
  public static final int INTERNAL_FRAME_LAST = 25555;

  /**
   * Internal frame opened event
   */
  public static final int INTERNAL_FRAME_OPENED = 25549;

  /**
   * Creates a <code>JInternalFrameEvent</code> object.
   * 
   * @param source The source of this event.
   * @param id Then event ID of this event.
   */
  public InternalFrameEvent(JInternalFrame source, int id)
  {
    super(source, id);
  }

  /**
   * Returns the <code>JInternalFrame</code> object stored in this event.
   */
  public JInternalFrame getInternalFrame()
  {
    return (JInternalFrame) source;
  }
}
