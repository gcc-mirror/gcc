/* VMAccessControlState.java -- per-thread state for the access controller.
   Copyright (C) 2006 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
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


package java.security;

import java.util.LinkedList;

class VMAccessControlState
{
  /**
   * A list of {@link AccessControlContext} objects (which can be
   * null) for each call to {@link AccessController#doPrivileged()} in
   * the thread's call stack.
   */
  private LinkedList contexts = new LinkedList();

  /**
   * A flag indicating that we are within a call to {@link
   * VMAccessController#getContext()}.
   */
  private boolean inGetContext = false;

  /**
   * Not directly instantiable: use getThreadState() instead.
   */
  private VMAccessControlState() {}

  /**
   * Return an object representing the access control state of this
   * thread.
   *
   * @return The access control state of this thread, or
   * <code>null</code> if the VM is not initialized to the point of
   * being able to return this.
   */
  static native VMAccessControlState getThreadState();

  /**
   * Indicate whether this thread is within a call to {@link
   * VMAccessController#getContext()}.
   *
   * @return <code>true</code> if this thread is within a call to
   * {@link VMAccessController#getContext()}.
   */
  boolean isInGetContext()
  {
    return inGetContext;
  }

  /**
   * Specify whether this thread is within a call to {@link
   * VMAccessController#getContext()}.
   */
  void setInGetContext(boolean inGetContext)
  {
    this.inGetContext = inGetContext;
  }
  
  /**
   * Return a list of {@link AccessControlContext} objects (which can
   * be null) for each call to {@link AccessController#doPrivileged()}
   * in the thread's call stack.
   *
   * @return a list of {@link AccessControlContext} objects.
   */
  LinkedList getContexts()
  {
    return contexts;
  }
}
