/* ActiveEvent.java -- a self-dispatching event
   Copyright (C) 2000, 2002 Free Software Foundation

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


package java.awt;

/**
 * An interface for events which can dispatch themselves in another thread.
 * This has two uses: first, if your code is in a critical section, calling a
 * synchronized method might deadlock. But by using an ActiveEvent to call
 * the second section, it will not obtain the lock until you have left the
 * critical section, avoiding deadlock. The second use is for calling
 * untrusted code. For example, system code should use an ActiveEvent to
 * invoke user code securely.
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @since 1.2
 * @status updated to 1.4
 */
public interface ActiveEvent
{
  /**
   * Dispatch the event, according to what the event needs done. Invoked
   * automatically if this is placed on the <code>EventDispatchQueue</code>.
   */
  void dispatch();
} // interface ActiveEvent
