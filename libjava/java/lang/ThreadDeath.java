/* ThreadDeath.java - special exception registering Thread death
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2005  Free Software Foundation, Inc.

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


package java.lang;

/**
 * ThreadDeath is thrown in a thread when someone calls <code>stop()</code>
 * on that thread. <b>Important:</b> Make sure you rethrow this exception
 * if you catch it.  If you don't, the thread will not die.
 *
 * <p>This is an Error rather than an exception, so that normal code will
 * not catch it. It is intended for asynchronous cleanup when using the
 * deprecated Thread.stop() method.
 *
 * @author John Keiser
 * @author Tom Tromey (tromey@cygnus.com)
 * @see Thread#stop()
 * @status updated to 1.4
 */
public class ThreadDeath extends Error
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -4417128565033088268L;

  /**
   * Create an error without a message.
   */
  public ThreadDeath()
  {
  }
}
