/* StepFilter.java -- a step filter
   Copyright (C) 2005, 2007 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.event.filters;

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.event.Event;
import gnu.classpath.jdwp.exception.InvalidThreadException;
import gnu.classpath.jdwp.id.ThreadId;

/**
 * "An event filter which restricts reported step events to those which
 * satisfy depth and size constraints. This modifier can only be used with
 * step event kinds."
 *
 * This "filter" is not really a filter. It is simply a way to communicate
 * stepping information in a convenient way between the JDWP backend and
 * the virtual machine.
 * 
 * Consequently, this "filter" always matches.
 * 
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class StepFilter
  implements IEventFilter
{
  private ThreadId _tid;
  private int _size;
  private int _depth;

  /**
   * Constructs a new StepFilter
   *
   * @param tid    ID of the thread in which to step
   * @param size   size of each step
   * @param depth  relative call stack limit
   * @throws InvalidThreadException if thread is invalid
   */
  public StepFilter (ThreadId tid, int size, int depth)
    throws InvalidThreadException
  {
    if (tid.getReference().get () == null)
      throw new InvalidThreadException (tid.getId ());

    _tid = tid;
    _size = size;
    _depth = depth;
  }

  /**
   * Returns the thread in which to step
   *
   * @return the thread's ID
   */
  public ThreadId getThread ()
  {
    return _tid;
  }

  /**
   * Returns the size of each step (insn, line)
   *
   * @return the step size
   * @see gnu.classpath.jdwp.JdwpConstants.StepSize
   */
  public int getSize ()
  {
    return _size;
  }

  /**
   * Returns the relative call stack limit (into, over, out)
   *
   * @return how to step
   * @see gnu.classpath.jdwp.JdwpConstants.StepDepth
   */
  public int getDepth ()
  {
    return _depth;
  }

  /**
   * Does the given event match the filter?
   *
   * @param event  the <code>Event</code> to scrutinize
   */
  public boolean matches (Event event)
  {
    return true;
  }
}
