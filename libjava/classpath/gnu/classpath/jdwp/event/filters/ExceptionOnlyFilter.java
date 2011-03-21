/* ExceptionOnlyFilter.java -- filter for excetions by caught/uncaught and type
   Copyright (C) 2005, 2006 Free Software Foundation

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

import gnu.classpath.jdwp.event.Event;
import gnu.classpath.jdwp.exception.InvalidClassException;
import gnu.classpath.jdwp.id.ReferenceTypeId;

/**
 * Restricts reported exceptions by their class and whether they are caught
 * or uncaught.
 *
 * This modifier can be used with exception event kinds only.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class ExceptionOnlyFilter
  implements IEventFilter
{
  private ReferenceTypeId _refId;
  private boolean _caught;
  private boolean _uncaught;

  /**
   * Constructs a new ExceptionOnlyFilter
   *
   * @param  refId     ID of the exception to report(null for all exceptions)
   * @param  caught    Report caught exceptions
   * @param  uncaught  Report uncaught exceptions
   * @throws InvalidClassException if refid is invalid
   */
  public ExceptionOnlyFilter (ReferenceTypeId refId, boolean caught,
                              boolean uncaught)
    throws InvalidClassException
  {
    if (refId != null && refId.getReference().get() == null)
      throw new InvalidClassException(refId.getId());

    _refId = refId;
    _caught = caught;
    _uncaught = uncaught;
  }

  /**
   * Returns the exception class to report (<code>null</code> for all)
   *
   * @return the class's ID
   */
  public ReferenceTypeId getType ()
  {
    return _refId;
  }


  /**
   * Does the given event match the filter?
   *
   * @param event the <code>Event</code> to scrutinize
   */
  public boolean matches(Event event)
  {
    boolean classMatch = true;

    // if not allowing all exceptions check if the exception matches
    if (_refId != null)
      {
        try
          {
            Class klass
              = (Class) event.getParameter(Event.EVENT_EXCEPTION_CLASS);
            classMatch = klass == _refId.getType();
          }
        catch (InvalidClassException ex)
          {
            classMatch = false;
          }
      }

    // check against the caught and uncaught options
    Boolean caught
      = (Boolean) event.getParameter(Event.EVENT_EXCEPTION_CAUGHT);

    return classMatch && ((caught.booleanValue()) ? _caught : _uncaught);
  }

}
