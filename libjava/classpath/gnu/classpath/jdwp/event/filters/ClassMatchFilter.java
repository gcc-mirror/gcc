/* ClassMatchFilter.java -- filter on class name (inclusive)
   Copyright (C) 2005 Free Software Foundation

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
import gnu.classpath.jdwp.exception.InvalidStringException;

/**
 * An event filter which includes events matching a 
 * specified class pattern (exact match or start/end with "*").
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class ClassMatchFilter
  implements IEventFilter
{
  // Pattern to match
  private String _pattern;

  /**
   * Constructs a new <code>ClassMatchFilter</code>
   *
   * @param  pattern  the pattern to use
   * @throws InvalidStringException  if pattern is invalid
   */
  public ClassMatchFilter (String pattern)
    throws InvalidStringException
  {
    int index = pattern.indexOf ('*');
    if (index != -1 && index != 0 && index != (pattern.length () - 1))
      {
	// '*' must be first char or last char
	throw new InvalidStringException ("pattern may be an exact match or "
					  + "start/end with \"*\"");
      }
    _pattern = pattern;
  }

  /**
   * Returns the pattern to be matched
   *
   * @return the pattern
   */
  public String getPattern ()
  {
    return _pattern;
  }

  /**
   * Does the given event match the filter?
   *
   * @param event  the <code>Event</code> to scrutinize
   */
  public boolean matches (Event event)
  {
    Object type = event.getParameter (Event.EVENT_CLASS);
    if (type != null)
      {
	Class eventClass = (Class) type;
	String name = eventClass.getName ();
	
	if (_pattern.startsWith ("*"))
	  return name.endsWith (_pattern.substring (1));
	else if (_pattern.endsWith ("*"))
	  {
	    int end = _pattern.length () - 1;
	    return name.startsWith (_pattern.substring (0, end));
	  }
	else
	  return name.matches (_pattern);
      }

    return false;
  }
}
