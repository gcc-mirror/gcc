/* InvalidNameException.java -- Exception indicating an invalid component/name
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package javax.naming;

/**
 * Exception indicating an invalid component or <code>Name</code>.
 * Thrown when a <code>Name</code> or component of a name is encountered that
 * does not follow the syntactic rules of a particular <code>Name</code> class.
 *
 * @author Anthony Green (green@redhat.com)
 * @author Mark Wielaard (mark@klomp.org)
 */
public class InvalidNameException extends NamingException
{
  /**
   * Creates a new exception without setting any of its fields.
   */
  public InvalidNameException ()
  {
    super ();
  }

  /**
   * Creates a new exception and sets the detailed message field.
   * All other fields are not set.
   */
  public InvalidNameException (String msg)
  {
    super (msg);
  }
}
