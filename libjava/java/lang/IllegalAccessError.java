/* IllegalAccessError.java -- thrown when linking to an inaccessible member
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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
 * An <code>IllegalAccessError</code> is thrown when an attempt is made to
 * call a method, or access or modify a field that the application does not
 * have access to.  Because this error is usually caught by a compiler,
 * the error only occurs at runtime when the definition of a class has
 * changed in a way that is incompatible with the previously compiled
 * application.
 *
 * @author Brian Jones
 * @author Tom Tromey <tromey@cygnus.com>
 * @status updated to 1.4
 */
public class IllegalAccessError extends IncompatibleClassChangeError
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -8988904074992417891L;

  /**
   * Create an error without a message.
   */
  public IllegalAccessError()
  {
  }

  /**
   * Create an error with a message.
   *
   * @param s the message
   */
  public IllegalAccessError(String s)
  {
    super(s);
  }
}
