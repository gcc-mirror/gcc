/* IllegalAccessError.java 
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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


package java.lang;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct. 
 */

/**
 * An <code>IllegalAccessError</code> is thrown when an attempt is made to 
 * call a method, or access or modify a field that the application does not
 * have access to.  Because this error is usually caught by a compiler, 
 * the error only occurs at runtime when the definition of a class has
 * changed in a way that is incompatible with the previously compiled 
 * application.
 *
 * @since JDK 1.0
 * 
 * @author Brian Jones
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 1, 1998
 */
public class IllegalAccessError extends IncompatibleClassChangeError
{
  static final long serialVersionUID = -8988904074992417891L;

  /**
   * Create an error without a message.
   */
  public IllegalAccessError()
    {
      super();
    }

  /**
   * Create an error with a message.
   */
  public IllegalAccessError(String s)
    {
      super(s);
    }
}
