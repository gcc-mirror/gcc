/* Attributes.java -- exception thrown to indicate an problem with a jar file
   Copyright (C) 2000 Free Software Foundation, Inc.

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

package java.util.jar;

import java.util.zip.ZipException;

/**
 * This exception is thrown to indicate an problem with a jar file.
 * It can be constructed with or without a descriptive message of the problem.
 * <p>
 * Note that none of the methods in the java.util.jar package actually declare
 * to throw this exception, most just declare that they throw an IOException
 * which is super class of JarException.
 * 
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */

public class JarException extends ZipException
{
  // Constructors

  /**
   * Create a new JarException without a descriptive error message.
   */
  public JarException()
  {
    super();
  }

  /**
   * Create a new JarException with a descriptive error message indicating
   * what went wrong. This message can later be retrieved by calling the
   * <code>getMessage()</code> method.
   * @see java.lang.Throwable@getMessage()
   *
   * @param message The descriptive error message
   */
  public JarException(String message)
  {
    super(message);
  }
}
