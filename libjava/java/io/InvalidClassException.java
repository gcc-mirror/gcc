/* InvalidClassException.java -- An I/O operation was interrupted.
   Copyright (C) 1998 Free Software Foundation, Inc.

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


package java.io;

/**
  * This exception is thrown when there is some sort of problem with a
  * class during a serialization operation.  This could be that the
  * versions don't match, that there are unknown datatypes in the class
  * or that the class doesn't have a default no-arg constructor.
  * <p>
  * The field <code>classname</code> will contain the name of the
  * class that caused the problem if known.  The getMessage() method
  * for this exception will always include the name of that class
  * if known.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class InvalidClassException extends ObjectStreamException
{
  /**
    * The name of the class which encountered the error.
    */
  public String classname;

  /**
    * Create a new InvalidClassException with a descriptive error message String
    *
    * @param message The descriptive error message
    */
  public InvalidClassException(String message)
  {
    super(message);
  }

  /**
    * Create a new InvalidClassException with a descriptive error message 
    * String, and the name of the class that caused the problem.
    * 
    * @param classname The number of bytes tranferred before the interruption
    * @param message The descriptive error message
    */
  public InvalidClassException(String classname, String message)
  {
    super(message);
    this.classname = classname;
  }

  /**
    * Returns the descriptive error message for this exception.  It will
    * include the class name that caused the problem if known.  This method
    * overrides Throwable.getMessage()
    *
    * @return A descriptive error message
    */
  public String getMessage()
  {
    return super.getMessage() + (classname == null ? "" : ": " + classname);
  }
}

