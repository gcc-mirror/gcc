/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/** An exception of this type is thrown by AWT in some situations,
 * usually when major problems are encountered.
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */
public class AWTError extends Error
{
  /** Make a new instance of AWTError
   * @param s Text associated with the exception
   */
  public AWTError (String s)
  {
    super (s);
  }
}
