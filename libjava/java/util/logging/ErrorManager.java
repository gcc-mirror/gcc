/* ErrorManager.java
   -- a class for dealing with errors that a Handler encounters
      during logging

Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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
exception statement from your version.

*/


package java.util.logging;

/**
 * An <code>ErrorManager</code> deals with errors that a <code>Handler</code>
 * encounters while logging.
 *
 * @see Handler#setErrorManager(ErrorManager)
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class ErrorManager
{
  /* The values have been taken from Sun's public J2SE 1.4 API
   * documentation.
   * See http://java.sun.com/j2se/1.4/docs/api/constant-values.html
   */

  /**
   * Indicates that there was a failure that does not readily
   * fall into any of the other categories.
   */
  public static final int GENERIC_FAILURE = 0;


  /**
   * Indicates that there was a problem upon writing to
   * an output stream.
   */
  public static final int WRITE_FAILURE = 1;


  /**
   * Indicates that there was a problem upon flushing
   * an output stream.
   */
  public static final int FLUSH_FAILURE = 2;


  /**
   * Indicates that there was a problem upon closing
   * an output stream.
   */
  public static final int CLOSE_FAILURE = 3;

    
  /**
   * Indicates that there was a problem upon opening
   * an output stream.
   */
  public static final int OPEN_FAILURE = 4;


  /**
   * Indicates that there was a problem upon formatting
   * the message of a log record.
   */
  public static final int FORMAT_FAILURE = 5;


  /**
   * Indicates whether the {@link #error} method of this ErrorManager
   * has ever been used.
   *
   * Declared volatile in order to correctly support the
   * double-checked locking idiom (once the revised Java Memory Model
   * gets adopted); see Classpath bug #2944.
   */
  private volatile boolean everUsed = false;


  public ErrorManager()
  {
  }


  /**
   * Reports an error that occured upon logging.  The default implementation
   * emits the very first error to System.err, ignoring subsequent errors.
   *
   * @param message a message describing the error, or <code>null</code> if
   *                there is no suitable description.
   *
   * @param ex      an exception, or <code>null</code> if the error is not
   *                related to an exception.
   *
   * @param errorCode  one of the defined error codes, for example
   *                   <code>ErrorManager.CLOSE_FAILURE</code>.
   */
  public void error(String message, Exception ex, int errorCode)
  {
    if (everUsed)
      return;

    synchronized (this)
    {
      /* The double check is intentional. If the first check was
       * omitted, the monitor would have to be entered every time
       * error() method was called. If the second check was
       * omitted, the code below could be executed by multiple
       * threads simultaneously.
       *
       * This is the 'double-checked locking' idiom, which is broken
       * with the current version of the Java memory model.  However,
       * we assume that JVMs will have adopted a revised version of
       * the Java Memory Model by the time GNU Classpath gains
       * widespread acceptance. See Classpath bug #2944.
       */
      if (everUsed)
	return;

      everUsed = true;
    }

    String codeMsg;
    switch (errorCode)
    {
    case GENERIC_FAILURE:
      codeMsg = "GENERIC_FAILURE";
      break;

    case WRITE_FAILURE:
      codeMsg = "WRITE_FAILURE";
      break;

    case FLUSH_FAILURE:
      codeMsg = "FLUSH_FAILURE";
      break;

    case CLOSE_FAILURE:
      codeMsg = "CLOSE_FAILURE";
      break;

    case OPEN_FAILURE:
      codeMsg = "OPEN_FAILURE";
      break;

    case FORMAT_FAILURE:
      codeMsg = "FORMAT_FAILURE";
      break;

    default:
      codeMsg = String.valueOf(errorCode);
      break;
    }

    System.err.println("Error upon logging: " + codeMsg);
    if ((message != null) && (message.length() > 0))
      System.err.println(message);

    if (ex != null)
      ex.printStackTrace();
  }
}

