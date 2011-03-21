/* TransformerException.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package javax.xml.transform;

import gnu.java.lang.CPStringBuilder;

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * An exception occurred during the transformation process.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class TransformerException
  extends Exception
{
  private static final long serialVersionUID = 975798773772956428L;

  // Field names fixed by serialization spec.
  private SourceLocator  locator;
  private Throwable  containedException;

  /**
   * Constructor with a detail message.
   */
  public TransformerException(String msg)
  {
    this(msg, null, null);
  }

  /**
   * Constructor with an underlying cause.
   */
  public TransformerException(Throwable cause)
  {
    this(cause.getMessage(), null, cause);
  }

  /**
   * Constructor with a detail message and underlying cause.
   */
  public TransformerException(String msg, Throwable cause)
  {
    this(msg, null, cause);
  }

  /**
   * Constructor with a detail message and locator.
   */
  public TransformerException(String msg, SourceLocator locator)
  {
    this(msg, locator, null);
  }

  /**
   * Constructor with detail message, locator and underlying cause.
   */
  public TransformerException(String msg, SourceLocator locator,
                              Throwable cause)
  {
    super(msg);
    this.locator = locator;
    if (cause != null)
      {
        initCause(cause);
        this.containedException = cause;
      }
  }

  /**
   * Returns a locator indicating where the error occurred.
   */
  public SourceLocator getLocator()
  {
    return locator;
  }

  /**
   * Sets the locator indicating where the error occurred.
   */
  public void setLocator(SourceLocator location)
  {
    locator = location;
  }

  /**
   * Returns the underlying cause of this exception.
   */
  public Throwable getException()
  {
    return containedException;
  }

  /**
   * Returns the underlying cause of this exception.
   */
  public Throwable getCause()
  {
    return containedException;
  }

  /**
   * Initializes the root cause of this exception.
   * This method may be called only once, and will be called by the
   * constructor if a non-null cause is specified.
   * Really phenomenally poor API design.
   * @param cause the underlying cause
   * @exception IllegalArgumentException if this exception is passed as the
   * argument
   * @exception IllegalStateException if a cause has already been
   * initialized
   */
  public Throwable initCause(Throwable cause)
  {
    if (this.containedException != null)
      {
        throw new IllegalStateException();
      }
    if (cause == this)
      {
        throw new IllegalArgumentException();
      }
    this.containedException = cause;
    return this;
  }

  /**
   * Returns the exception message with location information appended.
   */
  public String getMessageAndLocation()
  {
    return (locator == null) ? getMessage() :
      getMessage() + ": " + getLocationAsString();
  }

  /**
   * Returns the location information as a string.
   */
  public String getLocationAsString()
  {
    if (locator == null)
      {
        return null;
      }
    String publicId = locator.getPublicId();
    String systemId = locator.getSystemId();
    int lineNumber = locator.getLineNumber();
    int columnNumber = locator.getColumnNumber();
    CPStringBuilder buffer = new CPStringBuilder ();
    if (publicId != null)
      {
        buffer.append ("publicId=");
        buffer.append (publicId);
      }
    if (systemId != null)
      {
        if (buffer.length() > 0)
          {
            buffer.append(' ');
          }
        buffer.append ("systemId=");
        buffer.append (systemId);
      }
    if (lineNumber != -1)
      {
        if (buffer.length() > 0)
          {
            buffer.append(' ');
          }
        buffer.append ("lineNumber=");
        buffer.append (lineNumber);
      }
    if (columnNumber != -1)
      {
        if (buffer.length() > 0)
          {
            buffer.append(' ');
          }
        buffer.append ("columnNumber=");
        buffer.append (columnNumber);
      }
    return buffer.toString();
  }

  public void printStackTrace()
  {
    printStackTrace(System.out);
  }

  public void printStackTrace(PrintStream s)
  {
    super.printStackTrace(s);
    if (containedException != null)
      {
        s.print("caused by ");
        containedException.printStackTrace(s);
      }
  }

  public void printStackTrace(PrintWriter s)
  {
    super.printStackTrace(s);
    if (containedException != null)
      {
        s.print("caused by ");
        containedException.printStackTrace(s);
      }
  }

}
