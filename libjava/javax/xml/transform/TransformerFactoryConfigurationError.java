/* TransformerFactoryConfigurationError.java -- 
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
package javax.xml.transform;

/**
 * An error occurred during configuration of the transformer factory.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class TransformerFactoryConfigurationError
  extends Error
{
  
  private final Exception  exception;

  /**
   * Constructor with no detail message.
   */
  public TransformerFactoryConfigurationError()
  {
    this(null, null);
  }

  /**
   * Constructor with a detail message.
   */
  public TransformerFactoryConfigurationError(String msg)
  {
    this(null, msg);
  }

  /**
   * Constructor with an underlying cause.
   */
  public TransformerFactoryConfigurationError(Exception e)
  {
    this(e, null);
  }

  /**
   * Constructor with an underlying cause and detail message.
   */
  public TransformerFactoryConfigurationError(Exception e, String msg)
  {
    super(msg);
    exception = e;
  }

  /**
   * Returns the detail message.
   */
  public String getMessage()
  {
    String message = super.getMessage();
    if (message == null && exception != null)
      {
        message = exception.getMessage();
      }
    return message;
  }

  /**
   * Returns the underlying cause.
   */
  public Exception getException()
  {
    return exception;
  }
  
}
