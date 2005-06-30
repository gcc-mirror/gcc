/* AuthenticationException.java --
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


package javax.security.sasl;

/**
 * <p>This exception is thrown by a SASL mechanism implementation to indicate
 * that the SASL exchange has failed due to reasons related to authentication,
 * such as an invalid identity, passphrase, or key.</p>
 *
 * <p>Note that the lack of an <code>AuthenticationException</code> does not
 * mean that the failure was not due to an authentication error. A SASL
 * mechanism implementation might throw the more general {@link SaslException}
 * instead of <code>AuthenticationException</code> if it is unable to determine
 * the nature of the failure, or if does not want to disclose the nature of the
 * failure, for example, due to security reasons.</p>
 */
public class AuthenticationException extends SaslException
{

  // Constants and variables
  // -------------------------------------------------------------------------

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Constructs a new instance of <code>AuthenticationException</code>. The
   * root exception and the detailed message are <code>null</code>.
   */
  public AuthenticationException()
  {
    super();
  }

  /**
   * Constructs a new instance of <code>AuthenticationException</code> with a
   * detailed message. The root exception is <code>null</code>.
   *
   * @param detail a possibly <code>null</code> string containing details of
   * the exception.
   * @see Throwable#getMessage()
   */
  public AuthenticationException(String detail)
  {
    super(detail);
  }

  /**
   * Constructs a new instance of <code>AuthenticationException</code> with a
   * detailed message and a root exception.
   *
   * @param detail a possibly <code>null</code> string containing details of
   * the exception.
   * @param ex a possibly <code>null</code> root exception that caused this
   * exception.
   * @see Throwable#getMessage()
   * @see SaslException#getCause()
   */
  public AuthenticationException(String detail, Throwable ex)
  {
    super(detail, ex);
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------
}
