/* LoginModule.java -- interface for login implementations.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.security.auth.spi;

import java.util.Map;

import javax.security.auth.Subject;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.login.LoginException;

/**
 * The base interface for login methods in the Java Authentication and
 * Authorization Service (JAAS).
 *
 * <p>This interface is used by service providers that implement login
 * services, and is used internally by the JAAS system. It is not useful
 * to application programmers, who should use the {@link
 * javax.security.auth.login.LoginContext} instead.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public interface LoginModule
{
  /**
   * Abort the current login attempt. This is called after {@link #login()}
   * if the overall login attempt fails (that is, if one of the other login
   * modules that is REQUIRED or REQUISITE fails). This method should clean
   * up this module's saved state, if any.
   *
   * @return True if the abort succeeded, or false if this module should
   * be ignored.
   * @throws LoginException If the abort fails.
   */
  boolean abort() throws LoginException;

  /**
   * Commit the current login attempt. This is called after {@link
   * #login()} if the overall login attempt succeeds (that is, all
   * methods have satisfied all REQUIRED, REQUISITE, SUFFICIENT and
   * OPTIONAL module requirements).
   *
   * @return True if the commit succeeded, or false if this module
   * should be ignored.
   * @throws LoginException If the commit fails.
   */
  boolean commit() throws LoginException;

  /**
   * Initializes this login module. This method is called when the
   * instance implementing this interface is instantiated, and should
   * perform any initialization based on the given parameters.
   * Implementations should ignore state variables and options they do
   * not recognize.
   *
   * @param subject The subject being authenticated.
   * @param handler The callback handler for user input.
   * @param sharedState A mapping that is shared between all login
   * modules.
   * @param options A mapping of options given to this module.
   */
  void initialize(Subject subject, CallbackHandler handler,
                  Map sharedState, Map options);

  /**
   * Authenticates a subject to the system. This is the primary
   * mechanism by which subjects are authenticated, and typically
   * implementations will ask for credentials (for example, a user
   * name and password) which will then be verified.
   *
   * @return True if the subject was authenticated, or false if this
   * module should be ignored.
   * @throws LoginException If this method fails.
   */
  boolean login() throws LoginException;

  /**
   * Logs a subject out. This is primarily used for modules that must
   * destroy or remove the authentication state associated with a
   * logged-in subject.
   *
   * @return True if the logout succeeds, or false if this module
   * should be ignored.
   * @throws LoginException If this method fails.
   */
  boolean logout() throws LoginException;
}
