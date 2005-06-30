/* CallbackHandler.java -- base interface for callback handlers.
   Copyright (C) 2003, Free Software Foundation, Inc.

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


package javax.security.auth.callback;

import java.io.IOException;

/**
 * <p>An application implements a <code>CallbackHandler</code> and passes it to
 * underlying security services so that they may interact with the application
 * to retrieve specific authentication data, such as usernames and passwords, or
 * to display certain information, such as error and warning messages.</p>
 *
 * <p><code>CallbackHandler</code>s are implemented in an application-dependent
 * fashion. For example, implementations for an application with a graphical
 * user interface (GUI) may pop up windows to prompt for requested information
 * or to display error messages. An implementation may also choose to obtain
 * requested information from an alternate source without asking the end user.</p>
 *
 * <p>Underlying security services make requests for different types of
 * information by passing individual Callbacks to the <code>CallbackHandler</code>.
 * The <code>CallbackHandler</code> implementation decides how to retrieve and
 * display information depending on the {@link Callback}s passed to it. For
 * example, if the underlying service needs a username and password to
 * authenticate a user, it uses a {@link NameCallback} and
 * {@link PasswordCallback}. The <code>CallbackHandler</code> can then choose
 * to prompt for a username and password serially, or to prompt for both in a
 * single window.</p>
 *
 * <p>A default <code>CallbackHandler</code> class implementation may be
 * specified in the <code>auth.login.defaultCallbackHandler</code> security
 * property. The security property can be set in the Java security properties
 * file located in the file named
 * <code>&lt;JAVA_HOME>/lib/security/java.security</code>, where
 * <code>&lt;JAVA_HOME></code> refers to the directory where the SDK was
 * installed.</p>
 *
 * <p>If the security property is set to the fully qualified name of a
 * <code>CallbackHandler</code> implementation class, then a
 * <code>LoginContext</code>will load the specified <code>CallbackHandler</code>
 * and pass it to the underlying <code>LoginModules</code>. The
 * <code>LoginContext</code> only loads the default handler if one was not
 * provided.</p>
 *
 * <p>All default handler implementations must provide a public zero-argument
 * constructor.</p>
 *
 */
public interface CallbackHandler
{

  /**
   * <p>Retrieve or display the information requested in the provided
   * {@link Callback}s.</p>
   *
   * <p>The <code>handle()</code> method implementation checks the instance(s)
   * of the {@link Callback} object(s) passed in to retrieve or display the
   * requested information. The following example is provided to help
   * demonstrate what an <code>handle()</code> method implementation might look
   * like. This example code is for guidance only. Many details, including
   * proper error handling, are left out for simplicity.</p>
   *
   * <pre>
   *public void handle(Callback[] callbacks)
   *throws IOException, UnsupportedCallbackException {
   *   for (int i = 0; i < callbacks.length; i++) {
   *      if (callbacks[i] instanceof TextOutputCallback) {
   *         // display the message according to the specified type
   *         TextOutputCallback toc = (TextOutputCallback)callbacks[i];
   *         switch (toc.getMessageType()) {
   *         case TextOutputCallback.INFORMATION:
   *            System.out.println(toc.getMessage());
   *            break;
   *         case TextOutputCallback.ERROR:
   *            System.out.println("ERROR: " + toc.getMessage());
   *            break;
   *         case TextOutputCallback.WARNING:
   *            System.out.println("WARNING: " + toc.getMessage());
   *            break;
   *         default:
   *            throw new IOException("Unsupported message type: "
   *                  + toc.getMessageType());
   *         }
   *      } else if (callbacks[i] instanceof NameCallback) {
   *         // prompt the user for a username
   *         NameCallback nc = (NameCallback)callbacks[i];
   *         // ignore the provided defaultName
   *         System.err.print(nc.getPrompt());
   *         System.err.flush();
   *         nc.setName((new BufferedReader(
   *               new InputStreamReader(System.in))).readLine());
   *      } else if (callbacks[i] instanceof PasswordCallback) {
   *         // prompt the user for sensitive information
   *         PasswordCallback pc = (PasswordCallback)callbacks[i];
   *         System.err.print(pc.getPrompt());
   *         System.err.flush();
   *         pc.setPassword(readPassword(System.in));
   *      } else {
   *         throw new UnsupportedCallbackException(
   *               callbacks[i], "Unrecognized Callback");
   *      }
   *   }
   *}
   *
   * // Reads user password from given input stream.
   *private char[] readPassword(InputStream in) throws IOException {
   *   // insert code to read a user password from the input stream
   *}
   * </pre>
   *
   * @param callbacks an array of {@link Callback} objects provided by an
   * underlying security service which contains the information requested to
   * be retrieved or displayed.
   * @throws IOException if an input or output error occurs.
   * @throws UnsupportedCallbackException if the implementation of this method
   * does not support one or more of the Callbacks specified in the
   * <code>callbacks</code> parameter.
   */
  void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException;
}
