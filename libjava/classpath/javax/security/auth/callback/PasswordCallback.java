/* PasswordCallback.java -- callback for passwords.
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

import java.io.Serializable;

/**
 * Underlying security services instantiate and pass a <code>PasswordCallback</code>
 * to the <code>handle()</code> method of a {@link CallbackHandler} to retrieve
 * password information.
 *
 * @see CallbackHandler,
 */
public class PasswordCallback implements Callback, Serializable
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /**
   * @serial
   * @since 1.4
   */
  private String prompt;

  /**
   * @serial
   * @since 1.4
   */
  private boolean echoOn;

  /**
   * @serial
   * @since 1.4
   */
  private char[] inputPassword;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Construct a <code>PasswordCallback</code> with a prompt and a boolean
   * specifying whether the password should be displayed as it is being typed.
   *
   * @param prompt the prompt used to request the password.
   * @param echoOn <code>true</code> if the password should be displayed as it
   * is being typed.
   * @throws IllegalArgumentException if <code>prompt</code> is <code>null</code>
   * or if <code>prompt</code> has a length of <code>0</code>.
   */
  public PasswordCallback(String prompt, boolean echoOn)
  {
    super();

    setPrompt(prompt);
    this.echoOn = echoOn;
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------

  /**
   * Get the prompt.
   *
   * @return the prompt.
   */
  public String getPrompt()
  {
    return prompt;
  }

  /**
   * Return whether the password should be displayed as it is being typed.
   *
   * @return the whether the password should be displayed as it is being typed.
   */
  public boolean isEchoOn()
  {
    return echoOn;
  }

  /**
   * <p>Set the retrieved password.</p>
   *
   * <p>This method makes a copy of the input password before storing it.</p>
   *
   * @param password the retrieved password, which may be <code>null</code>.
   * @see #getPassword()
   */
  public void setPassword(char[] password)
  {
    inputPassword = (password == null ? null : (char[]) password.clone());
  }

  /**
   * <p>Get the retrieved password.</p>
   *
   * <p>This method returns a copy of the retrieved password.</p>
   *
   * @return the retrieved password, which may be <code>null</code>.
   * @see #setPassword(char[])
   */
  public char[] getPassword()
  {
    return (inputPassword == null ? null : (char[]) inputPassword.clone());
  }

  /** Clear the retrieved password. */
  public void clearPassword()
  {
    if (inputPassword != null)
      {
        for (int i = 0; i < inputPassword.length; i++)
          {
            inputPassword[i] = '\0';
          }
        inputPassword = null;
      }
  }

  private void setPrompt(String prompt) throws IllegalArgumentException
  {
    if ((prompt == null) || (prompt.length() == 0))
      {
        throw new IllegalArgumentException("invalid prompt");
      }
    this.prompt = prompt;
  }
}
