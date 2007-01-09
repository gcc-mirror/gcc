/* AbstractCallbackHandler.java -- 
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.security.auth.callback;

import gnu.java.security.Engine;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.Security;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.ChoiceCallback;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.LanguageCallback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextInputCallback;
import javax.security.auth.callback.TextOutputCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

public abstract class AbstractCallbackHandler implements CallbackHandler
{

  // Fields.
  // -------------------------------------------------------------------------

  private static final String SERVICE = "CallbackHandler";

  protected final ResourceBundle messages;

  private final String name;

  // Constructors.
  // -------------------------------------------------------------------------

  protected AbstractCallbackHandler (final String name)
  {
    super();
    messages = PropertyResourceBundle.getBundle("gnu/javax/security/auth/callback/MessagesBundle");
    this.name = name;
  }

  /**
   * Create an instance of <code>CallbackHandler</code> of the designated
   * <code>type</code> from the first Security Provider which offers it.
   * 
   * @param type the type of callback handler to create.
   * @return a newly created instance of <code>ClassbackHandler</code>.
   * @throws NoSuchAlgorithmException if no security provider is found to offer
   *           an implementation of <code>CallbackHandler</code> of the
   *           designated <code>type</code>.
   */
  public static CallbackHandler getInstance(String type)
      throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(type, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new NoSuchAlgorithmException(type);
  }

  /**
   * Create an instance of <code>CallbackHandler</code> of the designated
   * <code>type</code> from the named security <code>provider</code>.
   * 
   * @param type the type of callback handler to create.
   * @param provider a named security provider to use.
   * @return a newly created instance of <code>ClassbackHandler</code>.
   * @throws NoSuchAlgorithmException if no security provider is found to offer
   *           an implementation of <code>CallbackHandler</code> of the
   *           designated <code>type</code>.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>type</code> is an empty string.
   */
  public static CallbackHandler getInstance(String type, String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(type, p);
  }

  /**
   * Create an instance of <code>CallbackHandler</code> of the designated
   * <code>type</code> from the designated security <code>provider</code>.
   * 
   * @param type the type of callback handler to create.
   * @param provider a security provider to use.
   * @return a newly created instance of <code>ClassbackHandler</code>.
   * @throws NoSuchAlgorithmException if no security provider is found to offer
   *           an implementation of <code>CallbackHandler</code> of the
   *           designated <code>type</code>.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>type</code> is an empty string.
   */
  public static CallbackHandler getInstance(String type, Provider provider)
    throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("CallbackHandler of type [")
        .append(type).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        return (CallbackHandler) Engine.getInstance(SERVICE, type, provider);
      }
    catch (InvocationTargetException x)
      {
        cause = x.getCause();
        if (cause instanceof NoSuchAlgorithmException)
          throw (NoSuchAlgorithmException) cause;
        if (cause == null)
          cause = x;
      }
    catch (ClassCastException x)
      {
        cause = x;
      }
    NoSuchAlgorithmException x = new NoSuchAlgorithmException(sb.toString());
    x.initCause(cause);
    throw x;
  }

  public void handle(Callback[] callbacks)
    throws IOException, UnsupportedCallbackException
  {
    if (callbacks == null)
      throw new NullPointerException();
    for (int i = 0; i < callbacks.length; i++)
      {
        if (callbacks[i] == null)
          continue;
        if (callbacks[i] instanceof ChoiceCallback)
          handleChoice((ChoiceCallback) callbacks[i]);
        else if (callbacks[i] instanceof ConfirmationCallback)
          handleConfirmation((ConfirmationCallback) callbacks[i]);
        else if (callbacks[i] instanceof LanguageCallback)
          handleLanguage((LanguageCallback) callbacks[i]);
        else if (callbacks[i] instanceof NameCallback)
          handleName((NameCallback) callbacks[i]);
        else if (callbacks[i] instanceof PasswordCallback)
          handlePassword((PasswordCallback) callbacks[i]);
        else if (callbacks[i] instanceof TextInputCallback)
          handleTextInput((TextInputCallback) callbacks[i]);
        else if (callbacks[i] instanceof TextOutputCallback)
          handleTextOutput((TextOutputCallback) callbacks[i]);
        else
          handleOther(callbacks[i]);
      }
  }

  public final String getName ()
  {
    return name;
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  /**
   * Handles a {@link ChoiceCallback}.
   *
   * @param callback The choice callback.
   * @throws IOException If an I/O error occurs.
   */
  protected abstract void handleChoice(ChoiceCallback callback)
    throws IOException;

  /**
   * Handles a {@link ConfirmationCallback}.
   *
   * @param callback The confirmation callback.
   * @throws IOException If an I/O error occurs.
   */
  protected abstract void handleConfirmation(ConfirmationCallback callback)
    throws IOException;

  /**
   * Handles a {@link LanguageCallback}.
   *
   * @param callback The language callback.
   * @throws IOException If an I/O error occurs.
   */
  protected abstract void handleLanguage(LanguageCallback callback)
    throws IOException;

  /**
   * Handles a {@link NameCallback}.
   *
   * @param callback The name callback.
   * @throws IOException If an I/O error occurs.
   */
  protected abstract void handleName(NameCallback callback)
    throws IOException;

  /**
   * Handles a {@link PasswordCallback}.
   *
   * @param callback The password callback.
   * @throws IOException If an I/O error occurs.
   */
  protected abstract void handlePassword(PasswordCallback callback)
    throws IOException;

  /**
   * Handles a {@link TextInputCallback}.
   *
   * @param callback The text input callback.
   * @throws IOException If an I/O error occurs.
   */
  protected abstract void handleTextInput(TextInputCallback callback)
    throws IOException;

  /**
   * Handles a {@link TextOutputCallback}.
   *
   * @param callback The text output callback.
   * @throws IOException If an I/O error occurs.
   */
  protected abstract void handleTextOutput(TextOutputCallback callback)
    throws IOException;

  /**
   * Handles an unknown callback. The default implementation simply throws
   * an {@link UnsupportedCallbackException}.
   *
   * @param callback The callback to handle.
   * @throws IOException If an I/O error occurs.
   * @throws UnsupportedCallbackException If the specified callback is not
   *   supported.
   */
  protected void handleOther(Callback callback)
    throws IOException, UnsupportedCallbackException
  {
    throw new UnsupportedCallbackException(callback);
  }
}
