/* TextInputCallback.java -- callbacks for user input.
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
 * Underlying security services instantiate and pass a <code>TextInputCallback</code>
 * to the <code>handle()</code> method of a {@link CallbackHandler} to retrieve
 * generic text information.
 *
 * @see CallbackHandler
 */
public class TextInputCallback implements Callback, Serializable
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
  private String defaultText;

  /**
   * @serial
   * @since 1.4
   */
  private String inputText;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Construct a <code>TextInputCallback</code> with a prompt.
   *
   * @param prompt the prompt used to request the information.
   * @throws IllegalArgumentException if <code>prompt</code> is <code>null</code>
   * or if <code>prompt</code> has a length of <code>0</code>.
   */
  public TextInputCallback(String prompt) throws IllegalArgumentException
  {
    super();

    setPrompt(prompt);
  }

  /**
   * Construct a <code>TextInputCallback</code> with a prompt and default
   * input value.
   *
   * @param prompt the prompt used to request the information.
   * @param defaultText the text to be used as the default text displayed with
   * the prompt.
   * @throws IllegalArgumentException if <code>prompt</code> is <code>null</code>,
   * if <code>prompt</code> has a length of <code>0</code>, if
   * <code>defaultText</code> is <code>null</code> or if <code>defaultText</code>
   * has a length of <code>0</code>.
   */
  public TextInputCallback(String prompt, String defaultText)
    throws IllegalArgumentException
  {
    super();

    setPrompt(prompt);
    setDefaultText(defaultText);
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
   * Get the default text.
   *
   * @return the default text, or <code>null</code> if this
   * <code>TextInputCallback</code> was not instantiated with
   * <code>defaultText</code>.
   */
  public String getDefaultText()
  {
    return defaultText;
  }

  /**
   * Set the retrieved text.
   *
   * @param text the retrieved text, which may be <code>null</code>.
   */
  public void setText(String text)
  {
    this.inputText = text;
  }

  /**
   * Get the retrieved text.
   *
   * @return the retrieved text, which may be <code>null</code>.
   */
  public String getText()
  {
    return inputText;
  }

  private void setPrompt(String prompt) throws IllegalArgumentException
  {
    if ((prompt == null) || (prompt.length() == 0))
      {
	throw new IllegalArgumentException("invalid prompt");
      }
    this.prompt = prompt;
  }

  private void setDefaultText(String defaultText) throws IllegalArgumentException
  {
    if ((defaultText == null) || (defaultText.length() == 0))
      {
	throw new IllegalArgumentException("invalid default text");
      }
    this.defaultText = defaultText;
  }
}
