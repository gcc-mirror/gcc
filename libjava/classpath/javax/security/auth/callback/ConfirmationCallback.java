/* ConfirmationCallback.java -- callback for confirmations.
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
 * Underlying security services instantiate and pass a
 * <code>ConfirmationCallback</code> to the <code>handle()</code> method of a
 * {@link CallbackHandler} to ask for YES/NO, OK/CANCEL, YES/NO/CANCEL or other
 * similar confirmations.
 *
 * @see CallbackHandler
 */
public class ConfirmationCallback implements Callback, Serializable
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /**
   * <p>Unspecified option type.</p>
   *
   * <p>The <code>getOptionType</code> method returns this value if this
   * <code>ConfirmationCallback</code> was instantiated with <code>options</code>
   * instead of an <code>optionType</code>.</p>
   */
  public static final int UNSPECIFIED_OPTION = -1;

  /**
   * <p>YES/NO confirmation option.</p>
   *
   * <p>An underlying security service specifies this as the <code>optionType</code>
   * to a <code>ConfirmationCallback</code> constructor if it requires a
   * confirmation which can be answered with either <code>YES</code> or
   * <code>NO</code>.</p>
   */
  public static final int YES_NO_OPTION = 0;

  /**
   * <p>YES/NO/CANCEL confirmation confirmation option.</p>
   *
   * <p>An underlying security service specifies this as the <code>optionType</code>
   * to a <code>ConfirmationCallback</code> constructor if it requires a
   * confirmation which can be answered with either <code>YES</code>,
   * <code>NO</code> or <code>CANCEL</code>.
   */
  public static final int YES_NO_CANCEL_OPTION = 1;

  /**
   * <p>OK/CANCEL confirmation confirmation option.</p>
   *
   * <p>An underlying security service specifies this as the <code>optionType</code>
   * to a <code>ConfirmationCallback</code> constructor if it requires a
   * confirmation which can be answered with either <code>OK</code> or
   * <code>CANCEL</code>.</p>
   */
  public static final int OK_CANCEL_OPTION = 2;

  /**
   * <p>YES option.</p>
   *
   * <p>If an <code>optionType</code> was specified to this
   * <code>ConfirmationCallback</code>, this option may be specified as a
   * <code>defaultOption</code> or returned as the selected index.</p>
   */
  public static final int YES = 0;

  /**
   * <p>NO option.</p>
   *
   * <p>If an <code>optionType</code> was specified to this
   * <code>ConfirmationCallback</code>, this option may be specified as a
   * <code>defaultOption</code> or returned as the selected index.</p>
   */
  public static final int NO = 1;

  /**
   * <p>CANCEL option.</p>
   *
   * <p>If an <code>optionType</code> was specified to this
   * <code>ConfirmationCallback</code>, this option may be specified as a
   * <code>defaultOption</code> or returned as the selected index.</p>
   */
  public static final int CANCEL = 2;

  /**
   * <p>OK option.</p>
   *
   * <p>If an <code>optionType</code> was specified to this
   * <code>ConfirmationCallback</code>, this option may be specified as a
   * <code>defaultOption</code> or returned as the selected index.</p>
   */
  public static final int OK = 3;

  /** INFORMATION message type. */
  public static final int INFORMATION = 0;

  /** WARNING message type. */
  public static final int WARNING = 1;

  /** ERROR message type. */
  public static final int ERROR = 2;

  /**
   * @serial
   * @since 1.4
   */
  private String prompt;

  /**
   * @serial
   * @since 1.4
   */
  private int messageType;

  /**
   * @serial
   * @since 1.4
   */
  private int optionType;

  /**
   * @serial
   * @since 1.4
   */
  private int defaultOption;

  /**
   * @serial
   * @since 1.4
   */
  private String[] options = null;

  /**
   * @serial
   * @since 1.4
   */
  private int selection;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * <p>Construct a <code>ConfirmationCallback</code> with a message type, an
   * option type and a default option.</p>
   *
   * <p>Underlying security services use this constructor if they require
   * either a YES/NO, YES/NO/CANCEL or OK/CANCEL confirmation.</p>
   *
   * @param messageType the message type (INFORMATION, WARNING or ERROR).
   * @param optionType the option type (YES_NO_OPTION, YES_NO_CANCEL_OPTION or
   * OK_CANCEL_OPTION).
   * @param defaultOption the default option from the provided optionType (YES,
   * NO, CANCEL or OK).
   * @throws IllegalArgumentException if <code>messageType</code> is not either
   * <code>INFORMATION</code>, <code>WARNING</code>, or <code>ERROR</code>, if
   * <code>optionType</code> is not either <code>YES_NO_OPTION</code>,
   * <code>YES_NO_CANCEL_OPTION</code>, or <code>OK_CANCEL_OPTION</code>, or if
   * <code>defaultOption</code> does not correspond to one of the options in
   * <code>optionType</code>.
   */
  public ConfirmationCallback(int messageType, int optionType, int defaultOption)
    throws IllegalArgumentException
  {
    super();

    setMessageType(messageType);
    setOptionType(optionType, defaultOption);
    this.defaultOption = defaultOption;
  }

  /**
   * <p>Construct a <code>ConfirmationCallback</code> with a message type, a
   * list of options and a default option.</p>
   *
   * <p>Underlying security services use this constructor if they require a
   * confirmation different from the available preset confirmations provided
   * (for example, CONTINUE/ABORT or STOP/GO). The confirmation options are
   * listed in the <code>options</code> array, and are displayed by the
   * {@link CallbackHandler} implementation in a manner consistent with the
   * way preset options are displayed.</p>
   *
   * @param messageType the message type (INFORMATION, WARNING or ERROR).
   * @param options the list of confirmation options.
   * @param defaultOption the default option, represented as an index into the
   * <code>options</code> array.
   * @throws IllegalArgumentException if <code>messageType</code> is not either
   * <code>INFORMATION</code>, <code>WARNING</code>, or <code>ERROR</code>, if
   * <code>options</code> is <code>null</code>, if <code>options</code> has a
   * length of <code>0</code>, if any element from <code>options</code> is
   * <code>null</code>, if any element from <code>options</code> has a length
   * of <code>0</code>, or if <code>defaultOption</code> does not lie within
   * the array boundaries of <code>options</code>.
   */
  public ConfirmationCallback(int messageType, String[] options, int defaultOption)
  {
    super();

    setMessageType(messageType);
    setOptions(options, defaultOption);
    this.defaultOption = defaultOption;
  }

  /**
   * <p>Construct a <code>ConfirmationCallback</code> with a prompt, message
   * type, an option type and a default option.</p>
   *
   * <p>Underlying security services use this constructor if they require
   * either a YES/NO, YES/NO/CANCEL or OK/CANCEL confirmation.</p>
   *
   * @param prompt the prompt used to describe the list of options.
   * @param messageType the message type (INFORMATION, WARNING or ERROR).
   * @param optionType the option type (YES_NO_OPTION, YES_NO_CANCEL_OPTION or
   * OK_CANCEL_OPTION).
   * @param defaultOption the default option from the provided optionType (YES,
   * NO, CANCEL or OK).
   * @throws IllegalArgumentException if <code>prompt</code> is <code>null</code>,
   * if <code>prompt</code> has a length of <code>0</code>, if
   * <code>messageType</code> is not either <code>INFORMATION</code>,
   * <code>WARNING</code>, or <code>ERROR</code>, if <code>optionType</code> is
   * not either <code>YES_NO_OPTION</code>, <code>YES_NO_CANCEL_OPTION</code>,
   * or <code>OK_CANCEL_OPTION</code>, or if <code>defaultOption</code> does
   * not correspond to one of the options in <code>optionType</code>.
   */
  public ConfirmationCallback(String prompt, int messageType, int optionType,
                              int defaultOption)
  {
    super();

    setPrompt(prompt);
    setMessageType(messageType);
    setOptionType(optionType, defaultOption);
    this.defaultOption = defaultOption;
  }

  /**
   * <p>Construct a <code>ConfirmationCallback</code> with a prompt, message
   * type, a list of options and a default option.</p>
   *
   * <p>Underlying security services use this constructor if they require a
   * confirmation different from the available preset confirmations provided
   * (for example, CONTINUE/ABORT or STOP/GO). The confirmation options are
   * listed in the <code>options</code> array, and are displayed by the
   * {@link CallbackHandler} implementation in a manner consistent with the
   * way preset options are displayed.</p>
   *
   * @param prompt the prompt used to describe the list of options.
   * @param messageType the message type (INFORMATION, WARNING or ERROR).
   * @param options the list of confirmation options.
   * @param defaultOption the default option, represented as an index into the
   * <code>options</code> array.
   * @throws IllegalArgumentException if <code>prompt</code> is <code>null</code>,
   * if <code>prompt</code> has a length of <code>0</code>, if
   * <code>messageType</code> is not either <code>INFORMATION</code>,
   * <code>WARNING</code>, or <code>ERROR</code>, if <code>options</code> is
   * <code>null</code>, if <code>options</code> has a length of <code>0</code>,
   * if any element from <code>options</code> is <code>null</code>, if any
   * element from <code>options</code> has a length of <code>0</code>, or if
   * <code>defaultOption</code> does not lie within the array boundaries of
   * <code>options</code>.
   */
  public ConfirmationCallback(String prompt, int messageType, String[] options,
                              int defaultOption)
  {
    super();

    setPrompt(prompt);
    setMessageType(messageType);
    setOptions(options, defaultOption);
    this.defaultOption = defaultOption;
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------

  /**
   * Get the prompt.
   *
   * @return the prompt, or <code>null</code> if this
   * <code>ConfirmationCallback</code> was instantiated without a prompt.
   */
  public String getPrompt()
  {
    return prompt;
  }

  /**
   * Get the message type.
   *
   * @return the message type (INFORMATION, WARNING or ERROR).
   */
  public int getMessageType()
  {
    return messageType;
  }

  /**
   * <p>Get the option type.</p>
   *
   * <p>If this method returns {@link #UNSPECIFIED_OPTION}, then this
   * <code>ConfirmationCallback</code> was instantiated with <code>options</code>
   * instead of an <code>optionType</code>. In this case, invoke the
   * {@link #getOptions()} method to determine which confirmation options to
   * display.</p>
   *
   * @return the option type (YES_NO_OPTION, YES_NO_CANCEL_OPTION or
   * OK_CANCEL_OPTION), or UNSPECIFIED_OPTION if this
   * <code>ConfirmationCallback</code> was instantiated with <code>options</code>
   * instead of an <code>optionType</code>.
   */
  public int getOptionType()
  {
    if (options != null)
      {
        return UNSPECIFIED_OPTION;
      }
    return optionType;
  }

  /**
   * Get the confirmation options.
   *
   * @return the list of confirmation options, or <code>null</code> if this
   * <code>ConfirmationCallback</code> was instantiated with an
   * <code>optionType</code> instead of <code>options</code>.
   */
  public String[] getOptions()
  {
    return options;
  }

  /**
   * Get the default option.
   *
   * @return the default option, represented as <code>YES</code>, <code>NO</code>,
   * <code>OK</code> or <code>CANCEL</code> if an <code>optionType</code> was
   * specified to the constructor of this <code>ConfirmationCallback</code>.
   * Otherwise, this method returns the default option as an index into the
   * <code>options</code> array specified to the constructor of this
   * <code>ConfirmationCallback</code>.
   */
  public int getDefaultOption()
  {
    return defaultOption;
  }

  /**
   * Set the selected confirmation option.
   *
   * @param selection the selection represented as <code>YES</code>,
   * <code>NO</code>, <code>OK</code> or <code>CANCEL</code> if an
   * <code>optionType</code> was specified to the constructor of this
   * <code>ConfirmationCallback</code>. Otherwise, the <code>selection</code>
   * represents the index into the <code>options</code> array specified to the
   * constructor of this <code>ConfirmationCallback</code>.
   * @see #getSelectedIndex()
   */
  public void setSelectedIndex(int selection)
  {
    if (options != null)
      {
        setOptions(options, selection);
      }
    else
      {
        setOptionType(optionType, selection);
      }
  }

  /**
   * Get the selected confirmation option.
   *
   * @return the selected confirmation option represented as <code>YES</code>,
   * <code>NO</code>, <code>OK</code> or <code>CANCEL</code> if an
   * <code>optionType</code> was specified to the constructor of this
   * <code>ConfirmationCallback</code>. Otherwise, this method returns the
   * selected confirmation option as an index into the <code>options</code>
   * array specified to the constructor of this <code>ConfirmationCallback</code>.
   * @see #setSelectedIndex(int)
   */
  public int getSelectedIndex()
  {
    return this.selection;
  }

  private void setMessageType(int messageType) throws IllegalArgumentException
  {
    switch (messageType)
      {
      case INFORMATION:
      case WARNING:
      case ERROR: this.messageType = messageType; break;
      default: throw new IllegalArgumentException("illegal message type");
      }
  }

  private void setOptionType(int optionType, int selectedOption)
    throws IllegalArgumentException
  {
    switch (optionType)
      {
      case YES_NO_OPTION:
        this.optionType = optionType;
        switch (selectedOption)
          {
          case YES:
          case NO: this.selection = selectedOption; break;
          default: throw new IllegalArgumentException("invalid option");
          }
        break;
      case YES_NO_CANCEL_OPTION:
        this.optionType = optionType;
        switch (selectedOption)
          {
          case YES:
          case NO:
          case CANCEL: this.selection = selectedOption; break;
          default: throw new IllegalArgumentException("invalid option");
          }
        break;
      case OK_CANCEL_OPTION:
        this.optionType = optionType;
        switch (selectedOption)
          {
          case OK:
          case CANCEL: this.selection = selectedOption; break;
          default: throw new IllegalArgumentException("invalid option");
          }
        break;
      default:
        throw new IllegalArgumentException("illegal option type");
      }
  }

  private void setOptions(String[] options, int selectedOption)
    throws IllegalArgumentException
  {
    if ((selectedOption < 0) || (selectedOption > options.length - 1))
      {
        throw new IllegalArgumentException("invalid selection");
      }
    if ((options == null) || (options.length == 0))
      {
        throw new IllegalArgumentException("options is null or empty");
      }
    for (int i = 0; i < options.length; i++)
      {
        if ((options[i] == null) || (options[i].length() == 0))
          {
            throw new IllegalArgumentException("options[" + i + "] is null or empty");
          }
      }
    this.options = options;
    this.selection = selectedOption;
  }

  private void setPrompt(String prompt) throws IllegalArgumentException
  {
    if ((prompt == null) || (prompt.length() == 0))
      {
        throw new IllegalArgumentException("prompt is null or empty");
      }
    this.prompt = prompt;
  }
}
