/* ChoiceCallback.java -- callback for a choice of values.
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


package javax.security.auth.callback;

import java.io.Serializable;

/**
 * Underlying security services instantiate and pass a
 * <code>ChoiceCallback</code> to the <code>handle()</code> method of a
 * {@link CallbackHandler} to display a list of choices and to retrieve the
 * selected choice(s).
 *
 * @see CallbackHandler
 */
public class ChoiceCallback implements Callback, Serializable
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /**
   * @serial
   * @since 1.4
   */
  private String prompt;

  /**
   * @serial the list of choices.
   * @since 1.4
   */
  private String[] choices;

  /**
   * @serial the choice to be used as the default choice.
   * @since 1.4
   */
  private int defaultChoice;

  /**
   * @serial whether multiple selections are allowed from the list of choices.
   * @since 1.4
   */
  private boolean multipleSelectionsAllowed;

  /**
   * @serial the selected choices, represented as indexes into the choices list.
   * @since 1.4
   */
  private int[] selections;

  // Constructor(s)
  //--------------------------------------------------------------------------

  /**
   * Construct a <code>ChoiceCallback</code> with a prompt, a list of choices,
   * a default choice, and a boolean specifying whether or not multiple
   * selections from the list of choices are allowed.
   *
   * @param prompt the prompt used to describe the list of choices.
   * @param choices the list of choices.
   * @param defaultChoice the choice to be used as the default choice when the
   * list of choices are displayed. This value is represented as an index into
   * the <code>choices</code> array.
   * @param multipleSelectionsAllowed boolean specifying whether or not
   * multiple selections can be made from the list of choices.
   * @throws IllegalArgumentException if <code>prompt</code> is <code>null</code>,
   * if <code>prompt</code> has a length of <code>0</code>, if <code>choices</code>
   * is <code>null</code>, if <code>choices</code> has a length of <code>0</code>,
   * if any element from <code>choices</code> is <code>null</code>, if any
   * element from <code>choices</code> has a length of <code>0</code> or if
   * <code>defaultChoice</code> does not fall within the array boundaries of
   * <code>choices</code>.
   */
  public ChoiceCallback(String prompt, String[] choices, int defaultChoice,
			boolean multipleSelectionsAllowed)
  {
    super();

    setPrompt(prompt);
    setChoices(choices);
    if (defaultChoice < 0 || defaultChoice >= this.choices.length)
      {
	throw new IllegalArgumentException("default choice is out of bounds");
      }
    this.defaultChoice = defaultChoice;
    this.multipleSelectionsAllowed = multipleSelectionsAllowed;
  }

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
   * Get the list of choices.
   *
   * @return the list of choices.
   */
  public String[] getChoices()
  {
    return choices;
  }

  /**
   * Get the defaultChoice.
   *
   * @return the defaultChoice, represented as an index into the choices list.
   */
  public int getDefaultChoice()
  {
    return defaultChoice;
  }

  /**
   * Get the boolean determining whether multiple selections from the choices
   * list are allowed.
   *
   * @return whether multiple selections are allowed.
   */
  public boolean allowMultipleSelections()
  {
    return multipleSelectionsAllowed;
  }

  /**
   * Set the selected choice.
   *
   * @param selection the selection represented as an index into the choices
   * list.
   * @see #getSelectedIndexes()
   */
  public void setSelectedIndex(int selection)
  {
    this.selections = new int[1];
    this.selections[0] = selection;
  }

  /**
   * Set the selected choices.
   *
   * @param selections the selections represented as indexes into the choices
   * list.
   * @throws UnsupportedOperationException if multiple selections are not
   * allowed, as determined by <code>allowMultipleSelections</code>.
   * @see #getSelectedIndexes()
   */
  public void setSelectedIndexes(int[] selections)
  {
    if (!multipleSelectionsAllowed)
      {
	throw new UnsupportedOperationException("not allowed");
      }

    this.selections = selections;
  }

  /**
   * Get the selected choices.
   *
   * @return the selected choices, represented as indexes into the choices list.
   * @see #setSelectedIndexes(int[])
   */
  public int[] getSelectedIndexes()
  {
    return selections;
  }

  private void setPrompt(String prompt) throws IllegalArgumentException
  {
    if ((prompt == null) || (prompt.length() == 0))
      {
	throw new IllegalArgumentException("invalid prompt");
      }
    this.prompt = prompt;
  }

  private void setChoices(String[] choices) throws IllegalArgumentException
  {
    if (choices == null || choices.length == 0)
      {
	throw new IllegalArgumentException("invalid choices");
      }
    for (int i = 0; i < choices.length; i++)
      {
	if (choices[i] == null || choices[i].length() == 0)
	  {
	    throw new IllegalArgumentException("invalid choice at index #"+i);
	  }
      }
    this.choices = choices;
  }
}
