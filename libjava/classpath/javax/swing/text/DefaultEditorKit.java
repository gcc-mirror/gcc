/* DefaultEditorKit.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.text;

import gnu.java.lang.CPStringBuilder;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import javax.swing.Action;
import javax.swing.SwingConstants;

/**
 * The default implementation of {@link EditorKit}. This <code>EditorKit</code>
 * a plain text <code>Document</code> and several commands that together
 * make up a basic editor, like cut / copy + paste.
 *
 * @author original author unknown
 * @author Roman Kennke (roman@kennke.org)
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class DefaultEditorKit extends EditorKit
{
  static class SelectionPreviousWordAction
      extends TextAction
  {
    SelectionPreviousWordAction()
    {
      super(selectionPreviousWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getPreviousWord(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.moveDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class SelectionNextWordAction
      extends TextAction
  {
    SelectionNextWordAction()
    {
      super(selectionNextWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getNextWord(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.moveDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class SelectionBeginWordAction extends TextAction
  {
    SelectionBeginWordAction()
    {
      super(selectionBeginWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getWordStart(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.moveDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class SelectionEndWordAction extends TextAction
  {
    SelectionEndWordAction()
    {
      super(selectionEndWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getWordEnd(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.moveDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class BeginWordAction extends TextAction
  {
    BeginWordAction()
    {
      super(beginWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getWordStart(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.setDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class EndWordAction extends TextAction
  {
    EndWordAction()
    {
      super(endWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getWordEnd(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.setDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class PreviousWordAction
      extends TextAction
  {
    PreviousWordAction()
    {
      super(previousWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getPreviousWord(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.setDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class NextWordAction
      extends TextAction
  {
    NextWordAction()
    {
      super(nextWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      try
        {
          JTextComponent t = getTextComponent(event);

          if (t != null)
            {
              int offs = Utilities.getNextWord(t, t.getCaretPosition());

              Caret c = t.getCaret();
              c.setDot(offs);
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
        }
      catch(BadLocationException ble)
        {
          // Can't happen.
        }
    }
  }

  static class SelectAllAction
      extends TextAction
  {
    SelectAllAction()
    {
      super(selectAllAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          int offs = t.getDocument().getLength();
          Caret c = t.getCaret();
          c.setDot(0);
          c.moveDot(offs);
          try
            {
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class SelectionBeginAction
      extends TextAction
  {
    SelectionBeginAction()
    {
      super(selectionBeginAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          Caret c = t.getCaret();
          c.moveDot(0);
          try
            {
              c.setMagicCaretPosition(t.modelToView(0).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class SelectionEndAction
      extends TextAction
  {
    SelectionEndAction()
    {
      super(selectionEndAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          int offs = t.getDocument().getLength();
          Caret c = t.getCaret();
          c.moveDot(offs);
          try
            {
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class SelectionBeginLineAction
    extends TextAction
  {

    SelectionBeginLineAction()
    {
      super(selectionBeginLineAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          Caret c = t.getCaret();
          try
            {
              int offs = Utilities.getRowStart(t, c.getDot());
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class SelectionEndLineAction
      extends TextAction
  {
    SelectionEndLineAction()
    {
      super(selectionEndLineAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          Caret c = t.getCaret();
          try
            {
              int offs = Utilities.getRowEnd(t, c.getDot());
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class SelectLineAction extends TextAction
  {
    SelectLineAction()
    {
      super(selectLineAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          Caret c = t.getCaret();
          try
            {
              int offs1 = Utilities.getRowStart(t, c.getDot());
              int offs2 = Utilities.getRowEnd(t, c.getDot());
              c.setDot(offs2);
              c.moveDot(offs1);
              c.setMagicCaretPosition(t.modelToView(offs2).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class SelectWordAction extends TextAction
  {
    SelectWordAction()
    {
      super(selectWordAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          Caret c = t.getCaret();
          int dot = c.getDot();
          try
            {
              int wordStart = Utilities.getWordStart(t, dot);

              if (dot == wordStart)
                {
                  // Current cursor position is on the first character in a word.
                  c.setDot(wordStart);
                  c.moveDot(Utilities.getWordEnd(t, wordStart));
                }
              else
                {
                  // Current cursor position is not on the first character
                  // in a word.
                  int nextWord = Utilities.getNextWord(t, dot);
                  int previousWord = Utilities.getPreviousWord(t, dot);
                  int previousWordEnd = Utilities.getWordEnd(t, previousWord);

                  // Cursor position is in the space between two words. In such a
                  // situation just select the space.
                  if (dot >= previousWordEnd && dot <= nextWord)
                    {
                      c.setDot(previousWordEnd);
                      c.moveDot(nextWord);
                    }
                  else
                    {
                      // Cursor position is inside a word. Just select it then.
                      c.setDot(previousWord);
                      c.moveDot(previousWordEnd);
                    }
                }

              // If the position was updated change the magic caret position
              // as well.
              if (c.getDot() != dot)
                c.setMagicCaretPosition(t.modelToView(c.getDot()).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class SelectionDownAction
      extends TextAction.VerticalMovementAction
  {
    SelectionDownAction()
    {
      super(selectionDownAction, SwingConstants.SOUTH);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.moveDot(offs);
    }

  }

  static class SelectionUpAction
  extends TextAction.VerticalMovementAction
  {
    SelectionUpAction()
    {
      super(selectionUpAction, SwingConstants.NORTH);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.moveDot(offs);
    }

  }

  static class SelectionForwardAction
      extends TextAction.HorizontalMovementAction
  {
    SelectionForwardAction()
    {
      super(selectionForwardAction, SwingConstants.EAST);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.moveDot(offs);
    }
  }

  static class SelectionBackwardAction
      extends TextAction.HorizontalMovementAction
  {
    SelectionBackwardAction()
    {
      super(selectionBackwardAction, SwingConstants.WEST);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.moveDot(offs);
    }
  }

  static class DownAction
      extends TextAction.VerticalMovementAction
  {
    DownAction()
    {
      super(downAction, SwingConstants.SOUTH);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.setDot(offs);
    }
  }

  static class UpAction
      extends TextAction.VerticalMovementAction
  {
    UpAction()
    {
      super(upAction, SwingConstants.NORTH);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.setDot(offs);
    }

  }

  static class ForwardAction
      extends TextAction.HorizontalMovementAction
  {
    ForwardAction()
    {
      super(forwardAction, SwingConstants.EAST);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.setDot(offs);
    }

  }

  static class BackwardAction
      extends TextAction.HorizontalMovementAction
  {
    BackwardAction()
    {
      super(backwardAction, SwingConstants.WEST);
    }

    protected void actionPerformedImpl(Caret c, int offs)
    {
      c.setDot(offs);
    }

  }

  static class DeletePrevCharAction
      extends TextAction
  {
    DeletePrevCharAction()
    {
      super(deletePrevCharAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          try
            {
              int pos = t.getSelectionStart();
              int len = t.getSelectionEnd() - pos;

              if (len > 0)
                  t.getDocument().remove(pos, len);
              else if (pos > 0)
                {
                  pos--;
                  t.getDocument().remove(pos, 1);
                  Caret c = t.getCaret();
                  c.setDot(pos);
                  c.setMagicCaretPosition(t.modelToView(pos).getLocation());
                }
            }
          catch (BadLocationException e)
            {
              // FIXME: we're not authorized to throw this.. swallow it?
            }
        }
    }
  }

  static class DeleteNextCharAction
      extends TextAction
  {
    DeleteNextCharAction()
    {
      super(deleteNextCharAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          try
            {
              int pos = t.getSelectionStart();
              int len = t.getSelectionEnd() - pos;

              if (len > 0)
                  t.getDocument().remove(pos, len);
              else if (pos < t.getDocument().getLength())
                  t.getDocument().remove(pos, 1);

              Caret c = t.getCaret();
              c.setDot(pos);
              c.setMagicCaretPosition(t.modelToView(pos).getLocation());
            }
          catch (BadLocationException e)
            {
              // FIXME: we're not authorized to throw this.. swallow it?
            }
        }
    }
  }

  static class EndLineAction
      extends TextAction
  {
    EndLineAction()
    {
      super(endLineAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          try
            {
              int offs = Utilities.getRowEnd(t, t.getCaretPosition());
              if (offs > -1)
                {
                  Caret c = t.getCaret();
                  c.setDot(offs);
                  c.setMagicCaretPosition(t.modelToView(offs).getLocation());
                }
            }
          catch (BadLocationException ble)
            {
              // Nothing to do here
            }
        }
    }
  }

  static class BeginLineAction
      extends TextAction
  {
    BeginLineAction()
    {
      super(beginLineAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          try
            {
              int offs = Utilities.getRowStart(t, t.getCaretPosition());
              if (offs > -1)
                {
                  Caret c = t.getCaret();
                  c.setDot(offs);
                  c.setMagicCaretPosition(t.modelToView(offs).getLocation());
                }
            }
          catch (BadLocationException ble)
            {
              // Do nothing here.
            }
        }
    }
  }

  static class BeginAction extends TextAction
  {

    BeginAction()
    {
      super(beginAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          Caret c = t.getCaret();
          c.setDot(0);
          try
            {
              c.setMagicCaretPosition(t.modelToView(0).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  static class EndAction extends TextAction
  {

    EndAction()
    {
      super(endAction);
    }

    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        {
          int offs = t.getDocument().getLength();
          Caret c = t.getCaret();
          c.setDot(offs);
          try
            {
              c.setMagicCaretPosition(t.modelToView(offs).getLocation());
            }
          catch(BadLocationException ble)
            {
              // Can't happen.
            }
        }
    }
  }

  /**
   * Creates a beep on the PC speaker.
   *
   * @see Toolkit#beep()
   */
  public static class BeepAction extends TextAction
  {
    /**
     * Creates a new <code>BeepAction</code>.
     */
    public BeepAction()
    {
      super(beepAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      Toolkit.getDefaultToolkit().beep();
    }
  }

  /**
   * Copies the selected content into the system clipboard.
   *
   * @see Toolkit#getSystemClipboard()
   * @see CutAction
   * @see PasteAction
   */
  public static class CopyAction extends TextAction
  {

    /**
     * Create a new <code>CopyAction</code>.
     */
    public CopyAction()
    {
      super(copyAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      JTextComponent target = getTextComponent(event);
      if (target != null)
        target.copy();
    }
  }


  /**
   * Copies the selected content into the system clipboard and deletes the
   * selection.
   *
   * @see Toolkit#getSystemClipboard()
   * @see CopyAction
   * @see PasteAction
   */
  public static class CutAction extends TextAction
  {

    /**
     * Create a new <code>CutAction</code>.
     */
    public CutAction()
    {
      super(cutAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      JTextComponent target = getTextComponent(event);
      if (target != null)
        target.cut();
    }
  }

  /**
   * Copies content from the system clipboard into the editor.
   *
   * @see Toolkit#getSystemClipboard()
   * @see CopyAction
   * @see CutAction
   */
  public static class PasteAction extends TextAction
  {

    /**
     * Create a new <code>PasteAction</code>.
     */
    public PasteAction()
    {
      super(pasteAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      JTextComponent target = getTextComponent(event);
      if (target != null)
        target.paste();
    }
  }

  /**
   * This action is executed as default action when a KEY_TYPED
   * event is received and no keymap entry exists for that. The purpose
   * of this action is to filter out a couple of characters. This includes
   * the control characters and characters with the ALT-modifier.
   *
   * If an event does not get filtered, it is inserted into the document
   * of the text component. If there is some text selected in the text
   * component, this text will be replaced.
   */
  public static class DefaultKeyTypedAction
    extends TextAction
  {

    /**
     * Creates a new <code>DefaultKeyTypedAction</code>.
     */
    public DefaultKeyTypedAction()
    {
      super(defaultKeyTypedAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      // first we filter the following events:
      // - control characters
      // - key events with the ALT modifier
      JTextComponent target = getTextComponent(event);
      if ((target != null) && (event != null))
        {
          if ((target.isEditable()) && (target.isEnabled()))
            {
              String content = event.getActionCommand();
              int mod = event.getModifiers();
              if ((content != null) && (content.length() > 0)
                  && (mod & ActionEvent.ALT_MASK) == 0
                  && (mod & ActionEvent.CTRL_MASK) == 0)
                {
                  char c = content.charAt(0);
                  if ((c >= 0x20) && (c != 0x7F))
                    {
                      target.replaceSelection(content);
                    }
                }
            }
        }
    }
  }

  /**
   * This action inserts a newline character into the document
   * of the text component. This is typically triggered by hitting
   * ENTER on the keyboard.
   */
  public static class InsertBreakAction extends TextAction
  {

    /**
     * Creates a new <code>InsertBreakAction</code>.
     */
    public InsertBreakAction()
    {
      super(insertBreakAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        t.replaceSelection("\n");
    }
  }

  /**
   * Places content into the associated editor. If there currently is a
   * selection, this selection is replaced.
   */
  // FIXME: Figure out what this Action is supposed to do. Obviously text
  // that is entered by the user is inserted through DefaultKeyTypedAction.
  public static class InsertContentAction extends TextAction
  {

    /**
     * Creates a new <code>InsertContentAction</code>.
     */
    public InsertContentAction()
    {
      super(insertContentAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      // FIXME: Figure out what this Action is supposed to do. Obviously text
      // that is entered by the user is inserted through DefaultKeyTypedAction.
    }
  }

  /**
   * Inserts a TAB character into the text editor.
   */
  public static class InsertTabAction extends TextAction
  {

    /**
     * Creates a new <code>TabAction</code>.
     */
    public InsertTabAction()
    {
      super(insertTabAction);
    }

    /**
     * Performs the <code>Action</code>.
     *
     * @param event the action event describing the user action
     */
    public void actionPerformed(ActionEvent event)
    {
      JTextComponent t = getTextComponent(event);
      if (t != null)
        t.replaceSelection("\t");
    }
  }

  /**
   * The serial version of DefaultEditorKit.
   */
  private static final long serialVersionUID = 9017245433028523428L;

  /**
   * The name of the <code>Action</code> that moves the caret one character
   * backwards.
   *
   * @see #getActions()
   */
  public static final String backwardAction = "caret-backward";

  /**
   * The name of the <code>Action</code> that creates a beep in the speaker.
   *
   * @see #getActions()
   */
  public static final String beepAction = "beep";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the <code>Document</code>.
   *
   * @see #getActions()
   */
  public static final String beginAction = "caret-begin";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the current line.
   *
   * @see #getActions()
   */
  public static final String beginLineAction = "caret-begin-line";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the current paragraph.
   *
   * @see #getActions()
   */
  public static final String beginParagraphAction = "caret-begin-paragraph";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the current word.
   *
   * @see #getActions()
   */
  public static final String beginWordAction = "caret-begin-word";

  /**
   * The name of the <code>Action</code> that copies the selected content
   * into the system clipboard.
   *
   * @see #getActions()
   */
  public static final String copyAction = "copy-to-clipboard";

  /**
   * The name of the <code>Action</code> that copies the selected content
   * into the system clipboard and removes the selection.
   *
   * @see #getActions()
   */
  public static final String cutAction = "cut-to-clipboard";

  /**
   * The name of the <code>Action</code> that is performed by default if
   * a key is typed and there is no keymap entry.
   *
   * @see #getActions()
   */
  public static final String defaultKeyTypedAction = "default-typed";

  /**
   * The name of the <code>Action</code> that deletes the character that
   * follows the current caret position.
   *
   * @see #getActions()
   */
  public static final String deleteNextCharAction = "delete-next";

  /**
   * The name of the <code>Action</code> that deletes the character that
   * precedes the current caret position.
   *
   * @see #getActions()
   */
  public static final String deletePrevCharAction = "delete-previous";

  /**
   * The name of the <code>Action</code> that moves the caret one line down.
   *
   * @see #getActions()
   */
  public static final String downAction = "caret-down";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the <code>Document</code>.
   *
   * @see #getActions()
   */
  public static final String endAction = "caret-end";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the current line.
   *
   * @see #getActions()
   */
  public static final String endLineAction = "caret-end-line";

  /**
   * When a document is read and an CRLF is encountered, then we add a property
   * with this name and a value of &quot;\r\n&quot;.
   */
  public static final String EndOfLineStringProperty = "__EndOfLine__";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the current paragraph.
   *
   * @see #getActions()
   */
  public static final String endParagraphAction = "caret-end-paragraph";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the current word.
   *
   * @see #getActions()
   */
  public static final String endWordAction = "caret-end-word";

  /**
   * The name of the <code>Action</code> that moves the caret one character
   * forward.
   *
   * @see #getActions()
   */
  public static final String forwardAction = "caret-forward";

  /**
   * The name of the <code>Action</code> that inserts a line break.
   *
   * @see #getActions()
   */
  public static final String insertBreakAction = "insert-break";

  /**
   * The name of the <code>Action</code> that inserts some content.
   *
   * @see #getActions()
   */
  public static final String insertContentAction = "insert-content";

  /**
   * The name of the <code>Action</code> that inserts a TAB.
   *
   * @see #getActions()
   */
  public static final String insertTabAction = "insert-tab";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the next word.
   *
   * @see #getActions()
   */
  public static final String nextWordAction = "caret-next-word";

  /**
   * The name of the <code>Action</code> that moves the caret one page down.
   *
   * @see #getActions()
   */
  public static final String pageDownAction = "page-down";

  /**
   * The name of the <code>Action</code> that moves the caret one page up.
   *
   * @see #getActions()
   */
  public static final String pageUpAction = "page-up";

  /**
   * The name of the <code>Action</code> that copies content from the system
   * clipboard into the document.
   *
   * @see #getActions()
   */
  public static final String pasteAction = "paste-from-clipboard";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the previous word.
   *
   * @see #getActions()
   */
  public static final String previousWordAction = "caret-previous-word";

  /**
   * The name of the <code>Action</code> that sets the editor in read only
   * mode.
   *
   * @see #getActions()
   */
  public static final String readOnlyAction = "set-read-only";

  /**
   * The name of the <code>Action</code> that selects the whole document.
   *
   * @see #getActions()
   */
  public static final String selectAllAction = "select-all";

  /**
   * The name of the <code>Action</code> that moves the caret one character
   * backwards, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionBackwardAction = "selection-backward";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the document, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionBeginAction = "selection-begin";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the current line, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionBeginLineAction = "selection-begin-line";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the current paragraph, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionBeginParagraphAction =
    "selection-begin-paragraph";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the current word, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionBeginWordAction = "selection-begin-word";

  /**
   * The name of the <code>Action</code> that moves the caret one line down,
   * possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionDownAction = "selection-down";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the document, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionEndAction = "selection-end";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the current line, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionEndLineAction = "selection-end-line";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the current paragraph, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionEndParagraphAction =
    "selection-end-paragraph";

  /**
   * The name of the <code>Action</code> that moves the caret to the end
   * of the current word, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionEndWordAction = "selection-end-word";

  /**
   * The name of the <code>Action</code> that moves the caret one character
   * forwards, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionForwardAction = "selection-forward";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the next word, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionNextWordAction = "selection-next-word";

  /**
   * The name of the <code>Action</code> that moves the caret to the beginning
   * of the previous word, possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionPreviousWordAction =
    "selection-previous-word";

  /**
   * The name of the <code>Action</code> that moves the caret one line up,
   * possibly extending the current selection.
   *
   * @see #getActions()
   */
  public static final String selectionUpAction = "selection-up";

  /**
   * The name of the <code>Action</code> that selects the line around the
   * caret.
   *
   * @see #getActions()
   */
  public static final String selectLineAction = "select-line";

  /**
   * The name of the <code>Action</code> that selects the paragraph around the
   * caret.
   *
   * @see #getActions()
   */
  public static final String selectParagraphAction = "select-paragraph";

  /**
   * The name of the <code>Action</code> that selects the word around the
   * caret.
   *
   * @see #getActions()
   */
  public static final String selectWordAction = "select-word";

  /**
   * The name of the <code>Action</code> that moves the caret one line up.
   *
   * @see #getActions()
   */
  public static final String upAction = "caret-up";

  /**
   * The name of the <code>Action</code> that sets the editor in read-write
   * mode.
   *
   * @see #getActions()
   */
  public static final String writableAction = "set-writable";

  /**
   * Creates a new <code>DefaultEditorKit</code>.
   */
  public DefaultEditorKit()
  {
    // Nothing to do here.
  }

  /**
   * The <code>Action</code>s that are supported by the
   * <code>DefaultEditorKit</code>.
   */
  private static Action[] defaultActions =
  new Action[] {
    // These classes are public because they are so in the RI.
    new BeepAction(),
    new CopyAction(),
    new CutAction(),
    new DefaultKeyTypedAction(),
    new InsertBreakAction(),
    new InsertContentAction(),
    new InsertTabAction(),
    new PasteAction(),

    // These are (package-)private inner classes.
    new DeleteNextCharAction(),
    new DeletePrevCharAction(),

    new BeginLineAction(),
    new SelectionBeginLineAction(),

    new EndLineAction(),
    new SelectionEndLineAction(),

    new BackwardAction(),
    new SelectionBackwardAction(),

    new ForwardAction(),
    new SelectionForwardAction(),

    new UpAction(),
    new SelectionUpAction(),

    new DownAction(),
    new SelectionDownAction(),

    new NextWordAction(),
    new SelectionNextWordAction(),

    new PreviousWordAction(),
    new SelectionPreviousWordAction(),

    new BeginAction(),
    new SelectionBeginAction(),

    new EndAction(),
    new SelectionEndAction(),

    new BeginWordAction(),
    new SelectionBeginWordAction(),

    new EndWordAction(),
    new SelectionEndWordAction(),

    new SelectAllAction(),
    new SelectLineAction(),
    new SelectWordAction()
  };

  /**
   * Creates the <code>Caret</code> for this <code>EditorKit</code>. This
   * returns a {@link DefaultCaret} in this case.
   *
   * @return the <code>Caret</code> for this <code>EditorKit</code>
   */
  public Caret createCaret()
  {
    return new DefaultCaret();
  }

  /**
   * Creates the default {@link Document} that this <code>EditorKit</code>
   * supports. This is a {@link PlainDocument} in this case.
   *
   * @return the default {@link Document} that this <code>EditorKit</code>
   *         supports
   */
  public Document createDefaultDocument()
  {
    return new PlainDocument();
  }

  /**
   * Returns the <code>Action</code>s supported by this <code>EditorKit</code>.
   *
   * @return the <code>Action</code>s supported by this <code>EditorKit</code>
   */
  public Action[] getActions()
  {
    return defaultActions;
  }

  /**
   * Returns the content type that this <code>EditorKit</code> supports.
   * The <code>DefaultEditorKit</code> supports the content type
   * <code>text/plain</code>.
   *
   * @return the content type that this <code>EditorKit</code> supports
   */
  public String getContentType()
  {
    return "text/plain";
  }

  /**
   * Returns a {@link ViewFactory} that is able to create {@link View}s for
   * the <code>Element</code>s that are used in this <code>EditorKit</code>'s
   * model. This returns null which lets the UI of the text component supply
   * <code>View</code>s.
   *
   * @return a {@link ViewFactory} that is able to create {@link View}s for
   *         the <code>Element</code>s that are used in this
   *         <code>EditorKit</code>'s model
   */
  public ViewFactory getViewFactory()
  {
    return null;
  }

  /**
   * Reads a document of the supported content type from an {@link InputStream}
   * into the actual {@link Document} object.
   *
   * @param in the stream from which to read the document
   * @param document the document model into which the content is read
   * @param offset the offset inside to document where the content is inserted
   *
   * @throws BadLocationException if <code>offset</code> is an invalid location
   *         inside <code>document</code>
   * @throws IOException if something goes wrong while reading from
   *        <code>in</code>
   */
  public void read(InputStream in, Document document, int offset)
    throws BadLocationException, IOException
  {
    read(new InputStreamReader(in), document, offset);
  }

  /**
   * Reads a document of the supported content type from a {@link Reader}
   * into the actual {@link Document} object.
   *
   * @param in the reader from which to read the document
   * @param document the document model into which the content is read
   * @param offset the offset inside to document where the content is inserted
   *
   * @throws BadLocationException if <code>offset</code> is an invalid location
   *         inside <code>document</code>
   * @throws IOException if something goes wrong while reading from
   *        <code>in</code>
   */
  public void read(Reader in, Document document, int offset)
    throws BadLocationException, IOException
  {
    BufferedReader reader = new BufferedReader(in);

    String line;
    CPStringBuilder content = new CPStringBuilder();

    while ((line = reader.readLine()) != null)
      {
        content.append(line);
        content.append("\n");
      }

    document.insertString(offset, content.substring(0, content.length() - 1),
                          SimpleAttributeSet.EMPTY);
  }

  /**
   * Writes the <code>Document</code> (or a fragment of the
   * <code>Document</code>) to an {@link OutputStream} in the
   * supported content type format.
   *
   * @param out the stream to write to
   * @param document the document that should be written out
   * @param offset the beginning offset from where to write
   * @param len the length of the fragment to write
   *
   * @throws BadLocationException if <code>offset</code> or
   *         <code>offset + len</code>is an invalid location inside
   *         <code>document</code>
   * @throws IOException if something goes wrong while writing to
   *        <code>out</code>
   */
  public void write(OutputStream out, Document document, int offset, int len)
    throws BadLocationException, IOException
  {
    write(new OutputStreamWriter(out), document, offset, len);
  }

  /**
   * Writes the <code>Document</code> (or a fragment of the
   * <code>Document</code>) to a {@link Writer} in the
   * supported content type format.
   *
   * @param out the writer to write to
   * @param document the document that should be written out
   * @param offset the beginning offset from where to write
   * @param len the length of the fragment to write
   *
   * @throws BadLocationException if <code>offset</code> is an
   * invalid location inside <code>document</code>.
   * @throws IOException if something goes wrong while writing to
   *        <code>out</code>
   */
  public void write(Writer out, Document document, int offset, int len)
      throws BadLocationException, IOException
  {
    // Throw a BLE if offset is invalid
    if (offset < 0 || offset > document.getLength())
      throw new BadLocationException("Tried to write to invalid location",
                                     offset);

    // If they gave an overly large len, just adjust it
    if (offset + len > document.getLength())
      len = document.getLength() - offset;

    out.write(document.getText(offset, len));
  }
}
