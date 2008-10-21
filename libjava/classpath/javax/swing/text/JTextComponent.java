/* JTextComponent.java --
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

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.InputMethodListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.text.BreakIterator;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleEditableText;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleText;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;
import javax.swing.TransferHandler;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.InputMapUIResource;
import javax.swing.plaf.TextUI;

public abstract class JTextComponent extends JComponent
  implements Scrollable, Accessible
{
  /**
   * AccessibleJTextComponent implements accessibility hooks for
   * JTextComponent.  It allows an accessibility driver to read and
   * manipulate the text component's contents as well as update UI
   * elements such as the caret.
   */
  public class AccessibleJTextComponent extends AccessibleJComponent implements
      AccessibleText, CaretListener, DocumentListener, AccessibleAction,
      AccessibleEditableText
  {
    private static final long serialVersionUID = 7664188944091413696L;

    /**
     * The caret's offset.
     */
    private int caretDot;

    /**
     * Construct an AccessibleJTextComponent.
     */
    public AccessibleJTextComponent()
    {
      super();
      JTextComponent.this.addCaretListener(this);
      caretDot = getCaretPosition();
    }

    /**
     * Retrieve the current caret position.  The index of the first
     * caret position is 0.
     *
     * @return caret position
     */
    public int getCaretPosition()
    {
      return JTextComponent.this.getCaretPosition();
    }

    /**
     * Retrieve the current text selection.  If no text is selected
     * this method returns null.
     *
     * @return the currently selected text or null
     */
    public String getSelectedText()
    {
      return JTextComponent.this.getSelectedText();
    }

    /**
     * Retrieve the index of the first character in the current text
     * selection.  If there is no text in the text component, this
     * method returns 0.  If there is text in the text component, but
     * there is no selection, this method returns the current caret
     * position.
     *
     * @return the index of the first character in the selection, the
     * current caret position or 0
     */
    public int getSelectionStart()
    {
      if (getSelectedText() == null
          || (JTextComponent.this.getText().equals("")))
        return 0;
      return JTextComponent.this.getSelectionStart();
    }

    /**
     * Retrieve the index of the last character in the current text
     * selection.  If there is no text in the text component, this
     * method returns 0.  If there is text in the text component, but
     * there is no selection, this method returns the current caret
     * position.
     *
     * @return the index of the last character in the selection, the
     * current caret position or 0
     */
    public int getSelectionEnd()
    {
      return JTextComponent.this.getSelectionEnd();
    }

    /**
     * Handle a change in the caret position and fire any applicable
     * property change events.
     *
     * @param e - the caret update event
     */
    public void caretUpdate(CaretEvent e)
    {
      int dot = e.getDot();
      int mark = e.getMark();
      if (caretDot != dot)
        {
          firePropertyChange(ACCESSIBLE_CARET_PROPERTY, new Integer(caretDot),
                             new Integer(dot));
          caretDot = dot;
        }
      if (mark != dot)
        {
          firePropertyChange(ACCESSIBLE_SELECTION_PROPERTY, null,
                             getSelectedText());
        }
    }

    /**
     * Retreive the accessible state set of this component.
     *
     * @return the accessible state set of this component
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet state = super.getAccessibleStateSet();
      if (isEditable())
        state.add(AccessibleState.EDITABLE);
      return state;
    }

    /**
     * Retrieve the accessible role of this component.
     *
     * @return the accessible role of this component
     *
     * @see AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.TEXT;
    }

    /**
     * Retrieve an AccessibleEditableText object that controls this
     * text component.
     *
     * @return this
     */
    public AccessibleEditableText getAccessibleEditableText()
    {
      return this;
    }

    /**
     * Retrieve an AccessibleText object that controls this text
     * component.
     *
     * @return this
     *
     * @see AccessibleText
     */
    public AccessibleText getAccessibleText()
    {
      return this;
    }
    
    /**
     * Handle a text insertion event and fire an
     * AccessibleContext.ACCESSIBLE_TEXT_PROPERTY property change
     * event.
     *
     * @param e - the insertion event
     */
    public void insertUpdate(DocumentEvent e)
    {
      firePropertyChange(ACCESSIBLE_TEXT_PROPERTY, null,
                         new Integer(e.getOffset()));
    }

    /**
     * Handle a text removal event and fire an
     * AccessibleContext.ACCESSIBLE_TEXT_PROPERTY property change
     * event.
     *
     * @param e - the removal event
     */
    public void removeUpdate(DocumentEvent e)
    {
      firePropertyChange(ACCESSIBLE_TEXT_PROPERTY, null,
                         new Integer(e.getOffset()));
    }

    /**
     * Handle a text change event and fire an
     * AccessibleContext.ACCESSIBLE_TEXT_PROPERTY property change
     * event.
     *
     * @param e - text change event
     */
    public void changedUpdate(DocumentEvent e)
    {
      firePropertyChange(ACCESSIBLE_TEXT_PROPERTY, null,
                         new Integer(e.getOffset()));
    }

    /**
     * Get the index of the character at the given point, in component
     * pixel co-ordinates.  If the point argument is invalid this
     * method returns -1.
     *
     * @param p - a point in component pixel co-ordinates
     *
     * @return a character index, or -1
     */
    public int getIndexAtPoint(Point p)
    {
      return viewToModel(p);
    }

    /**
     * Calculate the bounding box of the character at the given index.
     * The returned x and y co-ordinates are relative to this text
     * component's top-left corner.  If the index is invalid this
     * method returns null.
     *
     * @param index - the character index
     *
     * @return a character's bounding box, or null
     */
    public Rectangle getCharacterBounds(int index)
    {
      // This is basically the same as BasicTextUI.modelToView().
      
      Rectangle bounds = null;
      if (index >= 0 && index < doc.getLength() - 1)
        {
          if (doc instanceof AbstractDocument)
            ((AbstractDocument) doc).readLock();
          try
            {
              TextUI ui = getUI();
              if (ui != null)
                {
                  // Get editor rectangle.
                  Rectangle rect = new Rectangle();
                  Insets insets = getInsets();
                  rect.x = insets.left;
                  rect.y = insets.top;
                  rect.width = getWidth() - insets.left - insets.right;
                  rect.height = getHeight() - insets.top - insets.bottom;
                  View rootView = ui.getRootView(JTextComponent.this);
                  if (rootView != null)
                    {
                      rootView.setSize(rect.width, rect.height);
                      Shape s = rootView.modelToView(index,
                                                     Position.Bias.Forward,
                                                     index + 1,
                                                     Position.Bias.Backward,
                                                     rect);
                      if (s != null)
                        bounds = s.getBounds();
                    }
                }
            }
          catch (BadLocationException ex)
            {
              // Ignore (return null).
            }
          finally
            {
              if (doc instanceof AbstractDocument)
                ((AbstractDocument) doc).readUnlock();
            }
        }
      return bounds;
    }

    /**
     * Return the length of the text in this text component.
     *
     * @return a character length
     */
    public int getCharCount()
    {
      return JTextComponent.this.getText().length();
    }

   /** 
    * Gets the character attributes of the character at index. If
    * the index is out of bounds, null is returned.
    *
    * @param index - index of the character
    * 
    * @return the character's attributes
    */
    public AttributeSet getCharacterAttribute(int index)
    {
      AttributeSet atts;
      if (doc instanceof AbstractDocument)
        ((AbstractDocument) doc).readLock();
      try
        {
          Element el = doc.getDefaultRootElement();
          while (! el.isLeaf())
            {
              int i = el.getElementIndex(index);
              el = el.getElement(i);
            }
          atts = el.getAttributes();
        }
      finally
        {
          if (doc instanceof AbstractDocument)
            ((AbstractDocument) doc).readUnlock();
        }
      return atts;
    }

    /**
     * Gets the text located at index. null is returned if the index
     * or part is invalid.
     * 
     * @param part - {@link #CHARACTER}, {@link #WORD}, or {@link #SENTENCE}
     * @param index - index of the part
     * 
     * @return the part of text at that index, or null
     */
    public String getAtIndex(int part, int index)
    {
      return getAtIndexImpl(part, index, 0);
    }
    
    /**
     * Gets the text located after index. null is returned if the index
     * or part is invalid.
     * 
     * @param part - {@link #CHARACTER}, {@link #WORD}, or {@link #SENTENCE}
     * @param index - index after the part
     * 
     * @return the part of text after that index, or null
     */
    public String getAfterIndex(int part, int index)
    {
      return getAtIndexImpl(part, index, 1);
    }

    /**
     * Gets the text located before index. null is returned if the index
     * or part is invalid.
     * 
     * @param part - {@link #CHARACTER}, {@link #WORD}, or {@link #SENTENCE}
     * @param index - index before the part
     * 
     * @return the part of text before that index, or null
     */
    public String getBeforeIndex(int part, int index)
    {
      return getAtIndexImpl(part, index, -1);
    }

    /**
     * Implements getAtIndex(), getBeforeIndex() and getAfterIndex().
     *
     * @param part the part to return, either CHARACTER, WORD or SENTENCE
     * @param index the index
     * @param dir the direction, -1 for backwards, 0 for here, +1 for forwards
     *
     * @return the resulting string
     */
    private String getAtIndexImpl(int part, int index, int dir)
    {
      String ret = null;
      if (doc instanceof AbstractDocument)
        ((AbstractDocument) doc).readLock();
      try
        {
          BreakIterator iter = null;
          switch (part)
          {
            case CHARACTER:
              iter = BreakIterator.getCharacterInstance(getLocale());
              break;
            case WORD:
              iter = BreakIterator.getWordInstance(getLocale());
              break;
            case SENTENCE:
              iter = BreakIterator.getSentenceInstance(getLocale());
              break;
            default:
              break;
          }
          String text = doc.getText(0, doc.getLength() - 1);
          iter.setText(text);
          int start = index;
          int end = index;
          switch (dir)
          {
          case 0:
            if (iter.isBoundary(index))
              {
                start = index;
                end = iter.following(index);
              }
            else
              {
                start = iter.preceding(index);
                end = iter.next();
              }
            break;
          case 1:
            start = iter.following(index);
            end = iter.next();
            break;
          case -1:
            end = iter.preceding(index);
            start = iter.previous();
            break;
          default:
            assert false;
          }
          ret = text.substring(start, end);
        }
      catch (BadLocationException ex)
        {
          // Ignore (return null).
        }
      finally
        {
          if (doc instanceof AbstractDocument)
            ((AbstractDocument) doc).readUnlock();
        }
      return ret;
    }

    /**
     * Returns the number of actions for this object. The zero-th
     * object represents the default action.
     * 
     * @return the number of actions (0-based).
     */
    public int getAccessibleActionCount()
    {
      return getActions().length;
    }
    
    /**
     * Returns the description of the i-th action. Null is returned if
     * i is out of bounds.
     * 
     * @param i - the action to get the description for
     * 
     * @return description of the i-th action
     */
    public String getAccessibleActionDescription(int i)
    {
      String desc = null;
      Action[] actions = getActions();
      if (i >= 0 && i < actions.length)
        desc = (String) actions[i].getValue(Action.NAME);
      return desc;
    }
    
    /**
     * Performs the i-th action. Nothing happens if i is 
     * out of bounds.
     *
     * @param i - the action to perform
     * 
     * @return true if the action was performed successfully
     */
    public boolean doAccessibleAction(int i)
    {
      boolean ret = false;
      Action[] actions = getActions();
      if (i >= 0 && i < actions.length)
        {
          ActionEvent ev = new ActionEvent(JTextComponent.this,
                                           ActionEvent.ACTION_PERFORMED, null);
          actions[i].actionPerformed(ev);
          ret = true;
        }
      return ret;
    }
    
    /**
     * Sets the text contents.
     *
     * @param s - the new text contents.
     */
    public void setTextContents(String s)
    {
      setText(s);
    }

    /**
     * Inserts the text at the given index.
     *
     * @param index - the index to insert the new text at.
     * @param s - the new text
     */
    public void insertTextAtIndex(int index, String s)
    {
      try
        {
          doc.insertString(index, s, null);
        }
      catch (BadLocationException ex)
        {
          // What should we do with this?
          ex.printStackTrace();
        }
    }

    /**
     * Gets the text between two indexes.
     *
     * @param start - the starting index (inclusive)
     * @param end - the ending index (exclusive)
     */
    public String getTextRange(int start, int end)
    {
      try
      {
        return JTextComponent.this.getText(start, end - start);
      }
      catch (BadLocationException ble)
      {
        return "";
      }
    }

    /**
     * Deletes the text between two indexes.
     *
     * @param start - the starting index (inclusive)
     * @param end - the ending index (exclusive)
     */
    public void delete(int start, int end)
    {
      replaceText(start, end, "");
    }

    /**
     * Cuts the text between two indexes. The text is put
     * into the system clipboard.
     *
     * @param start - the starting index (inclusive)
     * @param end - the ending index (exclusive)
     */
    public void cut(int start, int end)
    {
      JTextComponent.this.select(start, end);
      JTextComponent.this.cut();
    }

    /**
     * Pastes the text from the system clipboard to the given index.
     *
     * @param start - the starting index
     */
    public void paste(int start)
    {
      JTextComponent.this.setCaretPosition(start);
      JTextComponent.this.paste();
    }

    /**
     * Replaces the text between two indexes with the given text.
     *
     *
     * @param start - the starting index (inclusive)
     * @param end - the ending index (exclusive)
     * @param s - the text to paste
     */
    public void replaceText(int start, int end, String s)
    {
      JTextComponent.this.select(start, end);
      JTextComponent.this.replaceSelection(s);
    }

    /**
     * Selects the text between two indexes.
     *
     * @param start - the starting index (inclusive)
     * @param end - the ending index (exclusive)
     */
    public void selectText(int start, int end)
    {
      JTextComponent.this.select(start, end);
    }

    /**
     * Sets the attributes of all the text between two indexes.
     *
     * @param start - the starting index (inclusive)
     * @param end - the ending index (exclusive)
     * @param s - the new attribute set for the text in the range
     */
    public void setAttributes(int start, int end, AttributeSet s)
    {
      if (doc instanceof StyledDocument)
        {
          StyledDocument sdoc = (StyledDocument) doc;
          sdoc.setCharacterAttributes(start, end - start, s, true);
        }
    }
  }

  public static class KeyBinding
  {
    public KeyStroke key;
    public String actionName;

    /**
     * Creates a new <code>KeyBinding</code> instance.
     *
     * @param key a <code>KeyStroke</code> value
     * @param actionName a <code>String</code> value
     */
    public KeyBinding(KeyStroke key, String actionName)
    {
      this.key = key;
      this.actionName = actionName;
    }
  }

  /**
   * According to <a
   * href="http://java.sun.com/products/jfc/tsc/special_report/kestrel/keybindings.html">this
   * report</a>, a pair of private classes wraps a {@link
   * javax.swing.text.Keymap} in the new {@link InputMap} / {@link
   * ActionMap} interfaces, such that old Keymap-using code can make use of
   * the new framework.
   *
   * <p>A little bit of experimentation with these classes reveals the following
   * structure:
   *
   * <ul>
   *
   * <li>KeymapWrapper extends {@link InputMap} and holds a reference to
   * the underlying {@link Keymap}.</li>
   *
   * <li>KeymapWrapper maps {@link KeyStroke} objects to {@link Action}
   * objects, by delegation to the underlying {@link Keymap}.</li>
   *
   * <li>KeymapActionMap extends {@link ActionMap} also holds a reference to
   * the underlying {@link Keymap} but only appears to use it for listing 
   * its keys. </li>
   *
   * <li>KeymapActionMap maps all {@link Action} objects to
   * <em>themselves</em>, whether they exist in the underlying {@link
   * Keymap} or not, and passes other objects to the parent {@link
   * ActionMap} for resolving.
   *
   * </ul>
   */

  private class KeymapWrapper extends InputMap
  {
    Keymap map;

    public KeymapWrapper(Keymap k)
    {
      map = k;
    }

    public int size()
    {
      return map.getBoundKeyStrokes().length + super.size();
    }

    public Object get(KeyStroke ks)
    {
      Action mapped = null;
      Keymap m = map;
      while(mapped == null && m != null)
        {
          mapped = m.getAction(ks);
          if (mapped == null && ks.getKeyEventType() == KeyEvent.KEY_TYPED)
            mapped = m.getDefaultAction();
          if (mapped == null)
            m = m.getResolveParent();
        }

      if (mapped == null)
        return super.get(ks);
      else
        return mapped;
    }

    public KeyStroke[] keys()
    {
      KeyStroke[] superKeys = super.keys();
      KeyStroke[] mapKeys = map.getBoundKeyStrokes(); 
      KeyStroke[] bothKeys = new KeyStroke[superKeys.length + mapKeys.length];
      for (int i = 0; i < superKeys.length; ++i)
        bothKeys[i] = superKeys[i];
      for (int i = 0; i < mapKeys.length; ++i)
        bothKeys[i + superKeys.length] = mapKeys[i];
      return bothKeys;
    }

    public KeyStroke[] allKeys()
    {
      KeyStroke[] superKeys = super.allKeys();
      KeyStroke[] mapKeys = map.getBoundKeyStrokes();
      int skl = 0;
      int mkl = 0;
      if (superKeys != null)
        skl = superKeys.length;
      if (mapKeys != null)
        mkl = mapKeys.length;
      KeyStroke[] bothKeys = new KeyStroke[skl + mkl];
      for (int i = 0; i < skl; ++i)
        bothKeys[i] = superKeys[i];
      for (int i = 0; i < mkl; ++i)
        bothKeys[i + skl] = mapKeys[i];
      return bothKeys;
    }
  }

  private class KeymapActionMap extends ActionMap
  {
    Keymap map;

    public KeymapActionMap(Keymap k)
    {
      map = k;
    }

    public Action get(Object cmd)
    {
      if (cmd instanceof Action)
        return (Action) cmd;
      else
        return super.get(cmd);
    }

    public int size()
    {
      return map.getBoundKeyStrokes().length + super.size();
    }

    public Object[] keys() 
    {
      Object[] superKeys = super.keys();
      Object[] mapKeys = map.getBoundKeyStrokes(); 
      Object[] bothKeys = new Object[superKeys.length + mapKeys.length];
      for (int i = 0; i < superKeys.length; ++i)
        bothKeys[i] = superKeys[i];
      for (int i = 0; i < mapKeys.length; ++i)
        bothKeys[i + superKeys.length] = mapKeys[i];
      return bothKeys;      
    }

    public Object[] allKeys()
    {
      Object[] superKeys = super.allKeys();
      Object[] mapKeys = map.getBoundKeyStrokes(); 
      Object[] bothKeys = new Object[superKeys.length + mapKeys.length];
      for (int i = 0; i < superKeys.length; ++i)
        bothKeys[i] = superKeys[i];
      for (int i = 0; i < mapKeys.length; ++i)
        bothKeys[i + superKeys.length] = mapKeys[i];
      return bothKeys;
    }

  }

  static class DefaultKeymap implements Keymap
  {
    String name;
    Keymap parent;
    Hashtable map;
    Action defaultAction;

    public DefaultKeymap(String name)
    {
      this.name = name;
      this.map = new Hashtable();
    }

    public void addActionForKeyStroke(KeyStroke key, Action a)
    {
      map.put(key, a);
    }

    /**
     * Looks up a KeyStroke either in the current map or the parent Keymap;
     * does <em>not</em> return the default action if lookup fails.
     *
     * @param key The KeyStroke to look up an Action for.
     *
     * @return The mapping for <code>key</code>, or <code>null</code>
     * if no mapping exists in this Keymap or any of its parents.
     */
    public Action getAction(KeyStroke key)
    {
      if (map.containsKey(key))
        return (Action) map.get(key);
      else if (parent != null)
        return parent.getAction(key);
      else
        return null;
    }

    public Action[] getBoundActions()
    {
      Action [] ret = new Action[map.size()];
      Enumeration e = map.elements();
      int i = 0;
      while (e.hasMoreElements())
        {
          ret[i++] = (Action) e.nextElement();
        }
      return ret;
    }

    public KeyStroke[] getBoundKeyStrokes()
    {
      KeyStroke [] ret = new KeyStroke[map.size()];
      Enumeration e = map.keys();
      int i = 0;
      while (e.hasMoreElements())
        {
          ret[i++] = (KeyStroke) e.nextElement();
        }
      return ret;
    }

    public Action getDefaultAction()
    {
      return defaultAction;
    }

    public KeyStroke[] getKeyStrokesForAction(Action a)
    {
      int i = 0;
      Enumeration e = map.keys();
      while (e.hasMoreElements())
        {
          if (map.get(e.nextElement()).equals(a))
            ++i;
        }
      KeyStroke [] ret = new KeyStroke[i];
      i = 0;
      e = map.keys();
      while (e.hasMoreElements())
        {          
          KeyStroke k = (KeyStroke) e.nextElement();
          if (map.get(k).equals(a))
            ret[i++] = k;            
        }
      return ret;
    }

    public String getName()
    {
      return name;
    }

    public Keymap getResolveParent()
    {
      return parent;
    }

    public boolean isLocallyDefined(KeyStroke key)
    {
      return map.containsKey(key);
    }

    public void removeBindings()
    {
      map.clear();
    }

    public void removeKeyStrokeBinding(KeyStroke key)
    {
      map.remove(key);
    }

    public void setDefaultAction(Action a)
    {
      defaultAction = a;
    }

    public void setResolveParent(Keymap p)
    {
      parent = p;
    }
  }

  class DefaultTransferHandler extends TransferHandler
  {
    public boolean canImport(JComponent component, DataFlavor[] flavors)
    {
      JTextComponent textComponent = (JTextComponent) component;
      
      if (! (textComponent.isEnabled()
	     && textComponent.isEditable()
	     && flavors != null))
        return false;

      for (int i = 0; i < flavors.length; ++i)
	if (flavors[i].equals(DataFlavor.stringFlavor))
	   return true;

      return false;
    }
    
    public void exportToClipboard(JComponent component, Clipboard clipboard,
				  int action)
    {
      JTextComponent textComponent = (JTextComponent) component;
      int start = textComponent.getSelectionStart();
      int end = textComponent.getSelectionEnd();

      if (start == end)
        return;

      try
        {
          // Copy text to clipboard.
          String data = textComponent.getDocument().getText(start, end);
          StringSelection selection = new StringSelection(data);
          clipboard.setContents(selection, null);

          // Delete selected text on cut action.
          if (action == MOVE)
            doc.remove(start, end - start);
        }
      catch (BadLocationException e)
        {
          // Ignore this and do nothing.
        }
    }
    
    public int getSourceActions()
    {
      return NONE;
    }

    public boolean importData(JComponent component, Transferable transferable)
    {
      DataFlavor flavor = null;
      DataFlavor[] flavors = transferable.getTransferDataFlavors();

      if (flavors == null)
        return false;

      for (int i = 0; i < flavors.length; ++i)
        if (flavors[i].equals(DataFlavor.stringFlavor))
          flavor = flavors[i];
      
      if (flavor == null)
        return false;

      try
        {
          JTextComponent textComponent = (JTextComponent) component;
          String data = (String) transferable.getTransferData(flavor);
          textComponent.replaceSelection(data);
          return true;
        }
      catch (IOException e)
        {
          // Ignored.
        }
      catch (UnsupportedFlavorException e)
        {
          // Ignored.
        }

      return false;
    }
  }

  private static final long serialVersionUID = -8796518220218978795L;
  
  public static final String DEFAULT_KEYMAP = "default";
  public static final String FOCUS_ACCELERATOR_KEY = "focusAcceleratorKey";
  
  private static DefaultTransferHandler defaultTransferHandler;
  private static Hashtable keymaps = new Hashtable();
  private Keymap keymap;
  private char focusAccelerator = '\0';
  private NavigationFilter navigationFilter;

  /**
   * Get a Keymap from the global keymap table, by name.
   *
   * @param n The name of the Keymap to look up
   *
   * @return A Keymap associated with the provided name, or
   * <code>null</code> if no such Keymap exists
   *
   * @see #addKeymap
   * @see #removeKeymap
   * @see #keymaps
   */
  public static Keymap getKeymap(String n)
  {
    return (Keymap) keymaps.get(n);
  }

  /**
   * Remove a Keymap from the global Keymap table, by name.
   *
   * @param n The name of the Keymap to remove
   *
   * @return The keymap removed from the global table
   *
   * @see #addKeymap
   * @see #getKeymap()
   * @see #keymaps
   */  
  public static Keymap removeKeymap(String n)
  {
    Keymap km = (Keymap) keymaps.get(n);
    keymaps.remove(n);
    return km;
  }

  /**
   * Create a new Keymap with a specific name and parent, and add the new
   * Keymap to the global keymap table. The name may be <code>null</code>,
   * in which case the new Keymap will <em>not</em> be added to the global
   * Keymap table. The parent may also be <code>null</code>, which is
   * harmless.
   * 
   * @param n The name of the new Keymap, or <code>null</code>
   * @param parent The parent of the new Keymap, or <code>null</code>
   *
   * @return The newly created Keymap
   *
   * @see #removeKeymap
   * @see #getKeymap()
   * @see #keymaps
   */
  public static Keymap addKeymap(String n, Keymap parent)
  {
    Keymap k = new DefaultKeymap(n);
    k.setResolveParent(parent);
    if (n != null)
      keymaps.put(n, k);
    return k;
  }

  /**
   * Get the current Keymap of this component.
   *
   * @return The component's current Keymap
   *
   * @see #setKeymap
   * @see #keymap
   */
  public Keymap getKeymap() 
  {
    return keymap;
  }

  /**
   * Set the current Keymap of this component, installing appropriate
   * {@link KeymapWrapper} and {@link KeymapActionMap} objects in the
   * {@link InputMap} and {@link ActionMap} parent chains, respectively,
   * and fire a property change event with name <code>"keymap"</code>.
   *
   * @see #getKeymap()
   * @see #keymap
   */
  public void setKeymap(Keymap k) 
  {

    // phase 1: replace the KeymapWrapper entry in the InputMap chain.
    // the goal here is to always maintain the following ordering:
    //
    //   [InputMap]? -> [KeymapWrapper]? -> [InputMapUIResource]*
    // 
    // that is to say, component-specific InputMaps need to remain children
    // of Keymaps, and Keymaps need to remain children of UI-installed
    // InputMaps (and the order of each group needs to be preserved, of
    // course).
    
    KeymapWrapper kw = (k == null ? null : new KeymapWrapper(k));
    InputMap childInputMap = getInputMap(JComponent.WHEN_FOCUSED);
    if (childInputMap == null)
      setInputMap(JComponent.WHEN_FOCUSED, kw);
    else
      {
        while (childInputMap.getParent() != null 
               && !(childInputMap.getParent() instanceof KeymapWrapper)
               && !(childInputMap.getParent() instanceof InputMapUIResource))
          childInputMap = childInputMap.getParent();

        // option 1: there is nobody to replace at the end of the chain
        if (childInputMap.getParent() == null)
          childInputMap.setParent(kw);
        
        // option 2: there is already a KeymapWrapper in the chain which
        // needs replacing (possibly with its own parents, possibly without)
        else if (childInputMap.getParent() instanceof KeymapWrapper)
          {
            if (kw == null)
              childInputMap.setParent(childInputMap.getParent().getParent());
            else
              {
                kw.setParent(childInputMap.getParent().getParent());
                childInputMap.setParent(kw);
              }
          }

        // option 3: there is an InputMapUIResource in the chain, which marks
        // the place where we need to stop and insert ourselves
        else if (childInputMap.getParent() instanceof InputMapUIResource)
          {
            if (kw != null)
              {
                kw.setParent(childInputMap.getParent());
                childInputMap.setParent(kw);
              }
          }
      }

    // phase 2: replace the KeymapActionMap entry in the ActionMap chain

    KeymapActionMap kam = (k == null ? null : new KeymapActionMap(k));
    ActionMap childActionMap = getActionMap();
    if (childActionMap == null)
      setActionMap(kam);
    else
      {
        while (childActionMap.getParent() != null 
               && !(childActionMap.getParent() instanceof KeymapActionMap)
               && !(childActionMap.getParent() instanceof ActionMapUIResource))
          childActionMap = childActionMap.getParent();

        // option 1: there is nobody to replace at the end of the chain
        if (childActionMap.getParent() == null)
          childActionMap.setParent(kam);
        
        // option 2: there is already a KeymapActionMap in the chain which
        // needs replacing (possibly with its own parents, possibly without)
        else if (childActionMap.getParent() instanceof KeymapActionMap)
          {
            if (kam == null)
              childActionMap.setParent(childActionMap.getParent().getParent());
            else
              {
                kam.setParent(childActionMap.getParent().getParent());
                childActionMap.setParent(kam);
              }
          }

        // option 3: there is an ActionMapUIResource in the chain, which marks
        // the place where we need to stop and insert ourselves
        else if (childActionMap.getParent() instanceof ActionMapUIResource)
          {
            if (kam != null)
              {
                kam.setParent(childActionMap.getParent());
                childActionMap.setParent(kam);
              }
          }
      }

    // phase 3: update the explicit keymap field

    Keymap old = keymap;
    keymap = k;
    firePropertyChange("keymap", old, k);
  }

  /**
   * Resolves a set of bindings against a set of actions and inserts the
   * results into a {@link Keymap}. Specifically, for each provided binding
   * <code>b</code>, if there exists a provided action <code>a</code> such
   * that <code>a.getValue(Action.NAME) == b.ActionName</code> then an
   * entry is added to the Keymap mapping <code>b</code> to
   * <code>a</code>.
   *
   * @param map The Keymap to add new mappings to
   * @param bindings The set of bindings to add to the Keymap
   * @param actions The set of actions to resolve binding names against
   *
   * @see Action#NAME
   * @see Action#getValue
   * @see KeyBinding#actionName
   */
  public static void loadKeymap(Keymap map, 
                                JTextComponent.KeyBinding[] bindings, 
                                Action[] actions)
  {
    Hashtable acts = new Hashtable(actions.length);
    for (int i = 0; i < actions.length; ++i)
      acts.put(actions[i].getValue(Action.NAME), actions[i]);
      for (int i = 0; i < bindings.length; ++i)
      if (acts.containsKey(bindings[i].actionName))
        map.addActionForKeyStroke(bindings[i].key, (Action) acts.get(bindings[i].actionName));
  }

  /**
   * Returns the set of available Actions this component's associated
   * editor can run.  Equivalent to calling
   * <code>getUI().getEditorKit().getActions()</code>. This set of Actions
   * is a reasonable value to provide as a parameter to {@link
   * #loadKeymap}, when resolving a set of {@link KeyBinding} objects
   * against this component.
   *
   * @return The set of available Actions on this component's {@link EditorKit}
   *
   * @see TextUI#getEditorKit
   * @see EditorKit#getActions()
   */
  public Action[] getActions()
  {
    return getUI().getEditorKit(this).getActions();
  }
    
  // These are package-private to avoid an accessor method.
  Document doc;
  Caret caret;
  boolean editable;
  
  private Highlighter highlighter;
  private Color caretColor;
  private Color disabledTextColor;
  private Color selectedTextColor;
  private Color selectionColor;
  private Insets margin;
  private boolean dragEnabled;

  /**
   * Creates a new <code>JTextComponent</code> instance.
   */
  public JTextComponent()
  {
    Keymap defkeymap = getKeymap(DEFAULT_KEYMAP);
    if (defkeymap == null)
      {
        defkeymap = addKeymap(DEFAULT_KEYMAP, null);
        defkeymap.setDefaultAction(new DefaultEditorKit.DefaultKeyTypedAction());
      }

    setFocusable(true);
    setEditable(true);
    enableEvents(AWTEvent.KEY_EVENT_MASK);
    setOpaque(true);
    updateUI();
  }

  public void setDocument(Document newDoc)
  {
    Document oldDoc = doc;
    try
      {
        if (oldDoc instanceof AbstractDocument)
          ((AbstractDocument) oldDoc).readLock();

        doc = newDoc;
        firePropertyChange("document", oldDoc, newDoc);
      }
    finally
      {
        if (oldDoc instanceof AbstractDocument)
          ((AbstractDocument) oldDoc).readUnlock();
      }
    revalidate();
    repaint();
  }

  public Document getDocument()
  {
    return doc;
  }

  /**
   * Get the <code>AccessibleContext</code> of this object.
   *
   * @return an <code>AccessibleContext</code> object
   */
  public AccessibleContext getAccessibleContext()
  {
    return new AccessibleJTextComponent();
  }

  public void setMargin(Insets m)
  {
    margin = m;
  }

  public Insets getMargin()
  {
    return margin;
  }

  public void setText(String text)
  {
    try
      {
        if (doc instanceof AbstractDocument)
          ((AbstractDocument) doc).replace(0, doc.getLength(), text, null);
        else
          {
            doc.remove(0, doc.getLength());
            doc.insertString(0, text, null);
          }
      }
    catch (BadLocationException e)
      {
        // This can never happen.
        throw (InternalError) new InternalError().initCause(e);
      }
  }

  /**
   * Retrieves the current text in this text document.
   *
   * @return the text
   *
   * @exception NullPointerException if the underlaying document is null
   */
  public String getText()
  {
    if (doc == null)
      return null;

    try
      {
        return doc.getText(0, doc.getLength());
      }
    catch (BadLocationException e)
      {
        // This should never happen.
        return "";
      }
  }

  /**
   * Retrieves a part of the current text in this document.
   *
   * @param offset the postion of the first character
   * @param length the length of the text to retrieve
   *
   * @return the text
   *
   * @exception BadLocationException if arguments do not hold pre-conditions
   */
  public String getText(int offset, int length)
    throws BadLocationException
  {
    return getDocument().getText(offset, length);
  }

  /**
   * Retrieves the currently selected text in this text document.
   *
   * @return the selected text
   *
   * @exception NullPointerException if the underlaying document is null
   */
  public String getSelectedText()
  {
    int start = getSelectionStart();
    int offset = getSelectionEnd() - start;
    
    if (offset <= 0)
      return null;
    
    try
      {
        return doc.getText(start, offset);
      }
    catch (BadLocationException e)
      {
        // This should never happen.
        return null;
      }
  }

  /**
   * Returns a string that specifies the name of the Look and Feel class
   * that renders this component.
   *
   * @return the string "TextComponentUI"
   */
  public String getUIClassID()
  {
    return "TextComponentUI";
  }

  /**
   * Returns a string representation of this JTextComponent.
   */
  protected String paramString()
  {
    // TODO: Do something useful here.
    return super.paramString();
  }

  /**
   * This method returns the label's UI delegate.
   *
   * @return The label's UI delegate.
   */
  public TextUI getUI()
  {
    return (TextUI) ui;
  }

  /**
   * This method sets the label's UI delegate.
   *
   * @param newUI The label's UI delegate.
   */
  public void setUI(TextUI newUI)
  {
    super.setUI(newUI);
  }

  /**
   * This method resets the label's UI delegate to the default UI for the
   * current look and feel.
   */
  public void updateUI()
  {
    setUI((TextUI) UIManager.getUI(this));
  }

  public Dimension getPreferredScrollableViewportSize()
  {
    return getPreferredSize();
  }

  public int getScrollableUnitIncrement(Rectangle visible, int orientation,
                                        int direction)
  {
    // We return 1/10 of the visible area as documented in Sun's API docs.
    if (orientation == SwingConstants.HORIZONTAL)
      return visible.width / 10;
    else if (orientation == SwingConstants.VERTICAL)
      return visible.height / 10;
    else
      throw new IllegalArgumentException("orientation must be either "
                                      + "javax.swing.SwingConstants.VERTICAL "
                                      + "or "
                                      + "javax.swing.SwingConstants.HORIZONTAL"
                                         );
  }

  public int getScrollableBlockIncrement(Rectangle visible, int orientation,
                                         int direction)
  {
    // We return the whole visible area as documented in Sun's API docs.
    if (orientation == SwingConstants.HORIZONTAL)
      return visible.width;
    else if (orientation == SwingConstants.VERTICAL)
      return visible.height;
    else
      throw new IllegalArgumentException("orientation must be either "
                                      + "javax.swing.SwingConstants.VERTICAL "
                                      + "or "
                                      + "javax.swing.SwingConstants.HORIZONTAL"
                                         );
  }

  /**
   * Checks whether this text component it editable.
   *
   * @return true if editable, false otherwise
   */
  public boolean isEditable()
  {
    return editable;
  }

  /**
   * Enables/disabled this text component's editability.
   *
   * @param newValue true to make it editable, false otherwise.
   */
  public void setEditable(boolean newValue)
  {
    if (editable == newValue)
      return;
    
    boolean oldValue = editable;
    editable = newValue;
    firePropertyChange("editable", oldValue, newValue);
  }

  /**
   * The <code>Caret</code> object used in this text component.
   *
   * @return the caret object
   */
  public Caret getCaret()
  {
    return caret;
  }

  /**
   * Sets a new <code>Caret</code> for this text component.
   *
   * @param newCaret the new <code>Caret</code> to set
   */
  public void setCaret(Caret newCaret)
  {
    if (caret != null)
      caret.deinstall(this);
    
    Caret oldCaret = caret;
    caret = newCaret;

    if (caret != null)
      caret.install(this);
    
    firePropertyChange("caret", oldCaret, newCaret);
  }

  public Color getCaretColor()
  {
    return caretColor;
  }

  public void setCaretColor(Color newColor)
  {
    Color oldCaretColor = caretColor;
    caretColor = newColor;
    firePropertyChange("caretColor", oldCaretColor, newColor);
  }

  public Color getDisabledTextColor()
  {
    return disabledTextColor;
  }

  public void setDisabledTextColor(Color newColor)
  {
    Color oldColor = disabledTextColor;
    disabledTextColor = newColor;
    firePropertyChange("disabledTextColor", oldColor, newColor);
  }

  public Color getSelectedTextColor()
  {
    return selectedTextColor;
  }

  public void setSelectedTextColor(Color newColor)
  {
    Color oldColor = selectedTextColor;
    selectedTextColor = newColor;
    firePropertyChange("selectedTextColor", oldColor, newColor);
  }

  public Color getSelectionColor()
  {
    return selectionColor;
  }

  public void setSelectionColor(Color newColor)
  {
    Color oldColor = selectionColor;
    selectionColor = newColor;
    firePropertyChange("selectionColor", oldColor, newColor);
  }

  /**
   * Retrisves the current caret position.
   *
   * @return the current position
   */
  public int getCaretPosition()
  {
    return caret.getDot();
  }

  /**
   * Sets the caret to a new position.
   *
   * @param position the new position
   */
  public void setCaretPosition(int position)
  {
    if (doc == null)
      return;

    if (position < 0 || position > doc.getLength())
      throw new IllegalArgumentException();

    caret.setDot(position);
  }

  /**
   * Moves the caret to a given position. This selects the text between
   * the old and the new position of the caret.
   */
  public void moveCaretPosition(int position)
  {
    if (doc == null)
      return;

    if (position < 0 || position > doc.getLength())
      throw new IllegalArgumentException();

    caret.moveDot(position);
  }

  public Highlighter getHighlighter()
  {
    return highlighter;
  }

  public void setHighlighter(Highlighter newHighlighter)
  {
    if (highlighter != null)
      highlighter.deinstall(this);
    
    Highlighter oldHighlighter = highlighter;
    highlighter = newHighlighter;

    if (highlighter != null)
      highlighter.install(this);
    
    firePropertyChange("highlighter", oldHighlighter, newHighlighter);
  }

  /**
   * Returns the start postion of the currently selected text.
   *
   * @return the start postion
   */
  public int getSelectionStart()
  {
    return Math.min(caret.getDot(), caret.getMark());
  }

  /**
   * Selects the text from the given postion to the selection end position.
   *
   * @param start the start positon of the selected text.
   */
  public void setSelectionStart(int start)
  {
    select(start, getSelectionEnd());
  }

  /**
   * Returns the end postion of the currently selected text.
   *
   * @return the end postion
   */
  public int getSelectionEnd()
  {
    return Math.max(caret.getDot(), caret.getMark());
  }

  /**
   * Selects the text from the selection start postion to the given position.
   *
   * @param end the end positon of the selected text.
   */
  public void setSelectionEnd(int end)
  {
    select(getSelectionStart(), end);
  }

  /**
   * Selects a part of the content of the text component.
   *
   * @param start the start position of the selected text
   * @param end the end position of the selected text
   */
  public void select(int start, int end)
  {
    int length = doc.getLength();
    
    start = Math.max(start, 0);
    start = Math.min(start, length);

    end = Math.max(end, start);
    end = Math.min(end, length);

    setCaretPosition(start);
    moveCaretPosition(end);
  }

  /**
   * Selects the whole content of the text component.
   */
  public void selectAll()
  {
    select(0, doc.getLength());
  }

  public synchronized void replaceSelection(String content)
  {
    int dot = caret.getDot();
    int mark = caret.getMark();

    // If content is empty delete selection.
    if (content == null)
      {
        caret.setDot(dot);
        return;
      }

    try
      {
        int start = getSelectionStart();
        int end = getSelectionEnd();

        // Remove selected text.
        if (dot != mark)
          doc.remove(start, end - start);

        // Insert new text.
        doc.insertString(start, content, null);

        // Set dot to new position,
        dot = start + content.length();
        setCaretPosition(dot);
        
        // and update it's magic position.
        caret.setMagicCaretPosition(modelToView(dot).getLocation());
      }
    catch (BadLocationException e)
      {
        // This should never happen.
      }
  }

  public boolean getScrollableTracksViewportHeight()
  {
    if (getParent() instanceof JViewport)
      return getParent().getHeight() > getPreferredSize().height;

    return false;
  }

  public boolean getScrollableTracksViewportWidth()
  {
    boolean res = false;
    Container c = getParent();
    if (c instanceof JViewport)
      res = ((JViewport) c).getExtentSize().width > getPreferredSize().width;

    return res;
  }

  /**
   * Adds a <code>CaretListener</code> object to this text component.
   *
   * @param listener the listener to add
   */
  public void addCaretListener(CaretListener listener)
  {
    listenerList.add(CaretListener.class, listener);
  }

  /**
   * Removed a <code>CaretListener</code> object from this text component.
   *
   * @param listener the listener to remove
   */
  public void removeCaretListener(CaretListener listener)
  {
    listenerList.remove(CaretListener.class, listener);
  }

  /**
   * Returns all added <code>CaretListener</code> objects.
   *
   * @return an array of listeners
   */
  public CaretListener[] getCaretListeners()
  {
    return (CaretListener[]) getListeners(CaretListener.class);
  }

  /**
   * Notifies all registered <code>CaretListener</code> objects that the caret
   * was updated.
   *
   * @param event the event to send
   */
  protected void fireCaretUpdate(CaretEvent event)
  {
    CaretListener[] listeners = getCaretListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].caretUpdate(event);
  }

  /**
   * Adds an <code>InputListener</code> object to this text component.
   *
   * @param listener the listener to add
   */
  public void addInputMethodListener(InputMethodListener listener)
  {
    listenerList.add(InputMethodListener.class, listener);
  }

  /**
   * Removes an <code>InputListener</code> object from this text component.
   *
   * @param listener the listener to remove
   */
  public void removeInputMethodListener(InputMethodListener listener)
  {
    listenerList.remove(InputMethodListener.class, listener);
  }

  /**
   * Returns all added <code>InputMethodListener</code> objects.
   *
   * @return an array of listeners
   */
  public InputMethodListener[] getInputMethodListeners()
  {
    return (InputMethodListener[]) getListeners(InputMethodListener.class);
  }

  public Rectangle modelToView(int position) throws BadLocationException
  {
    return getUI().modelToView(this, position);
  }

  public boolean getDragEnabled()
  {
    return dragEnabled;
  }

  public void setDragEnabled(boolean enabled)
  {
    dragEnabled = enabled;
  }

  public int viewToModel(Point pt)
  {
    return getUI().viewToModel(this, pt);
  }

  public void copy()
  {
    if (isEnabled())
    doTransferAction("copy", TransferHandler.getCopyAction());
  }

  public void cut()
  {
    if (editable && isEnabled())
      doTransferAction("cut", TransferHandler.getCutAction());
  }

  public void paste()
  {
    if (editable && isEnabled())
      doTransferAction("paste", TransferHandler.getPasteAction());
  }

  private void doTransferAction(String name, Action action)
  {
    // Install default TransferHandler if none set.
    if (getTransferHandler() == null)
      {
        if (defaultTransferHandler == null)
          defaultTransferHandler = new DefaultTransferHandler();

        setTransferHandler(defaultTransferHandler);
      }

    // Perform action.
    ActionEvent event = new ActionEvent(this, ActionEvent.ACTION_PERFORMED,
                                        action.getValue(Action.NAME).toString());
    action.actionPerformed(event);
  }

  public void setFocusAccelerator(char newKey)
  {
    if (focusAccelerator == newKey)
      return;

    char oldKey = focusAccelerator;
    focusAccelerator = newKey;
    firePropertyChange(FOCUS_ACCELERATOR_KEY, oldKey, newKey);
  }
  
  public char getFocusAccelerator()
  {
    return focusAccelerator;
  }

  /**
   * @since 1.4
   */
  public NavigationFilter getNavigationFilter()
  {
    return navigationFilter;
  }

  /**
   * @since 1.4
   */
  public void setNavigationFilter(NavigationFilter filter)
  {
    navigationFilter = filter;
  }
  
  /**
   * Read and set the content this component. If not overridden, the
   * method reads the component content as a plain text.
   *
   * The second parameter of this method describes the input stream. It can
   * be String, URL, File and so on. If not null, this object is added to
   * the properties of the associated document under the key
   * {@link Document#StreamDescriptionProperty}.
   *
   * @param input an input stream to read from.
   * @param streamDescription an object, describing the stream.
   *
   * @throws IOException if the reader throws it.
   *
   * @see #getDocument()
   * @see Document#getProperty(Object)
   */
  public void read(Reader input, Object streamDescription)
            throws IOException
  {
    if (streamDescription != null)
      {
        Document d = getDocument();
        if (d != null)
          d.putProperty(Document.StreamDescriptionProperty, streamDescription);
      }

    CPStringBuilder b = new CPStringBuilder();
    int c;

    // Read till -1 (EOF).
    while ((c = input.read()) >= 0)
      b.append((char) c);

    setText(b.toString());
  }

  /**
   * Write the content of this component to the given stream. If not
   * overridden, the method writes the component content as a plain text.
   *
   * @param output the writer to write into.
   *
   * @throws IOException if the writer throws it.
   */
  public void write(Writer output)
             throws IOException
  {
    output.write(getText());
  }

  /**
   * Returns the tooltip text for this text component for the given mouse
   * event. This forwards the call to
   * {@link TextUI#getToolTipText(JTextComponent, Point)}.
   *
   * @param ev the mouse event
   *
   * @return the tooltip text for this text component for the given mouse
   *         event
   */
  public String getToolTipText(MouseEvent ev)
  {
    return getUI().getToolTipText(this, ev.getPoint());
  }
}
