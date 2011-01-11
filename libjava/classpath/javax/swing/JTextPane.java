/* JTextPane.java -- A powerful text widget supporting styled text
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Component;

import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;

/**
 * A powerful text component that supports styled content as well as
 * embedding images and components. It is entirely based on a
 * {@link StyledDocument} content model and a {@link StyledEditorKit}.
 *
 * @author Roman Kennke (roman@kennke.org)
 * @author Andrew Selkirk
 */
public class JTextPane
  extends JEditorPane
{
  /**
   * Creates a new <code>JTextPane</code> with a <code>null</code> document.
   */
  public JTextPane()
  {
    super();
  }

  /**
   * Creates a new <code>JTextPane</code> and sets the specified
   * <code>document</code>.
   *
   * @param document the content model to use
   */
  public JTextPane(StyledDocument document)
  {
    this();
    setStyledDocument(document);
  }

  /**
   * Returns the UI class ID. This is <code>TextPaneUI</code>.
   *
   * @return <code>TextPaneUI</code>
   */
  public String getUIClassID()
  {
    return "TextPaneUI";
  }

  /**
   * Sets the content model for this <code>JTextPane</code>.
   * <code>JTextPane</code> can only be used with {@link StyledDocument}s,
   * if you try to set a different type of <code>Document</code>, an
   * <code>IllegalArgumentException</code> is thrown.
   *
   * @param document the content model to set
   *
   * @throws IllegalArgumentException if <code>document</code> is not an
   *         instance of <code>StyledDocument</code>
   *
   * @see #setStyledDocument
   */
  public void setDocument(Document document)
  {
    if (document != null && !(document instanceof StyledDocument))
      throw new IllegalArgumentException
        ("JTextPane can only handle StyledDocuments");

    setStyledDocument((StyledDocument) document);
  }

  /**
   * Returns the {@link StyledDocument} that is the content model for
   * this <code>JTextPane</code>. This is a typed wrapper for
   * {@link #getDocument()}.
   *
   * @return the content model of this <code>JTextPane</code>
   */
  public StyledDocument getStyledDocument()
  {
    return (StyledDocument) super.getDocument();
  }

  /**
   * Sets the content model for this <code>JTextPane</code>.
   *
   * @param document the content model to set
   */
  public void setStyledDocument(StyledDocument document)
  {
    super.setDocument(document);
  }

  /**
   * Replaces the currently selected text with the specified
   * <code>content</code>. If there is no selected text, this results
   * in a simple insertion at the current caret position. If there is
   * no <code>content</code> specified, this results in the selection
   * beeing deleted.
   *
   * @param content the text with which the selection is replaced
   */
  public void replaceSelection(String content)
  {
    Caret caret = getCaret();
    StyledDocument doc = getStyledDocument();
    AttributeSet a = getInputAttributes().copyAttributes();
    if (doc == null)
      return;

    int dot = caret.getDot();
    int mark = caret.getMark();

    int p0 = Math.min (dot, mark);
    int p1 = Math.max (dot, mark);

    try
      {
        if (doc instanceof AbstractDocument)
          ((AbstractDocument)doc).replace(p0, p1 - p0, content, a);
        else
          {
            // Remove selected text.
            if (dot != mark)
              doc.remove(p0, p1 - p0);
            // Insert new text.
            if (content != null && content.length() > 0)
              doc.insertString(p0, content, a);
          }
      }
    catch (BadLocationException e)
      {
        throw new AssertionError
          ("No BadLocationException should be thrown here");
      }
  }

  /**
   * Inserts an AWT or Swing component into the text at the current caret
   * position.
   *
   * @param component the component to be inserted
   */
  public void insertComponent(Component component)
  {
    SimpleAttributeSet atts = new SimpleAttributeSet();
    atts.addAttribute(StyleConstants.ComponentAttribute, component);
    atts.addAttribute(StyleConstants.NameAttribute,
                      StyleConstants.ComponentElementName);
    try
      {
        getDocument().insertString(getCaret().getDot(), " ", atts);
      }
    catch (BadLocationException ex)
      {
        AssertionError err = new AssertionError("Unexpected bad location");
        err.initCause(ex);
        throw err;
      }
  }

  /**
   * Inserts an <code>Icon</code> into the text at the current caret position.
   *
   * @param icon the <code>Icon</code> to be inserted
   */
  public void insertIcon(Icon icon)
  {
    MutableAttributeSet inputAtts = getInputAttributes();
    inputAtts.removeAttributes(inputAtts);
    StyleConstants.setIcon(inputAtts, icon);
    replaceSelection(" ");
    inputAtts.removeAttributes(inputAtts);
  }

  /**
   * Adds a style into the style hierarchy. Unspecified style attributes
   * can be resolved in the <code>parent</code> style, if one is specified.
   *
   * While it is legal to add nameless styles (<code>nm == null</code),
   * you must be aware that the client application is then responsible
   * for managing the style hierarchy, since unnamed styles cannot be
   * looked up by their name.
   *
   * @param nm the name of the style or <code>null</code> if the style should
   *           be unnamed
   * @param parent the parent in which unspecified style attributes are
   *           resolved, or <code>null</code> if that is not necessary
   *
   * @return the newly created <code>Style</code>
   */
  public Style addStyle(String nm, Style parent)
  {
    return getStyledDocument().addStyle(nm, parent);
  }

  /**
   * Removes a named <code>Style</code> from the style hierarchy.
   *
   * @param nm the name of the <code>Style</code> to be removed
   */
  public void removeStyle(String nm)
  {
    getStyledDocument().removeStyle(nm);
  }

  /**
   * Looks up and returns a named <code>Style</code>.
   *
   * @param nm the name of the <code>Style</code>
   *
   * @return the found <code>Style</code> of <code>null</code> if no such
   *         <code>Style</code> exists
   */
  public Style getStyle(String nm)
  {
    return getStyledDocument().getStyle(nm);
  }

  /**
   * Returns the logical style of the paragraph at the current caret position.
   *
   * @return the logical style of the paragraph at the current caret position
   */
  public Style getLogicalStyle()
  {
    return getStyledDocument().getLogicalStyle(getCaretPosition());
  }

  /**
   * Sets the logical style for the paragraph at the current caret position.
   *
   * @param style the style to set for the current paragraph
   */
  public void setLogicalStyle(Style style)
  {
    getStyledDocument().setLogicalStyle(getCaretPosition(), style);
  }

  /**
   * Returns the text attributes for the character at the current caret
   * position.
   *
   * @return the text attributes for the character at the current caret
   *         position
   */
  public AttributeSet getCharacterAttributes()
  {
    StyledDocument doc = getStyledDocument();
    Element el = doc.getCharacterElement(getCaretPosition());
    return el.getAttributes();
  }

  /**
   * Sets text attributes for the current selection. If there is no selection
   * the text attributes are applied to newly inserted text
   *
   * @param attribute the text attributes to set
   * @param replace if <code>true</code>, the attributes of the current
   *     selection are overridden, otherwise they are merged
   *
   * @see #getInputAttributes
   */
  public void setCharacterAttributes(AttributeSet attribute,
                                     boolean replace)
  {
    int dot = getCaret().getDot();
    int start = getSelectionStart();
    int end = getSelectionEnd();
    if (start == dot && end == dot)
      // There is no selection, update insertAttributes instead
      {
        MutableAttributeSet inputAttributes =
          getStyledEditorKit().getInputAttributes();
        if (replace)
          inputAttributes.removeAttributes(inputAttributes);
        inputAttributes.addAttributes(attribute);
      }
    else
      getStyledDocument().setCharacterAttributes(start, end - start, attribute,
                                                 replace);
  }

  /**
   * Returns the text attributes of the paragraph at the current caret
   * position.
   *
   * @return the attributes of the paragraph at the current caret position
   */
  public AttributeSet getParagraphAttributes()
  {
    StyledDocument doc = getStyledDocument();
    Element el = doc.getParagraphElement(getCaretPosition());
    return el.getAttributes();
  }

  /**
   * Sets text attributes for the paragraph at the current selection.
   * If there is no selection the text attributes are applied to
   * the paragraph at the current caret position.
   *
   * @param attribute the text attributes to set
   * @param replace if <code>true</code>, the attributes of the current
   *     selection are overridden, otherwise they are merged
   */
  public void setParagraphAttributes(AttributeSet attribute,
                                     boolean replace)
  {
    // TODO
  }

  /**
   * Returns the attributes that are applied to newly inserted text.
   * This is a {@link MutableAttributeSet}, so you can easily modify these
   * attributes.
   *
   * @return the attributes that are applied to newly inserted text
   */
  public MutableAttributeSet getInputAttributes()
  {
    return getStyledEditorKit().getInputAttributes();
  }

  /**
   * Returns the {@link StyledEditorKit} that is currently used by this
   * <code>JTextPane</code>.
   *
   * @return the current <code>StyledEditorKit</code> of this
   *         <code>JTextPane</code>
   */
  protected final StyledEditorKit getStyledEditorKit()
  {
    return (StyledEditorKit) getEditorKit();
  }

  /**
   * Creates the default {@link EditorKit} that is used in
   * <code>JTextPane</code>s. This is an instance of {@link StyledEditorKit}.
   *
   * @return the default {@link EditorKit} that is used in
   *         <code>JTextPane</code>s
   */
  protected EditorKit createDefaultEditorKit()
  {
    return new StyledEditorKit();
  }

  /**
   * Sets the {@link EditorKit} to use for this <code>JTextPane</code>.
   * <code>JTextPane</code>s can only handle {@link StyledEditorKit}s,
   * if client programs try to set a different type of <code>EditorKit</code>
   * then an IllegalArgumentException is thrown
   *
   * @param editor the <code>EditorKit</code> to set
   *
   * @throws IllegalArgumentException if <code>editor</code> is no
   *         <code>StyledEditorKit</code>
   */
  public final void setEditorKit(EditorKit editor)
  {
    if (!(editor instanceof StyledEditorKit))
      throw new IllegalArgumentException
        ("JTextPanes can only handle StyledEditorKits");
    super.setEditorKit(editor);
  }

  /**
   * Returns a param string that can be used for debugging.
   *
   * @return a param string that can be used for debugging.
   */
  protected String paramString()
  {
    return super.paramString(); // TODO
  }
}
