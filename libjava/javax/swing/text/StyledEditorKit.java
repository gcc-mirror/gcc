/* StyledEditorKit.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package javax.swing.text;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;

import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;

/**
 * StyledEditorKit
 *
 * @author Andrew Selkirk
 */
public class StyledEditorKit extends DefaultEditorKit
{
  private static final long serialVersionUID = 7002391892985555948L;

  /**
   * UnderlineAction
   */
  public static class UnderlineAction extends StyledEditorKit.StyledTextAction
  {
    /**
     * Constructor UnderlineAction
     */
    public UnderlineAction()
    {
      super("TODO");
      // TODO
    }

    /**
     * actionPerformed
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    }
  }

  /**
   * ItalicAction
   */
  public static class ItalicAction extends StyledEditorKit.StyledTextAction
  {
    /**
     * Constructor ItalicAction
     */
    public ItalicAction()
    {
      super("TODO");
      // TODO
    }

    /**
     * actionPerformed
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    }
  }

  /**
   * BoldAction
   */
  public static class BoldAction extends StyledEditorKit.StyledTextAction
  {
    /**
     * Constructor BoldAction
     */
    public BoldAction()
    {
      super("TODO");
      // TODO
    }

    /**
     * actionPerformed
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    }
  }

  /**
   * AlignmentAction
   */
  public static class AlignmentAction extends StyledEditorKit.StyledTextAction
  {
    /**
     * a
     */
    private int a;

    /**
     * Constructor AlignmentAction
     * @param nm TODO
     * @param a TODO
     */
    public AlignmentAction(String nm, int a)
    {
      super("TODO");
      // TODO
    }

    /**
     * actionPerformed
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    }
  }

  /**
   * ForegroundAction
   */
  public static class ForegroundAction extends StyledEditorKit.StyledTextAction
  {
    /**
     * fg
     */
    private Color fg;

    /**
     * Constructor ForegroundAction
     * @param nm TODO
     * @param fg TODO
     */
    public ForegroundAction(String nm, Color fg)
    {
      super("TODO");
      // TODO
    }

    /**
     * actionPerformed
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    }
  }

  /**
   * FontSizeAction
   */
  public static class FontSizeAction extends StyledEditorKit.StyledTextAction
  {
    /**
     * size
     */
    private int size;

    /**
     * Constructor FontSizeAction
     * @param nm TODO
     * @param size TODO
     */
    public FontSizeAction(String nm, int size)
    {
      super("TODO");
      // TODO
    }

    /**
     * actionPerformed
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    }
  }

  /**
   * FontFamilyAction
   */
  public static class FontFamilyAction extends StyledEditorKit.StyledTextAction
  {
    /**
     * family
     */
    private String family;

    /**
     * Constructor FontFamilyAction
     * @param nm TODO
     * @param family TODO
     */
    public FontFamilyAction(String nm, String family)
    {
      super("TODO");
      // TODO
    }

    /**
     * actionPerformed
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    }
  }

  /**
   * StyledTextAction
   */
  public abstract static class StyledTextAction extends TextAction
  {
    /**
     * Constructor StyledTextAction
     * @param nm TODO
     */
    public StyledTextAction(String nm)
    {
      super(nm);
      // TODO
    }

    /**
     * getEditor
     * @param event TODO
     * @returns JEditorPane
     */
    protected final JEditorPane getEditor(ActionEvent event)
    {
      return null; // TODO
    }

    /**
     * setCharacterAttributes
     * @param value0 TODO
     * @param value1 TODO
     * @param value2 TODO
     */
    protected final void setCharacterAttributes(JEditorPane value0,
                                                AttributeSet value1,
                                                boolean value2)
    {
      // TODO
    }

    /**
     * getStyledDocument
     * @param value0 TODO
     * @returns StyledDocument
     */
    protected final StyledDocument getStyledDocument(JEditorPane value0)
    {
      return null; // TODO
    }

    /**
     * getStyledEditorKit
     * @param value0 TODO
     * @returns StyledEditorKit
     */
    protected final StyledEditorKit getStyledEditorKit(JEditorPane value0)
    {
      return null; // TODO
    }

    /**
     * setParagraphAttributes
     * @param value0 TODO
     * @param value1 TODO
     * @param value2 TODO
     */
    protected final void setParagraphAttributes(JEditorPane value0,
                                                AttributeSet value1,
                                                boolean value2)
    {
      // TODO
    }
  }

  /**
   * StyledViewFactory
   */
  static class StyledViewFactory
    implements ViewFactory
  {
    /**
     * Constructor StyledViewFactory
     */
    StyledViewFactory()
    {
      // TODO
    }

    /**
     * create
     * @param value0 TODO
     * @returns View
     */
    public View create(Element value0)
    {
      return null; // TODO
    }
  }

  /**
   * AttributeTracker
   */
  class AttributeTracker
    implements CaretListener, PropertyChangeListener, Serializable
  {
    /**
     * Constructor AttributeTracker
     * @param value0 TODO
     */
    AttributeTracker(StyledEditorKit value0)
    {
      // TODO
    }

    /**
     * updateInputAttributes
     * @param value0 TODO
     * @param value1 TODO
     * @param value2 TODO
     */
    void updateInputAttributes(int value0, int value1, JTextComponent value2)
    {
      // TODO
    }

    /**
     * propertyChange
     * @param value0 TODO
     */
    public void propertyChange(PropertyChangeEvent value0)
    {
      // TODO
    }

    /**
     * caretUpdate
     * @param value0 TODO
     */
    public void caretUpdate(CaretEvent value0)
    {
      // TODO
    }
  }

  /**
   * currentRun
   */
  Element currentRun;

  /**
   * currentParagraph
   */
  Element currentParagraph;

  /**
   * inputAttributes
   */
  MutableAttributeSet inputAttributes;

  /**
   * Constructor StyledEditorKit
   */
  public StyledEditorKit()
  {
    // TODO
  }

  /**
   * clone
   * @returns Object
   */
  public Object clone()
  {
    return null; // TODO
  }

  /**
   * getActions
   * @returns Action[]
   */
  public Action[] getActions()
  {
    return null; // TODO
  }

  /**
   * getInputAttributes
   * @returns MutableAttributeSet
   */
  public MutableAttributeSet getInputAttributes()
  {
    return null; // TODO
  }

  /**
   * getCharacterAttributeRun
   * @returns Element
   */
  public Element getCharacterAttributeRun()
  {
    return null; // TODO
  }

  /**
   * createDefaultDocument
   * @returns Document
   */
  public Document createDefaultDocument()
  {
    return null; // TODO
  }

  /**
   * install
   * @param component TODO
   */
  public void install(JEditorPane component)
  {
    // TODO
  }

  /**
   * deinstall
   * @param component TODO
   */
  public void deinstall(JEditorPane component)
  {
    // TODO
  }

  /**
   * getViewFactory
   * @returns ViewFactory
   */
  public ViewFactory getViewFactory()
  {
    return null; // TODO
  }

  /**
   * createInputAttributes
   * @param element TODO
   * @param set TODO
   */
  protected void createInputAttributes(Element element, MutableAttributeSet set)
  {
    // TODO
  }
}
