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
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;

/**
 * StyledEditorKit
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class StyledEditorKit extends DefaultEditorKit
{
  static final long serialVersionUID = 7002391892985555948L;

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * UnderlineAction
	 */
	public static class UnderlineAction extends StyledEditorKit.StyledTextAction {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor UnderlineAction
		 */
		public UnderlineAction() {
			super("TODO");
			// TODO
		} // UnderlineAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // UnderlineAction

	/**
	 * ItalicAction
	 */
	public static class ItalicAction extends StyledEditorKit.StyledTextAction {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor ItalicAction
		 */
		public ItalicAction() {
			super("TODO");
			// TODO
		} // ItalicAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // ItalicAction

	/**
	 * BoldAction
	 */
	public static class BoldAction extends StyledEditorKit.StyledTextAction {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor BoldAction
		 */
		public BoldAction() {
			super("TODO");
			// TODO
		} // BoldAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // BoldAction

	/**
	 * AlignmentAction
	 */
	public static class AlignmentAction extends StyledEditorKit.StyledTextAction {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * a
		 */
		private int a;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AlignmentAction
		 * @param nm TODO
		 * @param a TODO
		 */
		public AlignmentAction(String nm, int a) {
			super("TODO");
			// TODO
		} // AlignmentAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // AlignmentAction

	/**
	 * ForegroundAction
	 */
	public static class ForegroundAction extends StyledEditorKit.StyledTextAction {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * fg
		 */
		private Color fg;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor ForegroundAction
		 * @param nm TODO
		 * @param fg TODO
		 */
		public ForegroundAction(String nm, Color fg) {
			super("TODO");
			// TODO
		} // ForegroundAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // ForegroundAction

	/**
	 * FontSizeAction
	 */
	public static class FontSizeAction extends StyledEditorKit.StyledTextAction {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * size
		 */
		private int size;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor FontSizeAction
		 * @param nm TODO
		 * @param size TODO
		 */
		public FontSizeAction(String nm, int size) {
			super("TODO");
			// TODO
		} // FontSizeAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // FontSizeAction

	/**
	 * FontFamilyAction
	 */
	public static class FontFamilyAction extends StyledEditorKit.StyledTextAction {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * family
		 */
		private String family;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor FontFamilyAction
		 * @param nm TODO
		 * @param family TODO
		 */
		public FontFamilyAction(String nm, String family) {
			super("TODO");
			// TODO
		} // FontFamilyAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * actionPerformed
		 * @param event TODO
		 */
		public void actionPerformed(ActionEvent event) {
			// TODO
		} // actionPerformed()


	} // FontFamilyAction

	/**
	 * StyledTextAction
	 */
	public abstract static class StyledTextAction extends TextAction {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor StyledTextAction
		 * @param nm TODO
		 */
		public StyledTextAction(String nm) {
			super(nm);
			// TODO
		} // StyledTextAction()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getEditor
		 * @param event TODO
		 * @returns JEditorPane
		 */
		protected final JEditorPane getEditor(ActionEvent event) {
			return null; // TODO
		} // getEditor()

		/**
		 * setCharacterAttributes
		 * @param value0 TODO
		 * @param value1 TODO
		 * @param value2 TODO
		 */
		protected final void setCharacterAttributes(JEditorPane value0, AttributeSet value1, boolean value2) {
			// TODO
		} // setCharacterAttributes()

		/**
		 * getStyledDocument
		 * @param value0 TODO
		 * @returns StyledDocument
		 */
		protected final StyledDocument getStyledDocument(JEditorPane value0) {
			return null; // TODO
		} // getStyledDocument()

		/**
		 * getStyledEditorKit
		 * @param value0 TODO
		 * @returns StyledEditorKit
		 */
		protected final StyledEditorKit getStyledEditorKit(JEditorPane value0) {
			return null; // TODO
		} // getStyledEditorKit()

		/**
		 * setParagraphAttributes
		 * @param value0 TODO
		 * @param value1 TODO
		 * @param value2 TODO
		 */
		protected final void setParagraphAttributes(JEditorPane value0, AttributeSet value1, boolean value2) {
			// TODO
		} // setParagraphAttributes()


	} // StyledTextAction

	/**
	 * StyledViewFactory
	 */
	static class StyledViewFactory implements ViewFactory {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor StyledViewFactory
		 */
		StyledViewFactory() {
			// TODO
		} // StyledViewFactory()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * create
		 * @param value0 TODO
		 * @returns View
		 */
		public View create(Element value0) {
			return null; // TODO
		} // create()


	} // StyledViewFactory

	/**
	 * AttributeTracker
	 */
	 class AttributeTracker implements CaretListener, PropertyChangeListener, Serializable {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AttributeTracker
		 * @param value0 TODO
		 */
		AttributeTracker(StyledEditorKit value0) {
			// TODO
		} // AttributeTracker()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * updateInputAttributes
		 * @param value0 TODO
		 * @param value1 TODO
		 * @param value2 TODO
		 */
		void updateInputAttributes(int value0, int value1, JTextComponent value2) {
			// TODO
		} // updateInputAttributes()

		/**
		 * propertyChange
		 * @param value0 TODO
		 */
		public void propertyChange(PropertyChangeEvent value0) {
			// TODO
		} // propertyChange()

		/**
		 * caretUpdate
		 * @param value0 TODO
		 */
		public void caretUpdate(CaretEvent value0) {
			// TODO
		} // caretUpdate()


	} // AttributeTracker


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

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


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor StyledEditorKit
	 */
	public StyledEditorKit() {
		// TODO
	} // StyledEditorKit()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * clone
	 * @returns Object
	 */
	public Object clone() {
		return null; // TODO
	} // clone()

	/**
	 * getActions
	 * @returns Action[]
	 */
	public Action[] getActions() {
		return null; // TODO
	} // getActions()

	/**
	 * getInputAttributes
	 * @returns MutableAttributeSet
	 */
	public MutableAttributeSet getInputAttributes() {
		return null; // TODO
	} // getInputAttributes()

	/**
	 * getCharacterAttributeRun
	 * @returns Element
	 */
	public Element getCharacterAttributeRun() {
		return null; // TODO
	} // getCharacterAttributeRun()

	/**
	 * createDefaultDocument
	 * @returns Document
	 */
	public Document createDefaultDocument() {
		return null; // TODO
	} // createDefaultDocument()

	/**
	 * install
	 * @param component TODO
	 */
	public void install(JEditorPane component) {
		// TODO
	} // install()

	/**
	 * deinstall
	 * @param component TODO
	 */
	public void deinstall(JEditorPane component) {
		// TODO
	} // deinstall()

	/**
	 * getViewFactory
	 * @returns ViewFactory
	 */
	public ViewFactory getViewFactory() {
		return null; // TODO
	} // getViewFactory()

	/**
	 * createInputAttributes
	 * @param element TODO
	 * @param set TODO
	 */
	protected void createInputAttributes(Element element,
			MutableAttributeSet set) {
		// TODO
	} // createInputAttributes()


} // StyledEditorKit
