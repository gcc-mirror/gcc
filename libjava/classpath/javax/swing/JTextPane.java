/* JTextPane.java --
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
import java.io.IOException;
import java.io.ObjectOutputStream;

import javax.swing.text.AttributeSet;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;

/**
 * JTextPane
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JTextPane extends JEditorPane {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "TextPaneUI";


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JTextPane
	 */
	public JTextPane() {
		// TODO
	} // JTextPane()

	/**
	 * Constructor JTextPane
	 * @param document TODO
	 */
	public JTextPane(StyledDocument document) {
		// TODO
	} // JTextPane()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getUIClassID
	 * @returns String
	 */
	public String getUIClassID() {
		return uiClassID;
	} // getUIClassID()

	/**
	 * setDocument
	 * @param document TODO
	 */
	public void setDocument(Document document) {
		super.setDocument(document); // TODO
	} // setDocument()

	/**
	 * getStyledDocument
	 * @returns StyledDocument
	 */
	public StyledDocument getStyledDocument() {
		return null; // TODO
	} // getStyledDocument()

	/**
	 * setStyledDocument
	 * @param document TODO
	 */
	public void setStyledDocument(StyledDocument document) {
		// TODO
	} // setStyledDocument()

	/**
	 * replaceSelection
	 * @param content TODO
	 */
	public void replaceSelection(String content) {
		super.replaceSelection(content); // TODO
	} // replaceSelection()

	/**
	 * insertComponent
	 * @param component TODO
	 */
	public void insertComponent(Component component) {
		// TODO
	} // insertComponent()

	/**
	 * insertIcon
	 * @param icon TODO
	 */
	public void insertIcon(Icon icon) {
		// TODO
	} // insertIcon()

	/**
	 * addStyle
	 * @param nm TODO
	 * @param parent TODO
	 * @returns Style
	 */
	public Style addStyle(String nm, Style parent) {
		return null; // TODO
	} // addStyle()

	/**
	 * removeStyle
	 * @param nm TODO
	 */
	public void removeStyle(String nm) {
		// TODO
	} // removeStyle()

	/**
	 * getStyle
	 * @param nm TODO
	 * @returns Style
	 */
	public Style getStyle(String nm) {
		return null; // TODO
	} // getStyle()

	/**
	 * getLogicalStyle
	 * @returns Style
	 */
	public Style getLogicalStyle() {
		return null; // TODO
	} // getLogicalStyle()

	/**
	 * setLogicalStyle
	 * @param style TODO
	 */
	public void setLogicalStyle(Style style) {
		// TODO
	} // setLogicalStyle()

	/**
	 * getCharacterAttributes
	 * @returns AttributeSet
	 */
	public AttributeSet getCharacterAttributes() {
		return null; // TODO
	} // getCharacterAttributes()

	/**
	 * setCharacterAttributes
	 * @param attribute TODO
	 * @param replace TODO
	 */
	public void setCharacterAttributes(AttributeSet attribute,
			boolean replace) {
		// TODO
	} // setCharacterAttributes()

	/**
	 * getParagraphAttributes
	 * @returns AttributeSet
	 */
	public AttributeSet getParagraphAttributes() {
		return null; // TODO
	} // getParagraphAttributes()

	/**
	 * setParagraphAttributes
	 * @param attribute TODO
	 * @param replace TODO
	 */
	public void setParagraphAttributes(AttributeSet attribute,
			boolean replace) {
		// TODO
	} // setParagraphAttributes()

	/**
	 * getInputAttributes
	 * @returns MutableAttributeSet
	 */
	public MutableAttributeSet getInputAttributes() {
		return null; // TODO
	} // getInputAttributes()

	/**
	 * getStyledEditorKit
	 * @returns StyledEditorKit
	 */
	protected final StyledEditorKit getStyledEditorKit() {
		return null; // TODO
	} // getStyledEditorKit()

	/**
	 * createDefaultEditorKit
	 * @returns EditorKit
	 */
	protected EditorKit createDefaultEditorKit() {
		return super.createDefaultEditorKit(); // TODO
	} // createDefaultEditorKit()

	/**
	 * setEditorKit
	 * @param editor TODO
	 */
	public final void setEditorKit(EditorKit editor) {
		super.setEditorKit(editor); // TODO
	} // setEditorKit()

	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return super.paramString(); // TODO
	} // paramString()


} // JTextPane
