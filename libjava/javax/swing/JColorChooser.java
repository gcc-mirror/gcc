/* JColorChooser.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package javax.swing;

// Imports
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.accessibility.*;
import javax.swing.colorchooser.*;
import javax.swing.plaf.*;

/**
 * JColorChooser
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JColorChooser extends JComponent implements Accessible {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJColorChooser
	 */
	protected class AccessibleJColorChooser extends JComponent.AccessibleJComponent {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJColorChooser
		 * @param component TODO
		 */
		protected AccessibleJColorChooser(JColorChooser component) {
			super(component);
			// TODO
		} // AccessibleJColorChooser()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.COLOR_CHOOSER;
		} // getAccessibleRole()


	} // AccessibleJColorChooser


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "ColorChooserUI";

	/**
	 * selectionModel
	 */
	private ColorSelectionModel selectionModel;

	/**
	 * previewPanel
	 */
	private JComponent previewPanel;

	/**
	 * chooserPanels
	 */
	private AbstractColorChooserPanel[] chooserPanels;

	/**
	 * SELECTION_MODEL_PROPERTY
	 */
	public static final String SELECTION_MODEL_PROPERTY = "selectionModel";

	/**
	 * PREVIEW_PANEL_PROPERTY
	 */
	public static final String PREVIEW_PANEL_PROPERTY = "previewPanel";

	/**
	 * CHOOSER_PANELS_PROPERTY
	 */
	public static final String CHOOSER_PANELS_PROPERTY = "chooserPanels";

	/**
	 * accessibleContext
	 */
	protected AccessibleContext accessibleContext;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JColorChooser
	 */
	public JColorChooser() {
		// TODO
	} // JColorChooser()

	/**
	 * Constructor JColorChooser
	 * @param initial TODO
	 */
	public JColorChooser(Color initial) {
		// TODO
	} // JColorChooser()

	/**
	 * Constructor JColorChooser
	 * @param model TODO
	 */
	public JColorChooser(ColorSelectionModel model) {
		// TODO
	} // JColorChooser()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * writeObject
	 * @param stream TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream stream) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * setColor
	 * @param color TODO
	 */
	public void setColor(Color color) {
		// TODO
	} // setColor()

	/**
	 * setColor
	 * @param r TODO
	 * @param g TODO
	 * @param b TODO
	 */
	public void setColor(int r, int g, int b) {
		// TODO
	} // setColor()

	/**
	 * setColor
	 * @param color TODO
	 */
	public void setColor(int color) {
		// TODO
	} // setColor()

	/**
	 * showDialog
	 * @param component TODO
	 * @param title TODO
	 * @param initial TODO
	 * @returns Color
	 */
	public static Color showDialog(Component component, String title,
			Color initial) {
		return null; // TODO
	} // showDialog()

	/**
	 * createDialog
	 * @param component TODO
	 * @param title TODO
	 * @param modal TODO
	 * @param chooserPane TODO
	 * @param okListener TODO
	 * @param cancelListener TODO
	 * @returns JDialog
	 */
	public static JDialog createDialog(Component component, String title,
			boolean modal, JColorChooser chooserPane,
			ActionListener okListener, ActionListener cancelListener) {
		return null; // TODO
	} // createDialog()

	/**
	 * getUI
	 * @returns ColorChooserUI
	 */
	public ColorChooserUI getUI() {
		return (ColorChooserUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(ColorChooserUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((ColorChooserUI) UIManager.get(this));
		invalidate();
	} // updateUI()

	/**
	 * getUIClassID
	 * @returns String
	 */
	public String getUIClassID() {
		return uiClassID;
	} // getUIClassID()

	/**
	 * getColor
	 * @returns Color
	 */
	public Color getColor() {
		return null; // TODO
	} // getColor()

	/**
	 * setPreviewPanel
	 * @param component TODO
	 */
	public void setPreviewPanel(JComponent component) {
		// TODO
	} // setPreviewPanel()

	/**
	 * getPreviewPanel
	 * @returns JComponent
	 */
	public JComponent getPreviewPanel() {
		return null; // TODO
	} // getPreviewPanel()

	/**
	 * addChooserPanel
	 * @param panel TODO
	 */
	public void addChooserPanel(AbstractColorChooserPanel panel) {
		// TODO
	} // addChooserPanel()

	/**
	 * removeChooserPanel
	 * @param panel TODO
	 * @returns AbstractColorChooserPanel
	 */
	public AbstractColorChooserPanel removeChooserPanel(
			AbstractColorChooserPanel panel) {
		return null; // TODO
	} // removeChooserPanel()

	/**
	 * setChooserPanels
	 * @param panels TODO
	 */
	public void setChooserPanels(AbstractColorChooserPanel[] panels) {
		// TODO
	} // setChooserPanels()

	/**
	 * getChooserPanels
	 * @returns AbstractColorChooserPanel[]
	 */
	public AbstractColorChooserPanel[] getChooserPanels() {
		return null; // TODO
	} // getChooserPanels()

	/**
	 * getSelectionModel
	 * @returns ColorSelectionModel
	 */
	public ColorSelectionModel getSelectionModel() {
		return null; // TODO
	} // getSelectionModel()

	/**
	 * setSelectionModel
	 * @param model TODO
	 */
	public void setSelectionModel(ColorSelectionModel model) {
		// TODO
	} // setSelectionModel()

	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return null; // TODO
	} // paramString()

	/**
	 * getAccessibleContext
	 * @returns AccessibleContext
	 */
	public AccessibleContext getAccessibleContext() {
		if (accessibleContext == null) {
			accessibleContext = new AccessibleJColorChooser(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JColorChooser
