/* JFileChooser.java --
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

import java.awt.Component;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.FileChooserUI;

/**
 * JFileChooser
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JFileChooser extends JComponent implements Accessible {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJFileChooser
	 */
	protected class AccessibleJFileChooser extends AccessibleJComponent {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJFileChooser
		 * @param component TODO
		 */
		protected AccessibleJFileChooser(JFileChooser component) {
			super(component);
			// TODO
		} // AccessibleJFileChooser()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.FILE_CHOOSER;
		} // getAccessibleRole()


	} // AccessibleJFileChooser


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "FileChooserUI";

	/**
	 * OPEN_DIALOG
	 */
	public static final int OPEN_DIALOG = 0;

	/**
	 * SAVE_DIALOG
	 */
	public static final int SAVE_DIALOG = 1;

	/**
	 * CUSTOM_DIALOG
	 */
	public static final int CUSTOM_DIALOG = 2;

	/**
	 * CANCEL_OPTION
	 */
	public static final int CANCEL_OPTION = 1;

	/**
	 * APPROVE_OPTION
	 */
	public static final int APPROVE_OPTION = 0;

	/**
	 * ERROR_OPTION
	 */
	public static final int ERROR_OPTION = -1;

	/**
	 * FILES_ONLY
	 */
	public static final int FILES_ONLY = 0;

	/**
	 * DIRECTORIES_ONLY
	 */
	public static final int DIRECTORIES_ONLY = 1;

	/**
	 * FILES_AND_DIRECTORIES
	 */
	public static final int FILES_AND_DIRECTORIES = 2;

	/**
	 * CANCEL_SELECTION
	 */
	public static final String CANCEL_SELECTION = "CancelSelection";

	/**
	 * APPROVE_SELECTION
	 */
	public static final String APPROVE_SELECTION = "ApproveSelection";

	/**
	 * APPROVE_BUTTON_TEXT_CHANGED_PROPERTY
	 */
	public static final String APPROVE_BUTTON_TEXT_CHANGED_PROPERTY = "ApproveButtonTextChangedProperty";

	/**
	 * APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY
	 */
	public static final String APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY = "ApproveButtonToolTipTextChangedProperty";

	/**
	 * APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY
	 */
	public static final String APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY = "ApproveButtonMnemonicChangedProperty";

	/**
	 * CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY
	 */
	public static final String CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY = "ControlButtonsAreShownChangedProperty";

	/**
	 * DIRECTORY_CHANGED_PROPERTY
	 */
	public static final String DIRECTORY_CHANGED_PROPERTY = "directoryChanged";

	/**
	 * SELECTED_FILE_CHANGED_PROPERTY
	 */
	public static final String SELECTED_FILE_CHANGED_PROPERTY = "SelectedFileChangedProperty";

	/**
	 * SELECTED_FILES_CHANGED_PROPERTY
	 */
	public static final String SELECTED_FILES_CHANGED_PROPERTY = "SelectedFilesChangedProperty";

	/**
	 * MULTI_SELECTION_ENABLED_CHANGED_PROPERTY
	 */
	public static final String MULTI_SELECTION_ENABLED_CHANGED_PROPERTY = "MultiSelectionEnabledChangedProperty";

	/**
	 * FILE_SYSTEM_VIEW_CHANGED_PROPERTY
	 */
	public static final String FILE_SYSTEM_VIEW_CHANGED_PROPERTY = "FileSystemViewChanged";

	/**
	 * FILE_VIEW_CHANGED_PROPERTY
	 */
	public static final String FILE_VIEW_CHANGED_PROPERTY = "fileViewChanged";

	/**
	 * FILE_HIDING_CHANGED_PROPERTY
	 */
	public static final String FILE_HIDING_CHANGED_PROPERTY = "FileHidingChanged";

	/**
	 * FILE_FILTER_CHANGED_PROPERTY
	 */
	public static final String FILE_FILTER_CHANGED_PROPERTY = "fileFilterChanged";

	/**
	 * FILE_SELECTION_MODE_CHANGED_PROPERTY
	 */
	public static final String FILE_SELECTION_MODE_CHANGED_PROPERTY = "fileSelectionChanged";

	/**
	 * ACCESSORY_CHANGED_PROPERTY
	 */
	public static final String ACCESSORY_CHANGED_PROPERTY = "AccessoryChangedProperty";

	/**
	 * ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY
	 */
	public static final String ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY = "acceptAllFileFilterUsedChanged";

	/**
	 * DIALOG_TITLE_CHANGED_PROPERTY
	 */
	public static final String DIALOG_TITLE_CHANGED_PROPERTY = "DialogTitleChangedProperty";

	/**
	 * DIALOG_TYPE_CHANGED_PROPERTY
	 */
	public static final String DIALOG_TYPE_CHANGED_PROPERTY = "DialogTypeChangedProperty";

	/**
	 * CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY
	 */
	public static final String CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY = "ChoosableFileFilterChangedProperty";

	/**
	 * dialogTitle
	 */
	private String dialogTitle;

	/**
	 * approveButtonText
	 */
	private String approveButtonText;

	/**
	 * approveButtonToolTipText
	 */
	private String approveButtonToolTipText;

	/**
	 * approveButtonMnemonic
	 */
	private int approveButtonMnemonic;

	/**
	 * actionListener
	 */
	private ActionListener actionListener;

	/**
	 * filters
	 */
	private Vector filters;

	/**
	 * dialog
	 */
	private JDialog dialog;

	/**
	 * dialogType
	 */
	private int dialogType;

	/**
	 * returnValue
	 */
	private int returnValue;

	/**
	 * accessory
	 */
	private JComponent accessory;

	/**
	 * fileView
	 */
	private FileView fileView;

	/**
	 * uiFileView
	 */
	private FileView uiFileView;

	/**
	 * controlsShown
	 */
	private boolean controlsShown;

	/**
	 * useFileHiding
	 */
	private boolean useFileHiding;

	/**
	 * fileSelectionMode
	 */
	private int fileSelectionMode;

	/**
	 * multiSelectionEnabled
	 */
	private boolean multiSelectionEnabled;

	/**
	 * useAcceptAllFileFilter
	 */
	private boolean useAcceptAllFileFilter;

	/**
	 * fileFilter
	 */
	private FileFilter fileFilter;

	/**
	 * fileSystemView
	 */
	private FileSystemView fileSystemView;

	/**
	 * currentDirectory
	 */
	private File currentDirectory;

	/**
	 * selectedFile
	 */
	private File selectedFile;

	/**
	 * selectedFiles
	 */
	private File[] selectedFiles;

	/**
	 * accessibleContext
	 */
	protected AccessibleContext accessibleContext;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JFileChooser
	 */
	public JFileChooser() {
		// TODO
	} // JFileChooser()

	/**
	 * Constructor JFileChooser
	 * @param currentDirectoryPath TODO
	 */
	public JFileChooser(String currentDirectoryPath) {
		// TODO
	} // JFileChooser()

	/**
	 * Constructor JFileChooser
	 * @param currentDirectory TODO
	 */
	public JFileChooser(File currentDirectory) {
		// TODO
	} // JFileChooser()

	/**
	 * Constructor JFileChooser
	 * @param value0 TODO
	 */
	public JFileChooser(FileSystemView fsv) {
		// TODO
	} // JFileChooser()

	/**
	 * Constructor JFileChooser
	 * @param currentDirectory TODO
	 * @param fsv TODO
	 */
	public JFileChooser(File currentDirectory, FileSystemView fsv) {
		// TODO
	} // JFileChooser()

	/**
	 * Constructor JFileChooser
	 * @param currentDirectoryPath TODO
	 * @param fsv TODO
	 */
	public JFileChooser(String currentDirectoryPath, FileSystemView fsv) {
		// TODO
	} // JFileChooser()


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
	 * getName
	 * @param file TODO
	 * @returns String
	 */
	public String getName(File file) {
		return null; // TODO
	} // getName()

	/**
	 * setup
	 * @param view TODO
	 */
	protected void setup(FileSystemView view) {
		// TODO
	} // setup()

	/**
	 * accept
	 * @param file TODO
	 * @returns boolean
	 */
	public boolean accept(File file) {
		return false; // TODO
	} // accept()

	/**
	 * getSelectedFile
	 * @returns File
	 */
	public File getSelectedFile() {
		return null; // TODO
	} // getSelectedFile()

	/**
	 * setSelectedFile
	 * @param file TODO
	 */
	public void setSelectedFile(File file) {
		// TODO
	} // setSelectedFile()

	/**
	 * getSelectedFiles
	 * @returns File[]
	 */
	public File[] getSelectedFiles() {
		return null; // TODO
	} // getSelectedFiles()

	/**
	 * setSelectedFiles
	 * @param files TODO
	 */
	public void setSelectedFiles(File[] files) {
		// TODO
	} // setSelectedFiles()

	/**
	 * getCurrentDirectory
	 * @returns File
	 */
	public File getCurrentDirectory() {
		return null; // TODO
	} // getCurrentDirectory()

	/**
	 * setCurrentDirectory
	 * @param directory TODO
	 */
	public void setCurrentDirectory(File directory) {
		// TODO
	} // setCurrentDirectory()

	/**
	 * changeToParentDirectory
	 */
	public void changeToParentDirectory() {
		// TODO
	} // changeToParentDirectory()

	/**
	 * rescanCurrentDirectory
	 */
	public void rescanCurrentDirectory() {
		// TODO
	} // rescanCurrentDirectory()

	/**
	 * ensureFileIsVisible
	 * @param file TODO
	 */
	public void ensureFileIsVisible(File file) {
		// TODO
	} // ensureFileIsVisible()

	/**
	 * showOpenDialog
	 * @param parent TODO
	 * @returns int
	 */
	public int showOpenDialog(Component parent) {
		return 0; // TODO
	} // showOpenDialog()

	/**
	 * showSaveDialog
	 * @param parent TODO
	 * @returns int
	 */
	public int showSaveDialog(Component parent) {
		return 0; // TODO
	} // showSaveDialog()

	/**
	 * showDialog
	 * @param parent TODO
	 * @param approveButtonText TODO
	 * @returns int
	 */
	public int showDialog(Component parent, String approveButtonText) {
		return 0; // TODO
	} // showDialog()

	/**
	 * getControlButtonsAreShown
	 * @returns boolean
	 */
	public boolean getControlButtonsAreShown() {
		return false; // TODO
	} // getControlButtonsAreShown()

	/**
	 * setControlButtonsAreShown
	 * @param value TODO
	 */
	public void setControlButtonsAreShown(boolean value) {
		// TODO
	} // setControlButtonsAreShown()

	/**
	 * getDialogType
	 * @returns int
	 */
	public int getDialogType() {
		return 0; // TODO
	} // getDialogType()

	/**
	 * setDialogType
	 * @param type TODO
	 */
	public void setDialogType(int type) {
		// TODO
	} // setDialogType()

	/**
	 * setDialogTitle
	 * @param title TODO
	 */
	public void setDialogTitle(String title) {
		// TODO
	} // setDialogTitle()

	/**
	 * getDialogTitle
	 * @returns String
	 */
	public String getDialogTitle() {
		return null; // TODO
	} // getDialogTitle()

	/**
	 * setApproveButtonToolTipText
	 * @param text TODO
	 */
	public void setApproveButtonToolTipText(String text) {
		// TODO
	} // setApproveButtonToolTipText()

	/**
	 * getApproveButtonToolTipText
	 * @returns String
	 */
	public String getApproveButtonToolTipText() {
		return null; // TODO
	} // getApproveButtonToolTipText()

	/**
	 * getApproveButtonMnemonic
	 * @returns int
	 */
	public int getApproveButtonMnemonic() {
		return 0; // TODO
	} // getApproveButtonMnemonic()

	/**
	 * setApproveButtonMnemonic
	 * @param mnemonic TODO
	 */
	public void setApproveButtonMnemonic(int mnemonic) {
		// TODO
	} // setApproveButtonMnemonic()

	/**
	 * setApproveButtonMnemonic
	 * @param mnemonic TODO
	 */
	public void setApproveButtonMnemonic(char mnemonic) {
		// TODO
	} // setApproveButtonMnemonic()

	/**
	 * setApproveButtonText
	 * @param text TODO
	 */
	public void setApproveButtonText(String text) {
		// TODO
	} // setApproveButtonText()

	/**
	 * getApproveButtonText
	 * @returns String
	 */
	public String getApproveButtonText() {
		return null; // TODO
	} // getApproveButtonText()

	/**
	 * getChoosableFileFilters
	 * @returns FileFilter[]
	 */
	public FileFilter[] getChoosableFileFilters() {
		return null; // TODO
	} // getChoosableFileFilters()

	/**
	 * addChoosableFileFilter
	 * @param filter TODO
	 */
	public void addChoosableFileFilter(FileFilter filter) {
		// TODO
	} // addChoosableFileFilter()

	/**
	 * removeChoosableFileFilter
	 * @param filter TODO
	 * @returns boolean
	 */
	public boolean removeChoosableFileFilter(FileFilter filter) {
		return false; // TODO
	} // removeChoosableFileFilter()

	/**
	 * resetChoosableFileFilters
	 */
	public void resetChoosableFileFilters() {
		// TODO
	} // resetChoosableFileFilters()

	/**
	 * getAcceptAllFileFilter
	 * @returns FileFilter
	 */
	public FileFilter getAcceptAllFileFilter() {
		return null; // TODO
	} // getAcceptAllFileFilter()

	/**
	 * isAcceptAllFileFilterUsed
	 * @returns boolean
	 */
	public boolean isAcceptAllFileFilterUsed() {
		return false; // TODO
	} // isAcceptAllFileFilterUsed()

	/**
	 * setAcceptAllFileFilterUsed
	 * @param value TODO
	 */
	public void setAcceptAllFileFilterUsed(boolean value) {
		// TODO
	} // setAcceptAllFileFilterUsed()

	/**
	 * getAccessory
	 * @returns JComponent
	 */
	public JComponent getAccessory() {
		return null; // TODO
	} // getAccessory()

	/**
	 * setAccessory
	 * @param accessory TODO
	 */
	public void setAccessory(JComponent accessory) {
		// TODO
	} // setAccessory()

	/**
	 * setFileSelectionMode
	 * @param mode TODO
	 */
	public void setFileSelectionMode(int mode) {
		// TODO
	} // setFileSelectionMode()

	/**
	 * getFileSelectionMode
	 * @returns int
	 */
	public int getFileSelectionMode() {
		return 0; // TODO
	} // getFileSelectionMode()

	/**
	 * isFileSelectionEnabled
	 * @returns boolean
	 */
	public boolean isFileSelectionEnabled() {
		return false; // TODO
	} // isFileSelectionEnabled()

	/**
	 * isDirectorySelectionEnabled
	 * @returns boolean
	 */
	public boolean isDirectorySelectionEnabled() {
		return false; // TODO
	} // isDirectorySelectionEnabled()

	/**
	 * isMultiSelectionEnabled
	 * @returns boolean
	 */
	public boolean isMultiSelectionEnabled() {
		return false; // TODO
	} // isMultiSelectionEnabled()

	/**
	 * setMultiSelectionEnabled
	 * @param enabled TODO
	 */
	public void setMultiSelectionEnabled(boolean enabled) {
		// TODO
	} // setMultiSelectionEnabled()

	/**
	 * isFileHidingEnabled
	 * @returns boolean
	 */
	public boolean isFileHidingEnabled() {
		return false; // TODO
	} // isFileHidingEnabled()

	/**
	 * setFileHidingEnabled
	 * @param enabled TODO
	 */
	public void setFileHidingEnabled(boolean enabled) {
		// TODO
	} // setFileHidingEnabled()

	/**
	 * getFileFilter
	 * @returns FileFilter
	 */
	public FileFilter getFileFilter() {
		return null; // TODO
	} // getFileFilter()

	/**
	 * setFileFilter
	 * @param filter TODO
	 */
	public void setFileFilter(FileFilter filter) {
		// TODO
	} // setFileFilter()

	/**
	 * getFileView
	 * @returns FileView
	 */
	public FileView getFileView() {
		return null; // TODO
	} // getFileView()

	/**
	 * setFileView
	 * @param view TODO
	 */
	public void setFileView(FileView view) {
		// TODO
	} // setFileView()

	/**
	 * getDescription
	 * @param file TODO
	 * @returns String
	 */
	public String getDescription(File file) {
		return null; // TODO
	} // getDescription()

	/**
	 * getTypeDescription
	 * @param file TODO
	 * @returns String
	 */
	public String getTypeDescription(File file) {
		return null; // TODO
	} // getTypeDescription()

	/**
	 * getIcon
	 * @param file TODO
	 * @returns Icon
	 */
	public Icon getIcon(File file) {
		return null; // TODO
	} // getIcon()

	/**
	 * isTraversable
	 * @param file TODO
	 * @returns boolean
	 */
	public boolean isTraversable(File file) {
		return false; // TODO
	} // isTraversable()

	/**
	 * getFileSystemView
	 * @returns FileSystemView
	 */
	public FileSystemView getFileSystemView() {
		return null; // TODO
	} // getFileSystemView()

	/**
	 * setFileSystemView
	 * @param fsv TODO
	 */
	public void setFileSystemView(FileSystemView fsv) {
		// TODO
	} // setFileSystemView()

	/**
	 * approveSelection
	 */
	public void approveSelection() {
		// TODO
	} // approveSelection()

	/**
	 * cancelSelection
	 */
	public void cancelSelection() {
		// TODO
	} // cancelSelection()

	/**
	 * addActionListener
	 * @param listener TODO
	 */
	public void addActionListener(ActionListener listener) {
		// TODO
	} // addActionListener()

	/**
	 * removeActionListener
	 * @param listener TODO
	 */
	public void removeActionListener(ActionListener listener) {
		// TODO
	} // removeActionListener()

	/**
	 * fireActionPerformed
	 * @param command TODO
	 */
	protected void fireActionPerformed(String command) {
		// TODO
	} // fireActionPerformed()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((FileChooserUI) UIManager.get(this));
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
	 * getUI
	 * @returns FileChooserUI
	 */
	public FileChooserUI getUI() {
		return (FileChooserUI) ui;
	} // getUI()

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
			accessibleContext = new AccessibleJFileChooser(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JFileChooser
