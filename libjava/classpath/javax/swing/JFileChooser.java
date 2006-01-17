/* JFileChooser.java --
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

package javax.swing;

import java.awt.Component;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.io.File;
import java.util.ArrayList;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.JDialog;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.FileChooserUI;


/**
 * A component that provides the user a dialog box to browse through a
 * filesystem and choose one or more files or directories.
 *
 * A JFileChooser can be configured to filter the displayed file list
 * by adding a {@link FileFilter} instance using
 * {@link #addChoosableFileFilter(FileFilter)}. Additional components can
 * be embedded in the file chooser using {@link #setAccessory(JComponent)}.
 * The JFileChooser properties also provide mechanisms to customize the
 * behaviour of the file chooser.
 *
 * @author Kim Ho (kho@luxsci.net)
 */
public class JFileChooser extends JComponent implements Accessible
{
  private static final long serialVersionUID = 3162921138695327837L;

  /** 
   * A dialog type for selecting a file to open. 
   * @see #setDialogType(int)
   */
  public static final int OPEN_DIALOG = 0;

  /** 
   * A dialog type for selecting a file to save.  
   * @see #setDialogType(int)
   */
  public static final int SAVE_DIALOG = 1;

  /** 
   * A dialog type for some custom purpose.
   * @see #setDialogType(int)
   */
  public static final int CUSTOM_DIALOG = 2;

  /** 
   * A return value indicating the file chooser has been closed by cancelling.
   * 
   * @see #showOpenDialog(Component)
   * @see #showSaveDialog(Component) 
   */
  public static final int CANCEL_OPTION = 1;

  /** 
   * A return value indicating the file chooser has been closed by approving
   * the selection.
   * @see #showOpenDialog(Component)
   * @see #showSaveDialog(Component) 
   */
  public static final int APPROVE_OPTION = 0;

  /** 
   * A return value indicating the file chooser has been closed by some error.
   * @see #showOpenDialog(Component)
   * @see #showSaveDialog(Component) 
   */
  public static final int ERROR_OPTION = -1;

  /** 
   * A selection mode constant indicating acceptance of files only.
   * @see #setFileSelectionMode(int)
   */
  public static final int FILES_ONLY = 0;

  /** 
   * A selection mode constant indicating acceptance of directories only. 
   * @see #setFileSelectionMode(int)
   */
  public static final int DIRECTORIES_ONLY = 1;

  /** 
   * A selection mode constant indicating acceptance of files and directories.
   * @see #setFileSelectionMode(int)
   */
  public static final int FILES_AND_DIRECTORIES = 2;

  /** 
   * Action command string for cancelling the current selection.
   * @see #cancelSelection()
   */
  public static final String CANCEL_SELECTION = "CancelSelection";

  /** 
   * Action command string for approving the current selection.
   * @see #cancelSelection()
   */
  public static final String APPROVE_SELECTION = "ApproveSelection";

  /**
   * The name of the property for the approve button text.
   * @see #setApproveButtonText(String) 
   */
  public static final String APPROVE_BUTTON_TEXT_CHANGED_PROPERTY =
    "ApproveButtonTextChangedProperty";

  /**
   * The name of the property for the approve button tool tip text.
   * @see #setApproveButtonToolTipText(String)
   */
  public static final String APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY =
    "ApproveButtonToolTipTextChangedProperty";

  /**
   * The name of the property for the approve button mnemonic.
   * @see #setApproveButtonMnemonic(int)
   */
  public static final String APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY =
    "ApproveButtonMnemonicChangedProperty";

  /**
   * The name of the property for control button visibility.
   * @see #setControlButtonsAreShown(boolean)
   */
  public static final String CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY =
    "ControlButtonsAreShownChangedProperty";

  /**
   * The name of the property for the current directory.
   * @see #setCurrentDirectory(File)  
   */
  public static final String DIRECTORY_CHANGED_PROPERTY = "directoryChanged";

  /**
   * The name of the property for the selected file.
   * @see #setSelectedFile(File)
   */
  public static final String SELECTED_FILE_CHANGED_PROPERTY =
    "SelectedFileChangedProperty";

  /**
   * The name of the property for the selected files.
   * @see #setSelectedFiles(File[])
   */
  public static final String SELECTED_FILES_CHANGED_PROPERTY =
    "SelectedFilesChangedProperty";

  /** 
   * The name of the property for multi-selection.
   * @see #setMultiSelectionEnabled(boolean) 
   */
  public static final String MULTI_SELECTION_ENABLED_CHANGED_PROPERTY =
    "MultiSelectionEnabledChangedProperty";

  /**
   * The name of the 'file system view' property.
   * @see #setFileSystemView(FileSystemView) 
   */
  public static final String FILE_SYSTEM_VIEW_CHANGED_PROPERTY =
    "FileSystemViewChanged";

  /**
   * The name of the 'file view' property.
   * @see #setFileView(FileView) 
   */
  public static final String FILE_VIEW_CHANGED_PROPERTY = "fileViewChanged";

  /**
   * The name of the 'file hiding enabled' property.
   * @see #setFileHidingEnabled(boolean)
   */
  public static final String FILE_HIDING_CHANGED_PROPERTY =
    "FileHidingChanged";

  /**
   * The name of the 'file filter' property.
   * @see #setFileFilter(FileFilter)
   */
  public static final String FILE_FILTER_CHANGED_PROPERTY =
    "fileFilterChanged";

  /**
   * The name of the 'file selection mode' property.
   * @see #setFileSelectionMode(int)
   */
  public static final String FILE_SELECTION_MODE_CHANGED_PROPERTY =
    "fileSelectionChanged";

  /**
   * The name of the 'accessory' property.
   * @see #setAccessory(JComponent)
   */
  public static final String ACCESSORY_CHANGED_PROPERTY =
    "AccessoryChangedProperty";

  /**
   * The name of the 'accept all file filter used' property.
   * @see #setAcceptAllFileFilterUsed(boolean)
   */
  public static final String ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY =
    "acceptAllFileFilterUsedChanged";

  /**
   * The name of the 'dialog title' property.
   * @see #setDialogTitle(String)
   */
  public static final String DIALOG_TITLE_CHANGED_PROPERTY =
    "DialogTitleChangedProperty";

  /**
   * The name of the 'dialog type' property.
   * @see #setDialogType(int)
   */
  public static final String DIALOG_TYPE_CHANGED_PROPERTY =
    "DialogTypeChangedProperty";

  /**
   * The name of the 'choosable file filters' property.
   * @see #addChoosableFileFilter(FileFilter)
   */
  public static final String CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY =
    "ChoosableFileFilterChangedProperty";

  /** 
   * The accessible context. 
   * @see #getAccessibleContext()
   */
  protected AccessibleContext accessibleContext;

  /** 
   * The file system view.
   * @see #setFileSystemView(FileSystemView)
   */
  private FileSystemView fsv;

  /**
   * The accessory component.
   * @see #setAccessory(JComponent)
   */
  private JComponent accessory;

  /**
   * The approve button mnemonic.
   * @see #setApproveButtonMnemonic(int)
   */
  private int approveButtonMnemonic = 0;

  /**
   * The approve button text.
   * @see #setApproveButtonText(String)
   */
  private String approveButtonText;

  /**
   * The approve button tool tip text.
   * @see #setApproveButtonToolTipText(String)
   */
  private String approveButtonToolTipText;

  /**
   * The choosable file filters.
   * @see #addChoosableFileFilter(FileFilter)
   */
  private ArrayList choosableFilters = new ArrayList();

  /**
   * A flag controlling whether the accept all file filter is used.
   * @see #setAcceptAllFileFilterUsed(boolean)
   */
  private boolean isAcceptAll = true;

  /**
   * The dialog title.
   * @see #setDialogTitle(String)
   */
  private String dialogTitle;

  /**
   * The dialog type.
   * @see #setDialogType(int)
   */
  private int dialogType = OPEN_DIALOG;

  /**
   * The return value for the dialog.
   * @see #showOpenDialog(Component)
   * @see #showSaveDialog(Component)
   */
  private int retval = ERROR_OPTION;

  /**
   * A flag indicating whether the file chooser allows multiple selection.
   * @see #isMultiSelectionEnabled()
   */
  private boolean multiSelection = false;

  /**
   * A flag indicating whether file hiding is enabled.
   * @see #isFileHidingEnabled()
   */
  private boolean fileHiding = true;

  /**
   * The file selection mode.
   * @see #setFileSelectionMode(int) 
   */
  private int fileSelectionMode = FILES_AND_DIRECTORIES;

  /** 
   * The file view.
   * @see #setFileView(FileView)
   */
  private FileView fv = null;

  /** 
   * A flag controlling whether or not the control buttons are visible. 
   * @see #setControlButtonsAreShown(boolean) 
   */
  private boolean controlButtonsShown = true;

  /** 
   * The current directory. 
   * @see #setCurrentDirectory(File)
   */
  private File currentDir = null;

  /** 
   * The current file filter.
   * @see #setFileFilter(FileFilter)
   */
  private FileFilter currentFilter = null;

  /** 
   * An array of selected files.
   * @see #setSelectedFiles(File[]) 
   */
  private File[] selectedFiles;

  /** 
   * The selected file. 
   * @see #setSelectedFile(File)
   */
  private File selectedFile;

  /**
   * Creates a new <code>JFileChooser</code> object.
   */
  public JFileChooser()
  {
    setup(null);
    setCurrentDirectory(null);
  }

  /**
   * Creates a new <code>JFileChooser</code> object.
   *
   * @param currentDirectoryPath the directory that should initially be
   *        shown in the filechooser (if <code>null</code>, the user's home 
   *        directory is used).
   */
  public JFileChooser(String currentDirectoryPath)
  {
    this(currentDirectoryPath, null);
  }

  /**
   * Creates a new <code>JFileChooser</code> object with the specified 
   * directory and {@link FileSystemView}.
   *
   * @param currentDirectoryPath  the directory that should initially be
   *        shown in the filechooser (if <code>null</code>, the user's home 
   *        directory is used).
   * @param fsv  the file system view (if <code>null</code>, the default file
   *             system view is used).
   */
  public JFileChooser(String currentDirectoryPath, FileSystemView fsv)
  {
    setup(fsv);
    File dir = null;
    if (currentDirectoryPath != null)
      dir = getFileSystemView().createFileObject(currentDirectoryPath);
    setCurrentDirectory(dir);
  }

  /**
   * Creates a new <code>JFileChooser</code> object.
   *
   * @param currentDirectory  the directory that should initially be
   *        shown in the filechooser (if <code>null</code>, the user's home 
   *        directory is used).
   */
  public JFileChooser(File currentDirectory)
  {
    setup(null);
    setCurrentDirectory(currentDirectory);
  }

  /**
   * Creates a new <code>JFileChooser</code> object.
   *
   * @param fsv  the file system view (if <code>null</code>, the default file
   *             system view is used).
   */
  public JFileChooser(FileSystemView fsv)
  {
    setup(fsv);
    setCurrentDirectory(null);
  }

  /**
   * Creates a new <code>JFileChooser</code> object.
   *
   * @param currentDirectory  the directory that should initially be
   *        shown in the filechooser (if <code>null</code>, the user's home 
   *        directory is used).
   * @param fsv  the file system view (if <code>null</code>, the default file
   *             system view is used).
   */
  public JFileChooser(File currentDirectory, FileSystemView fsv)
  {
    setup(fsv);
    setCurrentDirectory(currentDirectory);
  }

  /**
   * Sets up the file chooser.  This method is called by all the constructors.
   *
   * @param view  the file system view (if <code>null</code>, the default file
   *              system view is used).
   * 
   * @see FileSystemView#getFileSystemView()
   */
  protected void setup(FileSystemView view)
  {
    if (view == null)
      view = FileSystemView.getFileSystemView();
    setFileSystemView(view);
    updateUI();
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
   */
  public void setDragEnabled(boolean b)
  {
    // FIXME: Implement
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean getDragEnabled()
  {
    // FIXME: Implement
    return false;
  }

  /**
   * Returns the selected file, if there is one.
   *
   * @return The selected file (possibly <code>null</code>).
   * 
   * @see #setSelectedFile(File)
   */
  public File getSelectedFile()
  {
    return selectedFile;
  }

  /**
   * Sets the selected file and sends a {@link PropertyChangeEvent} to all
   * registered listeners.  The property name is 
   * {@link #SELECTED_FILE_CHANGED_PROPERTY}.
   *
   * @param file  the file (<code>null</code> permitted).
   */
  public void setSelectedFile(File file)
  {
    if (selectedFile == null || !selectedFile.equals(file))
      {
	File old = selectedFile;
	selectedFile = file;
	firePropertyChange(SELECTED_FILE_CHANGED_PROPERTY, old, selectedFile);
      }
  }

  /**
   * Returns the selected file or files in an array.  If no files are selected,
   * an empty array is returned.
   *
   * @return An array of the selected files (possibly empty).
   */
  public File[] getSelectedFiles()
  {
    if (selectedFiles != null)
      return selectedFiles;
    if (selectedFile != null)
      return new File[] { selectedFile };
    return new File[0];
  }

  /**
   * Sets the selected files and sends a {@link PropertyChangeEvent} (with the 
   * name {@link #SELECTED_FILES_CHANGED_PROPERTY}) to all registered 
   * listeners.  
   *
   * @param selectedFiles  the selected files (<code>null</code> permitted).
   */
  public void setSelectedFiles(File[] selectedFiles)
  {
    if (selectedFiles == null)
      selectedFiles = new File[0];
    if (selectedFiles.length > 0)
      setSelectedFile(selectedFiles[0]);
    else
      setSelectedFile(null);
    if (this.selectedFiles != selectedFiles)
      {
	File[] old = this.selectedFiles;
	this.selectedFiles = selectedFiles;
	firePropertyChange(SELECTED_FILES_CHANGED_PROPERTY, old, selectedFiles);
      }

  }

  /**
   * Returns the current directory.
   *
   * @return The current directory.
   */
  public File getCurrentDirectory()
  {
    return currentDir;
  }

  /**
   * Sets the current directory and fires a {@link PropertyChangeEvent} (with 
   * the property name {@link #DIRECTORY_CHANGED_PROPERTY}) to all registered 
   * listeners.  If <code>dir</code> is <code>null</code>, the current 
   * directory is set to the default directory returned by the file system
   * view.
   *
   * @param dir  the new directory (<code>null</code> permitted).
   * 
   * @see FileSystemView#getDefaultDirectory()
   */
  public void setCurrentDirectory(File dir)
  {
    if (currentDir != dir || dir == null)
      {
	if (dir == null)
	  dir = fsv.getDefaultDirectory();

	File old = currentDir;
	currentDir = dir;
	firePropertyChange(DIRECTORY_CHANGED_PROPERTY, old, currentDir);
      }
  }

  /**
   * Called by the UI delegate when the parent directory is changed.
   */
  public void changeToParentDirectory()
  {
    setCurrentDirectory(fsv.getParentDirectory(currentDir));
  }

  /**
   * Rescans the current directory (this is handled by the UI delegate).
   */
  public void rescanCurrentDirectory()
  {
    getUI().rescanCurrentDirectory(this);
  }

  /**
   * Ensures the the specified file is visible (this is handled by the 
   * UI delegate).
   *
   * @param f  the file.
   */
  public void ensureFileIsVisible(File f)
  {
    getUI().ensureFileIsVisible(this, f);
  }

  /**
   * Displays the file chooser in a modal dialog using the 
   * {@link #OPEN_DIALOG} type.
   *
   * @param parent  the parent component.
   *
   * @return A return value indicating how the dialog was closed (one of 
   *         {@link #APPROVE_OPTION}, {@link #CANCEL_OPTION} and 
   *         {@link #ERROR_OPTION}).
   *
   * @throws HeadlessException DOCUMENT ME!
   */
  public int showOpenDialog(Component parent) throws HeadlessException
  {
    JDialog d = createDialog(parent);

    // FIXME: Remove when we get ancestor property
    d.setTitle("Open");
    setDialogType(OPEN_DIALOG);

    retval = ERROR_OPTION;

    Insets i = d.getInsets();
    d.setSize(500 + i.top + i.bottom, d.getPreferredSize().height);
    d.show();
    return retval;
  }

  /**
   * Displays the file chooser in a modal dialog using the 
   * {@link #SAVE_DIALOG} type.
   *
   * @param parent  the parent component.
   *
   * @return A return value indicating how the dialog was closed (one of 
   *         {@link #APPROVE_OPTION}, {@link #CANCEL_OPTION} and 
   *         {@link #ERROR_OPTION}).
   *
   * @throws HeadlessException DOCUMENT ME!
   */
  public int showSaveDialog(Component parent) throws HeadlessException
  {
    JDialog d = createDialog(parent);
    setDialogType(SAVE_DIALOG);

    retval = ERROR_OPTION;

    Insets i = d.getInsets();
    d.setSize(500 + i.top + i.bottom, d.getPreferredSize().height);
    d.show();
    return retval;
  }

  /**
   * Displays the file chooser in a modal dialog using the 
   * {@link #CUSTOM_DIALOG} type.
   *
   * @param parent  the parent component.
   *
   * @return A return value indicating how the dialog was closed (one of 
   *         {@link #APPROVE_OPTION}, {@link #CANCEL_OPTION} and 
   *         {@link #ERROR_OPTION}).
   *
   * @throws HeadlessException DOCUMENT ME!
   */
  public int showDialog(Component parent, String approveButtonText)
                 throws HeadlessException
  {
    JDialog d = createDialog(parent);
    setApproveButtonText(approveButtonText);
    setDialogType(CUSTOM_DIALOG);

    retval = ERROR_OPTION;

    Insets i = d.getInsets();
    d.setSize(500 + i.top + i.bottom, d.getPreferredSize().height);
    d.show();
    return retval;
  }

  /**
   * Creates a modal dialog in which to display the file chooser.
   *
   * @param parent  the parent component.
   *
   * @return The dialog.
   *
   * @throws HeadlessException DOCUMENT ME!
   */
  protected JDialog createDialog(Component parent) throws HeadlessException
  {
    Frame toUse = (Frame) SwingUtilities.getAncestorOfClass(Frame.class, parent);
    if (toUse == null)
      toUse = SwingUtilities.getOwnerFrame();

    JDialog dialog = new JDialog(toUse);
    setSelectedFile(null);
    dialog.getContentPane().add(this);
    dialog.setModal(true);
    dialog.invalidate();
    dialog.repaint();

    return dialog;
  }

  /**
   * Returns the flag that controls whether or not the control buttons are
   * shown on the file chooser.
   *
   * @return A boolean.
   * 
   * @see #setControlButtonsAreShown(boolean)
   */
  public boolean getControlButtonsAreShown()
  {
    return controlButtonsShown;
  }

  /**
   * Sets the flag that controls whether or not the control buttons are
   * shown and, if it changes, sends a {@link PropertyChangeEvent} (with the
   * property name {@link #CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY}) to
   * all registered listeners.
   *
   * @param b  the new value for the flag.
   */
  public void setControlButtonsAreShown(boolean b)
  {
    if (controlButtonsShown != b)
      {
	controlButtonsShown = b;
	firePropertyChange(CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY,
	                   ! controlButtonsShown, controlButtonsShown);
      }
  }

  /**
   * Returns the type of file chooser.
   *
   * @return {@link #OPEN_DIALOG}, {@link #SAVE_DIALOG} or 
   * {@link #CUSTOM_DIALOG}.
   * 
   * @see #setDialogType(int)
   */
  public int getDialogType()
  {
    return dialogType;
  }

  /**
   * Sets the dialog type and fires a {@link PropertyChangeEvent} (with the
   * property name {@link #DIALOG_TYPE_CHANGED_PROPERTY}) to all 
   * registered listeners.
   *
   * @param dialogType  the dialog type (one of: {@link #OPEN_DIALOG},
   * {@link #SAVE_DIALOG}, {@link #CUSTOM_DIALOG}).
   * 
   * @throws IllegalArgumentException if <code>dialogType</code> is not valid.
   */
  public void setDialogType(int dialogType)
  {
    if (dialogType != OPEN_DIALOG && dialogType != SAVE_DIALOG
        && dialogType != CUSTOM_DIALOG)
      throw new IllegalArgumentException("Choose allowable dialogType.");

    if (this.dialogType != dialogType)
      {
	int old = this.dialogType;
	this.dialogType = dialogType;
	firePropertyChange(DIALOG_TYPE_CHANGED_PROPERTY, old, this.dialogType);
      }
  }

  /**
   * Sets the dialog title and sends a {@link PropertyChangeEvent} (with the 
   * property name {@link #DIALOG_TITLE_CHANGED_PROPERTY}) to all 
   * registered listeners.
   *
   * @param dialogTitle  the dialog title (<code>null</code> permitted).
   * 
   * @see #getDialogTitle()
   */
  public void setDialogTitle(String dialogTitle)
  {
    if (this.dialogTitle != dialogTitle)
      {
	String old = this.dialogTitle;
	this.dialogTitle = dialogTitle;
	firePropertyChange(DIALOG_TITLE_CHANGED_PROPERTY, old, this.dialogTitle);
      }
  }

  /**
   * Returns the dialog title.
   *
   * @return The dialog title (possibly <code>null</code>).
   * 
   * @see #setDialogTitle(String)
   */
  public String getDialogTitle()
  {
    return dialogTitle;
  }

  /**
   * Sets the tool tip text for the approve button and sends a 
   * {@link PropertyChangeEvent} (with the property name
   * {@link #APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY}) to all 
   * registered listeners.
   *
   * @param toolTipText  the text.
   */
  public void setApproveButtonToolTipText(String toolTipText)
  {
    if (approveButtonToolTipText != toolTipText)
      {
	String oldText = approveButtonToolTipText;
	approveButtonToolTipText = toolTipText;
	firePropertyChange(APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY,
	                   oldText, approveButtonToolTipText);
      }
  }

  /**
   * Returns the tool tip text for the approve button.
   *
   * @return The tool tip text for the approve button.
   * 
   * @see #setApproveButtonToolTipText(String)
   */
  public String getApproveButtonToolTipText()
  {
    return approveButtonToolTipText;
  }

  /**
   * Returns the approve button mnemonic, or zero if no mnemonic has been set.
   *
   * @return The approve button mnemonic.
   * 
   * @see #setApproveButtonMnemonic(int)
   */
  public int getApproveButtonMnemonic()
  {
    return approveButtonMnemonic;
  }

  /**
   * Sets the mnemonic for the approve button and sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * {@link #APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY}) to all registered 
   * listeners.
   *
   * @param mnemonic  the mnemonic.
   * 
   * @see #setApproveButtonMnemonic(char)
   */
  public void setApproveButtonMnemonic(int mnemonic)
  {
    if (approveButtonMnemonic != mnemonic)
      {
	int oldMnemonic = approveButtonMnemonic;
	approveButtonMnemonic = mnemonic;
	firePropertyChange(APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY,
	                   oldMnemonic, approveButtonMnemonic);
      }
  }

  /**
   * Sets the mnemonic for the approve button and sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * {@link #APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY}) to all registered 
   * listeners.
   *
   * @param mnemonic  the mnemonic.
   * 
   * @see #setApproveButtonMnemonic(int)
   */
  public void setApproveButtonMnemonic(char mnemonic)
  {
    setApproveButtonMnemonic((int) Character.toUpperCase(mnemonic));
  }

  /**
   * Sets the approve button text and fires a {@link PropertyChangeEvent} 
   * (with the property name {@link #APPROVE_BUTTON_TEXT_CHANGED_PROPERTY}) to 
   * all registered listeners.
   *
   * @param approveButtonText  the text (<code>null</code> permitted).
   * 
   * @see #getApproveButtonText()
   */
  public void setApproveButtonText(String approveButtonText)
  {
    if (this.approveButtonText != approveButtonText)
      {
	String oldText = this.approveButtonText;
	this.approveButtonText = approveButtonText;
	firePropertyChange(APPROVE_BUTTON_TEXT_CHANGED_PROPERTY, oldText,
	                   this.approveButtonText);
      }
  }

  /**
   * Returns the approve button text.
   *
   * @return The approve button text (possibly <code>null</code>).
   * 
   * @see #setApproveButtonText(String)
   */
  public String getApproveButtonText()
  {
    return approveButtonText;
  }

  /**
   * Returns the available file filters for this file chooser.
   *
   * @return The available file filters.
   */
  public FileFilter[] getChoosableFileFilters()
  {
    return (FileFilter[]) choosableFilters.toArray(new FileFilter[choosableFilters.size()]);
  }

  /**
   * Adds a file filter to the list of available filters and sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * {@link #CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY}) to all registered 
   * listeners.
   *
   * @param filter  the filter (<code>null</code> permitted).
   */
  public void addChoosableFileFilter(FileFilter filter)
  {
    if (filter != null)
      {
        FileFilter[] old = getChoosableFileFilters();
        choosableFilters.add(filter);
        FileFilter[] newFilters = getChoosableFileFilters();
        firePropertyChange(CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY, old, 
              newFilters);
      }
    setFileFilter(filter);
  }

  /**
   * Removes a file filter from the list of available filters and sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * {@link #CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY}) to all registered 
   * listeners.
   *
   * @param f  the file filter.
   *
   * @return <code>true</code> if the filter was removed and 
   *         <code>false</code> otherwise.
   */
  public boolean removeChoosableFileFilter(FileFilter f)
  {
    if (f == currentFilter)
      setFileFilter(null);
    FileFilter[] old = getChoosableFileFilters();
    if (! choosableFilters.remove(f))
      return false;
    FileFilter[] newFilters = getChoosableFileFilters();
    firePropertyChange(CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY, old, newFilters);
    return true;
  }

  /**
   * Clears the list of choosable file filters and installs the 'accept all'
   * filter from the UI delegate.
   */
  public void resetChoosableFileFilters()
  {
    choosableFilters.clear();
    choosableFilters.add(getUI().getAcceptAllFileFilter(this));
    setFileFilter((FileFilter) choosableFilters.get(0));
  }

  /**
   * Returns the 'accept all' file filter from the UI delegate.
   *
   * @return The 'accept all' file filter.
   */
  public FileFilter getAcceptAllFileFilter()
  {
    return getUI().getAcceptAllFileFilter(this);
  }

  /**
   * Returns the flag that controls whether or not the 'accept all' file 
   * filter is included in the list of filters.
   *
   * @return A boolean.
   * 
   * @see #setAcceptAllFileFilterUsed(boolean)
   */
  public boolean isAcceptAllFileFilterUsed()
  {
    return isAcceptAll;
  }

  /**
   * Sets the flag that controls whether or not the 'accept all' file filter
   * is included in the list of filters, and sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * {@link #ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY}) to all registered 
   * listeners.
   *
   * @param b  the new value of the flag.
   */
  public void setAcceptAllFileFilterUsed(boolean b)
  {
    if (isAcceptAll != b)
      {
	isAcceptAll = b;
        if (b)
          addChoosableFileFilter(getAcceptAllFileFilter());
        else 
          removeChoosableFileFilter(getAcceptAllFileFilter());
	firePropertyChange(ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY,
	                   ! isAcceptAll, isAcceptAll);
      }
  }

  /**
   * Returns the accessory component for the file chooser.  The default
   * value is <code>null</code>.
   *
   * @return The accessory component (possibly <code>null</code>).
   * 
   * @see #setAccessory(JComponent)
   */
  public JComponent getAccessory()
  {
    return accessory;
  }

  /**
   * Sets the accessory component for the file chooser and sends a 
   * {@link PropertyChangeEvent} to all registered listeners.  The property
   * name is {@link #ACCESSORY_CHANGED_PROPERTY}.
   *
   * @param newAccessory  the accessory component.
   */
  public void setAccessory(JComponent newAccessory)
  {
    if (accessory != newAccessory)
      {
	JComponent old = accessory;
	accessory = newAccessory;
	firePropertyChange(ACCESSORY_CHANGED_PROPERTY, old, accessory);
      }
  }

  /**
   * Sets the file selection mode and sends a {@link PropertyChangeEvent}
   * to all registered listeners.  The property name is 
   * {@link #FILE_SELECTION_MODE_CHANGED_PROPERTY}.
   *
   * @param mode  the mode ({@link #FILES_ONLY}, {@link #DIRECTORIES_ONLY} or
   *              {@link #FILES_AND_DIRECTORIES}).
   * 
   * @throws IllegalArgumentException if the mode is invalid.
   */
  public void setFileSelectionMode(int mode)
  {
    if (mode != FILES_ONLY && mode != DIRECTORIES_ONLY
        && mode != FILES_AND_DIRECTORIES)
      throw new IllegalArgumentException("Choose a correct file selection mode.");
    if (fileSelectionMode != mode)
      {
	int old = fileSelectionMode;
	fileSelectionMode = mode;
	firePropertyChange(FILE_SELECTION_MODE_CHANGED_PROPERTY, old,
	                   fileSelectionMode);
      }
  }

  /**
   * Returns the file selection mode, one of: {@link #FILES_ONLY}, 
   * {@link #DIRECTORIES_ONLY} or {@link #FILES_AND_DIRECTORIES}.  The
   * default is {@link #FILES_ONLY}.
   *
   * @return The file selection mode.
   * 
   * @see #setFileSelectionMode(int)
   */
  public int getFileSelectionMode()
  {
    return fileSelectionMode;
  }

  /**
   * Returns <code>true</code> if file selection is enabled, and 
   * <code>false</code> otherwise.  File selection is enabled when the
   * file selection mode is {@link #FILES_ONLY} or 
   * {@link #FILES_AND_DIRECTORIES}.
   *
   * @return <code>true</code> if file selection is enabled.
   * 
   * @see #getFileSelectionMode()
   */
  public boolean isFileSelectionEnabled()
  {
    return (fileSelectionMode == FILES_ONLY
           || fileSelectionMode == FILES_AND_DIRECTORIES);
  }

  /**
   * Returns <code>true</code> if directory selection is enabled, and 
   * <code>false</code> otherwise.  Directory selection is enabled when the
   * file selection mode is {@link #DIRECTORIES_ONLY} or 
   * {@link #FILES_AND_DIRECTORIES}.
   *
   * @return <code>true</code> if file selection is enabled.
   * 
   * @see #getFileSelectionMode()
   */
  public boolean isDirectorySelectionEnabled()
  {
    return (fileSelectionMode == DIRECTORIES_ONLY
           || fileSelectionMode == FILES_AND_DIRECTORIES);
  }

  /**
   * Sets the flag that controls whether multiple selections are allowed in 
   * this filechooser and sends a {@link PropertyChangeEvent} (with the 
   * property name {@link #MULTI_SELECTION_ENABLED_CHANGED_PROPERTY}) to all 
   * registered listeners.
   *
   * @param b  the new value of the flag.
   */
  public void setMultiSelectionEnabled(boolean b)
  {
    if (multiSelection != b)
      {
	multiSelection = b;
	firePropertyChange(MULTI_SELECTION_ENABLED_CHANGED_PROPERTY,
	                   ! multiSelection, multiSelection);
      }
  }

  /**
   * Returns <code>true</code> if multiple selections are allowed within this
   * file chooser, and <code>false</code> otherwise.
   *
   * @return A boolean.
   * 
   * @see #setMultiSelectionEnabled(boolean)
   */
  public boolean isMultiSelectionEnabled()
  {
    return multiSelection;
  }

  /**
   * Returns <code>true</code> if hidden files are to be hidden, and 
   * <code>false</code> otherwise.
   *
   * @return A boolean.
   * 
   * @see #setFileHidingEnabled(boolean)
   */
  public boolean isFileHidingEnabled()
  {
    return fileHiding;
  }

  /**
   * Sets the flag that controls whether or not hidden files are displayed,
   * and sends a {@link PropertyChangeEvent} (with the property name
   * {@link #FILE_HIDING_CHANGED_PROPERTY}) to all registered listeners.
   *
   * @param b  the new value of the flag.
   */
  public void setFileHidingEnabled(boolean b)
  {
    if (fileHiding != b)
      {
	fileHiding = b;
	firePropertyChange(FILE_HIDING_CHANGED_PROPERTY, ! fileHiding,
	                   fileHiding);
      }
  }

  /**
   * Sets the file filter and sends a {@link PropertyChangeEvent} (with the
   * property name {@link #FILE_FILTER_CHANGED_PROPERTY}) to all registered 
   * listeners.
   *
   * @param filter  the filter (<code>null</code> permitted).
   */
  public void setFileFilter(FileFilter filter)
  {
    if (currentFilter != filter)
      {
        if (filter != null && !choosableFilters.contains(filter))
          addChoosableFileFilter(filter);
        FileFilter old = currentFilter;
        currentFilter = filter;
        firePropertyChange(FILE_FILTER_CHANGED_PROPERTY, old, currentFilter);
      }
  }

  /**
   * Returns the file filter.
   *
   * @return The file filter.
   * 
   * @see #setFileFilter(FileFilter)
   */
  public FileFilter getFileFilter()
  {
    return currentFilter;
  }

  /**
   * Sets a custom {@link FileView} for the file chooser and sends a 
   * {@link PropertyChangeEvent} to all registered listeners.  The property
   * name is {@link #FILE_VIEW_CHANGED_PROPERTY}.
   *
   * @param fileView  the file view (<code>null</code> permitted).
   *
   * @see #getFileView()
   */
  public void setFileView(FileView fileView)
  {
    if (fv != fileView)
      {
	FileView old = fv;
	fv = fileView;
	firePropertyChange(FILE_VIEW_CHANGED_PROPERTY, old, fv);
      }
  }

  /**
   * Returns the custom {@link FileView} for the file chooser.
   *
   * @return The file view (possibly <code>null</code>).
   */
  public FileView getFileView()
  {
    return fv;
  }

  /**
   * Returns the name of the file, generated by the current (or default)
   * {@link FileView}.
   *
   * @param f  the file.
   *
   * @return The file name.
   */
  public String getName(File f)
  {
    String name = null;
    if (fv != null)
      name = fv.getName(f);
    if (name == null)
      name = getUI().getFileView(this).getName(f);
    return name;
  }

  /**
   * Returns the description of the file, generated by the current (or default)
   * {@link FileView}.
   *
   * @param f  the file.
   *
   * @return The file description.
   */
  public String getDescription(File f)
  {
    String result = null;
    if (fv != null)
      result = fv.getDescription(f);
    if (result == null)
      result = getUI().getFileView(this).getDescription(f);
    return result;
  }

  /**
   * Returns the type description for the file, generated by the current (or 
   * default) {@link FileView}.
   *
   * @param f  the file.
   *
   * @return The file type description.
   */
  public String getTypeDescription(File f)
  {
    String result = null;
    if (fv != null)
      result = getFileView().getTypeDescription(f);
    if (result == null)
      result = getUI().getFileView(this).getTypeDescription(f);
    return result;
  }

  /**
   * Returns the icon provided by the current (or default) {@link FileView}.
   *
   * @param f  the file.
   *
   * @return An icon representing the file.
   */
  public Icon getIcon(File f)
  {
    Icon result = null;
    if (fv != null)
      result = fv.getIcon(f);
    if (result == null)
      result = getUI().getFileView(this).getIcon(f);
    return result;
  }

  /**
   * Returns <code>true</code> if the file is traversable, and 
   * <code>false</code> otherwise.
   *
   * @param f  the file or directory.
   *
   * @return A boolean.
   */
  public boolean isTraversable(File f)
  {
    return getFileSystemView().isTraversable(f).booleanValue();
  }

  /**
   * Returns <code>true</code> if the file is accepted by the current
   * file filter.
   *
   * @param f  the file.
   *
   * @return A boolean.
   */
  public boolean accept(File f)
  {
    if (f == null)
      return true;
    FileFilter ff = getFileFilter();
    if (ff != null) 
      return ff.accept(f);
    else
      return true;
  }

  /**
   * Sets the file system view for the file chooser and sends a 
   * {@link PropertyChangeEvent} to all registered listeners.
   *
   * @param fsv  the file system view.
   */
  public void setFileSystemView(FileSystemView fsv)
  {
    if (this.fsv != fsv)
      {
	FileSystemView old = this.fsv;
	this.fsv = fsv;
	firePropertyChange(FILE_SYSTEM_VIEW_CHANGED_PROPERTY, old, this.fsv);
      }
  }

  /**
   * Returns the file system view being used by this file chooser.
   *
   * @return The file system view.
   * 
   * @see #setFileSystemView(FileSystemView)
   */
  public FileSystemView getFileSystemView()
  {
    return fsv;
  }

  /**
   * Approves the selection.  An {@link ActionEvent} is sent to all registered
   * listeners.
   */
  public void approveSelection()
  {
    retval = APPROVE_OPTION;
    fireActionPerformed(APPROVE_SELECTION);
  }

  /**
   * Cancels the selection. An {@link ActionEvent} is sent to all registered
   * listeners.
   */
  public void cancelSelection()
  {
    retval = CANCEL_OPTION;
    fireActionPerformed(CANCEL_SELECTION);
  }

  /**
   * Adds an {@link ActionListener} to the file chooser.
   *
   * @param l  the listener.
   */
  public void addActionListener(ActionListener l)
  {
    listenerList.add(ActionListener.class, l);
  }

  /**
   * Removes an {@link ActionListener} from this file chooser.
   *
   * @param l  the listener.
   */
  public void removeActionListener(ActionListener l)
  {
    try
      {
	listenerList.remove(ActionListener.class, l);
      }
    catch (IllegalArgumentException e)
      {
	e.printStackTrace();
      }
  }

  /**
   * Returns the action listeners registered with this file chooser.
   *
   * @return An array of listeners.
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) getListeners(ActionListener.class);
  }

  /**
   * Sends an @link {ActionEvent} to all registered listeners.
   *
   * @param command  the action command.
   */
  protected void fireActionPerformed(String command)
  {
    ActionListener[] list = getActionListeners();
    ActionEvent event = new ActionEvent(this, ActionEvent.ACTION_PERFORMED,
                                        command);

    for (int i = 0; i < list.length; i++)
      list[i].actionPerformed(event);
  }

  /**
   * Installs the UI delegate for the current look and feel.
   */
  public void updateUI()
  {
    setUI((FileChooserUI) UIManager.getUI(this));
    revalidate();
  }

  /**
   * Returns the UI delegate class identifier.
   *
   * @return <code>FileChooserUI</code>.
   */
  public String getUIClassID()
  {
    return "FileChooserUI";
  }

  /**
   * Returns the UI delegate for the component.
   *
   * @return The UI delegate.
   */
  public FileChooserUI getUI()
  {
    return (FileChooserUI) ui;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected String paramString()
  {
    return "JFileChooser";
  }

  /**
   * Returns the accessible context.
   *
   * @return The accessible context.
   */
  public AccessibleContext getAccessibleContext()
  {
    return new AccessibleJFileChooser();
  }

  /**
   * Accessibility support for JFileChooser
   */
  protected class AccessibleJFileChooser 
    extends JComponent.AccessibleJComponent
  {
    protected AccessibleJFileChooser()
    {
      // Nothing to do here.
    }
    
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.FILE_CHOOSER;
    }
  }
}
