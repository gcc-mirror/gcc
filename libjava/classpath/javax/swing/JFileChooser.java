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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.JDialog;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.FileChooserUI;


/**
 * DOCUMENT ME!
 */
public class JFileChooser extends JComponent implements Accessible
{
  private static final long serialVersionUID = 3162921138695327837L;

  /** DOCUMENT ME! */
  public static final int OPEN_DIALOG = 0;

  /** DOCUMENT ME! */
  public static final int SAVE_DIALOG = 1;

  /** DOCUMENT ME! */
  public static final int CUSTOM_DIALOG = 2;

  /** DOCUMENT ME! */
  public static final int CANCEL_OPTION = 1;

  /** DOCUMENT ME! */
  public static final int APPROVE_OPTION = 0;

  /** DOCUMENT ME! */
  public static final int ERROR_OPTION = -1;

  /** DOCUMENT ME! */
  public static final int FILES_ONLY = 0;

  /** DOCUMENT ME! */
  public static final int DIRECTORIES_ONLY = 1;

  /** DOCUMENT ME! */
  public static final int FILES_AND_DIRECTORIES = 2;

  /** DOCUMENT ME! */
  public static final String CANCEL_SELECTION = "CancelSelection";

  /** DOCUMENT ME! */
  public static final String APPROVE_SELECTION = "ApproveSelection";

  /** DOCUMENT ME! */
  public static final String APPROVE_BUTTON_TEXT_CHANGED_PROPERTY =
    "ApproveButtonTextChangedProperty";

  /** DOCUMENT ME! */
  public static final String APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY =
    "ApproveButtonToolTipTextChangedProperty";

  /** DOCUMENT ME! */
  public static final String APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY =
    "ApproveButtonMnemonicChangedProperty";

  /** DOCUMENT ME! */
  public static final String CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY =
    "ControlButtonsAreShownChangedProperty";

  /** DOCUMENT ME! */
  public static final String DIRECTORY_CHANGED_PROPERTY = "directoryChanged";

  /** DOCUMENT ME! */
  public static final String SELECTED_FILE_CHANGED_PROPERTY =
    "SelectedFileChangedProperty";

  /** DOCUMENT ME! */
  public static final String SELECTED_FILES_CHANGED_PROPERTY =
    "SelectedFilesChangedProperty";

  /** DOCUMENT ME! */
  public static final String MULTI_SELECTION_ENABLED_CHANGED_PROPERTY =
    "MultiSelectionEnabledChangedProperty";

  /** DOCUMENT ME! */
  public static final String FILE_SYSTEM_VIEW_CHANGED_PROPERTY =
    "FileSystemViewChanged";

  /** DOCUMENT ME! */
  public static final String FILE_VIEW_CHANGED_PROPERTY = "fileViewChanged";

  /** DOCUMENT ME! */
  public static final String FILE_HIDING_CHANGED_PROPERTY =
    "FileHidingChanged";

  /** DOCUMENT ME! */
  public static final String FILE_FILTER_CHANGED_PROPERTY =
    "fileFilterChanged";

  /** DOCUMENT ME! */
  public static final String FILE_SELECTION_MODE_CHANGED_PROPERTY =
    "fileSelectionChanged";

  /** DOCUMENT ME! */
  public static final String ACCESSORY_CHANGED_PROPERTY =
    "AccessoryChangedProperty";

  /** DOCUMENT ME! */
  public static final String ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY =
    "acceptAllFileFilterUsedChanged";

  /** DOCUMENT ME! */
  public static final String DIALOG_TITLE_CHANGED_PROPERTY =
    "DialogTitleChangedProperty";

  /** DOCUMENT ME! */
  public static final String DIALOG_TYPE_CHANGED_PROPERTY =
    "DialogTypeChangedProperty";

  /** DOCUMENT ME! */
  public static final String CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY =
    "ChoosableFileFilterChangedProperty";

  /** DOCUMENT ME! */
  protected AccessibleContext accessibleContext;

  /** DOCUMENT ME! */
  private FileSystemView fsv;

  /** DOCUMENT ME! */
  private JComponent accessory;

  /** DOCUMENT ME! */
  private int approveButtonMnemonic = 0;

  /** DOCUMENT ME! */
  private String approveButtonText;

  /** DOCUMENT ME! */
  private String approveButtonToolTipText;

  /** DOCUMENT ME! */
  private ArrayList choosableFilters = new ArrayList();

  /** DOCUMENT ME! */
  private boolean isAcceptAll = true;

  /** DOCUMENT ME! */
  private String dialogTitle;

  /** DOCUMENT ME! */
  private int dialogType = OPEN_DIALOG;

  /** DOCUMENT ME! */
  private int retval = ERROR_OPTION;

  /** DOCUMENT ME! */
  private boolean multiSelection = false;

  /** DOCUMENT ME! */
  private boolean fileHiding = true;

  /** DOCUMENT ME! */
  private int fileSelectionMode = FILES_AND_DIRECTORIES;

  /** DOCUMENT ME! */
  private FileView fv = null;

  /** DOCUMENT ME! */
  private boolean controlButtonsShown = true;

  /** DOCUMENT ME! */
  private File currentDir = null;

  /** DOCUMENT ME! */
  private FileFilter currentFilter = null;

  /** DOCUMENT ME! */
  private File[] selectedFiles;

  /** DOCUMENT ME! */
  private File selectedFile;

  /**
   * Creates a new JFileChooser object.
   */
  public JFileChooser()
  {
    setup(null);
    setCurrentDirectory(null);
  }

  /**
   * Creates a new JFileChooser object.
   *
   * @param currentDirectoryPath DOCUMENT ME!
   */
  public JFileChooser(String currentDirectoryPath)
  {
    setup(null);
    setCurrentDirectory(fsv.createFileObject(currentDirectoryPath));
  }

  /**
   * Creates a new JFileChooser object with the specified directory and
   * FileSystemView.
   *
   * @param currentDirectoryPath the directory that should initially be
   *     shown the filechooser
   * @param fsv the FileSystemView object to use
   */
  public JFileChooser(String currentDirectoryPath, FileSystemView fsv)
  {
    setup(fsv);
    setCurrentDirectory(fsv.createFileObject(currentDirectoryPath));
  }

  /**
   * Creates a new JFileChooser object.
   *
   * @param currentDirectory DOCUMENT ME!
   */
  public JFileChooser(File currentDirectory)
  {
    setup(null);
    setCurrentDirectory(currentDirectory);
  }

  /**
   * Creates a new JFileChooser object.
   *
   * @param fsv DOCUMENT ME!
   */
  public JFileChooser(FileSystemView fsv)
  {
    setup(fsv);
    setCurrentDirectory(null);
  }

  /**
   * Creates a new JFileChooser object.
   *
   * @param currentDirectory DOCUMENT ME!
   * @param fsv DOCUMENT ME!
   */
  public JFileChooser(File currentDirectory, FileSystemView fsv)
  {
    setup(fsv);
    setCurrentDirectory(currentDirectory);
  }

  /**
   * DOCUMENT ME!
   *
   * @param view DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File getSelectedFile()
  {
    return selectedFile;
  }

  /**
   * DOCUMENT ME!
   *
   * @param file DOCUMENT ME!
   */
  public void setSelectedFile(File file)
  {
    if (selectedFile != file)
      {
	File old = selectedFile;
	selectedFile = file;
	firePropertyChange(SELECTED_FILE_CHANGED_PROPERTY, old, selectedFile);
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File[] getSelectedFiles()
  {
    if (selectedFiles != null)
      return selectedFiles;
    if (selectedFile != null)
      return new File[] { selectedFile };
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param selectedFiles DOCUMENT ME!
   */
  public void setSelectedFiles(File[] selectedFiles)
  {
    if (this.selectedFiles != selectedFiles)
      {
	File[] old = this.selectedFiles;
	this.selectedFiles = selectedFiles;
	firePropertyChange(SELECTED_FILES_CHANGED_PROPERTY, old, selectedFiles);
      }

    if (selectedFiles != null)
      setSelectedFile(selectedFiles[0]);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public File getCurrentDirectory()
  {
    return currentDir;
  }

  /**
   * DOCUMENT ME!
   *
   * @param dir DOCUMENT ME!
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
   * DOCUMENT ME!
   */
  public void changeToParentDirectory()
  {
    setCurrentDirectory(fsv.getParentDirectory(currentDir));
  }

  /**
   * DOCUMENT ME!
   */
  public void rescanCurrentDirectory()
  {
    getUI().rescanCurrentDirectory(this);
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   */
  public void ensureFileIsVisible(File f)
  {
    getUI().ensureFileIsVisible(this, f);
  }

  /**
   * DOCUMENT ME!
   *
   * @param parent DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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

    d.pack();
    d.show();
    return retval;
  }

  /**
   * DOCUMENT ME!
   *
   * @param parent DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   *
   * @throws HeadlessException DOCUMENT ME!
   */
  public int showSaveDialog(Component parent) throws HeadlessException
  {
    JDialog d = createDialog(parent);
    setDialogType(SAVE_DIALOG);

    retval = ERROR_OPTION;

    d.pack();
    d.show();
    return retval;
  }

  /**
   * DOCUMENT ME!
   *
   * @param parent DOCUMENT ME!
   * @param approveButtonText DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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

    d.pack();
    d.show();
    return retval;
  }

  /**
   * DOCUMENT ME!
   *
   * @param parent DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean getControlButtonsAreShown()
  {
    return controlButtonsShown;
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getDialogType()
  {
    return dialogType;
  }

  /**
   * DOCUMENT ME!
   *
   * @param dialogType DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param dialogTitle DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getDialogTitle()
  {
    return dialogTitle;
  }

  /**
   * DOCUMENT ME!
   *
   * @param toolTipText DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getApproveButtonToolTipText()
  {
    return approveButtonToolTipText;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getApproveButtonMnemonic()
  {
    return approveButtonMnemonic;
  }

  /**
   * DOCUMENT ME!
   *
   * @param mnemonic DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param mnemonic DOCUMENT ME!
   */
  public void setApproveButtonMnemonic(char mnemonic)
  {
    setApproveButtonMnemonic((int) Character.toUpperCase(mnemonic));
  }

  /**
   * DOCUMENT ME!
   *
   * @param approveButtonText DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getApproveButtonText()
  {
    return approveButtonText;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public FileFilter[] getChoosableFileFilters()
  {
    return (FileFilter[]) choosableFilters.toArray(new FileFilter[0]);
  }

  /**
   * DOCUMENT ME!
   *
   * @param filter DOCUMENT ME!
   */
  public void addChoosableFileFilter(FileFilter filter)
  {
    FileFilter[] old = getChoosableFileFilters();
    choosableFilters.add(filter);
    FileFilter[] newFilters = getChoosableFileFilters();
    firePropertyChange(CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY, old, newFilters);
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean removeChoosableFileFilter(FileFilter f)
  {
    FileFilter[] old = getChoosableFileFilters();
    if (! choosableFilters.remove(f))
      return false;
    FileFilter[] newFilters = getChoosableFileFilters();
    firePropertyChange(CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY, old, newFilters);
    return true;
  }

  /**
   * DOCUMENT ME!
   */
  public void resetChoosableFileFilters()
  {
    choosableFilters.clear();
    choosableFilters.add(getUI().getAcceptAllFileFilter(this));
    setFileFilter((FileFilter) choosableFilters.get(0));
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public FileFilter getAcceptAllFileFilter()
  {
    return getUI().getAcceptAllFileFilter(this);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isAcceptAllFileFilterUsed()
  {
    return isAcceptAll;
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
   */
  public void setAcceptAllFileFilterUsed(boolean b)
  {
    if (isAcceptAll != b)
      {
	isAcceptAll = b;
	firePropertyChange(ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY,
	                   ! isAcceptAll, isAcceptAll);
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JComponent getAccessory()
  {
    return accessory;
  }

  /**
   * DOCUMENT ME!
   *
   * @param newAccessory DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param mode DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getFileSelectionMode()
  {
    return fileSelectionMode;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isFileSelectionEnabled()
  {
    return (fileSelectionMode == FILES_ONLY
           || fileSelectionMode == FILES_AND_DIRECTORIES);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isDirectorySelectionEnabled()
  {
    return (fileSelectionMode == DIRECTORIES_ONLY
           || fileSelectionMode == FILES_AND_DIRECTORIES);
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isMultiSelectionEnabled()
  {
    return multiSelection;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isFileHidingEnabled()
  {
    return fileHiding;
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param filter DOCUMENT ME!
   */
  public void setFileFilter(FileFilter filter)
  {
    if (currentFilter != filter)
      {
	FileFilter old = currentFilter;
	currentFilter = filter;
	firePropertyChange(FILE_FILTER_CHANGED_PROPERTY, old, currentFilter);
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public FileFilter getFileFilter()
  {
    return currentFilter;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fileView DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public FileView getFileView()
  {
    return fv;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  private FileView getInternalFileView()
  {
    if (fv == null)
      return getUI().getFileView(this);
    return fv;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getName(File f)
  {
    return getInternalFileView().getName(f);
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getDescription(File f)
  {
    return getInternalFileView().getDescription(f);
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getTypeDescription(File f)
  {
    return getInternalFileView().getTypeDescription(f);
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Icon getIcon(File f)
  {
    return getInternalFileView().getIcon(f);
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isTraversable(File f)
  {
    return getFileSystemView().isTraversable(f).booleanValue();
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean accept(File f)
  {
    if (f == null)
      return false;
    return getFileFilter().accept(f);
  }

  /**
   * DOCUMENT ME!
   *
   * @param fsv DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public FileSystemView getFileSystemView()
  {
    return fsv;
  }

  /**
   * DOCUMENT ME!
   */
  public void approveSelection()
  {
    retval = APPROVE_OPTION;
    fireActionPerformed(APPROVE_SELECTION);
  }

  /**
   * DOCUMENT ME!
   */
  public void cancelSelection()
  {
    retval = CANCEL_OPTION;
    fireActionPerformed(CANCEL_SELECTION);
  }

  /**
   * DOCUMENT ME!
   *
   * @param l DOCUMENT ME!
   */
  public void addActionListener(ActionListener l)
  {
    listenerList.add(ActionListener.class, l);
  }

  /**
   * DOCUMENT ME!
   *
   * @param l DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) getListeners(ActionListener.class);
  }

  /**
   * DOCUMENT ME!
   *
   * @param command DOCUMENT ME!
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
   * DOCUMENT ME!
   */
  public void updateUI()
  {
    setUI((FileChooserUI) UIManager.getUI(this));
    revalidate();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getUIClassID()
  {
    return "FileChooserUI";
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }
}
