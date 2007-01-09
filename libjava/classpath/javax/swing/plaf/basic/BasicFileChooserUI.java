/* BasicFileChooserUI.java --
   Copyright (C) 2005  Free Software Foundation, Inc.

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

package javax.swing.plaf.basic;

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.FileChooserUI;
import javax.swing.plaf.metal.MetalIconFactory;


/**
 * A UI delegate for the {@link JFileChooser} component under the 
 * {@link BasicLookAndFeel}.
 */
public class BasicFileChooserUI extends FileChooserUI
{
  /**
   * A file filter that accepts all files.
   */
  protected class AcceptAllFileFilter extends FileFilter
  {
    /**
     * Creates a new instance.
     */
    public AcceptAllFileFilter()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns <code>true</code> always, as all files are accepted by this
     * filter.
     *
     * @param f  the file.
     *
     * @return Always <code>true</code>.
     */
    public boolean accept(File f)
    {
      return true;
    }

    /**
     * Returns a description for this filter.
     *
     * @return A description for the file filter.
     */
    public String getDescription()
    {
      return acceptAllFileFilterText;
    }
  }

  /**
   * Handles a user action to approve the dialog selection.
   * 
   * @see BasicFileChooserUI#getApproveSelectionAction()
   */
  protected class ApproveSelectionAction extends AbstractAction
  {
    /**
     * Creates a new ApproveSelectionAction object.
     */
    protected ApproveSelectionAction()
    {
      super("approveSelection");
    }

    /**
     * Sets the current selection and closes the dialog.
     * 
     * @param e  the action event.
     */
    public void actionPerformed(ActionEvent e)
    {
      Object obj = null;
      if (parentPath != null)
        obj = new String(parentPath + getFileName());
      else
        obj = filechooser.getSelectedFile();
      if (obj != null)
        {
          File f = filechooser.getFileSystemView().createFileObject(obj.toString());
          File currSelected = filechooser.getSelectedFile();
          if (filechooser.isTraversable(f))
            {
              filechooser.setCurrentDirectory(currSelected);
              filechooser.rescanCurrentDirectory();
            }
          else
            {
              filechooser.approveSelection();
              closeDialog();
            }
        }
      else
        {
          File f = new File(filechooser.getCurrentDirectory(), getFileName());
	  if ( selectedDir != null )
	    f = selectedDir;
          if (filechooser.isTraversable(f))
            {
              filechooser.setCurrentDirectory(f);
              filechooser.rescanCurrentDirectory();
            }
          else
            {
              filechooser.setSelectedFile(f);
              filechooser.approveSelection();
              closeDialog();
            }
        }
    }
  }

  /**
   * Provides presentation information about files and directories.
   */
  protected class BasicFileView extends FileView
  {
    /** Storage for cached icons. */
    protected Hashtable<File, Icon> iconCache = new Hashtable<File, Icon>();

    /**
     * Creates a new instance.
     */
    public BasicFileView()
    {
      // Nothing to do here.
    }

    /**
     * Adds an icon to the cache, associating it with the given file/directory.
     *
     * @param f  the file/directory.
     * @param i  the icon.
     */
    public void cacheIcon(File f, Icon i)
    {
      iconCache.put(f, i);
    }

    /**
     * Clears the icon cache.
     */
    public void clearIconCache()
    {
      iconCache.clear();
    }

    /**
     * Retrieves the icon associated with the specified file/directory, if 
     * there is one.
     *
     * @param f  the file/directory.
     *
     * @return The cached icon (or <code>null</code>).
     */
    public Icon getCachedIcon(File f)
    {
      return (Icon) iconCache.get(f);
    }

    /**
     * Returns a description of the given file/directory.  In this 
     * implementation, the description is the same as the name returned by 
     * {@link #getName(File)}.
     *
     * @param f  the file/directory.
     *
     * @return A description of the given file/directory.
     */
    public String getDescription(File f)
    {
      return getName(f);
    }

    /**
     * Returns an icon appropriate for the given file or directory.
     *
     * @param f  the file/directory.
     *
     * @return An icon.
     */
    public Icon getIcon(File f)
    {
      Icon val = getCachedIcon(f);
      if (val != null)
	return val;
      if (filechooser.isTraversable(f))
	val = directoryIcon;
      else
	val = fileIcon;
      cacheIcon(f, val);
      return val;
    }

    /**
     * Returns the name for the given file/directory.
     *
     * @param f  the file/directory.
     *
     * @return The name of the file/directory.
     */
    public String getName(File f)
    {
      String name = null;
      if (f != null)
        {
          JFileChooser c = getFileChooser();
          FileSystemView v = c.getFileSystemView();
          name = v.getSystemDisplayName(f);
        }
      return name;
    }

    /**
     * Returns a localised description for the type of file/directory.
     *
     * @param f  the file/directory.
     *
     * @return A type description for the given file/directory.
     */
    public String getTypeDescription(File f)
    {
      if (filechooser.isTraversable(f))
	return dirDescText;
      else
	return fileDescText;
    }

    /**
     * Returns {@link Boolean#TRUE} if the given file/directory is hidden,
     * and {@link Boolean#FALSE} otherwise.
     *
     * @param f  the file/directory.
     *
     * @return {@link Boolean#TRUE} or {@link Boolean#FALSE}.
     */
    public Boolean isHidden(File f)
    {
      return Boolean.valueOf(filechooser.getFileSystemView().isHiddenFile(f));
    }
  }

  /**
   * Handles an action to cancel the file chooser.
   * 
   * @see BasicFileChooserUI#getCancelSelectionAction()
   */
  protected class CancelSelectionAction extends AbstractAction
  {
    /**
     * Creates a new <code>CancelSelectionAction</code> object.
     */
    protected CancelSelectionAction()
    {
      super(null);
    }

    /**
     * Cancels the selection and closes the dialog.
     *
     * @param e  the action event (ignored).
     */
    public void actionPerformed(ActionEvent e)
    {
      filechooser.setSelectedFile(null);
      filechooser.setSelectedFiles(null);
      filechooser.cancelSelection();
      closeDialog();
    }
  }

  /**
   * An action to handle changes to the parent directory (for example, via
   * a click on the "up folder" button).
   * 
   * @see BasicFileChooserUI#getChangeToParentDirectoryAction()
   */
  protected class ChangeToParentDirectoryAction extends AbstractAction
  {
    /**
     * Creates a new <code>ChangeToParentDirectoryAction</code> object.
     */
    protected ChangeToParentDirectoryAction()
    {
      super("Go Up");
    }

    /**
     * Handles the action event.
     *
     * @param e  the action event.
     */
    public void actionPerformed(ActionEvent e)
    {
      filechooser.changeToParentDirectory();
      filechooser.revalidate();
      filechooser.repaint();
    }
  }

  /**
   * A mouse listener that handles double-click events.
   * 
   * @see BasicFileChooserUI#createDoubleClickListener(JFileChooser, JList)
   */
  protected class DoubleClickListener extends MouseAdapter
  {

    /** DOCUMENT ME! */
    private Object lastSelected;

    /** DOCUMENT ME! */
    private JList list;

    /**
     * Creates a new DoubleClickListener object.
     *
     * @param list DOCUMENT ME!
     */
    public DoubleClickListener(JList list)
    {
      this.list = list;
      lastSelected = list.getSelectedValue();
      setDirectorySelected(false);
    }

    /**
     * Handles a mouse click event.
     * 
     * @param e  the event.
     */
    public void mouseClicked(MouseEvent e)
    {
      Object p = list.getSelectedValue();
      if (p == null)
        return;
      FileSystemView fsv = filechooser.getFileSystemView();
      if (e.getClickCount() >= 2 && lastSelected != null &&
          p.toString().equals(lastSelected.toString()))
        {
          File f = fsv.createFileObject(lastSelected.toString());
          if (filechooser.isTraversable(f))
            {
              filechooser.setCurrentDirectory(f);
              filechooser.rescanCurrentDirectory();
            }
          else
            {
              filechooser.setSelectedFile(f);
              filechooser.approveSelection();
              closeDialog();
            }
        }
      else // single click
        {
          String path = p.toString();
          File f = fsv.createFileObject(path);
          filechooser.setSelectedFile(f);
          
          if (filechooser.isMultiSelectionEnabled())
            {
              int[] inds = list.getSelectedIndices();
              File[] allFiles = new File[inds.length];
              for (int i = 0; i < inds.length; i++)
                allFiles[i] = (File) list.getModel().getElementAt(inds[i]);
              filechooser.setSelectedFiles(allFiles);
            }
          
          if (filechooser.isTraversable(f))
            {
              setDirectorySelected(true);
              setDirectory(f);
            }
          else
            {
              setDirectorySelected(false);
              setDirectory(null);
            }
          lastSelected = path;
          parentPath = f.getParent();
	    
          if (f.isFile())
            setFileName(f.getName());
          else if (filechooser.getFileSelectionMode() != 
		   JFileChooser.FILES_ONLY)
            setFileName(path);
        }
    }

    /**
     * Handles a mouse entered event (NOT IMPLEMENTED).
     * 
     * @param e  the mouse event.
     */
    public void mouseEntered(MouseEvent e)
    {
      // FIXME: Implement
    }
  }

  /**
   * An action that changes the file chooser to display the user's home 
   * directory. 
   * 
   * @see BasicFileChooserUI#getGoHomeAction()
   */
  protected class GoHomeAction extends AbstractAction
  {
    /**
     * Creates a new <code>GoHomeAction</code> object.
     */
    protected GoHomeAction()
    {
      super("Go Home");
    }

    /**
     * Sets the directory to the user's home directory, and repaints the
     * file chooser component.
     *
     * @param e  the action event (ignored).
     */
    public void actionPerformed(ActionEvent e)
    {
      filechooser.setCurrentDirectory(filechooser.getFileSystemView()
                                                 .getHomeDirectory());
      filechooser.revalidate();
      filechooser.repaint();
    }
  }

  /**
   * An action that handles the creation of a new folder/directory.
   * 
   * @see BasicFileChooserUI#getNewFolderAction()
   */
  protected class NewFolderAction extends AbstractAction
  {
    /**
     * Creates a new <code>NewFolderAction</code> object.
     */
    protected NewFolderAction()
    {
      super("New Folder");
    }

    /**
     * Handles the event by creating a new folder.
     *
     * @param e  the action event (ignored).
     */
    public void actionPerformed(ActionEvent e)
    {
      try
        {
	  filechooser.getFileSystemView().createNewFolder(filechooser
	                                                  .getCurrentDirectory());
        }
      catch (IOException ioe)
        {
	  return;
        }
      filechooser.rescanCurrentDirectory();
      filechooser.repaint();
    }
  }

  /**
   * A listener for selection events in the file list.
   * 
   * @see BasicFileChooserUI#createListSelectionListener(JFileChooser)
   */
  protected class SelectionListener implements ListSelectionListener
  {
    /**
     * Creates a new <code>SelectionListener</code> object.
     */
    protected SelectionListener()
    {
      // Nothing to do here.
    }

    /**
     * Sets the JFileChooser to the selected file on an update
     *
     * @param e DOCUMENT ME!
     */
    public void valueChanged(ListSelectionEvent e)
    {
      JList list = (JList) e.getSource();
      Object f = list.getSelectedValue();
      if (f == null)
	return;
      File file = filechooser.getFileSystemView().createFileObject(f.toString());
      if (! filechooser.isTraversable(file))
	{
	  selectedDir = null;
	  filechooser.setSelectedFile(file);
	}
      else
	{
	  selectedDir = file;
	  filechooser.setSelectedFile(null);
	}
    }
  }

  /**
   * DOCUMENT ME!
   * 
   * @see BasicFileChooserUI#getUpdateAction()
   */
  protected class UpdateAction extends AbstractAction
  {
    /**
     * Creates a new UpdateAction object.
     */
    protected UpdateAction()
    {
      super(null);
    }

    /**
     * NOT YET IMPLEMENTED.
     *
     * @param e  the action event.
     */
    public void actionPerformed(ActionEvent e)
    {
      // FIXME: implement this
    }
  }

  /** The localised mnemonic for the cancel button. */
  protected int cancelButtonMnemonic;

  /** The localised text for the cancel button. */
  protected String cancelButtonText;

  /** The localised tool tip text for the cancel button. */
  protected String cancelButtonToolTipText;

  /** An icon representing a computer. */
  protected Icon computerIcon;

  /** An icon for the "details view" button. */
  protected Icon detailsViewIcon;

  /** An icon representing a directory. */
  protected Icon directoryIcon;

  /** The localised Mnemonic for the open button. */
  protected int directoryOpenButtonMnemonic;

  /** The localised text for the open button. */
  protected String directoryOpenButtonText;

  /** The localised tool tip text for the open button. */
  protected String directoryOpenButtonToolTipText;

  /** An icon representing a file. */
  protected Icon fileIcon;

  /** An icon representing a floppy drive. */
  protected Icon floppyDriveIcon;

  /** An icon representing a hard drive. */
  protected Icon hardDriveIcon;

  /** The localised mnemonic for the "help" button. */
  protected int helpButtonMnemonic;

  /** The localised text for the "help" button. */
  protected String helpButtonText;

  /** The localised tool tip text for the help button. */
  protected String helpButtonToolTipText;

  /** An icon representing the user's home folder. */
  protected Icon homeFolderIcon;

  /** An icon for the "list view" button. */
  protected Icon listViewIcon;

  /** An icon for the "new folder" button. */
  protected Icon newFolderIcon = directoryIcon;

  /** The localised mnemonic for the "open" button. */
  protected int openButtonMnemonic;

  /** The localised text for the "open" button. */
  protected String openButtonText;

  /** The localised tool tip text for the "open" button. */
  protected String openButtonToolTipText;

  /** The localised mnemonic for the "save" button. */
  protected int saveButtonMnemonic;

  /** The localised text for the "save" button. */
  protected String saveButtonText;

  /** The localised tool tip text for the save button. */
  protected String saveButtonToolTipText;

  /** The localised mnemonic for the "update" button. */
  protected int updateButtonMnemonic;

  /** The localised text for the "update" button. */
  protected String updateButtonText;

  /** The localised tool tip text for the "update" button. */
  protected String updateButtonToolTipText;

  /** An icon for the "up folder" button. */
  protected Icon upFolderIcon;

  // -- begin private, but package local since used in inner classes --

  /** The file chooser component represented by this UI delegate. */
  JFileChooser filechooser;

  /** The model for the directory list. */
  BasicDirectoryModel model;

  /** The file filter for all files. */
  FileFilter acceptAll = new AcceptAllFileFilter();

  /** The default file view. */
  FileView fv = new BasicFileView();

  /** The accept (open/save) button. */
  JButton accept;

  /** An optional accessory panel. */
  JPanel accessoryPanel = new JPanel();

  /** A property change listener. */
  PropertyChangeListener propertyChangeListener;

  /** The text describing the filter for "all files". */
  String acceptAllFileFilterText;

  /** The text describing a directory type. */
  String dirDescText;

  /** The text describing a file type. */
  String fileDescText;

  /** Is a directory selected? */
  boolean dirSelected;

  /** The current directory. */
  File currDir;

  // FIXME: describe what is contained in the bottom panel
  /** The bottom panel. */
  JPanel bottomPanel;
  
  /** The close panel. */
  JPanel closePanel;

  /** Text box that displays file name */
  JTextField entry;
    
  /** Current parent path */
  String parentPath;
  
  /**
   * The action for the 'approve' button.
   * @see #getApproveSelectionAction()
   */
  private ApproveSelectionAction approveSelectionAction;
  
  /**
   * The action for the 'cancel' button.
   * @see #getCancelSelectionAction()
   */
  private CancelSelectionAction cancelSelectionAction;
  
  /**
   * The action for the 'go home' control button.
   * @see #getGoHomeAction()
   */
  private GoHomeAction goHomeAction;
  
  /**
   * The action for the 'up folder' control button.
   * @see #getChangeToParentDirectoryAction()
   */
  private ChangeToParentDirectoryAction changeToParentDirectoryAction;
  
  /**
   * The action for the 'new folder' control button.
   * @see #getNewFolderAction()
   */
  private NewFolderAction newFolderAction;
  
  /**
   * The action for ???.  // FIXME: what is this?
   * @see #getUpdateAction()
   */
  private UpdateAction updateAction;

  /**
   * When in FILES_ONLY, mode a directory cannot be selected, so
   * we save a reference to any it here. This is used to enter
   * the directory on "Open" when in that mode.
   */
  private File selectedDir;
  
  // -- end private --

  /**
   * Closes the dialog.
   */
  void closeDialog()
  {
    Window owner = SwingUtilities.windowForComponent(filechooser);
    if (owner instanceof JDialog)
      ((JDialog) owner).dispose();
  }

  /**
   * Creates a new <code>BasicFileChooserUI</code> object.
   *
   * @param b  the file chooser component.
   */
  public BasicFileChooserUI(JFileChooser b)
  {
  }

  /**
   * Returns a UI delegate for the given component.
   *
   * @param c  the component (should be a {@link JFileChooser}).
   *
   * @return A new UI delegate.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicFileChooserUI((JFileChooser) c);
  }

  /**
   * Installs the UI for the specified component.
   * 
   * @param c  the component (should be a {@link JFileChooser}).
   */
  public void installUI(JComponent c)
  {
    if (c instanceof JFileChooser)
      {
        JFileChooser fc = (JFileChooser) c;
        this.filechooser = fc;
        fc.resetChoosableFileFilters();
        createModel();
        clearIconCache();
        installDefaults(fc);
        installComponents(fc);
        installListeners(fc);
        
        File path = filechooser.getCurrentDirectory();
        if (path != null)
          parentPath = path.getParent();
      }
  }

  /**
   * Uninstalls this UI from the given component.
   * 
   * @param c  the component (should be a {@link JFileChooser}).
   */
  public void uninstallUI(JComponent c)
  {
    model = null;
    uninstallListeners(filechooser);
    uninstallComponents(filechooser);
    uninstallDefaults(filechooser);
    filechooser = null;
  }

  // FIXME: Indent the entries in the combobox
  // Made this method package private to access it from within inner classes
  // with better performance
  void boxEntries()
  {
    ArrayList parentFiles = new ArrayList();
    File parent = filechooser.getCurrentDirectory();
    if (parent == null)
      parent = filechooser.getFileSystemView().getDefaultDirectory();
    while (parent != null)
      {
        String name = parent.getName();
        if (name.equals(""))
          name = parent.getAbsolutePath();

        parentFiles.add(parentFiles.size(), name);
        parent = parent.getParentFile();
      }

    if (parentFiles.size() == 0)
      return;

  }  

  /**
   * Creates and install the subcomponents for the file chooser.
   *
   * @param fc  the file chooser.
   */
  public void installComponents(JFileChooser fc)
  {
  }

  /**
   * Uninstalls the components from the file chooser.
   *
   * @param fc  the file chooser.
   */
  public void uninstallComponents(JFileChooser fc)
  {
  }

  /**
   * Installs the listeners required by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void installListeners(JFileChooser fc)
  {
    propertyChangeListener = createPropertyChangeListener(filechooser);
    if (propertyChangeListener != null)
      filechooser.addPropertyChangeListener(propertyChangeListener);
    fc.addPropertyChangeListener(getModel());
  }

  /**
   * Uninstalls the listeners previously installed by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void uninstallListeners(JFileChooser fc)
  {
    if (propertyChangeListener != null)
      {
        filechooser.removePropertyChangeListener(propertyChangeListener);
        propertyChangeListener = null;
      }
    fc.removePropertyChangeListener(getModel());
  }

  /**
   * Installs the defaults for this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void installDefaults(JFileChooser fc)
  {
    installIcons(fc);
    installStrings(fc);
  }

  /**
   * Uninstalls the defaults previously added by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void uninstallDefaults(JFileChooser fc)
  {
    uninstallStrings(fc);
    uninstallIcons(fc);
  }

  /**
   * Installs the icons for this UI delegate.
   *
   * @param fc  the file chooser (ignored).
   */
  protected void installIcons(JFileChooser fc)
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    computerIcon = MetalIconFactory.getTreeComputerIcon();
    detailsViewIcon = defaults.getIcon("FileChooser.detailsViewIcon");
    directoryIcon = new MetalIconFactory.TreeFolderIcon();
    fileIcon = new MetalIconFactory.TreeLeafIcon();
    floppyDriveIcon = MetalIconFactory.getTreeFloppyDriveIcon();
    hardDriveIcon = MetalIconFactory.getTreeHardDriveIcon();
    homeFolderIcon = defaults.getIcon("FileChooser.homeFolderIcon");
    listViewIcon = defaults.getIcon("FileChooser.listViewIcon");
    newFolderIcon = defaults.getIcon("FileChooser.newFolderIcon");
    upFolderIcon = defaults.getIcon("FileChooser.upFolderIcon");
  }

  /**
   * Uninstalls the icons previously added by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void uninstallIcons(JFileChooser fc)
  {
    computerIcon = null;
    detailsViewIcon = null;
    directoryIcon = null;
    fileIcon = null;
    floppyDriveIcon = null;
    hardDriveIcon = null;
    homeFolderIcon = null;
    listViewIcon = null;
    newFolderIcon = null;
    upFolderIcon = null;
  }

  /**
   * Installs the strings used by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void installStrings(JFileChooser fc)
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    dirDescText = defaults.getString("FileChooser.directoryDescriptionText");
    fileDescText = defaults.getString("FileChooser.fileDescriptionText");

    acceptAllFileFilterText = defaults.getString("FileChooser.acceptAllFileFilterText");
    cancelButtonText = "Cancel";
    cancelButtonToolTipText = "Abort file chooser dialog";
    cancelButtonMnemonic = new Integer((String) UIManager.get("FileChooser.cancelButtonMnemonic")).intValue();

    directoryOpenButtonText = "Open";
    directoryOpenButtonToolTipText = "Open selected directory";
    directoryOpenButtonMnemonic 
        = new Integer((String) UIManager.get("FileChooser.directoryOpenButtonMnemonic")).intValue();
    
    helpButtonText = "Help";
    helpButtonToolTipText = "FileChooser help";
    helpButtonMnemonic = new Integer((String) UIManager.get("FileChooser.helpButtonMnemonic")).intValue();

    openButtonText = "Open";
    openButtonToolTipText = "Open selected file";
    openButtonMnemonic = new Integer((String) UIManager.get("FileChooser.openButtonMnemonic")).intValue();

    saveButtonText = "Save";
    saveButtonToolTipText = "Save selected file";
    saveButtonMnemonic = new Integer((String) UIManager.get("FileChooser.saveButtonMnemonic")).intValue();
  
    updateButtonText = "Update";
    updateButtonToolTipText = "Update directory listing";
    updateButtonMnemonic = new Integer((String) UIManager.get("FileChooser.updateButtonMnemonic")).intValue();
  }

  /**
   * Uninstalls the strings previously added by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void uninstallStrings(JFileChooser fc)
  {
    acceptAllFileFilterText = null;
    dirDescText = null;
    fileDescText = null;

    cancelButtonText = null;
    cancelButtonToolTipText = null;

    directoryOpenButtonText = null;
    directoryOpenButtonToolTipText = null;

    helpButtonText = null;
    helpButtonToolTipText = null;

    openButtonText = null;
    openButtonToolTipText = null;

    saveButtonText = null;
    saveButtonToolTipText = null;
    
    updateButtonText = null;
    updateButtonToolTipText = null;
  }

  /**
   * Creates a new directory model.
   */
  protected void createModel()
  {
    model = new BasicDirectoryModel(filechooser);
  }

  /**
   * Returns the directory model.
   *
   * @return The directory model.
   */
  public BasicDirectoryModel getModel()
  {
    return model;
  }

  /**
   * Creates a listener to handle changes to the properties of the given
   * file chooser component.
   * 
   * @param fc  the file chooser component.
   * 
   * @return A new listener.
   */
  public PropertyChangeListener createPropertyChangeListener(JFileChooser fc)
  {
    // The RI returns null here, so do we.
    return null;
  }

  /**
   * Returns the current file name.
   * 
   * @return The current file name.
   */
  public String getFileName()
  {
    return entry.getText();
  }

  /**
   * Returns the current directory name.
   *
   * @return The directory name.
   * 
   * @see #setDirectoryName(String)
   */
  public String getDirectoryName()
  {
    // XXX: I don't see a case where the thing returns something non-null..
    return null;
  }

  /**
   * Sets the file name.
   *
   * @param filename  the file name.
   * 
   * @see #getFileName()
   */
  public void setFileName(String filename)
  {
    // FIXME:  it might be the case that this method provides an access 
    // point for the JTextField (or whatever) a subclass is using...
    //this.filename = filename;
  }

  /**
   * Sets the directory name (NOT IMPLEMENTED).
   *
   * @param dirname  the directory name.
   * 
   * @see #getDirectoryName()
   */
  public void setDirectoryName(String dirname)
  {
    // FIXME: Implement
  }

  /**
   * Rescans the current directory.
   *
   * @param fc  the file chooser.
   */
  public void rescanCurrentDirectory(JFileChooser fc)
  {
    getModel().validateFileCache();
  }

  /**
   * NOT YET IMPLEMENTED.
   *
   * @param fc  the file chooser.
   * @param f  the file.
   */
  public void ensureFileIsVisible(JFileChooser fc, File f)
  {
    // XXX: Not sure what this does.
  }

  /**
   * Returns the {@link JFileChooser} component that this UI delegate 
   * represents.
   *
   * @return The component represented by this UI delegate.
   */
  public JFileChooser getFileChooser()
  {
    return filechooser;
  }

  /**
   * Returns the optional accessory panel.
   *
   * @return The optional accessory panel.
   */
  public JPanel getAccessoryPanel()
  {
    return accessoryPanel;
  }

  /**
   * Returns the approve (open or save) button for the dialog.
   *
   * @param fc  the file chooser.
   *
   * @return The button.
   */
  protected JButton getApproveButton(JFileChooser fc)
  {
    return accept;
  }

  /**
   * Returns the tool tip text for the approve (open/save) button.  This first
   * checks the file chooser to see if a value has been explicitly set - if
   * not, a default value appropriate for the type of file chooser is 
   * returned.
   *
   * @param fc  the file chooser.
   *
   * @return The tool tip text.
   */
  public String getApproveButtonToolTipText(JFileChooser fc)
  {
    if (fc.getApproveButtonToolTipText() != null)
      return fc.getApproveButtonToolTipText();
    else if (fc.getDialogType() == JFileChooser.SAVE_DIALOG)
      return saveButtonToolTipText;
    else
      return openButtonToolTipText;
  }

  /**
   * Clears the icon cache.
   */
  public void clearIconCache()
  {
    if (fv instanceof BasicFileView)
      ((BasicFileView) fv).clearIconCache();
  }

  /**
   * Creates a new listener to handle selections in the file list.
   *
   * @param fc  the file chooser component.
   *
   * @return A new instance of {@link SelectionListener}.
   */
  public ListSelectionListener createListSelectionListener(JFileChooser fc)
  {
    return new SelectionListener();
  }

  /**
   * Creates a new listener to handle double-click events.
   *
   * @param fc  the file chooser component.
   * @param list  the list.
   *
   * @return A new instance of {@link DoubleClickListener}.
   */
  protected MouseListener createDoubleClickListener(JFileChooser fc, JList list)
  {
    return new DoubleClickListener(list);
  }

  /**
   * Returns <code>true</code> if a directory is selected, and 
   * <code>false</code> otherwise.
   *
   * @return A boolean.
   */
  protected boolean isDirectorySelected()
  {
    return dirSelected;
  }

  /**
   * Sets the flag that indicates whether the current directory is selected.
   *
   * @param selected  the new flag value.
   */
  protected void setDirectorySelected(boolean selected)
  {
    dirSelected = selected;
  }

  /**
   * Returns the current directory.
   *
   * @return The current directory.
   */
  protected File getDirectory()
  {
    return currDir;
  }

  /**
   * Sets the current directory.
   *
   * @param f  the directory.
   */
  protected void setDirectory(File f)
  {
    currDir = f;
  }

  /**
   * Returns the "accept all" file filter.
   *
   * @param fc  the file chooser component.
   *
   * @return The "accept all" file filter.
   */
  public FileFilter getAcceptAllFileFilter(JFileChooser fc)
  {
    return acceptAll;
  }

  /**
   * Returns the default file view (NOT the file view from the file chooser,
   * if there is one).
   *
   * @param fc  the file chooser component.
   *
   * @return The file view.
   * 
   * @see JFileChooser#getFileView()
   */
  public FileView getFileView(JFileChooser fc)
  {
    return fv;
  }

  /**
   * Returns the dialog title.
   *
   * @param fc  the file chooser (<code>null</code> not permitted).
   *
   * @return The dialog title.
   * 
   * @see JFileChooser#getDialogTitle()
   */
  public String getDialogTitle(JFileChooser fc)
  {
    String result = fc.getDialogTitle();
    if (result == null)
      result = getApproveButtonText(fc);
    return result;
  }

  /**
   * Returns the approve button mnemonic.
   *
   * @param fc  the file chooser (<code>null</code> not permitted).
   *
   * @return The approve button mnemonic.
   * 
   * @see JFileChooser#getApproveButtonMnemonic()
   */
  public int getApproveButtonMnemonic(JFileChooser fc)
  {
    if (fc.getApproveButtonMnemonic() != 0)
      return fc.getApproveButtonMnemonic();
    else if (fc.getDialogType() == JFileChooser.SAVE_DIALOG)
      return saveButtonMnemonic;
    else
      return openButtonMnemonic;
  }

  /**
   * Returns the approve button text.
   *
   * @param fc  the file chooser (<code>null</code> not permitted).
   *
   * @return The approve button text.
   * 
   * @see JFileChooser#getApproveButtonText()
   */
  public String getApproveButtonText(JFileChooser fc)
  {
    String result = fc.getApproveButtonText();
    if (result == null)
      {
        if (fc.getDialogType() == JFileChooser.SAVE_DIALOG)
          result = saveButtonText;
        else
          result = openButtonText;
      }
    return result;
  }

  /**
   * Creates and returns a new action that will be used with the "new folder" 
   * button.
   *
   * @return A new instance of {@link NewFolderAction}.
   */
  public Action getNewFolderAction()
  {
    if (newFolderAction == null)
      newFolderAction = new NewFolderAction();
    return newFolderAction;
  }

  /**
   * Creates and returns a new action that will be used with the "home folder" 
   * button.
   *
   * @return A new instance of {@link GoHomeAction}.
   */
  public Action getGoHomeAction()
  {
    if (goHomeAction == null)
      goHomeAction = new GoHomeAction();
    return goHomeAction;
  }

  /**
   * Returns the action that handles events for the "up folder" control button.
   *
   * @return An instance of {@link ChangeToParentDirectoryAction}.
   */
  public Action getChangeToParentDirectoryAction()
  {
    if (changeToParentDirectoryAction == null)
      changeToParentDirectoryAction = new ChangeToParentDirectoryAction();
    return changeToParentDirectoryAction;
  }

  /**
   * Returns the action that handles events for the "approve" button.
   *
   * @return An instance of {@link ApproveSelectionAction}.
   */
  public Action getApproveSelectionAction()
  {
    if (approveSelectionAction == null)
      approveSelectionAction = new ApproveSelectionAction();
    return approveSelectionAction;
  }

  /**
   * Returns the action that handles events for the "cancel" button.
   *
   * @return An instance of {@link CancelSelectionAction}.
   */
  public Action getCancelSelectionAction()
  {
    if (cancelSelectionAction == null)
      cancelSelectionAction = new CancelSelectionAction();
    return cancelSelectionAction;
  }

  /**
   * Returns the update action (an instance of {@link UpdateAction}).
   *
   * @return An action. 
   */
  public Action getUpdateAction()
  {
    if (updateAction == null)
      updateAction = new UpdateAction();
    return updateAction;
  }
}
