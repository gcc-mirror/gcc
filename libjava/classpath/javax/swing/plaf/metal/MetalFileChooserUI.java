/* MetalFileChooserUI.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf.metal;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.text.NumberFormat;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.AbstractListModel;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.JViewport;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicFileChooserUI;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;

import java.sql.Date;

import java.text.DateFormat;

import java.util.List;


/**
 * A UI delegate for the {@link JFileChooser} component.  This class is only
 * partially implemented and is not usable yet.
 */
public class MetalFileChooserUI 
  extends BasicFileChooserUI
{
  
  /**
   * A renderer for the files and directories in the file chooser table.
   */
  class TableFileRenderer
    extends DefaultTableCellRenderer
  {
    
    /**
     * Creates a new renderer.
     */
    public TableFileRenderer()
    {
      super();
    }
    
    /**
     * Returns a component that can render the specified value.
     * 
     * @param table  the table
     * @param value  the string value of the cell
     * @param isSelected  is the item selected?
     * @param hasFocus  does the item have the focus?
     * @param row  the row
     * @param column  the column
     * 
     * @return The renderer.
     */
    public Component getTableCellRendererComponent(JTable table, Object value,
        boolean isSelected, boolean hasFocus, int row, int column)
    {
      if (column == 0)
        {
          FileView v = getFileView(getFileChooser());
          ListModel lm = fileList.getModel();
          if (row < lm.getSize())
            setIcon(v.getIcon((File) lm.getElementAt(row)));
        }
      else
        setIcon(null);
      
      setText(value.toString());
      setOpaque(true);
      setEnabled(table.isEnabled());
      setFont(fileList.getFont());
      
      if (startEditing && column == 0 || !isSelected)
        {
          setBackground(table.getBackground());
          setForeground(table.getForeground());
        }
      else
        {
          setBackground(table.getSelectionBackground());
          setForeground(table.getSelectionForeground());
        }

      if (hasFocus)
        setBorder(UIManager.getBorder("Table.focusCellHighlightBorder"));
      else
        setBorder(noFocusBorder);
      
      return this;
    }
  }
  
  /**
   * ActionListener for the list view.
   */
  class ListViewActionListener implements ActionListener
  {
    
    /**
     * This method is invoked when an action occurs.
     * 
     * @param e -
     *          the <code>ActionEvent</code> that occurred
     */
    public void actionPerformed(ActionEvent e)
    {
      if (!listView)
        {
          int[] index = fileTable.getSelectedRows();
          listView = true;
          JFileChooser fc = getFileChooser();
          fc.remove(fileTablePanel);
          createList(fc);

          fileList.getSelectionModel().clearSelection();
          if (index.length > 0)
              for (int i = 0; i < index.length; i++)
                fileList.getSelectionModel().addSelectionInterval(index[i], index[i]);
          
          fc.add(fileListPanel, BorderLayout.CENTER);
          fc.revalidate();
          fc.repaint();
        }
    }
  }
  
  /**
   * ActionListener for the details view.
   */
  class DetailViewActionListener implements ActionListener
  {
    
    /**
     * This method is invoked when an action occurs.
     * 
     * @param e -
     *          the <code>ActionEvent</code> that occurred
     */
    public void actionPerformed(ActionEvent e)
    {
      if (listView)
        {
          int[] index = fileList.getSelectedIndices();
          JFileChooser fc = getFileChooser();
          listView = false;
          fc.remove(fileListPanel);
          
          if (fileTable == null)
            createDetailsView(fc);
          else
            updateTable();

          fileTable.getSelectionModel().clearSelection();
          if (index.length > 0)
            {
              for (int i = 0; i < index.length; i++)
                fileTable.getSelectionModel().addSelectionInterval(index[i], index[i]);
            }
          
          fc.add(fileTablePanel, BorderLayout.CENTER);
          fc.revalidate();
          fc.repaint();
        }
    }
  }
  
  /**
   * A property change listener.
   */
  class MetalFileChooserPropertyChangeListener 
    implements PropertyChangeListener
  {
    /**
     * Default constructor.
     */
    public MetalFileChooserPropertyChangeListener()
    {
    }
    
    /**
     * Handles a property change event.
     * 
     * @param e  the event.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      JFileChooser filechooser = getFileChooser();
      
      String n = e.getPropertyName();
      if (n.equals(JFileChooser.MULTI_SELECTION_ENABLED_CHANGED_PROPERTY))
        {
          int mode = -1; 
          if (filechooser.isMultiSelectionEnabled())
            mode = ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;
          else
            mode = ListSelectionModel.SINGLE_SELECTION;
          
          if (listView)
            fileList.setSelectionMode(mode);
          else
            fileTable.setSelectionMode(mode);
        }
      else if (n.equals(JFileChooser.SELECTED_FILE_CHANGED_PROPERTY))
        {
          File file = filechooser.getSelectedFile();
          
          if (file != null
              && filechooser.getDialogType() == JFileChooser.SAVE_DIALOG)
            {
              if (file.isDirectory() && filechooser.isTraversable(file))
                {
                  directoryLabel = look;
                  dirLabel.setText(directoryLabel);
                  filechooser.setApproveButtonText(openButtonText);
                  filechooser.setApproveButtonToolTipText(openButtonToolTipText);
                }
              else if (file.isFile())
                {
                  directoryLabel = save;
                  dirLabel.setText(directoryLabel);
                  filechooser.setApproveButtonText(saveButtonText);
                  filechooser.setApproveButtonToolTipText(saveButtonToolTipText);
                }
            }
            
          if (file == null)
            setFileName(null);
          else
            setFileName(file.getName());
          int index = -1;
          index = getModel().indexOf(file);
          if (index >= 0)
            {
              if (listView)
                {
                  fileList.setSelectedIndex(index);
                  fileList.ensureIndexIsVisible(index);
                  fileList.revalidate();
                  fileList.repaint();
                }
              else
                {
                  fileTable.getSelectionModel().addSelectionInterval(index, index);
                  fileTable.scrollRectToVisible(fileTable.getCellRect(index, 0, true));
                  fileTable.revalidate();
                  fileTable.repaint();
                }
            }
        }
      
      else if (n.equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY))
        {
          if (listView)
            {
              fileList.clearSelection();
              fileList.revalidate();
              fileList.repaint();
            }
          else
            {
              fileTable.clearSelection();
              fileTable.revalidate();
              fileTable.repaint();
            }

          setDirectorySelected(false);
          File currentDirectory = filechooser.getCurrentDirectory();
          setDirectory(currentDirectory);
          boolean hasParent = (currentDirectory.getParentFile() != null);
          getChangeToParentDirectoryAction().setEnabled(hasParent);
        }
      
      else if (n.equals(JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY))
        {
          filterModel.propertyChange(e);
        }
      else if (n.equals(JFileChooser.FILE_FILTER_CHANGED_PROPERTY))
        {
          filterModel.propertyChange(e);
        }
      else if (n.equals(JFileChooser.DIALOG_TYPE_CHANGED_PROPERTY)
                 || n.equals(JFileChooser.DIALOG_TITLE_CHANGED_PROPERTY))
        {
          Window owner = SwingUtilities.windowForComponent(filechooser);
          if (owner instanceof JDialog)
            ((JDialog) owner).setTitle(getDialogTitle(filechooser));
          approveButton.setText(getApproveButtonText(filechooser));
          approveButton.setToolTipText(
                  getApproveButtonToolTipText(filechooser));
          approveButton.setMnemonic(getApproveButtonMnemonic(filechooser));
        }
      
      else if (n.equals(JFileChooser.APPROVE_BUTTON_TEXT_CHANGED_PROPERTY))
        approveButton.setText(getApproveButtonText(filechooser));
      
      else if (n.equals(
              JFileChooser.APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY))
        approveButton.setToolTipText(getApproveButtonToolTipText(filechooser));
      
      else if (n.equals(JFileChooser.APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY))
        approveButton.setMnemonic(getApproveButtonMnemonic(filechooser));

      else if (n.equals(
              JFileChooser.CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY))
        {
          if (filechooser.getControlButtonsAreShown())
            {
              topPanel.add(controls, BorderLayout.EAST);
            }
          else
            topPanel.remove(controls);
          topPanel.revalidate();
          topPanel.repaint();
          topPanel.doLayout();
        }
      
      else if (n.equals(
              JFileChooser.ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY))
        {
          if (filechooser.isAcceptAllFileFilterUsed())
            filechooser.addChoosableFileFilter(
                    getAcceptAllFileFilter(filechooser));
          else
            filechooser.removeChoosableFileFilter(
                    getAcceptAllFileFilter(filechooser));
        }
      
      else if (n.equals(JFileChooser.ACCESSORY_CHANGED_PROPERTY))
        {
          JComponent old = (JComponent) e.getOldValue();
          if (old != null)
            getAccessoryPanel().remove(old);
          JComponent newval = (JComponent) e.getNewValue();
          if (newval != null)
            getAccessoryPanel().add(newval);
        }
      
      if (n.equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY)
          || n.equals(JFileChooser.FILE_FILTER_CHANGED_PROPERTY)
          || n.equals(JFileChooser.FILE_HIDING_CHANGED_PROPERTY))
        {
          // Remove editing component
          if (fileTable != null)
            fileTable.removeAll();
          if (fileList != null)
            fileList.removeAll();
          startEditing = false;
          
          // Set text on button back to original.
          if (filechooser.getDialogType() == JFileChooser.SAVE_DIALOG)
            {
              directoryLabel = save;
              dirLabel.setText(directoryLabel);
              filechooser.setApproveButtonText(saveButtonText);
              filechooser.setApproveButtonToolTipText(saveButtonToolTipText);
            }
          
          rescanCurrentDirectory(filechooser);
        }
      
      filechooser.revalidate();
      filechooser.repaint();
    }
  };
  
  /** 
   * A combo box model containing the selected directory and all its parent
   * directories.
   */
  protected class DirectoryComboBoxModel
    extends AbstractListModel
    implements ComboBoxModel
  {
    /** Storage for the items in the model. */
    private List items;
    
    /** The index of the selected item. */
    private int selectedIndex;
    
    /**
     * Creates a new model.
     */
    public DirectoryComboBoxModel() 
    {
      items = new java.util.ArrayList();
      selectedIndex = -1;
    }
    
    /**
     * Returns the number of items in the model.
     * 
     * @return The number of items in the model.
     */
    public int getSize()
    {
      return items.size();
    }
    
    /**
     * Returns the item at the specified index.
     * 
     * @param index  the item index.
     * 
     * @return The item.
     */
    public Object getElementAt(int index)
    {
      return items.get(index);
    }
    
    /**
     * Returns the depth of the item at the given <code>index</code>.
     * 
     * @param index  the item index.
     * 
     * @return The depth.
     */
    public int getDepth(int index)
    {
      return Math.max(index, 0);
    }

    /**
     * Returns the selected item, or <code>null</code> if no item is selected.
     * 
     * @return The selected item, or <code>null</code>.
     */
    public Object getSelectedItem()
    {
      if (selectedIndex >= 0) 
        return items.get(selectedIndex);
      else
        return null;
    }
    
    /**
     * Sets the selected item.  This clears all the directories from the
     * existing list, and repopulates it with the new selected directory
     * and all its parent directories.
     * 
     * @param selectedDirectory  the selected directory.
     */
    public void setSelectedItem(Object selectedDirectory)
    {
      items.clear();
      FileSystemView fsv = getFileChooser().getFileSystemView();
      File parent = (File) selectedDirectory;
      while (parent != null)
        {
          items.add(0, parent);
          parent = fsv.getParentDirectory(parent);
        }
      selectedIndex = items.indexOf(selectedDirectory);
      fireContentsChanged(this, 0, items.size() - 1);
    }
    
  }

  /**
   * Handles changes to the selection in the directory combo box.
   */
  protected class DirectoryComboBoxAction
    extends AbstractAction
  {
    /**
     * Creates a new action.
     */
    protected DirectoryComboBoxAction()
    {
      // Nothing to do here.
    }
    
    /**
     * Handles the action event.
     * 
     * @param e  the event.
     */
    public void actionPerformed(ActionEvent e)
    {
      JFileChooser fc = getFileChooser();
      fc.setCurrentDirectory((File) directoryModel.getSelectedItem());
    }
  }

  /**
   * A renderer for the items in the directory combo box.
   */
  class DirectoryComboBoxRenderer
    extends DefaultListCellRenderer
  {
    /**
     * Creates a new renderer.
     */
    public DirectoryComboBoxRenderer(JFileChooser fc)
    { 
    }
    
    /**
     * Returns a component that can be used to paint the given value within 
     * the list.
     * 
     * @param list  the list.
     * @param value  the value (a {@link File}).
     * @param index  the item index.
     * @param isSelected  is the item selected?
     * @param cellHasFocus  does the list cell have focus?
     * 
     * @return The list cell renderer.
     */
    public Component getListCellRendererComponent(JList list, Object value,
        int index, boolean isSelected, boolean cellHasFocus)
    {
      FileView fileView = getFileView(getFileChooser());
      File file = (File) value;
      setIcon(fileView.getIcon(file));
      setText(fileView.getName(file));
      
      if (isSelected)
        {
          setBackground(list.getSelectionBackground());
          setForeground(list.getSelectionForeground());
        }
      else
        {
          setBackground(list.getBackground());
          setForeground(list.getForeground());
        }

      setEnabled(list.isEnabled());
      setFont(list.getFont());
      return this;
    }
  }

  /**
   * A renderer for the files and directories in the file chooser.
   */
  protected class FileRenderer
    extends DefaultListCellRenderer
  {
    
    /**
     * Creates a new renderer.
     */
    protected FileRenderer()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns a component that can render the specified value.
     * 
     * @param list  the list.
     * @param value  the value (a {@link File}).
     * @param index  the index.
     * @param isSelected  is the item selected?
     * @param cellHasFocus  does the item have the focus?
     * 
     * @return The renderer.
     */
    public Component getListCellRendererComponent(JList list, Object value,
        int index, boolean isSelected, boolean cellHasFocus)
    {
      FileView v = getFileView(getFileChooser());
      File f = (File) value;
      if (f != null)
	{
	  setText(v.getName(f));
	  setIcon(v.getIcon(f));
	}
      else
	{
	  setText("");
	  setIcon(null);
	}
      setOpaque(true);
      if (isSelected)
        {
          setBackground(list.getSelectionBackground());
          setForeground(list.getSelectionForeground());
        }
      else
        {
          setBackground(list.getBackground());
          setForeground(list.getForeground());
        }

      setEnabled(list.isEnabled());
      setFont(list.getFont());

      if (cellHasFocus)
        setBorder(UIManager.getBorder("List.focusCellHighlightBorder"));
      else
        setBorder(noFocusBorder);
      return this;
    }
  }

  /**
   * A combo box model for the file selection filters.
   */
  protected class FilterComboBoxModel
    extends AbstractListModel
    implements ComboBoxModel, PropertyChangeListener
  {

    /** Storage for the filters in the model. */
    protected FileFilter[] filters;

    /** The index of the selected file filter. */
    private Object selected;
    
    /**
     * Creates a new model.
     */
    protected FilterComboBoxModel()
    {
      filters = new FileFilter[1];
      filters[0] = getAcceptAllFileFilter(getFileChooser());
      selected = filters[0];
    }
    
    /**
     * Handles property changes.
     * 
     * @param e  the property change event.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals(JFileChooser.FILE_FILTER_CHANGED_PROPERTY))
        {
          JFileChooser fc = getFileChooser();
          FileFilter[] choosableFilters = fc.getChoosableFileFilters();
          filters = choosableFilters;
          fireContentsChanged(this, 0, filters.length);
          selected = e.getNewValue();
          fireContentsChanged(this, -1, -1);
        }
      else if (e.getPropertyName().equals(
              JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY))
        {
          // repopulate list
          JFileChooser fc = getFileChooser();
          FileFilter[] choosableFilters = fc.getChoosableFileFilters();
          filters = choosableFilters;
          fireContentsChanged(this, 0, filters.length);
        }
    }
    
    /**
     * Sets the selected filter.
     * 
     * @param filter  the filter (<code>null</code> ignored).
     */
    public void setSelectedItem(Object filter)
    {
      if (filter != null)
      {
          selected = filter;
          fireContentsChanged(this, -1, -1);
      }
    }
    
    /**
     * Returns the selected file filter.
     * 
     * @return The selected file filter.
     */
    public Object getSelectedItem()
    {
      return selected;
    }
    
    /**
     * Returns the number of items in the model.
     * 
     * @return The number of items in the model.
     */
    public int getSize()
    {
      return filters.length;
    }
    
    /**
     * Returns the item at the specified index.
     * 
     * @param index  the item index.
     * 
     * @return The item at the specified index.
     */
    public Object getElementAt(int index)
    {
      return filters[index];
    }
    
  }

  /**
   * A renderer for the items in the file filter combo box.
   */
  public class FilterComboBoxRenderer
    extends DefaultListCellRenderer
  {
    /**
     * Creates a new renderer.
     */
    public FilterComboBoxRenderer()
    {
      // Nothing to do here.
    }
    
    /**
     * Returns a component that can be used to paint the given value within 
     * the list.
     * 
     * @param list  the list.
     * @param value  the value (a {@link FileFilter}).
     * @param index  the item index.
     * @param isSelected  is the item selected?
     * @param cellHasFocus  does the list cell have focus?
     * 
     * @return This component as the renderer.
     */
    public Component getListCellRendererComponent(JList list, Object value,
        int index, boolean isSelected, boolean cellHasFocus)
    {
      super.getListCellRendererComponent(list, value, index, isSelected, 
                                         cellHasFocus);
      FileFilter filter = (FileFilter) value;
      setText(filter.getDescription());
      return this;
    }
  }

  /**
   * A listener for selection events in the file list.
   * 
   * @see #createListSelectionListener(JFileChooser)
   */
  class MetalFileChooserSelectionListener 
    implements ListSelectionListener
  {
    /**
     * Creates a new <code>SelectionListener</code> object.
     */
    protected MetalFileChooserSelectionListener()
    {
      // Do nothing here.
    }

    /**
     * Makes changes to different properties when
     * a value has changed in the filechooser's selection.
     *
     * @param e - the list selection event that occured.
     */
    public void valueChanged(ListSelectionEvent e)
    {
      File f = (File) fileList.getSelectedValue();
      if (f == null)
        return;
      JFileChooser filechooser = getFileChooser();
      if (! filechooser.isTraversable(f))
        filechooser.setSelectedFile(f);
      else
        filechooser.setSelectedFile(null);
    }
  }

  /**
   * A mouse listener for the {@link JFileChooser}.
   * This listener is used for editing filenames.
   */
  protected class SingleClickListener
    extends MouseAdapter
  {
    
    /** Stores instance of the list */
    JList list;
    
    /** 
     * Stores the current file that is being edited.
     * It is null if nothing is currently being edited.
     */
    File editFile;
    
    /** The current file chooser. */
    JFileChooser fc;
    
    /** The last file selected. */
    Object lastSelected;
    
    /** The textfield used for editing. */
    JTextField editField;
    
    /**
     * Creates a new listener.
     * 
     * @param list  the directory/file list.
     */
    public SingleClickListener(JList list)
    {
      this.list = list;
      editFile = null;
      fc = getFileChooser();
      lastSelected = null;
      startEditing = false;
    }
    
    /**
     * Receives notification of a mouse click event.
     * 
     * @param e  the event.
     */
    public void mouseClicked(MouseEvent e)
    {
      if (e.getClickCount() == 1 && e.getButton() == MouseEvent.BUTTON1)
        {
          int index = list.locationToIndex(e.getPoint());
          File[] sf = fc.getSelectedFiles();
          if ((!fc.isMultiSelectionEnabled() || (sf != null && sf.length <= 1))
              && index >= 0 && !startEditing && list.isSelectedIndex(index))
            {
              Object tmp = list.getModel().getElementAt(index);
              if (lastSelected != null && lastSelected.equals(tmp))
                editFile(index);
              lastSelected = tmp;
            }
          else
              completeEditing();
        }
      else
        completeEditing();
    }
    
    /**
     * Sets up the text editor for the current file.
     * 
     * @param index -
     *          the current index of the item in the list to be edited.
     */
    void editFile(int index)
    {
      Rectangle bounds = list.getCellBounds(index, index);
      list.scrollRectToVisible(bounds);
      editFile = (File) list.getModel().getElementAt(index);
      if (editFile.canWrite())
        {
          startEditing = true;
          editField = new JTextField(editFile.getName());
          editField.addActionListener(new EditingActionListener());
          
          Icon icon = getFileView(fc).getIcon(editFile);
          if (icon != null)
            {
              int padding = icon.getIconWidth() + 4;
              bounds.x += padding;
              bounds.width -= padding;
            }
          editField.setBounds(bounds);
          
          list.add(editField);
          
          editField.requestFocus();
          editField.selectAll();
        }
      else
        completeEditing();
      list.repaint();
    }
    
    /** 
     * Completes the editing.
     */
    void completeEditing()
    {
      if (editField != null && editFile != null)
        {
          String text = editField.getText();
          if (text != null && text != "" && !text.equals(fc.getName(editFile)))
              if (editFile.renameTo
                  (fc.getFileSystemView().createFileObject
                   (fc.getCurrentDirectory(), text)))
                  rescanCurrentDirectory(fc);
          list.remove(editField);
        }
      startEditing = false;
      editFile = null;
      lastSelected = null;
      editField = null;
      list.repaint();
    }
    
    /**
     * ActionListener for the editing text field.
     */
    class EditingActionListener implements ActionListener
    {
      
      /**
       * This method is invoked when an action occurs.
       * 
       * @param e -
       *          the <code>ActionEvent</code> that occurred
       */
      public void actionPerformed(ActionEvent e)
      {
        if (e.getActionCommand().equals("notify-field-accept"))
          completeEditing();
        else if (editField != null)
          {
            list.remove(editField);
            startEditing = false;
            editFile = null;
            lastSelected = null;
            editField = null;
            list.repaint();
          }
      }
    }
  }

  /**
   * A mouse listener for the {@link JFileChooser}.
   * This listener is used for the table
   */
  private class TableClickListener extends MouseAdapter
  {

    /** Stores instance of the table */
    JTable table;

    /** Stores instance of the file chooser */
    JFileChooser fc;

    /** The last selected file. */
    Object lastSelected = null;
    
    /** 
     * Stores the current file that is being edited.
     * It is null if nothing is currently being edited.
     */
    File editFile;
    
    /** The textfield used for editing. */
    JTextField editField;

    /**
     * Creates a new listener.
     * 
     * @param table
     *          the directory/file table
     * @param fc
     *          the JFileChooser
     */
    public TableClickListener(JTable table, JFileChooser fc)
    {
      this.table = table;
      this.fc = fc;
      lastSelected = fileList.getSelectedValue();
      setDirectorySelected(false);
      startEditing = false;
      editFile = null;
      editField = null;
    }

    /**
     * Receives notification of a mouse click event.
     * 
     * @param e
     *          the event.
     */
    public void mouseClicked(MouseEvent e)
    {
      int row = table.getSelectedRow();
      Object selVal = fileList.getModel().getElementAt(row);
      if (selVal == null)
        return;
      FileSystemView fsv = fc.getFileSystemView();
      if (e.getClickCount() == 1 &&
          selVal.equals(lastSelected) &&
          e.getButton() == MouseEvent.BUTTON1)
        {
          File[] sf = fc.getSelectedFiles();
          if ((!fc.isMultiSelectionEnabled() || (sf != null && sf.length <= 1))
              && !startEditing)
            {
              editFile = (File) selVal;
              editFile(row);
            }
        }
      else if (e.getClickCount() >= 2 &&
          selVal.equals(lastSelected))
        {
          if (startEditing)
            completeEditing();
          File f = fsv.createFileObject(lastSelected.toString());
          if (fc.isTraversable(f))
            {
              fc.setCurrentDirectory(f);
              fc.rescanCurrentDirectory();
            }
          else
            {
              fc.setSelectedFile(f);
              fc.approveSelection();
              closeDialog();
            }
        }
      else
        {
          if (startEditing)
            completeEditing();
          String path = selVal.toString();
          File f = fsv.createFileObject(path);
          fc.setSelectedFile(f);
          if (fc.isTraversable(f))
            {
              setDirectorySelected(true);
              setDirectory(f);
            }
          else
            {
              setDirectorySelected(false);
              setDirectory(null);
            }
          lastSelected = selVal;
          if (f.isFile())
            setFileName(path.substring(path.lastIndexOf("/") + 1));
          else if (fc.getFileSelectionMode() == JFileChooser.DIRECTORIES_ONLY)
            setFileName(path);
        }
      fileTable.repaint();
    }

    /**
     * Sets up the text editor for the current file.
     * 
     * @param row -
     *          the current row of the item in the list to be edited.
     */
    void editFile(int row)
    {
      Rectangle bounds = table.getCellRect(row, 0, true);
      table.scrollRectToVisible(bounds);
      if (editFile.canWrite())
        {
          startEditing = true;
          editField = new JTextField(editFile.getName());
          editField.addActionListener(new EditingActionListener());

          // Need to adjust y pos
          bounds.y = row * table.getRowHeight();
          editField.setBounds(bounds);
          
          table.add(editField);
          
          editField.requestFocus();
          editField.selectAll();
        }
      else
        completeEditing();
      table.repaint();
    }
    
    /** 
     * Completes the editing.
     */
    void completeEditing()
    {
      if (editField != null && editFile != null)
        {
          String text = editField.getText();
          if (text != null && text != "" && !text.equals(fc.getName(editFile)))
              if (editFile.renameTo
                  (fc.getFileSystemView().createFileObject
                   (fc.getCurrentDirectory(), text)))
                  rescanCurrentDirectory(fc);
          table.remove(editField);
        }
      startEditing = false;
      editFile = null;
      editField = null;
      table.repaint();
    }
    
    /**
     * ActionListener for the editing text field.
     */
    class EditingActionListener implements ActionListener
    {
      
      /**
       * This method is invoked when an action occurs.
       * 
       * @param e -
       *          the <code>ActionEvent</code> that occurred
       */
      public void actionPerformed(ActionEvent e)
      {
        if (e.getActionCommand().equals("notify-field-accept"))
          completeEditing();
        else if (editField != null)
          {
            table.remove(editField);
            startEditing = false;
            editFile = null;
            editField = null;
            table.repaint();
          }
      }
    }
    
    /**
     * Closes the dialog.
     */
    public void closeDialog()
    {
      Window owner = SwingUtilities.windowForComponent(fc);
      if (owner instanceof JDialog)
        ((JDialog) owner).dispose();
    }
  } 
  
  /** The text for a label describing the directory combo box. */
  private String directoryLabel;
  
  private JComboBox directoryComboBox;
  
  /** The model for the directory combo box. */
  DirectoryComboBoxModel directoryModel;
  
  /** The text for a label describing the file text field. */
  private String fileLabel;
  
  /** The file name text field. */
  private JTextField fileTextField;
  
  /** The text for a label describing the filter combo box. */
  private String filterLabel;

  /** 
   * The top panel (contains the directory combo box and the control buttons). 
   */
  private JPanel topPanel;
  
  /** A panel containing the control buttons ('up', 'home' etc.). */
  private JPanel controls;

  /** 
   * The panel that contains the filename field and the filter combobox. 
   */
  private JPanel bottomPanel;

  /** 
   * The panel that contains the 'Open' (or 'Save') and 'Cancel' buttons. 
   */
  private JPanel buttonPanel;
  
  private JButton approveButton;
  
  /** The file list. */
  JList fileList;
  
  /** The file table. */
  JTable fileTable;
  
  /** The panel containing the file list. */
  JPanel fileListPanel;
  
  /** The panel containing the file table. */
  JPanel fileTablePanel;
  
  /** The filter combo box model. */
  private FilterComboBoxModel filterModel;

  /** The action map. */
  private ActionMap actionMap;
  
  /** True if currently in list view. */
  boolean listView;
  
  /** True if we can or have started editing a cell. */
  boolean startEditing;
  
  /** The scrollpane used for the table and list. */
  JScrollPane scrollPane;
  
  /** The text for the label when saving. */
  String save;
  
  /** The text for the label when opening a directory. */
  String look;
  
  /** The label for the file combo box. */
  JLabel dirLabel;
  
  /** Listeners. */
  ListSelectionListener listSelList;
  MouseListener doubleClickList;
  SingleClickListener singleClickList;
  TableClickListener tableClickList;
  
  /**
   * A factory method that returns a UI delegate for the specified
   * component.
   * 
   * @param c  the component (which should be a {@link JFileChooser}).
   */
  public static ComponentUI createUI(JComponent c)
  {
    JFileChooser chooser = (JFileChooser) c;
    return new MetalFileChooserUI(chooser);
  }

  /**
   * Creates a new instance of this UI delegate.
   * 
   * @param filechooser  the file chooser component.
   */
  public MetalFileChooserUI(JFileChooser filechooser)
  {
    super(filechooser);
    bottomPanel = new JPanel(new GridLayout(3, 2));
    buttonPanel = new JPanel();
  }

  public void installUI(JComponent c)
  {
    super.installUI(c);
    actionMap = createActionMap();
  }
  
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);
    actionMap = null;
  }
  
  /**
   * Installs the sub-components of the file chooser.
   * 
   * @param fc  the file chooser component.
   */
  public void installComponents(JFileChooser fc)
  {
    fc.setLayout(new BorderLayout());
    topPanel = new JPanel(new BorderLayout());
    dirLabel = new JLabel(directoryLabel);
    topPanel.add(dirLabel, BorderLayout.WEST);
    this.controls = new JPanel();
    addControlButtons();
    
    JPanel dirPanel = new JPanel(new VerticalMidLayout());
    directoryModel = createDirectoryComboBoxModel(fc);
    directoryComboBox = new JComboBox(directoryModel);
    directoryComboBox.setRenderer(createDirectoryComboBoxRenderer(fc));
    dirPanel.add(directoryComboBox);
    topPanel.add(dirPanel);
    topPanel.add(controls, BorderLayout.EAST);
    topPanel.setBorder(BorderFactory.createEmptyBorder(8, 8, 0, 8));
    fc.add(topPanel, BorderLayout.NORTH);
    
    JPanel list = createList(fc);
    list.setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));
    fc.add(list, BorderLayout.CENTER);
    
    JPanel bottomPanel = getBottomPanel();
    filterModel = createFilterComboBoxModel();
    JComboBox fileFilterCombo = new JComboBox(filterModel);
    fileFilterCombo.setRenderer(createFilterComboBoxRenderer());
    
    fileTextField = new JTextField();
    JPanel fileNamePanel = new JPanel(new VerticalMidLayout());
    fileNamePanel.setBorder(BorderFactory.createEmptyBorder(0, 20, 0, 5));
    fileNamePanel.add(fileTextField);
    JPanel row1 = new JPanel(new BorderLayout());
    row1.add(new JLabel(this.fileLabel), BorderLayout.WEST);
    row1.add(fileNamePanel);
    bottomPanel.add(row1);
    
    JPanel row2 = new JPanel(new BorderLayout());
    row2.add(new JLabel(this.filterLabel), BorderLayout.WEST);
    row2.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
    row2.add(fileFilterCombo);
    bottomPanel.add(row2);
    JPanel buttonPanel = new JPanel(new ButtonLayout());
    
    approveButton = new JButton(getApproveSelectionAction());
    approveButton.setText(getApproveButtonText(fc));
    approveButton.setToolTipText(getApproveButtonToolTipText(fc));
    approveButton.setMnemonic(getApproveButtonMnemonic(fc));
    buttonPanel.add(approveButton);
    buttonPanel.setBorder(BorderFactory.createEmptyBorder(8, 0, 0, 0));
    
    JButton cancelButton = new JButton(getCancelSelectionAction());
    cancelButton.setText(cancelButtonText);
    cancelButton.setToolTipText(cancelButtonToolTipText);
    cancelButton.setMnemonic(cancelButtonMnemonic);
    buttonPanel.add(cancelButton);
    bottomPanel.add(buttonPanel, BorderLayout.SOUTH);
    bottomPanel.setBorder(BorderFactory.createEmptyBorder(0, 8, 8, 8));
    fc.add(bottomPanel, BorderLayout.SOUTH);
    
    fc.add(getAccessoryPanel(), BorderLayout.EAST);
  }
  
  /**
   * Uninstalls the components added by 
   * {@link #installComponents(JFileChooser)}.
   * 
   * @param fc  the file chooser.
   */
  public void uninstallComponents(JFileChooser fc)
  {
    fc.remove(bottomPanel);
    bottomPanel = null;
    fc.remove(fileListPanel);
    fc.remove(fileTablePanel);
    fileTablePanel = null;
    fileListPanel = null;
    fc.remove(topPanel);
    topPanel = null;
    
    directoryModel = null;
    fileTextField = null;
    directoryComboBox = null;
  }
  
  /**
   * Returns the panel that contains the 'Open' (or 'Save') and 'Cancel' 
   * buttons.
   * 
   * @return The panel.
   */
  protected JPanel getButtonPanel()
  {
    return buttonPanel;    
  }
  
  /**
   * Creates and returns a new panel that will be used for the controls at
   * the bottom of the file chooser.
   * 
   * @return A new panel.
   */
  protected JPanel getBottomPanel()
  {
    if (bottomPanel == null)
      bottomPanel = new JPanel(new GridLayout(3, 2));
    return bottomPanel;
  }
  
  /**
   * Fetches localised strings for use by the labels and buttons on the
   * file chooser.
   * 
   * @param fc  the file chooser.
   */
  protected void installStrings(JFileChooser fc)
  { 
     super.installStrings(fc);
     look = "Look In: ";
     save = "Save In: ";
     if (fc.getDialogType() == JFileChooser.SAVE_DIALOG)
       directoryLabel = save;
     else
       directoryLabel = look;
     
     fileLabel = "File Name: ";
     filterLabel = "Files of Type: ";
     
     this.cancelButtonMnemonic = 0;
     this.cancelButtonText = "Cancel";
     this.cancelButtonToolTipText = "Abort file chooser dialog";
     
     this.directoryOpenButtonMnemonic = 0;
     this.directoryOpenButtonText = "Open";
     this.directoryOpenButtonToolTipText = "Open selected directory";
     
     this.helpButtonMnemonic = 0;
     this.helpButtonText = "Help";
     this.helpButtonToolTipText = "Filechooser help";
     
     this.openButtonMnemonic = 0;
     this.openButtonText = "Open";
     this.openButtonToolTipText = "Open selected file";
     
     this.saveButtonMnemonic = 0;
     this.saveButtonText = "Save";
     this.saveButtonToolTipText = "Save selected file";
     
     this.updateButtonMnemonic = 0;
     this.updateButtonText = "Update";
     this.updateButtonToolTipText = "Update directory listing";   
  }
  
  /**
   * Installs the listeners required.
   * 
   * @param fc  the file chooser.
   */
  protected void installListeners(JFileChooser fc)
  {
    directoryComboBox.setAction(new DirectoryComboBoxAction());
    fc.addPropertyChangeListener(filterModel);
    listSelList = createListSelectionListener(fc);
    doubleClickList = this.createDoubleClickListener(fc, fileList);
    singleClickList = new SingleClickListener(fileList);
    fileList.addListSelectionListener(listSelList);
    fileList.addMouseListener(doubleClickList);
    fileList.addMouseListener(singleClickList);
    super.installListeners(fc);
  }
  
  protected void uninstallListeners(JFileChooser fc) 
  {
    super.uninstallListeners(fc);
    fc.removePropertyChangeListener(filterModel);
    directoryComboBox.setAction(null);
    fileList.removeListSelectionListener(listSelList);
    fileList.removeMouseListener(doubleClickList);
    fileList.removeMouseListener(singleClickList);
    
    if (fileTable != null)
      fileTable.removeMouseListener(tableClickList);
  }
  
  protected ActionMap getActionMap()
  {
    if (actionMap == null)
      actionMap = createActionMap();
    return actionMap;
  }
  
  /**
   * Creates and returns an action map.
   * 
   * @return The action map.
   */
  protected ActionMap createActionMap()
  {
    ActionMap map = new ActionMap();
    map.put("approveSelection", getApproveSelectionAction());
    map.put("cancelSelection", getCancelSelectionAction());
    map.put("Go Up", getChangeToParentDirectoryAction());
    return map;
  }

  /**
   * Creates a panel containing a list of files.
   * 
   * @param fc  the file chooser.
   * 
   * @return A panel.
   */
  protected JPanel createList(JFileChooser fc)
  {
    if (fileList == null)
      {
        fileListPanel = new JPanel(new BorderLayout());
        fileList = new JList(getModel());
        scrollPane = new JScrollPane(fileList);
        scrollPane.setVerticalScrollBarPolicy
                                        (JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        fileList.setLayoutOrientation(JList.VERTICAL_WRAP);
        fileList.setCellRenderer(new FileRenderer());
      }
    else
      {
        fileList.setModel(getModel());
        fileListPanel.removeAll();
        scrollPane.getViewport().setView(fileList);
      }
    fileListPanel.add(scrollPane);

    return fileListPanel;
  }
  
  /**
   * Creates a panel containing a table within a scroll pane.
   * 
   * @param fc  the file chooser.
   * 
   * @return The details view.
   */
  protected JPanel createDetailsView(JFileChooser fc)
  {
    fileTablePanel = new JPanel(new BorderLayout());
    
    Object[] cols = new Object[] {"Name", "Size", "Modified"};
    Object[][] rows = new Object[fileList.getModel().getSize()][3];
    
    fileTable = new JTable(new DefaultTableModel(rows, cols));
    
    if (fc.isMultiSelectionEnabled())
      fileTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    else
      fileTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    
    fileTable.setShowGrid(false);
    fileTable.setColumnSelectionAllowed(false);
    fileTable.setDefaultRenderer(Object.class, new TableFileRenderer());

    tableClickList = new TableClickListener(fileTable, fc);
    fileTable.addMouseListener(tableClickList);
    
    return updateTable();  
  }
  
  /**
   * Sets the values in the table, and puts it in the panel.
   * 
   * @return the panel containing the table.
   */
  JPanel updateTable()
  {
    DefaultTableModel mod = (DefaultTableModel) fileTable.getModel();
    ListModel lm = fileList.getModel();
    DateFormat dt = DateFormat.getDateTimeInstance(DateFormat.SHORT,
                                                   DateFormat.SHORT);
    File curr = null;
    int size = lm.getSize();
    int rc = mod.getRowCount();

    // If there are not enough rows
    for (int x = rc; x < size; x++)
      mod.addRow(new Object[3]);

    for (int i = 0; i < size; i++)
      {
        curr = (File) lm.getElementAt(i);
        fileTable.setValueAt(curr.getName(), i, 0);
        fileTable.setValueAt(formatSize(curr.length()), i, 1);
        fileTable.setValueAt(dt.format(new Date(curr.lastModified())), i, 2);
      }

    // If there are too many rows
    while (rc > size)
      mod.removeRow(--rc);

    scrollPane.getViewport().setView(fileTable);
    scrollPane.setColumnHeaderView(fileTable.getTableHeader());

    fileTablePanel.removeAll();
    fileTablePanel.add(scrollPane);

    return fileTablePanel;
  }
  
  /**
   * Formats bytes into the appropriate size.
   * 
   * @param bytes -
   *          the number of bytes to convert
   * @return a string representation of the size
   */
  private String formatSize(long bytes)
  {
    NumberFormat nf = NumberFormat.getNumberInstance();
    long mb = (long) Math.pow(2, 20);
    long kb = (long) Math.pow(2, 10);
    long gb = (long) Math.pow(2, 30);
    double size = 0;
    String id = "";
    
    if ((bytes / gb) >= 1)
      {
        size = (double) bytes / (double) gb;
        id = "GB";
      }
    else if ((bytes / mb) >= 1)
      {
        size = (double) bytes / (double) mb;
        id = "MB";
      }
    else if ((bytes / kb) >= 1)
      {
        size = (double) bytes / (double) kb;
        id = "KB";
      }
    else
      {
        size = bytes;
        id = "Bytes";
      }
    
    return nf.format(size) + " " + id;
  }
  /**
   * Creates a listener that monitors selections in the directory/file list
   * and keeps the {@link JFileChooser} component up to date.
   * 
   * @param fc  the file chooser.
   * 
   * @return The listener.
   * 
   * @see #installListeners(JFileChooser)
   */
  public ListSelectionListener createListSelectionListener(JFileChooser fc)
  {
    return new MetalFileChooserSelectionListener();
  }
  
  /**
   * Returns the preferred size for the file chooser component.
   * 
   * @return The preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    Dimension tp = topPanel.getPreferredSize();
    Dimension bp = bottomPanel.getPreferredSize();
    Dimension fl = fileListPanel.getPreferredSize();
    return new Dimension(fl.width, tp.height + bp.height + fl.height);
  }

  /**
   * Returns the minimum size for the file chooser component.
   * 
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    Dimension tp = topPanel.getMinimumSize();
    Dimension bp = bottomPanel.getMinimumSize();
    Dimension fl = fileListPanel.getMinimumSize();
    return new Dimension(fl.width, tp.height + bp.height + fl.height);
  }
  
  /**
   * Returns the maximum size for the file chooser component.
   * 
   * @return The maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
  }
  
  /**
   * Creates a property change listener that monitors the {@link JFileChooser}
   * for property change events and updates the component display accordingly.
   * 
   * @param fc  the file chooser.
   * 
   * @return The property change listener.
   * 
   * @see #installListeners(JFileChooser)
   */
  public PropertyChangeListener createPropertyChangeListener(JFileChooser fc)
  {
    return new MetalFileChooserPropertyChangeListener();
  }

  /**
   * Creates and returns a new instance of {@link DirectoryComboBoxModel}.
   * 
   * @return A new instance of {@link DirectoryComboBoxModel}.
   */
  protected MetalFileChooserUI.DirectoryComboBoxModel 
      createDirectoryComboBoxModel(JFileChooser fc)
  {
    return new DirectoryComboBoxModel();
  }

  /**
   * Creates a new instance of the renderer used in the directory
   * combo box.
   * 
   * @param fc  the file chooser.
   * 
   * @return The renderer.
   */
  protected DirectoryComboBoxRenderer createDirectoryComboBoxRenderer(
          JFileChooser fc)
  {
    return new DirectoryComboBoxRenderer(fc);
  }

  /**
   * Creates and returns a new instance of {@link FilterComboBoxModel}.
   * 
   * @return A new instance of {@link FilterComboBoxModel}.
   */
  protected FilterComboBoxModel createFilterComboBoxModel()
  {
    return new FilterComboBoxModel();  
  }

  /**
   * Creates and returns a new instance of {@link FilterComboBoxRenderer}.
   * 
   * @return A new instance of {@link FilterComboBoxRenderer}.
   */
  protected MetalFileChooserUI.FilterComboBoxRenderer 
      createFilterComboBoxRenderer()
  {
    return new FilterComboBoxRenderer(); 
  }

  /**
   * Adds the control buttons ('up', 'home' etc.) to the panel.
   */
  protected void addControlButtons()
  {
    JButton upButton = new JButton(getChangeToParentDirectoryAction());
    upButton.setText(null);
    upButton.setIcon(this.upFolderIcon);
    upButton.setMargin(new Insets(0, 0, 0, 0));
    controls.add(upButton);
    
    JButton homeButton = new JButton(getGoHomeAction());
    homeButton.setText(null);
    homeButton.setIcon(this.homeFolderIcon);
    homeButton.setMargin(new Insets(0, 0, 0, 0));
    controls.add(homeButton);
    
    JButton newFolderButton = new JButton(getNewFolderAction());
    newFolderButton.setText(null);
    newFolderButton.setIcon(this.newFolderIcon);
    newFolderButton.setMargin(new Insets(0, 0, 0, 0));
    controls.add(newFolderButton);
    
    JToggleButton listButton = new JToggleButton(this.listViewIcon);
    listButton.setMargin(new Insets(0, 0, 0, 0));
    listButton.addActionListener(new ListViewActionListener());
    listButton.setSelected(true);
    listView = true; 
    controls.add(listButton);
    
    JToggleButton detailButton = new JToggleButton(this.detailsViewIcon);
    detailButton.setMargin(new Insets(0, 0, 0, 0));
    detailButton.addActionListener(new DetailViewActionListener());
    detailButton.setSelected(false);
    controls.add(detailButton);

    ButtonGroup buttonGroup = new ButtonGroup();
    buttonGroup.add(listButton);
    buttonGroup.add(detailButton);
  }
  
  /**
   * Removes all the buttons from the control panel.
   */
  protected void removeControlButtons()
  {
    controls.removeAll();
    controls.revalidate();
    controls.repaint();
  }
  
  /**
   * Updates the current directory.
   * 
   * @param the file chooser to update.
   */
  public void rescanCurrentDirectory(JFileChooser fc)
  {
    directoryModel.setSelectedItem(fc.getCurrentDirectory());
    getModel().validateFileCache();
    if (!listView)
        updateTable();
    else
      createList(fc);
  }
  
  /**
   * Returns the file name in the text field.
   * 
   * @return The file name.
   */
  public String getFileName()
  {
    String result = null;
    if (fileTextField != null) 
      result = fileTextField.getText();
    return result;
  }
  
  /**
   * Sets the file name in the text field.
   * 
   * @param filename  the file name.
   */
  public void setFileName(String filename)
  {
    fileTextField.setText(filename);
  }
  
  /**
   * DOCUMENT ME!!
   * 
   * @param e - DOCUMENT ME!
   */
  public void valueChanged(ListSelectionEvent e)
  {
    // FIXME: Not sure what we should be doing here, if anything.
  }
  
  /**
   * Returns the approve button.
   * 
   * @return The approve button.
   */
  protected JButton getApproveButton(JFileChooser fc)
  {
    return approveButton;
  }

  /**
   * A layout manager that is used to arrange the subcomponents of the
   * {@link JFileChooser}.
   */
  class VerticalMidLayout implements LayoutManager
  {
    /**
     * Performs the layout.
     * 
     * @param parent  the container.
     */
    public void layoutContainer(Container parent) 
    {
      int count = parent.getComponentCount();
      if (count > 0)
        {
          Insets insets = parent.getInsets();
          Component c = parent.getComponent(0);
          Dimension prefSize = c.getPreferredSize();
          int h = parent.getHeight() - insets.top - insets.bottom;
          int adj = Math.max(0, (h - prefSize.height) / 2);
          c.setBounds(insets.left, insets.top + adj, parent.getWidth() 
              - insets.left - insets.right, 
              (int) Math.min(prefSize.getHeight(), h));
        }
    }
    
    /**
     * Returns the minimum layout size.
     * 
     * @param parent  the container.
     * 
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container parent) 
    {
      return preferredLayoutSize(parent);
    }
    
    /**
     * Returns the preferred layout size.
     * 
     * @param parent  the container.
     * 
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container parent) 
    {
      if (parent.getComponentCount() > 0)
        {
          return parent.getComponent(0).getPreferredSize();
        }
      else return null;
    }
    
    /**
     * This layout manager does not need to track components, so this 
     * method does nothing.
     * 
     * @param name  the name the component is associated with.
     * @param component  the component.
     */
    public void addLayoutComponent(String name, Component component) 
    {
      // do nothing
    }
    
    /**
     * This layout manager does not need to track components, so this 
     * method does nothing.
     * 
     * @param component  the component.
     */
    public void removeLayoutComponent(Component component) {
      // do nothing
    }
  }

  /**
   * A layout manager that is used to arrange buttons for the
   * {@link JFileChooser}.
   */
  class ButtonLayout implements LayoutManager
  {
    static final int GAP = 4;
      
    /**
     * Performs the layout.
     * 
     * @param parent  the container.
     */
    public void layoutContainer(Container parent) 
    {
      int count = parent.getComponentCount();
      if (count > 0)
        {
          // first find the widest button
          int maxW = 0;
          for (int i = 0; i < count; i++)
            {
              Component c = parent.getComponent(i);
              Dimension prefSize = c.getPreferredSize();
              maxW = Math.max(prefSize.width, maxW);
            }
  
          // then position the buttons
          Insets insets = parent.getInsets();
          int availableH = parent.getHeight() - insets.top - insets.bottom;
          int currentX = parent.getWidth() - insets.right;
          for (int i = count - 1; i >= 0; i--)
            {
              Component c = parent.getComponent(i);
              Dimension prefSize = c.getPreferredSize();      
              int adj = Math.max(0, (availableH - prefSize.height) / 2);
              currentX = currentX - prefSize.width;
              c.setBounds(currentX, insets.top + adj, prefSize.width, 
                  (int) Math.min(prefSize.getHeight(), availableH));
              currentX = currentX - GAP;
            }
        }
    }
    
    /**
     * Returns the minimum layout size.
     * 
     * @param parent  the container.
     * 
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container parent) 
    {
      return preferredLayoutSize(parent);
    }
    
    /**
     * Returns the preferred layout size.
     * 
     * @param parent  the container.
     * 
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container parent) 
    {
      Insets insets = parent.getInsets();
      int maxW = 0;
      int maxH = 0;
      int count = parent.getComponentCount();
      if (count > 0) 
        {
          for (int i = 0; i < count; i++)
            {
              Component c = parent.getComponent(i);
              Dimension d = c.getPreferredSize();
              maxW = Math.max(d.width, maxW);
              maxH = Math.max(d.height, maxH);
            }
        }
      return new Dimension(maxW * count + GAP * (count - 1) + insets.left 
              + insets.right, maxH + insets.top + insets.bottom);
    }
    
    /**
     * This layout manager does not need to track components, so this 
     * method does nothing.
     * 
     * @param name  the name the component is associated with.
     * @param component  the component.
     */
    public void addLayoutComponent(String name, Component component) 
    {
      // do nothing
    }
    
    /**
     * This layout manager does not need to track components, so this 
     * method does nothing.
     * 
     * @param component  the component.
     */
    public void removeLayoutComponent(Component component) {
      // do nothing
    }
  }

}
