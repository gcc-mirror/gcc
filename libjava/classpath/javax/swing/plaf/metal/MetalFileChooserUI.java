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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.AbstractListModel;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.UIManager;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicFileChooserUI;


/**
 * A UI delegate for the {@link JFileChooser} component.  This class is only
 * partially implemented and is not usable yet.
 */
public class MetalFileChooserUI extends BasicFileChooserUI
{
  /** 
   * A combo box model containing the selected directory and all its parent
   * directories.
   */
  protected class DirectoryComboBoxModel extends AbstractListModel
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
  protected class DirectoryComboBoxAction extends AbstractAction
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
   * A renderer for the files and directories in the file chooser.
   */
  protected class FileRenderer extends DefaultListCellRenderer
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
      setText(v.getName(f));
      setIcon(v.getIcon(f));
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
    private int selectedIndex;
    
    /**
     * Creates a new model.
     */
    protected FilterComboBoxModel()
    {
      filters = new FileFilter[1];
      filters[0] = getAcceptAllFileFilter(getFileChooser());
      selectedIndex = 0;
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
          selectedIndex = -1;
          FileFilter selected = (FileFilter) e.getNewValue();
          for (int i = 0; i < filters.length; i++)
            if (filters[i].equals(selected))
              selectedIndex = i;
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
     * @param filter  the filter.
     */
    public void setSelectedItem(Object filter)
    {
      // change the filter in the file chooser and let the property change
      // event trigger the change to the selected item
      getFileChooser().setFileFilter((FileFilter) filter);
    }
    
    /**
     * Returns the selected file filter.
     * 
     * @return The selected file filter.
     */
    public Object getSelectedItem()
    {
      if (selectedIndex >= 0) 
        return filters[selectedIndex];
      return null;
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
  public class FilterComboBoxRenderer extends DefaultListCellRenderer
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
     * @return A component.
     */
    public Component getListCellRendererComponent(JList list, Object value,
        int index, boolean isSelected, boolean cellHasFocus)
    {
      FileFilter filter = (FileFilter) value;
      return super.getListCellRendererComponent(list, filter.getDescription(),
              index, isSelected, cellHasFocus);
    }
  }

  /** The model for the directory combo box. */
  DirectoryComboBoxModel directoryModel;
  
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

}
