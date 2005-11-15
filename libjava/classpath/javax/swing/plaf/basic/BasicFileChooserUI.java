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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
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
import javax.swing.ButtonGroup;
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
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.FileChooserUI;


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
      // Nothing to do here.
    }

    /**
     * Sets the current selection and closes the dialog.
     * 
     * @param e  the action event.
     */
    public void actionPerformed(ActionEvent e)
    {
      Object obj = new String(parentPath + entry.getText());
      if (obj != null)
        {
          File f = filechooser.getFileSystemView().createFileObject(
                                                                    obj.toString());
          if (filechooser.isTraversable(f)
              && filechooser.isDirectorySelectionEnabled())
            filechooser.setCurrentDirectory(f);
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
    protected Hashtable iconCache = new Hashtable();

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
      return f.getName();
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
      // Nothing to do here.
    }

    /**
     * Cancels the selection and closes the dialog.
     *
     * @param e  the action event (ignored).
     */
    public void actionPerformed(ActionEvent e)
    {
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
      // Nothing to do here.
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
    /** A timer. */
    private Timer timer = null;

    /** DOCUMENT ME! */
    private Object lastSelected = null;

    /** DOCUMENT ME! */
    private JList list = null;

    /**
     * Creates a new DoubleClickListener object.
     *
     * @param list DOCUMENT ME!
     */
    public DoubleClickListener(JList list)
    {
      this.list = list;
      timer = new Timer(1000, null);
      timer.setRepeats(false);
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
      if (list.getSelectedValue() == null)
        return;
      FileSystemView fsv = filechooser.getFileSystemView();
      if (timer.isRunning()
          && list.getSelectedValue().toString().equals(lastSelected.toString()))
        {
          File f = fsv.createFileObject(lastSelected.toString());
          timer.stop();
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
      else
        {
          String path = list.getSelectedValue().toString();
          File f = fsv.createFileObject(path);
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
          parentPath = path.substring(0, path.lastIndexOf("/") + 1);
          entry.setText(path.substring(path.lastIndexOf("/") + 1));
          timer.restart();
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
      // Nothing to do here.
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
      // Nothing to do here.
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
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void valueChanged(ListSelectionEvent e)
    {
      Object f = filelist.getSelectedValue();
      if (f == null)
	return;
      File file = filechooser.getFileSystemView().createFileObject(f.toString());
      if (! filechooser.isTraversable(file))
	filechooser.setSelectedFile(file);
      else
	filechooser.setSelectedFile(null);
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
      // Nothing to do here.
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
  protected Icon computerIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        // FIXME: is this not implemented, or is the icon intentionally blank?
      }
    };

  /** An icon for the "details view" button. */
  protected Icon detailsViewIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	Color saved = g.getColor();
	g.translate(x, y);

	g.setColor(Color.GRAY);
	g.drawRect(1, 1, 15, 20);
	g.drawLine(17, 6, 23, 6);
	g.drawLine(17, 12, 23, 12);
	g.drawLine(17, 18, 23, 18);

	g.setColor(saved);
	g.translate(-x, -y);
      }
    };

  /** An icon representing a directory. */
  protected Icon directoryIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	Color saved = g.getColor();
	g.translate(x, y);

	Point ap = new Point(3, 7);
	Point bp = new Point(3, 21);
	Point cp = new Point(21, 21);
	Point dp = new Point(21, 12);
	Point ep = new Point(16, 12);
	Point fp = new Point(13, 7);

	Polygon dir = new Polygon(new int[] { ap.x, bp.x, cp.x, dp.x, ep.x, fp.x },
	                          new int[] { ap.y, bp.y, cp.y, dp.y, ep.y, fp.y },
	                          6);

	g.setColor(new Color(153, 204, 255));
	g.fillPolygon(dir);
	g.setColor(Color.BLACK);
	g.drawPolygon(dir);

	g.translate(-x, -y);
	g.setColor(saved);
      }
    };

  /** The localised Mnemonic for the open button. */
  protected int directoryOpenButtonMnemonic;

  /** The localised text for the open button. */
  protected String directoryOpenButtonText;

  /** The localised tool tip text for the open button. */
  protected String directoryOpenButtonToolTipText;

  /** An icon representing a file. */
  protected Icon fileIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	Color saved = g.getColor();
	g.translate(x, y);

	Point a = new Point(5, 4);
	Point b = new Point(5, 20);
	Point d = new Point(19, 20);
	Point e = new Point(19, 7);
	Point f = new Point(16, 4);

	Polygon p = new Polygon(new int[] { a.x, b.x, d.x, e.x, f.x, },
	                        new int[] { a.y, b.y, d.y, e.y, f.y }, 5);

	g.setColor(Color.WHITE);
	g.fillPolygon(p);
	g.setColor(Color.BLACK);
	g.drawPolygon(p);

	g.drawLine(16, 4, 14, 6);
	g.drawLine(14, 6, 19, 7);

	g.setColor(saved);
	g.translate(-x, -y);
      }
    };

  /** An icon representing a floppy drive. */
  protected Icon floppyDriveIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        // FIXME: is this not implemented, or is the icon intentionally blank?
      }
    };

  /** An icon representing a hard drive. */
  protected Icon hardDriveIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
        // FIXME: is this not implemented, or is the icon intentionally blank?
      }
    };

  /** The localised mnemonic for the "help" button. */
  protected int helpButtonMnemonic;

  /** The localised text for the "help" button. */
  protected String helpButtonText;

  /** The localised tool tip text for the help button. */
  protected String helpButtonToolTipText;

  /** An icon representing the user's home folder. */
  protected Icon homeFolderIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	Color saved = g.getColor();
	g.translate(x, y);

	Point a = new Point(12, 3);
	Point b = new Point(4, 10);
	Point d = new Point(20, 10);

	Polygon p = new Polygon(new int[] { a.x, b.x, d.x },
	                        new int[] { a.y, b.y, d.y }, 3);

	g.setColor(new Color(104, 51, 0));
	g.fillPolygon(p);
	g.setColor(Color.BLACK);
	g.drawPolygon(p);

	g.setColor(Color.WHITE);
	g.fillRect(8, 10, 8, 10);
	g.setColor(Color.BLACK);
	g.drawRect(8, 10, 8, 10);

	g.setColor(saved);
	g.translate(-x, -y);
      }
    };

  /** An icon for the "list view" button. */
  protected Icon listViewIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      // Not needed. Only simplifies things until we get real icons.
      private void paintPartial(Graphics g, int x, int y)
      {
	Color saved = g.getColor();
	g.translate(x, y);

	g.setColor(Color.GRAY);
	g.drawRect(1, 1, 7, 10);
	g.drawLine(8, 6, 11, 6);

	g.setColor(saved);
	g.translate(-x, -y);
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	Color saved = g.getColor();
	g.translate(x, y);

	paintPartial(g, 0, 0);
	paintPartial(g, 12, 0);
	paintPartial(g, 0, 12);
	paintPartial(g, 12, 12);

	g.setColor(saved);
	g.translate(-x, -y);
      }
    };

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
  protected Icon upFolderIcon = new Icon()
    {
      public int getIconHeight()
      {
	return ICON_SIZE;
      }

      public int getIconWidth()
      {
	return ICON_SIZE;
      }

      public void paintIcon(Component comp, Graphics g, int x, int y)
      {
	Color saved = g.getColor();
	g.translate(x, y);

	Point a = new Point(3, 7);
	Point b = new Point(3, 21);
	Point c = new Point(21, 21);
	Point d = new Point(21, 12);
	Point e = new Point(16, 12);
	Point f = new Point(13, 7);

	Polygon dir = new Polygon(new int[] { a.x, b.x, c.x, d.x, e.x, f.x },
	                          new int[] { a.y, b.y, c.y, d.y, e.y, f.y }, 6);

	g.setColor(new Color(153, 204, 255));
	g.fillPolygon(dir);
	g.setColor(Color.BLACK);
	g.drawPolygon(dir);

	a = new Point(12, 15);
	b = new Point(9, 18);
	c = new Point(15, 18);

	Polygon arrow = new Polygon(new int[] { a.x, b.x, c.x },
	                            new int[] { a.y, b.y, c.y }, 3);

	g.fillPolygon(arrow);

	g.drawLine(12, 15, 12, 22);

	g.translate(-x, -y);
	g.setColor(saved);
      }
    };

  // -- begin private, but package local since used in inner classes --

  /** The file chooser component represented by this UI delegate. */
  JFileChooser filechooser;

  /** The file list. */
  JList filelist;

  /** The combo box used to display/select file filters. */
  JComboBox filters;

  /** The model for the directory list. */
  BasicDirectoryModel model;

  /** The file filter for all files. */
  FileFilter acceptAll = new AcceptAllFileFilter();

  /** The default file view. */
  FileView fv = new BasicFileView();

  /** The icon size. */
  static final int ICON_SIZE = 24;

  /** A combo box for display/selection of parent directories. */
  JComboBox parents;

  /** The current file name. */
  String filename;

  /** The accept (open/save) button. */
  JButton accept;

  /** The cancel button. */
  JButton cancel;

  /** The button to move up to the parent directory. */
  JButton upFolderButton;

  /** The button to create a new directory. */
  JButton newFolderButton;

  /** The button to move to the user's home directory. */
  JButton homeFolderButton;

  /** An optional accessory panel. */
  JPanel accessoryPanel;

  /** A property change listener. */
  PropertyChangeListener propertyChangeListener;

  /** The text describing the filter for "all files". */
  String acceptAllFileFilterText;

  /** The text describing a directory type. */
  String dirDescText;

  /** The text describing a file type. */
  String fileDescText;

  /** Is a directory selected? */
  boolean dirSelected = false;

  /** The current directory. */
  File currDir = null;

  // FIXME: describe what is contained in the bottom panel
  /** The bottom panel. */
  JPanel bottomPanel;
  
  /** The close panel. */
  JPanel closePanel;

  /** Text box that displays file name */
  JTextField entry;
    
  /** Current parent path */
  String parentPath;
  
  // -- end private --
  private class ListLabelRenderer extends JLabel implements ListCellRenderer
  {
    /** DOCUMENT ME! */
    final Color selected = new Color(153, 204, 255);

    /**
     * Creates a new ListLabelRenderer object.
     */
    public ListLabelRenderer()
    {
      super();
      setOpaque(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param list DOCUMENT ME!
     * @param value DOCUMENT ME!
     * @param index DOCUMENT ME!
     * @param isSelected DOCUMENT ME!
     * @param cellHasFocus DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Component getListCellRendererComponent(JList list, Object value,
                                                  int index,
                                                  boolean isSelected,
                                                  boolean cellHasFocus)
    {
      setHorizontalAlignment(SwingConstants.LEFT);
      File file = (File) value;
      setText(filechooser.getName(file));
      setIcon(filechooser.getIcon(file));
      setBackground(isSelected ? selected : Color.WHITE);
      setForeground(Color.BLACK);

      return this;
    }
  }

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
    this.filechooser = b;
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
        fc.resetChoosableFileFilters();
        createModel();
        clearIconCache();
        installDefaults(fc);
        installComponents(fc);
        installListeners(fc);
        
        Object path = filechooser.getCurrentDirectory();
        if (path != null)
          parentPath = path.toString().substring(path.toString().lastIndexOf("/"));
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

    if (parents.getItemCount() > 0)
      parents.removeAllItems();
    for (int i = parentFiles.size() - 1; i >= 0; i--)
      parents.addItem(parentFiles.get(i));
    parents.setSelectedIndex(parentFiles.size() - 1);
    parents.revalidate();
    parents.repaint();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  private ItemListener createBoxListener()
  {
    return new ItemListener()
      {
	public void itemStateChanged(ItemEvent e)
	{
	  if (parents.getItemCount() - 1 == parents.getSelectedIndex())
	    return;
	  StringBuffer dir = new StringBuffer();
	  for (int i = 0; i <= parents.getSelectedIndex(); i++)
	    {
	      dir.append(parents.getItemAt(i));
	      dir.append(File.separatorChar);
	    }
	  filechooser.setCurrentDirectory(filechooser.getFileSystemView()
	                                             .createFileObject(dir
	                                                               .toString()));
	}
      };
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  private ItemListener createFilterListener()
  {
    return new ItemListener()
      {
	public void itemStateChanged(ItemEvent e)
	{
	  int index = filters.getSelectedIndex();
	  if (index == -1)
	    return;
	  filechooser.setFileFilter(filechooser.getChoosableFileFilters()[index]);
	}
      };
  }

  void filterEntries()
  {
    FileFilter[] list = filechooser.getChoosableFileFilters();
    if (filters.getItemCount() > 0)
      filters.removeAllItems();

    int index = -1;
    String selected = filechooser.getFileFilter().getDescription();
    for (int i = 0; i < list.length; i++)
      {
	if (selected.equals(list[i].getDescription()))
	  index = i;
	filters.addItem(list[i].getDescription());
      }
    filters.setSelectedIndex(index);
    filters.revalidate();
    filters.repaint();
  }

  /**
   * Creates and install the subcomponents for the file chooser.
   *
   * @param fc  the file chooser.
   */
  public void installComponents(JFileChooser fc)
  {
    JLabel look = new JLabel("Look In:");

    parents = new JComboBox();
    parents.setRenderer(new BasicComboBoxRenderer());
    boxEntries();
    look.setLabelFor(parents);
    JPanel parentsPanel = new JPanel();
    parentsPanel.add(look);
    parentsPanel.add(parents);
    JPanel buttonPanel = new JPanel();

    upFolderButton = new JButton();
    upFolderButton.setIcon(upFolderIcon);
    buttonPanel.add(upFolderButton);

    homeFolderButton = new JButton();
    homeFolderButton = new JButton(homeFolderIcon);
    buttonPanel.add(homeFolderButton);

    newFolderButton = new JButton();
    newFolderButton.setIcon(newFolderIcon);
    buttonPanel.add(newFolderButton);

    ButtonGroup toggles = new ButtonGroup();
    JToggleButton listViewButton = new JToggleButton();
    listViewButton.setIcon(listViewIcon);
    toggles.add(listViewButton);
    buttonPanel.add(listViewButton);

    JToggleButton detailsViewButton = new JToggleButton();
    detailsViewButton.setIcon(detailsViewIcon);
    toggles.add(detailsViewButton);
    buttonPanel.add(detailsViewButton);

    JPanel topPanel = new JPanel();
    parentsPanel.add(buttonPanel);
    topPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 0, 0));
    topPanel.add(parentsPanel);

    accessoryPanel = new JPanel();
    if (filechooser.getAccessory() != null)
      accessoryPanel.add(filechooser.getAccessory(), BorderLayout.CENTER);

    filelist = new JList(model);
    filelist.setVisibleRowCount(6);
    JScrollPane scrollp = new JScrollPane(filelist);
    scrollp.setPreferredSize(new Dimension(400, 175));
    filelist.setBackground(Color.WHITE);

    filelist.setLayoutOrientation(JList.VERTICAL_WRAP);
    filelist.setCellRenderer(new ListLabelRenderer());

    GridBagConstraints c = new GridBagConstraints();
    c.gridx = 0;
    c.gridy = 0;
    c.fill = GridBagConstraints.BOTH;
    c.weightx = 1;
    c.weighty = 1;

    JPanel centrePanel = new JPanel();
    centrePanel.setLayout(new GridBagLayout());
    centrePanel.add(scrollp, c);

    c.gridx = 1;
    centrePanel.add(accessoryPanel, c);

    JLabel fileNameLabel = new JLabel("File Name:");
    JLabel fileTypesLabel = new JLabel("Files of Type:");

    entry = new JTextField();
    filters = new JComboBox();
    filterEntries();

    fileNameLabel.setLabelFor(entry);
    fileNameLabel.setHorizontalTextPosition(SwingConstants.LEFT);
    fileTypesLabel.setLabelFor(filters);
    fileTypesLabel.setHorizontalTextPosition(SwingConstants.LEFT);

    closePanel = new JPanel();
    accept = getApproveButton(filechooser);
    cancel = new JButton(cancelButtonText);
    cancel.setMnemonic(cancelButtonMnemonic);
    cancel.setToolTipText(cancelButtonToolTipText);
    closePanel.add(accept);
    closePanel.add(cancel);

    c.anchor = GridBagConstraints.WEST;
    c.weighty = 0;
    c.weightx = 0;
    c.gridx = 0;

    bottomPanel = new JPanel();
    bottomPanel.setLayout(new GridBagLayout());
    bottomPanel.add(fileNameLabel, c);

    c.gridy = 1;
    bottomPanel.add(fileTypesLabel, c);
    c.gridx = 1;
    c.gridy = 0;
    c.weightx = 1;
    c.weighty = 1;
    bottomPanel.add(entry, c);

    c.gridy = 1;
    bottomPanel.add(filters, c);

    c.fill = GridBagConstraints.NONE;
    c.gridy = 2;
    c.anchor = GridBagConstraints.EAST;
    bottomPanel.add(closePanel, c);

    filechooser.setLayout(new BorderLayout());
    filechooser.add(topPanel, BorderLayout.NORTH);
    filechooser.add(centrePanel, BorderLayout.CENTER);
    filechooser.add(bottomPanel, BorderLayout.SOUTH);
  }

  /**
   * Uninstalls the components from the file chooser.
   *
   * @param fc  the file chooser.
   */
  public void uninstallComponents(JFileChooser fc)
  {
    parents = null;

    accept = null;
    cancel = null;
    upFolderButton = null;
    homeFolderButton = null;
    newFolderButton = null;

    filelist = null;
  }

  /**
   * Installs the listeners required by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void installListeners(JFileChooser fc)
  {
    propertyChangeListener = createPropertyChangeListener(filechooser);
    filechooser.addPropertyChangeListener(propertyChangeListener);

    //parents.addItemListener(createBoxListener());
    accept.addActionListener(getApproveSelectionAction());
    cancel.addActionListener(getCancelSelectionAction());
    upFolderButton.addActionListener(getChangeToParentDirectoryAction());
    homeFolderButton.addActionListener(getGoHomeAction());
    newFolderButton.addActionListener(getNewFolderAction());
    filters.addItemListener(createFilterListener());

    filelist.addMouseListener(createDoubleClickListener(filechooser, filelist));
    filelist.addListSelectionListener(createListSelectionListener(filechooser));
  }

  /**
   * Uninstalls the listeners previously installed by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void uninstallListeners(JFileChooser fc)
  {
    filechooser.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;
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
   * Installs the icons for this UI delegate (NOT YET IMPLEMENTED).
   *
   * @param fc  the file chooser.
   */
  protected void installIcons(JFileChooser fc)
  {
    // FIXME: Implement.
  }

  /**
   * Uninstalls the icons previously added by this UI delegate (NOT YET
   * IMPLEMENTED).
   *
   * @param fc  the file chooser.
   */
  protected void uninstallIcons(JFileChooser fc)
  {
    // FIXME: Implement.
  }

  /**
   * Installs the strings used by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void installStrings(JFileChooser fc)
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    acceptAllFileFilterText = defaults.getString("FileChooser.acceptAllFileFilterText");
    cancelButtonMnemonic = defaults.getInt("FileChooser.cancelButtonMnemonic");
    cancelButtonText = defaults.getString("FileChooser.cancelButtonText");
    cancelButtonToolTipText = defaults.getString("FileChooser.cancelButtonToolTipText");

    dirDescText = defaults.getString("FileChooser.directoryDescriptionText");
    fileDescText = defaults.getString("FileChooser.fileDescriptionText");

    helpButtonMnemonic = defaults.getInt("FileChooser.helpButtonMnemonic");
    helpButtonText = defaults.getString("FileChooser.helpButtonText");
    helpButtonToolTipText = defaults.getString("FileChooser.helpButtonToolTipText");

    openButtonMnemonic = defaults.getInt("FileChooser.openButtonMnemonic");
    openButtonText = defaults.getString("FileChooser.openButtonText");
    openButtonToolTipText = defaults.getString("FileChooser.openButtonToolTipText");

    saveButtonMnemonic = defaults.getInt("FileChooser.saveButtonMnemonic");
    saveButtonText = defaults.getString("FileChooser.saveButtonText");
    saveButtonToolTipText = defaults.getString("FileChooser.saveButtonToolTipText");
  }

  /**
   * Uninstalls the strings previously added by this UI delegate.
   *
   * @param fc  the file chooser.
   */
  protected void uninstallStrings(JFileChooser fc)
  {
    acceptAllFileFilterText = null;
    cancelButtonMnemonic = 0;
    cancelButtonText = null;
    cancelButtonToolTipText = null;

    dirDescText = null;
    fileDescText = null;

    helpButtonMnemonic = 0;
    helpButtonText = null;
    helpButtonToolTipText = null;

    openButtonMnemonic = 0;
    openButtonText = null;
    openButtonToolTipText = null;

    saveButtonMnemonic = 0;
    saveButtonText = null;
    saveButtonToolTipText = null;
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
    return new PropertyChangeListener()
    {
      public void propertyChange(PropertyChangeEvent e)
      {
        // FIXME: Multiple file selection waiting on JList multiple selection
        // bug.
        if (e.getPropertyName().equals(
                                       JFileChooser.SELECTED_FILE_CHANGED_PROPERTY))
          {
            if (filechooser.getSelectedFile() == null)
              setFileName(null);
            else
              setFileName(filechooser.getSelectedFile().toString());
            int index = -1;
            File file = filechooser.getSelectedFile();
            for (index = 0; index < model.getSize(); index++)
              if (((File) model.getElementAt(index)).equals(file))
                break;
            if (index == -1)
              return;
            filelist.setSelectedIndex(index);
            filelist.ensureIndexIsVisible(index);
            filelist.revalidate();
            filelist.repaint();
          }
        else if (e.getPropertyName().equals(
                                            JFileChooser.DIRECTORY_CHANGED_PROPERTY))
          {
            filelist.clearSelection();
            filelist.revalidate();
            filelist.repaint();
            setDirectorySelected(false);
            setDirectory(filechooser.getCurrentDirectory());
            boxEntries();
          }
        else if (e.getPropertyName().equals(
                                            JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY)
                 || e.getPropertyName().equals(
                                               JFileChooser.FILE_FILTER_CHANGED_PROPERTY))
          filterEntries();
        else if (e.getPropertyName().equals(
                                            JFileChooser.DIALOG_TYPE_CHANGED_PROPERTY)
                 || e.getPropertyName().equals(
                                               JFileChooser.DIALOG_TITLE_CHANGED_PROPERTY))
          {
            Window owner = SwingUtilities.windowForComponent(filechooser);
            if (owner instanceof JDialog)
              ((JDialog) owner).setTitle(getDialogTitle(filechooser));
            accept.setText(getApproveButtonText(filechooser));
            accept.setToolTipText(getApproveButtonToolTipText(filechooser));
            accept.setMnemonic(getApproveButtonMnemonic(filechooser));
          }
        else if (e.getPropertyName().equals(
                                            JFileChooser.APPROVE_BUTTON_TEXT_CHANGED_PROPERTY))
          accept.setText(getApproveButtonText(filechooser));
        else if (e.getPropertyName().equals(
                                            JFileChooser.APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY))
          accept.setToolTipText(getApproveButtonToolTipText(filechooser));
        else if (e.getPropertyName().equals(
                                            JFileChooser.APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY))
          accept.setMnemonic(getApproveButtonMnemonic(filechooser));
        else if (e.getPropertyName().equals(
                                            JFileChooser.CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY))
          {
            if (filechooser.getControlButtonsAreShown())
              {
                GridBagConstraints c = new GridBagConstraints();
                c.gridy = 1;
                bottomPanel.add(filters, c);

                c.fill = GridBagConstraints.BOTH;
                c.gridy = 2;
                c.anchor = GridBagConstraints.EAST;
                bottomPanel.add(closePanel, c);
                bottomPanel.revalidate();
                bottomPanel.repaint();
                bottomPanel.doLayout();
              }
            else
              bottomPanel.remove(closePanel);
          }
        else if (e.getPropertyName().equals(
                                            JFileChooser.ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY))
          {
            if (filechooser.isAcceptAllFileFilterUsed())
              filechooser.addChoosableFileFilter(getAcceptAllFileFilter(filechooser));
            else
              filechooser.removeChoosableFileFilter(getAcceptAllFileFilter(filechooser));
          }
        else if (e.getPropertyName().equals(
                                            JFileChooser.ACCESSORY_CHANGED_PROPERTY))
          {
            JComponent old = (JComponent) e.getOldValue();
            if (old != null)
              getAccessoryPanel().remove(old);
            JComponent newval = (JComponent) e.getNewValue();
            if (newval != null)
              getAccessoryPanel().add(newval);
          }
        if (e.getPropertyName().equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY)
            || e.getPropertyName().equals(
                                          JFileChooser.FILE_FILTER_CHANGED_PROPERTY)
            || e.getPropertyName().equals(
                                          JFileChooser.FILE_HIDING_CHANGED_PROPERTY))
          rescanCurrentDirectory(filechooser);

        filechooser.revalidate();
        filechooser.repaint();
      }
    };
  }

  /**
   * Returns the current file name.
   * 
   * @return The current file name.
   */
  public String getFileName()
  {
    return filename;
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
    this.filename = filename;
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
    filelist.revalidate();
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
   * Creates and returns an approve (open or save) button for the dialog.
   *
   * @param fc  the file chooser.
   *
   * @return The button.
   */
  public JButton getApproveButton(JFileChooser fc)
  {
    accept = new JButton(getApproveButtonText(fc));
    accept.setMnemonic(getApproveButtonMnemonic(fc));
    accept.setToolTipText(getApproveButtonToolTipText(fc));
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
   * Returns the file view for the file chooser.  This returns either the
   * file view that has been explicitly set for the {@link JFileChooser}, or
   * a default file view.
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
    String ret = fc.getDialogTitle();
    if (ret != null)
      return ret;
    switch (fc.getDialogType())
      {
      case JFileChooser.OPEN_DIALOG:
	ret = openButtonText;
	break;
      case JFileChooser.SAVE_DIALOG:
	ret = saveButtonText;
	break;
      default:
	ret = fc.getApproveButtonText();
	break;
      }
    if (ret == null)
      ret = openButtonText;
    return ret;
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
    if (fc.getApproveButtonText() != null)
      return fc.getApproveButtonText();
    else if (fc.getDialogType() == JFileChooser.SAVE_DIALOG)
      return saveButtonText;
    else
      return openButtonText;
  }

  /**
   * Creates and returns a new action that will be used with the "new folder" 
   * button.
   *
   * @return A new instance of {@link GoHomeAction}.
   */
  public Action getNewFolderAction()
  {
    return new NewFolderAction();
  }

  /**
   * Creates and returns a new action that will be used with the "home folder" 
   * button.
   *
   * @return A new instance of {@link GoHomeAction}.
   */
  public Action getGoHomeAction()
  {
    return new GoHomeAction();
  }

  /**
   * Creates and returns a new action that will be used with the "up folder" 
   * button.
   *
   * @return A new instance of {@link ChangeToParentDirectoryAction}.
   */
  public Action getChangeToParentDirectoryAction()
  {
    return new ChangeToParentDirectoryAction();
  }

  /**
   * Creates and returns a new action that will be used with the "approve" 
   * button.
   *
   * @return A new instance of {@link ApproveSelectionAction}.
   */
  public Action getApproveSelectionAction()
  {
    return new ApproveSelectionAction();
  }

  /**
   * Creates and returns a new action that will be used with the "cancel" 
   * button.
   *
   * @return A new instance of {@link CancelSelectionAction}.
   */
  public Action getCancelSelectionAction()
  {
    return new CancelSelectionAction();
  }

  /**
   * Creates and returns a new instance of {@link UpdateAction}.
   *
   * @return An action. 
   */
  public Action getUpdateAction()
  {
    return new UpdateAction();
  }
}
