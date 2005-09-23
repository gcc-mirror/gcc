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
import java.awt.event.ActionListener;
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
import javax.swing.BoxLayout;
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
 * DOCUMENT ME!
 */
public class BasicFileChooserUI extends FileChooserUI
{
  /**
   * DOCUMENT ME!
   */
  protected class AcceptAllFileFilter extends FileFilter
  {
    public AcceptAllFileFilter()
    {
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
      return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getDescription()
    {
      return acceptAllFileFilterText;
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class ApproveSelectionAction extends AbstractAction
  {
    /**
     * Creates a new ApproveSelectionAction object.
     */
    protected ApproveSelectionAction()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e)
    {
      Object obj = filelist.getSelectedValue();
      if (obj != null)
        {
	  File f = filechooser.getFileSystemView().createFileObject(obj
	                                                            .toString());
	  if (filechooser.isTraversable(f) && 
              filechooser.getFileSelectionMode() == JFileChooser.FILES_ONLY)
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
   * DOCUMENT ME!
   */
  protected class BasicFileView extends FileView
  {
    /** DOCUMENT ME! */
    protected Hashtable iconCache = new Hashtable();

    public BasicFileView()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param f DOCUMENT ME!
     * @param i DOCUMENT ME!
     */
    public void cacheIcon(File f, Icon i)
    {
      iconCache.put(f, i);
    }

    /**
     * DOCUMENT ME!
     */
    public void clearIconCache()
    {
      iconCache.clear();
    }

    /**
     * DOCUMENT ME!
     *
     * @param f DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Icon getCachedIcon(File f)
    {
      return (Icon) iconCache.get(f);
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
      return getName(f);
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
     * DOCUMENT ME!
     *
     * @param f DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getName(File f)
    {
      return f.getName();
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
      if (filechooser.isTraversable(f))
	return dirDescText;
      else
	return fileDescText;
    }

    /**
     * DOCUMENT ME!
     *
     * @param f DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Boolean isHidden(File f)
    {
      return new Boolean(filechooser.getFileSystemView().isHiddenFile(f));
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class CancelSelectionAction extends AbstractAction
  {
    /**
     * Creates a new CancelSelectionAction object.
     */
    protected CancelSelectionAction()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e)
    {
      filechooser.cancelSelection();
      closeDialog();
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class ChangeToParentDirectoryAction extends AbstractAction
  {
    /**
     * Creates a new ChangeToParentDirectoryAction object.
     */
    protected ChangeToParentDirectoryAction()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e)
    {
      filechooser.changeToParentDirectory();
      filechooser.revalidate();
      filechooser.repaint();
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class DoubleClickListener extends MouseAdapter
  {
    /** DOCUMENT ME! */
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
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
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
	  File f = fsv.createFileObject(list.getSelectedValue().toString());
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
	  lastSelected = list.getSelectedValue().toString();
	  timer.restart();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent e)
    {
      // FIXME: Implement
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class GoHomeAction extends AbstractAction
  {
    /**
     * Creates a new GoHomeAction object.
     */
    protected GoHomeAction()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
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
   * DOCUMENT ME!
   */
  protected class NewFolderAction extends AbstractAction
  {
    /**
     * Creates a new NewFolderAction object.
     */
    protected NewFolderAction()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
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
   * DOCUMENT ME!
   */
  protected class SelectionListener implements ListSelectionListener
  {
    /**
     * Creates a new SelectionListener object.
     */
    protected SelectionListener()
    {
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
   */
  protected class UpdateAction extends AbstractAction
  {
    /**
     * Creates a new UpdateAction object.
     */
    protected UpdateAction()
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e)
    {
    }
  }

  /** DOCUMENT ME! */
  protected int cancelButtonMnemonic;

  /** DOCUMENT ME! */
  protected String cancelButtonText;

  /** DOCUMENT ME! */
  protected String cancelButtonToolTipText;

  /** DOCUMENT ME! */
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
      }
    };

  /** DOCUMENT ME! */
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

  /** DOCUMENT ME! */
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

  /** DOCUMENT ME! */
  protected int directoryOpenButtonMnemonic;

  /** DOCUMENT ME! */
  protected String directoryOpenButtonText;

  /** DOCUMENT ME! */
  protected String directoryOpenButtonToolTipText;

  /** DOCUMENT ME! */
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

  /** DOCUMENT ME! */
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
      }
    };

  /** DOCUMENT ME! */
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
      }
    };

  /** DOCUMENT ME! */
  protected int helpButtonMnemonic;

  /** DOCUMENT ME! */
  protected String helpButtonText;

  /** DOCUMENT ME! */
  protected String helpButtonToolTipText;

  /** DOCUMENT ME! */
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

  /** DOCUMENT ME! */
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

  /** DOCUMENT ME! */
  protected Icon newFolderIcon = directoryIcon;

  /** DOCUMENT ME! */
  protected int openButtonMnemonic;

  /** DOCUMENT ME! */
  protected String openButtonText;

  /** DOCUMENT ME! */
  protected String openButtonToolTipText;

  /** DOCUMENT ME! */
  protected int saveButtonMnemonic;

  /** DOCUMENT ME! */
  protected String saveButtonText;

  /** DOCUMENT ME! */
  protected String saveButtonToolTipText;

  /** DOCUMENT ME! */
  protected int updateButtonMnemonic;

  /** DOCUMENT ME! */
  protected String updateButtonText;

  /** DOCUMENT ME! */
  protected String updateButtonToolTipText;

  /** DOCUMENT ME! */
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

  JFileChooser filechooser;

  /** DOCUMENT ME! */
  JList filelist;

  /** DOCUMENT ME! */
  JComboBox filters;

  /** DOCUMENT ME! */
  BasicDirectoryModel model;

  /** DOCUMENT ME! */
  FileFilter acceptAll = new AcceptAllFileFilter();

  /** DOCUMENT ME! */
  FileView fv = new BasicFileView();

  /** DOCUMENT ME! */
  static final int ICON_SIZE = 24;

  /** DOCUMENT ME! */
  JComboBox parents;

  /** DOCUMENT ME! */
  String filename;

  /** DOCUMENT ME! */
  JButton accept;

  /** DOCUMENT ME! */
  JButton cancel;

  /** DOCUMENT ME! */
  JButton upFolderButton;

  /** DOCUMENT ME! */
  JButton newFolderButton;

  /** DOCUMENT ME! */
  JButton homeFolderButton;

  /** DOCUMENT ME! */
  JPanel accessoryPanel;

  /** DOCUMENT ME! */
  PropertyChangeListener propertyChangeListener;

  /** DOCUMENT ME! */
  String acceptAllFileFilterText;

  /** DOCUMENT ME! */
  String dirDescText;

  /** DOCUMENT ME! */
  String fileDescText;

  /** DOCUMENT ME! */
  boolean dirSelected = false;

  /** DOCUMENT ME! */
  File currDir = null;

  JPanel bottomPanel;

  /** DOCUMENT ME! */
  JPanel closePanel;

  // -- end private --
  private class ListLabelRenderer
    extends JLabel
    implements ListCellRenderer
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
   * DOCUMENT ME!
   */
  public class CBLabelRenderer extends JLabel implements ListCellRenderer
  {
    /**
     * Creates a new CBLabelRenderer object.
     */
    public CBLabelRenderer()
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
      setIcon(directoryIcon);
      setText(value.toString());
      setForeground(Color.BLACK);
      setBackground(Color.WHITE);

      return this;
    }
  }

  void closeDialog()
  {
    Window owner = SwingUtilities.windowForComponent(filechooser);
    if (owner instanceof JDialog)
      ((JDialog) owner).dispose();
  }

  /**
   * Creates a new BasicFileChooserUI object.
   *
   * @param b DOCUMENT ME!
   */
  public BasicFileChooserUI(JFileChooser b)
  {
    this.filechooser = b;
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicFileChooserUI((JFileChooser) c);
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
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
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   */
  public void installComponents(JFileChooser fc)
  {
    JLabel look = new JLabel("Look In:");

    parents = new JComboBox();
    parents.setRenderer(new CBLabelRenderer());
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
    topPanel.setLayout(new java.awt.FlowLayout());
    topPanel.add(parentsPanel);
    topPanel.add(buttonPanel);

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

    JTextField entry = new JTextField();
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
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   */
  protected void uninstallListeners(JFileChooser fc)
  {
    filechooser.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   */
  protected void installDefaults(JFileChooser fc)
  {
    installIcons(fc);
    installStrings(fc);
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   */
  protected void uninstallDefaults(JFileChooser fc)
  {
    uninstallStrings(fc);
    uninstallIcons(fc);
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   */
  protected void installIcons(JFileChooser fc)
  {
    // FIXME: Implement.
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   */
  protected void uninstallIcons(JFileChooser fc)
  {
    // FIXME: Implement.
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
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
   * DOCUMENT ME!
   */
  protected void createModel()
  {
    model = new BasicDirectoryModel(filechooser);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public BasicDirectoryModel getModel()
  {
    return model;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public PropertyChangeListener createPropertyChangeListener(JFileChooser fc)
  {
    return new PropertyChangeListener()
      {
	public void propertyChange(PropertyChangeEvent e)
	{
	  // FIXME: Multiple file selection waiting on JList multiple selection bug.
	  if (e.getPropertyName().equals(JFileChooser.SELECTED_FILE_CHANGED_PROPERTY))
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
	  else if (e.getPropertyName().equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY))
	    {
	      filelist.clearSelection();
	      filelist.revalidate();
	      filelist.repaint();
	      setDirectorySelected(false);
	      setDirectory(filechooser.getCurrentDirectory());
	      boxEntries();
	    }
	  else if (e.getPropertyName().equals(JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY)
	           || e.getPropertyName().equals(JFileChooser.FILE_FILTER_CHANGED_PROPERTY))
	    filterEntries();
	  else if (e.getPropertyName().equals(JFileChooser.DIALOG_TYPE_CHANGED_PROPERTY)
	           || e.getPropertyName().equals(JFileChooser.DIALOG_TITLE_CHANGED_PROPERTY))
	    {
	      Window owner = SwingUtilities.windowForComponent(filechooser);
	      if (owner instanceof JDialog)
		((JDialog) owner).setTitle(getDialogTitle(filechooser));
	      accept.setText(getApproveButtonText(filechooser));
	      accept.setToolTipText(getApproveButtonToolTipText(filechooser));
	      accept.setMnemonic(getApproveButtonMnemonic(filechooser));
	    }
	  else if (e.getPropertyName().equals(JFileChooser.APPROVE_BUTTON_TEXT_CHANGED_PROPERTY))
	    accept.setText(getApproveButtonText(filechooser));
	  else if (e.getPropertyName().equals(JFileChooser.APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY))
	    accept.setToolTipText(getApproveButtonToolTipText(filechooser));
	  else if (e.getPropertyName().equals(JFileChooser.APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY))
	    accept.setMnemonic(getApproveButtonMnemonic(filechooser));
	  else if (e.getPropertyName().equals(JFileChooser.CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY))
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
	  else if (e.getPropertyName().equals(JFileChooser.ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY))
	    {
	      if (filechooser.isAcceptAllFileFilterUsed())
		filechooser.addChoosableFileFilter(getAcceptAllFileFilter(filechooser));
	      else
		filechooser.removeChoosableFileFilter(getAcceptAllFileFilter(filechooser));
	    }
	  else if (e.getPropertyName().equals(JFileChooser.ACCESSORY_CHANGED_PROPERTY))
	    {
	      JComponent old = (JComponent) e.getOldValue();
	      if (old != null)
		getAccessoryPanel().remove(old);
	      JComponent newval = (JComponent) e.getNewValue();
	      if (newval != null)
		getAccessoryPanel().add(newval);
	    }
	  if (e.getPropertyName().equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY)
	      || e.getPropertyName().equals(JFileChooser.FILE_FILTER_CHANGED_PROPERTY)
	      || e.getPropertyName().equals(JFileChooser.FILE_HIDING_CHANGED_PROPERTY))
	    rescanCurrentDirectory(filechooser);

	  filechooser.revalidate();
	  filechooser.repaint();
	}
      };
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getFileName()
  {
    return filename;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getDirectoryName()
  {
    // XXX: I don't see a case where the thing returns something non-null..
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param filename DOCUMENT ME!
   */
  public void setFileName(String filename)
  {
    this.filename = filename;
  }

  /**
   * DOCUMENT ME!
   *
   * @param dirname DOCUMENT ME!
   */
  public void setDirectoryName(String dirname)
  {
    // FIXME: Implement
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   */
  public void rescanCurrentDirectory(JFileChooser fc)
  {
    getModel().validateFileCache();
    filelist.revalidate();
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   * @param f DOCUMENT ME!
   */
  public void ensureFileIsVisible(JFileChooser fc, File f)
  {
    // XXX: Not sure what this does.
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JFileChooser getFileChooser()
  {
    return filechooser;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JPanel getAccessoryPanel()
  {
    return accessoryPanel;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JButton getApproveButton(JFileChooser fc)
  {
    accept = new JButton(getApproveButtonText(fc));
    accept.setMnemonic(getApproveButtonMnemonic(fc));
    accept.setToolTipText(getApproveButtonToolTipText(fc));
    return accept;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   */
  public void clearIconCache()
  {
    if (fv instanceof BasicFileView)
      ((BasicFileView) fv).clearIconCache();
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public ListSelectionListener createListSelectionListener(JFileChooser fc)
  {
    return new SelectionListener();
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   * @param list DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected MouseListener createDoubleClickListener(JFileChooser fc, JList list)
  {
    return new DoubleClickListener(list);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected boolean isDirectorySelected()
  {
    return dirSelected;
  }

  /**
   * DOCUMENT ME!
   *
   * @param selected DOCUMENT ME!
   */
  protected void setDirectorySelected(boolean selected)
  {
    dirSelected = selected;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected File getDirectory()
  {
    return currDir;
  }

  /**
   * DOCUMENT ME!
   *
   * @param f DOCUMENT ME!
   */
  protected void setDirectory(File f)
  {
    currDir = f;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public FileFilter getAcceptAllFileFilter(JFileChooser fc)
  {
    return acceptAll;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public FileView getFileView(JFileChooser fc)
  {
    if (fc.getFileView() != null)
      return fc.getFileView();
    return fv;
  }

  /**
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @param fc DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Action getNewFolderAction()
  {
    return new NewFolderAction();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Action getGoHomeAction()
  {
    return new GoHomeAction();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Action getChangeToParentDirectoryAction()
  {
    return new ChangeToParentDirectoryAction();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Action getApproveSelectionAction()
  {
    return new ApproveSelectionAction();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Action getCancelSelectionAction()
  {
    return new CancelSelectionAction();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Action getUpdateAction()
  {
    return new UpdateAction();
  }
}
