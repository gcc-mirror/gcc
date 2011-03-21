/* BasicTableHeaderUI.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.CellRendererPane;
import javax.swing.JComponent;
import javax.swing.LookAndFeel;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TableHeaderUI;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/**
 * Basic pluggable look and feel interface for JTableHeader.
 */
public class BasicTableHeaderUI extends TableHeaderUI
{
  /**
   * The width of the space (in both direction) around the column boundary,
   * where mouse cursor changes shape into "resize"
   */
  static int COLUMN_BOUNDARY_TOLERANCE = 3;

  public static ComponentUI createUI(JComponent h)
  {
    return new BasicTableHeaderUI();
  }

  /**
   * The table header that is using this interface.
   */
  protected JTableHeader header;

  /**
   * The mouse input listener, responsible for mouse manipulations with
   * the table header.
   */
  protected MouseInputListener mouseInputListener;

  /**
   * Paint the header cell.
   */
  protected CellRendererPane rendererPane;

  /**
   * The header cell border.
   */
  private Border cellBorder;

  /**
   * Original mouse cursor prior to resizing.
   */
  private Cursor originalCursor;

  /**
   * If not null, one of the columns is currently being dragged.
   */
  Rectangle draggingHeaderRect;

  /**
   * Handles column movement and rearrangement by mouse. The same instance works
   * both as mouse listener and the mouse motion listner.
   */
  public class MouseInputHandler
      implements MouseInputListener
  {
    /**
     * If true, the cursor is being already shown in the alternative "resize"
     * shape.
     */
    boolean showingResizeCursor;

    /**
     * The position, from where the cursor is dragged during resizing. Double
     * purpose field (absolute value during resizing and relative offset during
     * column dragging).
     */
    int draggingFrom = - 1;

    /**
     * The number of the column being dragged.
     */
    int draggingColumnNumber;

    /**
     * The previous preferred width of the column.
     */
    int prevPrefWidth = - 1;

    /**
     * The timer to coalesce column resizing events.
     */
    Timer timer;

    /**
     * Returns without action, part of the MouseInputListener interface.
     */
    public void mouseClicked(MouseEvent e)
    {
      // Nothing to do.
    }

    /**
     * If being in the resizing mode, handle resizing.
     */
    public void mouseDragged(MouseEvent e)
    {
      TableColumn resizeIt = header.getResizingColumn();
      if (resizeIt != null && header.getResizingAllowed())
        {
          // The timer is intialised on demand.
          if (timer == null)
            {
              // The purpose of timer is to coalesce events. If the queue
              // is free, the repaint event is fired immediately.
              timer = new Timer(1, new ActionListener()
              {
                public void actionPerformed(ActionEvent e)
                {
                  header.getTable().doLayout();
                }
              });
              timer.setRepeats(false);
              timer.setCoalesce(true);
            }
          resizeIt.setPreferredWidth(prevPrefWidth + e.getX() - draggingFrom);
          timer.restart();
        }
      else if (draggingHeaderRect != null && header.getReorderingAllowed())
        {
          draggingHeaderRect.x = e.getX() + draggingFrom;
          header.repaint();
        }
    }

    /**
     * Returns without action, part of the MouseInputListener interface.
     */
    public void mouseEntered(MouseEvent e)
    {
      // Nothing to do.
    }

    /**
     * Reset drag information of the column resizing.
     */
    public void mouseExited(MouseEvent e)
    {
      // Nothing to do.
    }

    /**
     * Change the mouse cursor if the mouse if above the column boundary.
     */
    public void mouseMoved(MouseEvent e)
    {
      // When dragging, the functionality is handled by the mouseDragged.
      if (e.getButton() == 0 && header.getResizingAllowed())
        {
          TableColumnModel model = header.getColumnModel();
          int n = model.getColumnCount();
          if (n < 2)
            // It must be at least two columns to have at least one boundary.
            // Otherwise, nothing to do.
            return;

          boolean onBoundary = false;

          int x = e.getX();
          int a = x - COLUMN_BOUNDARY_TOLERANCE;
          int b = x + COLUMN_BOUNDARY_TOLERANCE;

          int p = 0;

          Scan: for (int i = 0; i < n - 1; i++)
            {
              p += model.getColumn(i).getWidth();

              if (p >= a && p <= b)
                {
                  TableColumn column = model.getColumn(i);
                  onBoundary = true;

                  draggingFrom = x;
                  prevPrefWidth = column.getWidth();
                  header.setResizingColumn(column);
                  break Scan;
                }
            }

          if (onBoundary != showingResizeCursor)
            {
              // Change the cursor shape, if needed.
              if (onBoundary)
                {

                  originalCursor = header.getCursor();
                  if (p < x)
                    header.setCursor(Cursor.getPredefinedCursor(
                        Cursor.W_RESIZE_CURSOR));
                  else
                    header.setCursor(Cursor.getPredefinedCursor(
                        Cursor.E_RESIZE_CURSOR));
                }
              else
                {
                  header.setCursor(originalCursor);
                  header.setResizingColumn(null);
                }

              showingResizeCursor = onBoundary;
            }
        }
    }

    /**
     * Starts the dragging/resizing procedure.
     */
    public void mousePressed(MouseEvent e)
    {
      if (header.getResizingAllowed())
        {
          TableColumn resizingColumn = header.getResizingColumn();
          if (resizingColumn != null)
            {
              resizingColumn.setPreferredWidth(resizingColumn.getWidth());
              return;
            }
        }

      if (header.getReorderingAllowed())
        {
          TableColumnModel model = header.getColumnModel();
          int n = model.getColumnCount();
          if (n < 2)
            // It must be at least two columns to change the column location.
            return;

          boolean onBoundary = false;

          int x = e.getX();
          int p = 0;
          int col = - 1;

          Scan: for (int i = 0; i < n; i++)
            {
              p += model.getColumn(i).getWidth();
              if (p > x)
                {
                  col = i;
                  break Scan;
                }
            }
          if (col < 0)
            return;

          TableColumn dragIt = model.getColumn(col);
          header.setDraggedColumn(dragIt);

          draggingFrom = (p - dragIt.getWidth()) - x;
          draggingHeaderRect = new Rectangle(header.getHeaderRect(col));
          draggingColumnNumber = col;
        }
    }

    /**
     * Set all column preferred width to the current width to prevend abrupt
     * width changes during the next resize.
     */
    public void mouseReleased(MouseEvent e)
    {
      if (header.getResizingColumn() != null && header.getResizingAllowed())
        endResizing();
      if (header.getDraggedColumn() != null &&  header.getReorderingAllowed())
        endDragging(e);
    }

    /**
     * Stop resizing session.
     */
    void endResizing()
    {
      TableColumnModel model = header.getColumnModel();
      int n = model.getColumnCount();
      if (n > 2)
        {
          TableColumn c;
          for (int i = 0; i < n; i++)
            {
              c = model.getColumn(i);
              c.setPreferredWidth(c.getWidth());
            }
        }
      header.setResizingColumn(null);
      showingResizeCursor = false;
      if (timer != null)
        timer.stop();
      header.setCursor(originalCursor);
    }

    /**
     * Stop the dragging session.
     *
     * @param e the "mouse release" mouse event, needed to determing the final
     *          location for the dragged column.
     */
    void endDragging(MouseEvent e)
    {
      header.setDraggedColumn(null);
      draggingHeaderRect = null;

      TableColumnModel model = header.getColumnModel();

      // Find where have we dragged the column.
      int x = e.getX();
      int p = 0;

      int col = model.getColumnCount() - 1;
      int n = model.getColumnCount();

      // This loop does not find the column if the mouse if out of the
      // right boundary of the table header. Then we make this column the
      // rightmost column.
      Scan: for (int i = 0; i < n; i++)
        {
          p += model.getColumn(i).getWidth();
          if (p > x)
            {
              col = i;
              break Scan;
            }
        }

      header.getTable().moveColumn(draggingColumnNumber, col);
    }
  }

  /**
   * Create and return the mouse input listener.
   *
   * @return the mouse listener ({@link MouseInputHandler}, if not overridden.
   */
  protected MouseInputListener createMouseInputListener()
  {
    return new MouseInputHandler();
  }

  /**
   * Construct a new BasicTableHeaderUI, create mouse listeners.
   */
  public BasicTableHeaderUI()
  {
    mouseInputListener = createMouseInputListener();
  }

  protected void installDefaults()
  {
    LookAndFeel.installColorsAndFont(header, "TableHeader.background",
                                     "TableHeader.foreground",
                                     "TableHeader.font");
    cellBorder = UIManager.getBorder("TableHeader.cellBorder");
  }

  protected void installKeyboardActions()
  {
    // AFAICS, the RI does nothing here.
  }

  /**
   * Add the mouse listener and the mouse motion listener to the table
   * header. The listeners support table column resizing and rearrangement
   * by mouse.
   */
  protected void installListeners()
  {
    header.addMouseListener(mouseInputListener);
    header.addMouseMotionListener(mouseInputListener);
  }

  public void installUI(JComponent c)
  {
    header = (JTableHeader) c;
    rendererPane = new CellRendererPane();
    installDefaults();
    installKeyboardActions();
    installListeners();
  }

  protected void uninstallDefaults()
  {
    header.setBackground(null);
    header.setForeground(null);
    header.setFont(null);
  }

  protected void uninstallKeyboardActions()
  {
    // AFAICS, the RI does nothing here.
  }

  /**
   * Remove the previously installed listeners.
   */
  protected void uninstallListeners()
  {
    header.removeMouseListener(mouseInputListener);
    header.removeMouseMotionListener(mouseInputListener);
  }

  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallKeyboardActions();
    uninstallDefaults();
  }

  /**
   * Repaint the table header.
   */
  public void paint(Graphics gfx, JComponent c)
  {
    TableColumnModel cmod = header.getColumnModel();
    int ncols = cmod.getColumnCount();
    if (ncols == 0)
      return;

    Rectangle clip = gfx.getClipBounds();
    TableCellRenderer defaultRend = header.getDefaultRenderer();

    for (int i = 0; i < ncols; ++i)
      {
        Rectangle bounds = header.getHeaderRect(i);
        if (bounds.intersects(clip))
          {
            Rectangle oldClip = gfx.getClipBounds();
            TableColumn col = cmod.getColumn(i);
            TableCellRenderer rend = col.getHeaderRenderer();
            if (rend == null)
              rend = defaultRend;
            Object val = col.getHeaderValue();
            Component comp = rend.getTableCellRendererComponent(header.getTable(),
                                                                val,
                                                                false, // isSelected
                                                                false, // isFocused
                                                                -1, i);
            // FIXME: The following settings should be performed in
            // rend.getTableCellRendererComponent().
            comp.setFont(header.getFont());
            comp.setBackground(header.getBackground());
            comp.setForeground(header.getForeground());
            if (comp instanceof JComponent)
              ((JComponent) comp).setBorder(cellBorder);
            rendererPane.paintComponent(gfx, comp, header, bounds.x, bounds.y,
                                        bounds.width, bounds.height);
          }
      }

    // This displays a running rectangle that is much simplier than the total
    // animation, as it is seen in Sun's application.
    // TODO animate the collumn dragging like in Sun's jre.
    if (draggingHeaderRect != null)
      {
        gfx.setColor(header.getForeground());
        gfx.drawRect(draggingHeaderRect.x, draggingHeaderRect.y + 2,
            draggingHeaderRect.width - 1, draggingHeaderRect.height - 6);
      }
  }

  /**
   * Get the preferred header size.
   *
   * @param ignored unused
   *
   * @return the preferred size of the associated header.
   */
  public Dimension getPreferredSize(JComponent ignored)
  {
    TableColumnModel cmod = header.getColumnModel();
    TableCellRenderer defaultRend = header.getDefaultRenderer();
    int ncols = cmod.getColumnCount();
    Dimension ret = new Dimension(0, 0);
    int spacing = 0;

    if (header.getTable() != null
        && header.getTable().getIntercellSpacing() != null)
      spacing = header.getTable().getIntercellSpacing().width;

    for (int i = 0; i < ncols; ++i)
      {
        TableColumn col = cmod.getColumn(i);
        TableCellRenderer rend = col.getHeaderRenderer();
        if (rend == null)
          rend = defaultRend;
        Object val = col.getHeaderValue();
        Component comp = rend.getTableCellRendererComponent(header.getTable(),
                                                            val,
                                                            false, // isSelected
                                                            false, // isFocused
                                                            -1, i);
        comp.setFont(header.getFont());
        comp.setBackground(header.getBackground());
        comp.setForeground(header.getForeground());
        if (comp instanceof JComponent)
          ((JComponent) comp).setBorder(cellBorder);

        Dimension d = comp.getPreferredSize();
        ret.width += spacing;
        ret.height = Math.max(d.height, ret.height);
      }
    ret.width = cmod.getTotalColumnWidth();
    return ret;
  }


}
