package javax.swing.table;
import java.awt.Component;
import java.io.Serializable;
import javax.swing.JLabel;
import javax.swing.JTable;
/**
 * STUBBED
 */
public class DefaultTableCellRenderer extends JLabel
  implements TableCellRenderer, Serializable
{
  public static class UIResource extends DefaultTableCellRenderer
    implements javax.swing.plaf.UIResource
  {
    public UIResource()
    {
    }
  } // class UIResource

  public DefaultTableCellRenderer()
  {
  }

  public Component getTableCellRendererComponent(JTable table, Object value,
                                                 boolean isSelected,
                                                 boolean hasFocus,
                                                 int row, int column)
  {
    return null;
  }
} // class DefaultTableCellRenderer
