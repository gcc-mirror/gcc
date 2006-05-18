/* ListDemo.java -- Demostrates JList
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.examples.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;

public class ListDemo
  extends JPanel
  implements ActionListener
{

  private static class LabelCellRenderer 
    extends DefaultListCellRenderer
  {
    public Component getListCellRendererComponent(JList list,
                                                  Object value,
                                                  int index,
                                                  boolean isSelected,
                                                  boolean cellHasFocus)
    {
      Component c = super.getListCellRendererComponent(list, value, index, 
                                                       isSelected,
                                                       cellHasFocus);
      return c;
    }
  }

  private static class CheckCellRenderer 
    extends JCheckBox
    implements ListCellRenderer
  {
    public Component getListCellRendererComponent(JList list,
                                                  Object value,
                                                  int index,
                                                  boolean isSelected,
                                                  boolean cellHasFocus)
    {
      setSelected(isSelected);
      setText(value.toString());
      
      return this;
    }
  }

  ListDemo()
  {
    super();
    createContent();
  }

  private void createContent()
  {

    String foo[] = new String[] { 
      "non alcoholic ",
      "carbonated ",
      "malted ",
      "fresh squeezed ",
      "imported ",
      "high fructose ",
      "enriched "
    };
    
    String bar[] = new String[] { 
      "orange juice",
      "ginger beer",
      "yak milk",
      "corn syrup",
      "herbal remedy"
    };

    final DefaultListModel mod = new DefaultListModel();
    final JList list1 = new JList(mod);
    final JList list2 = new JList(mod);

    list2.setSelectionModel(list1.getSelectionModel());
    for (int i = 0; i < bar.length; ++i)
      for (int j = 0; j < foo.length; ++j)
        mod.addElement(foo[j] + bar[i]);

    list1.setCellRenderer(new LabelCellRenderer());
    list2.setCellRenderer(new CheckCellRenderer());

    JButton add = new JButton("add element");
    add.addActionListener(new ActionListener()
      {
        int i = 0;
        public void actionPerformed(ActionEvent e)
        {
          mod.addElement("new element " + i);
          ++i;
        }
      });

    JButton del = new JButton("delete selected");
    del.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          for (int i = 0; i < mod.getSize(); ++i)
            if (list1.isSelectedIndex(i))
              mod.remove(i);
        }
      });


    JSplitPane splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    splitter.add(new JScrollPane(list1), JSplitPane.LEFT);
    splitter.add(new JScrollPane(list2), JSplitPane.RIGHT);

    setLayout(new BorderLayout());
    JPanel p2 = new JPanel(); 
    p2.setLayout(new GridLayout(1, 2));
    p2.add(add);
    p2.add(del);

    add(p2, BorderLayout.NORTH);
    add(splitter, BorderLayout.CENTER);
  }

  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
  }

  /**
   * When the demo is run independently, the frame is displayed, so we should
   * initialise the content panel (including the demo content and a close 
   * button).  But when the demo is run as part of the Swing activity board,
   * only the demo content panel is used, the frame itself is never displayed,
   * so we can avoid this step.
   */
  void initFrameContent()
  {
    JPanel closePanel = new JPanel();
    JButton closeButton = new JButton("Close");
    closeButton.setActionCommand("CLOSE");
    closeButton.addActionListener(this);
    closePanel.add(closeButton);
    add(closePanel, BorderLayout.SOUTH);
  }

  public static void main(String[] args)
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         ListDemo app = new ListDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("List Demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a ListDemo.
   *
   * @return a DemoFactory that creates a ListDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new ListDemo();
      }
    };
  }
}
