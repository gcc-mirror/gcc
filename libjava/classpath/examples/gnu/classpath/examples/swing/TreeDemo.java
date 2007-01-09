/* TreeDemo.java -- Demostrates JTree
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;

public class TreeDemo
  extends JPanel
  implements ActionListener
{

  TreeDemo()
  {
    super();
    createContent();
  }

  private void createContent()
  {     
    // non-leafs
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("Exotic Subsistence");
    DefaultMutableTreeNode fruit = new DefaultMutableTreeNode("Interesting Fruit");
    DefaultMutableTreeNode veg = new DefaultMutableTreeNode("Extraordinary Vegetables");
    DefaultMutableTreeNode liq = new DefaultMutableTreeNode("Peculiar Liquids");
    
    // leafs
    DefaultMutableTreeNode f1 = new DefaultMutableTreeNode("Abiu");
    DefaultMutableTreeNode f2 = new DefaultMutableTreeNode("Bamboo Shoots");
    DefaultMutableTreeNode f3 = new DefaultMutableTreeNode("Breadfruit");
    DefaultMutableTreeNode f4 = new DefaultMutableTreeNode("Canistel");
    DefaultMutableTreeNode f5 = new DefaultMutableTreeNode("Duku");
    DefaultMutableTreeNode f6 = new DefaultMutableTreeNode("Guava");
    DefaultMutableTreeNode f7 = new DefaultMutableTreeNode("Jakfruit");
    DefaultMutableTreeNode f8 = new DefaultMutableTreeNode("Quaribea");
    
    DefaultMutableTreeNode v1 = new DefaultMutableTreeNode("Amaranth");
    DefaultMutableTreeNode v2 = new DefaultMutableTreeNode("Kiwano");
    DefaultMutableTreeNode v3 = new DefaultMutableTreeNode("Leeks");
    DefaultMutableTreeNode v4 = new DefaultMutableTreeNode("Luffa");
    DefaultMutableTreeNode v5 = new DefaultMutableTreeNode("Chayote");
    DefaultMutableTreeNode v6 = new DefaultMutableTreeNode("Jicama");
    DefaultMutableTreeNode v7 = new DefaultMutableTreeNode("Okra");
    
    DefaultMutableTreeNode l1 = new DefaultMutableTreeNode("Alcoholic");
    DefaultMutableTreeNode l11 = new DefaultMutableTreeNode("Caipirinha");
    DefaultMutableTreeNode l21 = new DefaultMutableTreeNode("Mojito");
    DefaultMutableTreeNode l31 = new DefaultMutableTreeNode("Margarita");
    DefaultMutableTreeNode l41 = new DefaultMutableTreeNode("Martini");
    DefaultMutableTreeNode l5 = new DefaultMutableTreeNode("Non Alcoholic");
    DefaultMutableTreeNode l55 = new DefaultMutableTreeNode("Babaji");
    DefaultMutableTreeNode l65 = new DefaultMutableTreeNode("Chikita");
    
    root.add(fruit);
    root.add(veg);
    root.add(liq);
    fruit.add(f1);
    fruit.add(f2);
    fruit.add(f3);
    fruit.add(f4);
    fruit.add(f5);
    fruit.add(f6);
    fruit.add(f7);
    fruit.add(f8);
    veg.add(v1);
    veg.add(v2);
    veg.add(v3);
    veg.add(v4);
    veg.add(v5);
    veg.add(v6);
    veg.add(v7);
    liq.add(l1);
    l1.add(l11);
    l1.add(l21);
    l1.add(l31);
    l1.add(l41);
    liq.add(l5);
    l5.add(l55);
    l5.add(l65);

    final JTree tree = new JTree(root);
    tree.setLargeModel(true);
    tree.setEditable(true);
    final DefaultTreeSelectionModel selModel = new DefaultTreeSelectionModel();
    selModel.setSelectionMode(
      DefaultTreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
    tree.setSelectionModel(selModel);
    
    // buttons to add and delete
    JButton add = new JButton("add element");
    add.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
           for (int i = 0; i < tree.getRowCount(); i++)
           {
              if (tree.isRowSelected(i))
              {
                 TreePath p = tree.getPathForRow(i);
                 DefaultMutableTreeNode n = (DefaultMutableTreeNode) p.
                                                  getLastPathComponent();
                 n.add(new DefaultMutableTreeNode("New Element"));
                 
                 // The expansion state of the parent node does not change
                 // by default. We will expand it manually, to ensure that the
                 // added node is immediately visible.
                 tree.expandPath(p);
                  
                 // Refresh the tree (.repaint would be not enough both in
                 // Classpath and Sun implementations).
                 DefaultTreeModel model = (DefaultTreeModel) tree.getModel();                 
                 model.reload(n);
                 break;
              }
           }
        }
      });
    
    // Demonstration of the various selection modes
    final JCheckBox cbSingle = new JCheckBox("single selection");
    cbSingle.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
      {
        if (cbSingle.isSelected())
          selModel.setSelectionMode(
            DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
        else
          selModel.setSelectionMode(
            DefaultTreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
      }
      });
    
    // Demonstration of the root visibility changes
    final JCheckBox cbRoot = new JCheckBox("root");
    cbRoot.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
      {
        tree.setRootVisible(cbRoot.isSelected());
      }
      });
    cbRoot.setSelected(true);
    
    // Demonstration of the tree selection listener.
    final JLabel choice = new JLabel("Make a choice");
    tree.getSelectionModel().addTreeSelectionListener(
      new TreeSelectionListener()
        {
          public void valueChanged(TreeSelectionEvent event)
          {
            TreePath was = event.getOldLeadSelectionPath();
            TreePath now = event.getNewLeadSelectionPath();
            String swas = 
              was == null ? "none":was.getLastPathComponent().toString();
            String snow = 
              now == null ? "none":now.getLastPathComponent().toString();
            choice.setText("From "+swas+" to "+snow);
          }
        }
      );
    
    setLayout(new BorderLayout());
    
    JPanel p2 = new JPanel(); 
    p2.add(add);
    p2.add(cbSingle);
    p2.add(cbRoot);

    tree.getSelectionModel().
      setSelectionMode(DefaultTreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);

    // Panel for selecting line style.
    ActionListener l = new ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        JRadioButton b = (JRadioButton) e.getSource();
        tree.putClientProperty("JTree.lineStyle", b.getText());
        tree.repaint();
      }
    };
    JPanel lineStylePanel = new JPanel();
    ButtonGroup buttons = new ButtonGroup();
    lineStylePanel.add(new JLabel("Line style: "));
    JRadioButton none = new JRadioButton("None");
    lineStylePanel.add(none);
    buttons.add(none);
    none.addActionListener(l);
    JRadioButton angled = new JRadioButton("Angled");
    lineStylePanel.add(angled);
    buttons.add(angled);
    angled.addActionListener(l);
    JRadioButton horizontal = new JRadioButton("Horizontal");
    lineStylePanel.add(horizontal);
    buttons.add(horizontal);
    horizontal.addActionListener(l);
    p2.add(lineStylePanel);

    add(p2, BorderLayout.NORTH);

    add(new JScrollPane(tree), BorderLayout.CENTER);
    add(choice, BorderLayout.SOUTH);
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
         TreeDemo app = new TreeDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("Tree Demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a TreeDemo.
   *
   * @return a DemoFactory that creates a TreeDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new TreeDemo();
      }
    };
  }
}
