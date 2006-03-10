/* MiniDemo.java --  A Swing demo suitable for embedded environments
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
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalIconFactory;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * A Swing demo suitable for embedded environments (e.g. small display,
 * b/w graphics etc).
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class MiniDemo extends JFrame
{

  /**
   * Creates a new MiniDemo instance.
   */
  MiniDemo()
  {
    createGUI();
  }

  private void createGUI()
  {
    JTabbedPane tabPane = new JTabbedPane(JTabbedPane.TOP,
                                          JTabbedPane.SCROLL_TAB_LAYOUT);

    // Setup scrolling list in first tab.
    Object[] listData = new Object[]{"Milk", "Beer", "Wine", "Water",
                                     "Orange juice", "Tea", "Coffee", "Whiskey",
                                     "Lemonade", "Apple juice", "Gin Tonic",
                                     "Pangalactic Garleblaster", "Coke"};
    JList list = new JList(listData);
    JScrollPane sp = new JScrollPane(list);
    tabPane.addTab("List", sp);

    // Setup some buttons in the second tab.
    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new GridLayout(4, 1));
    // JButtons
    JPanel jButtonPanel = new JPanel();
    jButtonPanel.setLayout(new BorderLayout());
    final JCheckBox buttonState1 = new JCheckBox("Enabled", true);
    jButtonPanel.add(buttonState1, BorderLayout.EAST);
    JPanel jButtonContainer = new JPanel();
    final JButton jButton1 = new JButton("JButton");
    final JButton jButton2 =
      new JButton(MetalIconFactory.getInternalFrameDefaultMenuIcon());
    jButtonContainer.add(jButton1);
    jButtonContainer.add(jButton2);
    jButtonPanel.add(jButtonContainer, BorderLayout.CENTER);
    buttonState1.addActionListener(
    new ActionListener()
    {
      public void actionPerformed(ActionEvent ev)
      {
        boolean enabled = buttonState1.isSelected();
        jButton1.setEnabled(enabled);
        jButton2.setEnabled(enabled);
      }
    });
    buttonPanel.add(jButtonPanel);
    // JToggleButtons
    JPanel jToggleButtonPanel = new JPanel();
    jToggleButtonPanel.setLayout(new BorderLayout());
    final JCheckBox buttonState2 = new JCheckBox("Enabled", true);
    jToggleButtonPanel.add(buttonState2, BorderLayout.EAST);
    JPanel jToggleButtonContainer = new JPanel();
    final JButton jToggleButton1 = new JButton("JToggleButton");
    final JButton jToggleButton2 =
      new JButton(MetalIconFactory.getInternalFrameDefaultMenuIcon());
    jToggleButtonContainer.add(jToggleButton1);
    jToggleButtonContainer.add(jToggleButton2);
    jToggleButtonPanel.add(jToggleButtonContainer, BorderLayout.CENTER);
    buttonState2.addActionListener(
    new ActionListener()
    {
      public void actionPerformed(ActionEvent ev)
      {
        boolean enabled = buttonState2.isSelected();
        jToggleButton1.setEnabled(enabled);
        jToggleButton2.setEnabled(enabled);
      }
    });
    buttonPanel.add(jToggleButtonPanel);
    tabPane.addTab("Buttons", buttonPanel);

    // ComboBoxes
    JPanel comboBoxPanel = new JPanel();
    JComboBox comboBox = new JComboBox(listData);
    comboBoxPanel.add(comboBox);
    tabPane.add("ComboBox", comboBoxPanel);

    // TextFields
    JPanel textFieldPanel = new JPanel();
    textFieldPanel.setLayout(new BoxLayout(textFieldPanel, BoxLayout.Y_AXIS));
    textFieldPanel.add(Box.createVerticalStrut(70));
    JPanel leftAlignedPanel = new JPanel(new BorderLayout());
    JPanel textFieldPanel1 = new JPanel();
    textFieldPanel1.setLayout(new BoxLayout(textFieldPanel1,
                                            BoxLayout.X_AXIS));
    final JTextField textfield1 = new JTextField("Hello World!");
    textfield1.setHorizontalAlignment(JTextField.LEFT);
    textfield1.setFont(new Font("Dialog", Font.PLAIN, 8));
    textFieldPanel1.add(textfield1);
    final JTextField textfield2 = new JTextField("Hello World!");
    textfield2.setHorizontalAlignment(JTextField.LEFT);
    textfield2.setFont(new Font("Dialog", Font.ITALIC, 12));
    textFieldPanel1.add(textfield2);
    final JTextField textfield3 = new JTextField("Hello World!");
    textfield3.setHorizontalAlignment(JTextField.LEFT);
    textfield3.setFont(new Font("Dialog", Font.BOLD, 14));
    textFieldPanel1.add(textfield3);
    leftAlignedPanel.add(textFieldPanel1);
    JPanel statePanel = new JPanel();
    statePanel.setLayout(new BoxLayout(statePanel, BoxLayout.Y_AXIS));
    statePanel.add(Box.createVerticalGlue());
    final JCheckBox enabled1 = new JCheckBox("enabled");
    enabled1.setSelected(true);
    enabled1.addActionListener(
    new ActionListener()
    {
      public void actionPerformed(ActionEvent ev)
      {
        boolean enabled = enabled1.isSelected();
        textfield1.setEnabled(enabled);
        textfield2.setEnabled(enabled);
        textfield3.setEnabled(enabled);
      }
    });
    statePanel.add(enabled1);
    final JCheckBox editable1 = new JCheckBox("editable");
    editable1.setSelected(true);
    editable1.addActionListener(
    new ActionListener()
    {
      public void actionPerformed(ActionEvent ev)
      {
        boolean editable = editable1.isSelected();
        textfield1.setEditable(editable);
        textfield2.setEditable(editable);
        textfield3.setEditable(editable);
      }
    });
    statePanel.add(editable1);
    statePanel.add(Box.createVerticalGlue());
    leftAlignedPanel.add(statePanel, BorderLayout.EAST);
    textFieldPanel.add(leftAlignedPanel);
    System.err.println(leftAlignedPanel.getPreferredSize());
    textFieldPanel.add(Box.createVerticalStrut(70));
    //panel.add(rightAlignedPanel);
    tabPane.add("TextField", textFieldPanel);
    setContentPane(tabPane);
  }

  /**
   * Starts the demo application.
   *
   * @param args the command line arguments (ignored)
   */
  public static void main(String[] args)
  {
    SwingUtilities.invokeLater(new Runnable() {
      public void run()
      {
        MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        MiniDemo demo = new MiniDemo();
        demo.setSize(320, 200);
        demo.setUndecorated(true);
        demo.setVisible(true);
        demo.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      }
    });
  }

}
