/* ComboBoxDemo.java -- An example showing various combo boxes in Swing.
   Copyright (C) 2005,  Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
*/


package gnu.classpath.examples.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalIconFactory;

/**
 * A simple demo showing various combo boxes in different states.
 */
public class ComboBoxDemo 
  extends JFrame 
  implements ActionListener 
{
 
  class CustomCellRenderer extends DefaultListCellRenderer
  {
    public Component getListCellRendererComponent(JList list,
                                              Object value,
                                              int index,
                                              boolean isSelected,
                                              boolean cellHasFocus)
    {
      DefaultListCellRenderer result = (DefaultListCellRenderer) 
          super.getListCellRendererComponent(list, value, index, isSelected, 
	  cellHasFocus);
      Icon icon = (Icon) value;
      result.setIcon(icon);
      result.setText("Index = " + index);
      return result;
    }
  }

  private JCheckBox comboState1;  
  private JComboBox combo1;
  private JComboBox combo2;

  private JCheckBox comboState2;    
  private JComboBox combo3;
  private JComboBox combo4;
    
  private JCheckBox comboState3;    
  private JComboBox combo5;
  private JComboBox combo6;

  private JCheckBox comboState4;    
  private JComboBox combo7;
  private JComboBox combo8;

  private JCheckBox comboState5;    
  private JComboBox combo9;
  private JComboBox combo10;
  
  private JCheckBox comboState6;
  private JComboBox combo11;
  private JComboBox combo12;
  
  /**
   * Creates a new demo instance.
   * 
   * @param title  the frame title.
   */
  public ComboBoxDemo(String title) 
  {
    super(title);
    JPanel content = createContent();
    JPanel closePanel = new JPanel();
    JButton closeButton = new JButton("Close");
    closeButton.setActionCommand("CLOSE");
    closeButton.addActionListener(this);
    closePanel.add(closeButton);
    content.add(closePanel, BorderLayout.SOUTH);
    getContentPane().add(content);
  }
       
  /**
   * Returns a panel with the demo content.  The panel
   * uses a BorderLayout(), and the BorderLayout.SOUTH area
   * is empty, to allow callers to add controls to the 
   * bottom of the panel if they want to (a close button is
   * added if this demo is being run as a standalone demo).
   */       
  JPanel createContent() 
  {
    JPanel content = new JPanel(new BorderLayout());
    JPanel panel = new JPanel(new GridLayout(6, 1));
    panel.add(createPanel1());
    panel.add(createPanel2());
    panel.add(createPanel3());
    panel.add(createPanel4());
    panel.add(createPanel5());
    panel.add(createPanel6());
    content.add(panel);
    return content;        
  }
    
  private JPanel createPanel1() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.comboState1 = new JCheckBox("Enabled", true);
    this.comboState1.setActionCommand("COMBO_STATE1");
    this.comboState1.addActionListener(this);
    panel.add(this.comboState1, BorderLayout.EAST);
        
    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("Regular: "));
    this.combo1 = new JComboBox(new Object[] {"Australia", "New Zealand", 
            "England"});
        
    this.combo2 = new JComboBox(new Object[] {"Australia", "New Zealand", 
            "England"});
    this.combo2.setEditable(true);
        
    controlPanel.add(combo1);
    controlPanel.add(combo2);
        
    panel.add(controlPanel);
     
    return panel;
  }
    
  private JPanel createPanel2() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.comboState2 = new JCheckBox("Enabled", true);
    this.comboState2.setActionCommand("COMBO_STATE2");
    this.comboState2.addActionListener(this);
    panel.add(this.comboState2, BorderLayout.EAST);
        
    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("Large Font: "));
    this.combo3 = new JComboBox(new Object[] {"Australia", "New Zealand", 
            "England"});
    this.combo3.setFont(new Font("Dialog", Font.PLAIN, 20));
        
    this.combo4 = new JComboBox(new Object[] {"Australia", "New Zealand", 
            "England"});
    this.combo4.setEditable(true);
    this.combo4.setFont(new Font("Dialog", Font.PLAIN, 20));
        
    controlPanel.add(combo3);
    controlPanel.add(combo4);
        
    panel.add(controlPanel);
     
    return panel;
  }

  private JPanel createPanel3() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.comboState3 = new JCheckBox("Enabled", true);
    this.comboState3.setActionCommand("COMBO_STATE3");
    this.comboState3.addActionListener(this);
    panel.add(this.comboState3, BorderLayout.EAST);
        
    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("Colored Background: "));
    this.combo5 = new JComboBox(new Object[] {"Australia", "New Zealand", 
            "England"});
    this.combo5.setBackground(Color.yellow);
    
    this.combo6 = new JComboBox(new Object[] {"Australia", "New Zealand", 
            "England"});
    this.combo6.setEditable(true);
    this.combo6.setBackground(Color.yellow);
        
    controlPanel.add(combo5);
    controlPanel.add(combo6);
        
    panel.add(controlPanel);
     
    return panel;
  }
    
  /**
   * This panel contains combo boxes that are empty.
   * 
   * @return A panel.
   */
  private JPanel createPanel4() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.comboState4 = new JCheckBox("Enabled", true);
    this.comboState4.setActionCommand("COMBO_STATE4");
    this.comboState4.addActionListener(this);
    panel.add(this.comboState4, BorderLayout.EAST);
        
    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("Empty: "));
    this.combo7 = new JComboBox();
    this.combo8 = new JComboBox();
    this.combo8.setEditable(true);
        
    controlPanel.add(combo7);
    controlPanel.add(combo8);
        
    panel.add(controlPanel);
     
    return panel;
  }
    
  /**
   * This panel contains combo boxes that are narrow but contain long text 
   * items.
   * 
   * @return A panel.
   */
  private JPanel createPanel5() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.comboState5 = new JCheckBox("Enabled", true);
    this.comboState5.setActionCommand("COMBO_STATE5");
    this.comboState5.addActionListener(this);
    panel.add(this.comboState5, BorderLayout.EAST);
        
    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("Narrow: "));
    this.combo9 = new JComboBox(new Object[] {
            "A really long item that will be truncated when displayed"});
    this.combo9.setPreferredSize(new Dimension(100, 30));
    this.combo10 = new JComboBox(new Object[] {
            "A really long item that will be truncated when displayed"});
    this.combo10.setPreferredSize(new Dimension(100, 30));
    this.combo10.setEditable(true);
        
    controlPanel.add(combo9);
    controlPanel.add(combo10);
        
    panel.add(controlPanel);
     
    return panel;
  }

  /**
   * This panel contains combo boxes with a custom renderer.
   * 
   * @return A panel.
   */
  private JPanel createPanel6() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.comboState6 = new JCheckBox("Enabled", true);
    this.comboState6.setActionCommand("COMBO_STATE6");
    this.comboState6.addActionListener(this);
    panel.add(this.comboState6, BorderLayout.EAST);
        
    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("Custom Renderer: "));
    this.combo11 = new JComboBox(new Object[] {
            MetalIconFactory.getFileChooserHomeFolderIcon(),
            MetalIconFactory.getFileChooserNewFolderIcon()});
    this.combo11.setPreferredSize(new Dimension(100, 30));
    this.combo11.setRenderer(new CustomCellRenderer());
    this.combo12 = new JComboBox(new Object[] {
            MetalIconFactory.getFileChooserHomeFolderIcon(),
            MetalIconFactory.getFileChooserNewFolderIcon()});
    this.combo12.setPreferredSize(new Dimension(100, 30));
    this.combo12.setRenderer(new CustomCellRenderer());
    this.combo12.setEditable(true);
        
    controlPanel.add(combo11);
    controlPanel.add(combo12);
        
    panel.add(controlPanel);
     
    return panel;
  }

  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("COMBO_STATE1")) 
    {
      combo1.setEnabled(comboState1.isSelected());
      combo2.setEnabled(comboState1.isSelected());
    }
    else if (e.getActionCommand().equals("COMBO_STATE2")) 
    {
      combo3.setEnabled(comboState2.isSelected());
      combo4.setEnabled(comboState2.isSelected());
    }
    else if (e.getActionCommand().equals("COMBO_STATE3")) 
    {
      combo5.setEnabled(comboState3.isSelected());
      combo6.setEnabled(comboState3.isSelected());
    }
    else if (e.getActionCommand().equals("COMBO_STATE4")) 
    {
      combo7.setEnabled(comboState4.isSelected());
      combo8.setEnabled(comboState4.isSelected());
    }
    else if (e.getActionCommand().equals("COMBO_STATE5")) 
    {
      combo9.setEnabled(comboState5.isSelected());
      combo10.setEnabled(comboState5.isSelected());
    }
    else if (e.getActionCommand().equals("COMBO_STATE6")) 
    {
      combo11.setEnabled(comboState6.isSelected());
      combo12.setEnabled(comboState6.isSelected());
    }
    else if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
  }

  public static void main(String[] args) 
  {
    try
    {
      UIManager.setLookAndFeel(new javax.swing.plaf.metal.MetalLookAndFeel());
    }
    catch (Exception e) {
        e.printStackTrace();
    }
    ComboBoxDemo app = new ComboBoxDemo("ComboBox Demo");
    app.pack();
    app.setVisible(true);
  }

}
