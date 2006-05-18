/* ButtonDemo.java -- An example showing various buttons in Swing.
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

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
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.plaf.metal.MetalIconFactory;

/**
 * A simple button demo showing various buttons in different states.
 */
public class ButtonDemo 
  extends JPanel 
  implements ActionListener 
{

  private JCheckBox buttonState;  
  private JButton button1;
  private JButton button2;
  private JButton button3;
  private JButton button4;

  private JCheckBox toggleState;    
  private JToggleButton toggle1;
  private JToggleButton toggle2;
  private JToggleButton toggle3;
  private JToggleButton toggle4;
    
  private JCheckBox checkBoxState;
  private JCheckBox checkBox1;
  private JCheckBox checkBox2;
  private JCheckBox checkBox3;

  private JCheckBox radioState;
  private JRadioButton radio1;
  private JRadioButton radio2;
  private JRadioButton radio3;

  /**
   * Creates a new demo instance.
   */
  public ButtonDemo() 
  {
    createContent();
    // initFrameContent() is only called (from main) when running this app 
    // standalone
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

  /**
   * Returns a panel with the demo content.  The panel
   * uses a BorderLayout(), and the BorderLayout.SOUTH area
   * is empty, to allow callers to add controls to the 
   * bottom of the panel if they want to (a close button is
   * added if this demo is being run as a standalone demo).
   */       
  private void createContent()
  {
    setLayout(new BorderLayout());
    JPanel panel = new JPanel(new GridLayout(4, 1));
    panel.add(createButtonPanel());
    panel.add(createTogglePanel());
    panel.add(createCheckBoxPanel());
    panel.add(createRadioPanel());
    add(panel);
  }
    
  private JPanel createButtonPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.buttonState = new JCheckBox("Enabled", true);
    this.buttonState.setActionCommand("BUTTON_STATE");
    this.buttonState.addActionListener(this);
    panel.add(this.buttonState, BorderLayout.EAST);
        
    JPanel buttonPanel = new JPanel();
    buttonPanel.setBorder(BorderFactory.createTitledBorder("JButton"));
    this.button1 = new JButton("Button 1");
        
    this.button2 = new JButton("Button 2");
    this.button2.setIcon(MetalIconFactory.getInternalFrameDefaultMenuIcon());
        
    this.button3 = new JButton("Button 3");
    this.button3.setIcon(MetalIconFactory.getFileChooserHomeFolderIcon());
    this.button3.setHorizontalTextPosition(SwingConstants.CENTER);
    this.button3.setVerticalTextPosition(SwingConstants.BOTTOM);
        
    this.button4 = new JButton("Button 4");
    this.button4.setIcon(MetalIconFactory.getFileChooserUpFolderIcon());
    this.button4.setText(null);
        
    buttonPanel.add(button1);
    buttonPanel.add(button2);
    buttonPanel.add(button3);
    buttonPanel.add(button4);
        
    panel.add(buttonPanel);
     
    return panel;
  }
    
  private JPanel createTogglePanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
       
    this.toggleState = new JCheckBox("Enabled", true);
    this.toggleState.setActionCommand("TOGGLE_STATE");
    this.toggleState.addActionListener(this);
        
    panel.add(this.toggleState, BorderLayout.EAST);
        
    JPanel buttonPanel = new JPanel();
    buttonPanel.setBorder(BorderFactory.createTitledBorder("JToggleButton"));
        
    this.toggle1 = new JToggleButton("Toggle 1");
        
    this.toggle2 = new JToggleButton("Toggle 2");
    this.toggle2.setIcon(MetalIconFactory.getInternalFrameDefaultMenuIcon());
        
    this.toggle3 = new JToggleButton("Toggle 3");
    this.toggle3.setIcon(MetalIconFactory.getFileChooserHomeFolderIcon());
    this.toggle3.setHorizontalTextPosition(SwingConstants.CENTER);
    this.toggle3.setVerticalTextPosition(SwingConstants.BOTTOM);
        
    this.toggle4 = new JToggleButton("Toggle 4");
    this.toggle4.setIcon(MetalIconFactory.getFileChooserUpFolderIcon());
    this.toggle4.setText(null);

    ButtonGroup toggleGroup = new ButtonGroup();
    toggleGroup.add(toggle1);
    toggleGroup.add(toggle2);
    toggleGroup.add(toggle3);
    toggleGroup.add(toggle4);
        
    buttonPanel.add(toggle1);
    buttonPanel.add(toggle2);
    buttonPanel.add(toggle3);
    buttonPanel.add(toggle4);
        
    panel.add(buttonPanel);
      
    return panel;
  }

  private JPanel createCheckBoxPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
      
    this.checkBoxState = new JCheckBox("Enabled", true);
    this.checkBoxState.setActionCommand("CHECKBOX_STATE");
    this.checkBoxState.addActionListener(this);
        
    panel.add(this.checkBoxState, BorderLayout.EAST);
        
    JPanel buttonPanel = new JPanel();
    buttonPanel.setBorder(BorderFactory.createTitledBorder("JCheckBox"));
    this.checkBox1 = new JCheckBox("CheckBox 1");
        
    this.checkBox2 = new JCheckBox("CheckBox 2");
       
    this.checkBox3 = new JCheckBox("CheckBox 3");
                
    buttonPanel.add(checkBox1);
    buttonPanel.add(checkBox2);
    buttonPanel.add(checkBox3);
        
    panel.add(buttonPanel);
        
    return panel;
  }

  private JPanel createRadioPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
        
    this.radioState = new JCheckBox("Enabled", true);
    this.radioState.setActionCommand("RADIO_STATE");
    this.radioState.addActionListener(this);
    panel.add(this.radioState, BorderLayout.EAST);
        
    JPanel buttonPanel = new JPanel();
    buttonPanel.setBorder(BorderFactory.createTitledBorder("JRadioButton"));
    this.radio1 = new JRadioButton("Radio 1");
        
    this.radio2 = new JRadioButton("Radio 2");
        
    this.radio3 = new JRadioButton("Radio 3");
        
    ButtonGroup radioGroup = new ButtonGroup();
    radioGroup.add(radio1);
    radioGroup.add(radio2);
    radioGroup.add(radio3);
        
    buttonPanel.add(radio1);
    buttonPanel.add(radio2);
    buttonPanel.add(radio3);
        
    panel.add(buttonPanel);
       
    return panel;
  }
    
  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("BUTTON_STATE")) 
    {
      button1.setEnabled(buttonState.isSelected());
      button2.setEnabled(buttonState.isSelected());
      button3.setEnabled(buttonState.isSelected());
      button4.setEnabled(buttonState.isSelected());
    }
    else if (e.getActionCommand().equals("TOGGLE_STATE")) 
    {
      toggle1.setEnabled(toggleState.isSelected());
      toggle2.setEnabled(toggleState.isSelected());
      toggle3.setEnabled(toggleState.isSelected());
      toggle4.setEnabled(toggleState.isSelected());
    }
    else if (e.getActionCommand().equals("CHECKBOX_STATE")) 
    {
      checkBox1.setEnabled(checkBoxState.isSelected());
      checkBox2.setEnabled(checkBoxState.isSelected());
      checkBox3.setEnabled(checkBoxState.isSelected());
    }
    else if (e.getActionCommand().equals("RADIO_STATE")) 
    {
      radio1.setEnabled(radioState.isSelected());
      radio2.setEnabled(radioState.isSelected());
      radio3.setEnabled(radioState.isSelected());
    }
    else if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
  }

  public static void main(String[] args) 
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         ButtonDemo app = new ButtonDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("ButtonDemo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a ButtonDemo.
   *
   * @return a DemoFactory that creates a ButtonDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new ButtonDemo();
      }
    };
  }
}
