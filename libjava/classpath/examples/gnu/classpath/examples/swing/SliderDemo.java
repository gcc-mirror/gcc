/* SliderDemo.java -- An example showing JSlider in various configurations.
   Copyright (C) 2005, 2006,  Free Software Foundation, Inc.

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

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.SwingUtilities;

public class SliderDemo
  extends JPanel
  implements ActionListener 
{

  JSlider hslider1;
  JSlider hslider2;
  JSlider hslider3;
  JSlider hslider4;
  JSlider hslider5;
  JSlider hslider6;
  JSlider hslider7;
  JSlider hslider8;
  
  JSlider vslider1;
  JSlider vslider2;
  JSlider vslider3;
  JSlider vslider4;
  JSlider vslider5;
  JSlider vslider6;
  JSlider vslider7;
  JSlider vslider8;

  JCheckBox enabledCheckBox;
  
  public SliderDemo() 
  {
    createContent();
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
    JPanel panel = new JPanel(new GridLayout(1, 2));
    panel.add(createHorizontalPanel());
    panel.add(createVerticalPanel());
    enabledCheckBox = new JCheckBox("Enabled");
    enabledCheckBox.setSelected(true);
    enabledCheckBox.setActionCommand("TOGGLE_ENABLED");
    enabledCheckBox.addActionListener(this);
    JPanel checkBoxPanel = new JPanel();
    checkBoxPanel.add(enabledCheckBox);
    JPanel panel2 = new JPanel(new BorderLayout());
    panel2.add(panel);
    panel2.add(checkBoxPanel, BorderLayout.SOUTH);
    add(panel2);
  }
    
  private JPanel createHorizontalPanel() 
  {
    JPanel panel = new JPanel(new GridLayout(8, 1));
  
    hslider1 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    panel.add(hslider1);
        
    hslider2 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    hslider2.setMajorTickSpacing(20);
    hslider2.setMinorTickSpacing(5);
    hslider2.setPaintTicks(true);
    panel.add(hslider2);

    hslider3 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    hslider3.setMajorTickSpacing(20);
    hslider3.setMinorTickSpacing(5);
    hslider3.setPaintLabels(true);
    hslider3.setPaintTicks(true);
    panel.add(hslider3);

    hslider4 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    hslider4.putClientProperty("JSlider.isFilled", Boolean.TRUE);
    hslider4.setMajorTickSpacing(20);
    hslider4.setMinorTickSpacing(5);
    hslider4.setPaintLabels(true);
    hslider4.setPaintTicks(true);
    panel.add(hslider4);

    hslider5 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    hslider5.setInverted(true);
    panel.add(hslider5);
        
    hslider6 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    hslider6.setInverted(true);
    hslider6.setMajorTickSpacing(20);
    hslider6.setMinorTickSpacing(5);
    hslider6.setPaintTicks(true);
    panel.add(hslider6);

    hslider7 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    hslider7.setInverted(true);
    hslider7.setMajorTickSpacing(20);
    hslider7.setMinorTickSpacing(5);
    hslider7.setPaintLabels(true);
    hslider7.setPaintTicks(true);
    panel.add(hslider7);

    hslider8 = new JSlider(JSlider.HORIZONTAL, 0, 100, 35);
    hslider8.putClientProperty("JSlider.isFilled", Boolean.TRUE);
    hslider8.setInverted(true);
    hslider8.setMajorTickSpacing(20);
    hslider8.setMinorTickSpacing(5);
    hslider8.setPaintLabels(true);
    hslider8.setPaintTicks(true);
    panel.add(hslider8);

    return panel;
  }
    
  private JPanel createVerticalPanel() 
  {
    JPanel panel = new JPanel(new GridLayout(1, 8));
  
    vslider1 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    panel.add(vslider1);
        
    vslider2 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    vslider2.setMajorTickSpacing(20);
    vslider2.setMinorTickSpacing(5);
    vslider2.setPaintTicks(true);
    panel.add(vslider2);

    vslider3 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    vslider3.setMajorTickSpacing(20);
    vslider3.setMinorTickSpacing(5);
    vslider3.setPaintLabels(true);
    vslider3.setPaintTicks(true);
    panel.add(vslider3);

    vslider4 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    vslider4.putClientProperty("JSlider.isFilled", Boolean.TRUE);
    vslider4.setMajorTickSpacing(20);
    vslider4.setMinorTickSpacing(5);
    vslider4.setPaintLabels(true);
    vslider4.setPaintTicks(true);
    panel.add(vslider4);

    vslider5 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    vslider5.setInverted(true);
    panel.add(vslider5);
        
    vslider6 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    vslider6.setInverted(true);
    vslider6.setMajorTickSpacing(20);
    vslider6.setMinorTickSpacing(5);
    vslider6.setPaintTicks(true);
    panel.add(vslider6);

    vslider7 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    vslider7.setInverted(true);
    vslider7.setMajorTickSpacing(20);
    vslider7.setMinorTickSpacing(5);
    vslider7.setPaintLabels(true);
    vslider7.setPaintTicks(true);
    panel.add(vslider7);

    vslider8 = new JSlider(JSlider.VERTICAL, 0, 100, 35);
    vslider8.putClientProperty("JSlider.isFilled", Boolean.TRUE);
    vslider8.setInverted(true);
    vslider8.setMajorTickSpacing(20);
    vslider8.setMinorTickSpacing(5);
    vslider8.setPaintLabels(true);
    vslider8.setPaintTicks(true);
    panel.add(vslider8);
    return panel;
  }
    
  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
    else if (e.getActionCommand().equals("TOGGLE_ENABLED"))
    {
      boolean enabled = enabledCheckBox.isSelected();
      hslider1.setEnabled(enabled);
      hslider2.setEnabled(enabled);
      hslider3.setEnabled(enabled);
      hslider4.setEnabled(enabled);
      hslider5.setEnabled(enabled);
      hslider6.setEnabled(enabled);
      hslider7.setEnabled(enabled);
      hslider8.setEnabled(enabled);
      vslider1.setEnabled(enabled);
      vslider2.setEnabled(enabled);
      vslider3.setEnabled(enabled);
      vslider4.setEnabled(enabled);
      vslider5.setEnabled(enabled);
      vslider6.setEnabled(enabled);
      vslider7.setEnabled(enabled);
      vslider8.setEnabled(enabled);
    }
  }
  public static void main(String[] args) 
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         SliderDemo app = new SliderDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("Slider Demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }


  /**
   * Returns a DemoFactory that creates a SliderDemo.
   *
   * @return a DemoFactory that creates a SliderDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new SliderDemo();
      }
    };
  }
}
