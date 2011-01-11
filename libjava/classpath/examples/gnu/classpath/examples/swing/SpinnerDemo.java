/* SpinnerDemo.java -- An example showing various spinners in Swing.
   Copyright (C) 2006,  Free Software Foundation, Inc.

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
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Calendar;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerListModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;

/**
 * A simple demo showing various spinners in different states.
 */
public class SpinnerDemo
  extends JPanel
  implements ActionListener
{
  private JCheckBox spinnerState1;
  private JSpinner spinner1;
  private JSpinner spinner2;

  private JCheckBox spinnerState2;
  private JSpinner spinner3;
  private JSpinner spinner4;

  private JCheckBox spinnerState3;
  private JSpinner spinner5;
  private JSpinner spinner6;

  /**
   * Creates a new demo instance.
   */
  public SpinnerDemo()
  {
    super();
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
    JPanel panel = new JPanel(new GridLayout(3, 1));
    panel.add(createPanel1());
    panel.add(createPanel2());
    panel.add(createPanel3());
    add(panel);
  }

  private JPanel createPanel1()
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.spinnerState1 = new JCheckBox("Enabled", true);
    this.spinnerState1.setActionCommand("COMBO_STATE1");
    this.spinnerState1.addActionListener(this);
    panel.add(this.spinnerState1, BorderLayout.EAST);

    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder(
        "Number Spinner: "));
    this.spinner1 = new JSpinner(new SpinnerNumberModel(5.0, 0.0, 10.0, 0.5));
    this.spinner2 = new JSpinner(new SpinnerNumberModel(50, 0, 100, 5));
    this.spinner2.setFont(new Font("Dialog", Font.PLAIN, 20));
    controlPanel.add(this.spinner1);
    controlPanel.add(this.spinner2);

    panel.add(controlPanel);

    return panel;
  }

  private JPanel createPanel2()
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.spinnerState2 = new JCheckBox("Enabled", true);
    this.spinnerState2.setActionCommand("COMBO_STATE2");
    this.spinnerState2.addActionListener(this);
    panel.add(this.spinnerState2, BorderLayout.EAST);

    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("Date Spinner: "));
    this.spinner3 = new JSpinner(new SpinnerDateModel(new Date(), null, null,
        Calendar.DATE));

    this.spinner4 = new JSpinner(new SpinnerDateModel(new Date(), null, null,
        Calendar.YEAR));
    this.spinner4.setFont(new Font("Dialog", Font.PLAIN, 20));

    controlPanel.add(this.spinner3);
    controlPanel.add(this.spinner4);

    panel.add(controlPanel);

    return panel;
  }

  private JPanel createPanel3()
  {
    JPanel panel = new JPanel(new BorderLayout());
    this.spinnerState3 = new JCheckBox("Enabled", true);
    this.spinnerState3.setActionCommand("COMBO_STATE3");
    this.spinnerState3.addActionListener(this);
    panel.add(this.spinnerState3, BorderLayout.EAST);

    JPanel controlPanel = new JPanel();
    controlPanel.setBorder(BorderFactory.createTitledBorder("List Spinner: "));
    this.spinner5 = new JSpinner(new SpinnerListModel(new Object[] {"Red",
        "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet"}));

    this.spinner6 = new JSpinner(new SpinnerListModel(new Object[] {"Red",
        "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet"}));
    this.spinner6.setValue("Yellow");
    this.spinner6.setFont(new Font("Dialog", Font.PLAIN, 20));

    controlPanel.add(this.spinner5);
    controlPanel.add(this.spinner6);

    panel.add(controlPanel);

    return panel;
  }

  public void actionPerformed(ActionEvent e)
  {
    if (e.getActionCommand().equals("COMBO_STATE1"))
    {
      spinner1.setEnabled(spinnerState1.isSelected());
      spinner2.setEnabled(spinnerState1.isSelected());
    }
    else if (e.getActionCommand().equals("COMBO_STATE2"))
    {
      spinner3.setEnabled(spinnerState2.isSelected());
      spinner4.setEnabled(spinnerState2.isSelected());
    }
    else if (e.getActionCommand().equals("COMBO_STATE3"))
    {
      spinner5.setEnabled(spinnerState3.isSelected());
      spinner6.setEnabled(spinnerState3.isSelected());
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
         SpinnerDemo app = new SpinnerDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("Spinner Demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a SpinnerDemo.
   *
   * @return a DemoFactory that creates a SpinnerDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new SpinnerDemo();
      }
    };
  }
}
