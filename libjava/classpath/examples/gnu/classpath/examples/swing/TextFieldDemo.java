/* TextFieldDemo.java -- An example showing various textfields in Swing.
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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.JTextComponent;

/**
 * A simple textfield demo showing various textfields in different states.
 */
public class TextFieldDemo 
  extends JFrame
  implements ActionListener
{

  /**
   * A custom caret for demonstration purposes. This class is inspired by the
   * CornerCaret from the OReilly Swing book.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class CornerCaret extends DefaultCaret
  {
    public CornerCaret()
    {
      super();
      setBlinkRate(500);
    }

    protected synchronized void damage(Rectangle r)
    {
      if (r == null) return;
      x = r.x;
      y = r.y + (r.height * 4 / 5 - 3);
      width = 5;
      height = 5;
      repaint();
    }

    public void paint(Graphics g)
    {
      JTextComponent comp = getComponent();
      if (comp == null) return;
      int dot = getDot();
      Rectangle r = null;
      try
        {
          r = comp.modelToView(dot);
        }
      catch (BadLocationException e)
        {
          return;
        }
      if (r == null) return;
      int dist = r.height * 4 / 5 - 3;
      if ((x != r.x) || (y != r.y + dist))
        {
          repaint();
          x = r.x;
          y = r.y + dist;
          width = 5;
          height = 5;
        }
      if (isVisible())
        {
          g.drawLine(r.x, r.y + dist, r.x, r.y + dist + 4);
          g.drawLine(r.x, r.y + dist + 4, r.x + 4, r.y + dist + 4);
        }
    }
  }

  /**
   * The left aligned textfields and state buttons.
   */
  JTextField textfield1;
  JTextField textfield2;  
  JTextField textfield3;
  JCheckBox enabled1;
  JCheckBox editable1;
JPanel textFieldPanel1;
  /**
   * The right aligned textfields and state buttons.
   */
  JTextField textfield4;
  JTextField textfield5;  
  JTextField textfield6;
  JCheckBox enabled2;
  JCheckBox editable2;

  /**
   * The centered textfields and state buttons.
   */
  JTextField textfield7;
  JTextField textfield8;  
  JTextField textfield9;
  JCheckBox enabled3;
  JCheckBox editable3;

  /**
   * The custom colored textfields and state buttons.
   */
  JTextField textfield10;
  JTextField textfield11;  
  JTextField textfield12;
  JTextField textfield13;
  JTextField textfield14;
  JCheckBox enabled4;
  JCheckBox editable4;

  /**
   * Some miscallenous textfield demos.
   */
  JTextField textfield15;
  JTextField textfield16;
  JCheckBox enabled5;
  JCheckBox editable5;

  /**
   * Creates a new demo instance.
   * 
   * @param title  the frame title.
   */
  public TextFieldDemo(String title) 
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
    JPanel panel = new JPanel(new GridLayout(5, 1));
    panel.add(createLeftAlignedPanel());
    panel.add(createRightAlignedPanel());
    panel.add(createCenteredPanel());
    panel.add(createCustomColoredPanel());
    panel.add(createMiscPanel());
    content.add(panel);
    //content.setPreferredSize(new Dimension(400, 300));
    return content;        
  }
    
  private JPanel createLeftAlignedPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder("Left aligned"));
    
    textFieldPanel1 = new JPanel();
    textFieldPanel1.setLayout(new BoxLayout(textFieldPanel1, BoxLayout.X_AXIS));

    textfield1 = new JTextField("Hello World!");
    textfield1.setHorizontalAlignment(JTextField.LEFT);
    textfield1.setFont(new Font("Dialog", Font.PLAIN, 8));
    textFieldPanel1.add(textfield1);

    textfield2 = new JTextField("Hello World!");
    textfield2.setHorizontalAlignment(JTextField.LEFT);
    textfield2.setFont(new Font("Dialog", Font.ITALIC, 12));
    textFieldPanel1.add(textfield2);

    textfield3 = new JTextField("Hello World!");
    textfield3.setHorizontalAlignment(JTextField.LEFT);
    textfield3.setFont(new Font("Dialog", Font.BOLD, 14));
    textFieldPanel1.add(textfield3);

    panel.add(textFieldPanel1);

    JPanel statePanel = new JPanel();
    statePanel.setLayout(new BoxLayout(statePanel, BoxLayout.Y_AXIS));
    statePanel.add(Box.createVerticalGlue());
    enabled1 = new JCheckBox("enabled");
    enabled1.setSelected(true);
    enabled1.addActionListener(this);
    enabled1.setActionCommand("ENABLED1");
    statePanel.add(enabled1);
    editable1 = new JCheckBox("editable");
    editable1.setSelected(true);
    editable1.addActionListener(this);
    editable1.setActionCommand("EDITABLE1");
    statePanel.add(editable1);
    statePanel.add(Box.createVerticalGlue());
    panel.add(statePanel, BorderLayout.EAST);

    return panel;
  }

  private JPanel createRightAlignedPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder("Right aligned"));
    
    JPanel textFieldPanel = new JPanel();
    textFieldPanel.setLayout(new BoxLayout(textFieldPanel, BoxLayout.X_AXIS));

    textfield4 = new JTextField("Hello World!");
    textfield4.setHorizontalAlignment(JTextField.RIGHT);
    textfield4.setFont(new Font("Dialog", Font.PLAIN, 8));
    textFieldPanel.add(textfield4);

    textfield5 = new JTextField("Hello World!");
    textfield5.setHorizontalAlignment(JTextField.RIGHT);
    textfield5.setFont(new Font("Dialog", Font.ITALIC, 12));
    textFieldPanel.add(textfield5);

    textfield6 = new JTextField("Hello World!");
    textfield6.setHorizontalAlignment(JTextField.RIGHT);
    textfield6.setFont(new Font("Dialog", Font.BOLD, 14));
    textFieldPanel.add(textfield6);

    panel.add(textFieldPanel);

    JPanel statePanel = new JPanel();
    statePanel.setLayout(new BoxLayout(statePanel, BoxLayout.Y_AXIS));
    statePanel.add(Box.createVerticalGlue());
    enabled2 = new JCheckBox("enabled");
    enabled2.setSelected(true);
    enabled2.addActionListener(this);
    enabled2.setActionCommand("ENABLED2");
    statePanel.add(enabled2);
    editable2 = new JCheckBox("editable");
    editable2.setSelected(true);
    editable2.addActionListener(this);
    editable2.setActionCommand("EDITABLE2");
    statePanel.add(editable2);
    statePanel.add(Box.createVerticalGlue());
    panel.add(statePanel, BorderLayout.EAST);

    return panel;
  }

  private JPanel createCenteredPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder("Centered"));
    
    JPanel textFieldPanel = new JPanel();
    textFieldPanel.setLayout(new BoxLayout(textFieldPanel, BoxLayout.X_AXIS));

    textfield7 = new JTextField("Hello World!");
    textfield7.setHorizontalAlignment(JTextField.CENTER);
    textfield7.setFont(new Font("Dialog", Font.PLAIN, 8));
    textFieldPanel.add(textfield7);

    textfield8 = new JTextField("Hello World!");
    textfield8.setHorizontalAlignment(JTextField.CENTER);
    textfield8.setFont(new Font("Dialog", Font.ITALIC, 12));
    textFieldPanel.add(textfield8);

    textfield9 = new JTextField("Hello World!");
    textfield9.setHorizontalAlignment(JTextField.CENTER);
    textfield9.setFont(new Font("Dialog", Font.BOLD, 14));
    textFieldPanel.add(textfield9);

    panel.add(textFieldPanel);

    JPanel statePanel = new JPanel();
    statePanel.setLayout(new BoxLayout(statePanel, BoxLayout.Y_AXIS));
    statePanel.add(Box.createVerticalGlue());
    enabled3 = new JCheckBox("enabled");
    enabled3.setSelected(true);
    enabled3.addActionListener(this);
    enabled3.setActionCommand("ENABLED3");
    statePanel.add(enabled3);
    editable3 = new JCheckBox("editable");
    editable3.setSelected(true);
    editable3.addActionListener(this);
    editable3.setActionCommand("EDITABLE3");
    statePanel.add(editable3);
    statePanel.add(Box.createVerticalGlue());
    panel.add(statePanel, BorderLayout.EAST);

    return panel;
  }

  private JPanel createCustomColoredPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    
    JPanel textFieldPanel = new JPanel();
    panel.setBorder(BorderFactory.createTitledBorder("Custom colors"));
    textFieldPanel.setLayout(new BoxLayout(textFieldPanel, BoxLayout.X_AXIS));

    textfield10 = new JTextField("custom foreground");
    textfield10.setForeground(Color.GREEN);
    textFieldPanel.add(textfield10);

    textfield11 = new JTextField("custom background");
    textfield11.setForeground(Color.YELLOW);
    textFieldPanel.add(textfield11);

    textfield12 = new JTextField("custom disabled textcolor");
    textfield12.setDisabledTextColor(Color.BLUE);
    textFieldPanel.add(textfield12);

    textfield13 = new JTextField("custom selected text color");
    textfield13.setSelectedTextColor(Color.RED);
    textFieldPanel.add(textfield13);

    textfield14 = new JTextField("custom selection color");
    textfield14.setSelectionColor(Color.CYAN);
    textFieldPanel.add(textfield14);

    panel.add(textFieldPanel);

    JPanel statePanel = new JPanel();
    statePanel.setLayout(new BoxLayout(statePanel, BoxLayout.Y_AXIS));
    statePanel.add(Box.createVerticalGlue());
    enabled4 = new JCheckBox("enabled");
    enabled4.setSelected(true);
    enabled4.addActionListener(this);
    enabled4.setActionCommand("ENABLED4");
    statePanel.add(enabled4);
    editable4 = new JCheckBox("editable");
    editable4.setSelected(true);
    editable4.addActionListener(this);
    editable4.setActionCommand("EDITABLE4");
    statePanel.add(editable4);
    statePanel.add(Box.createVerticalGlue());
    panel.add(statePanel, BorderLayout.EAST);

    return panel;
  }

  private JPanel createMiscPanel() 
  {
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder("Miscallenous"));
    
    JPanel textFieldPanel = new JPanel();
    textFieldPanel.setLayout(new BoxLayout(textFieldPanel, BoxLayout.X_AXIS));

    textfield15 = new JTextField("Custom Caret");
    textfield15.setCaret(new CornerCaret());
    textFieldPanel.add(textfield15);

    textfield16 = new JTextField("Custom Caret color");
    textfield16.setCaretColor(Color.MAGENTA);
    textFieldPanel.add(textfield16);

    panel.add(textFieldPanel);

    JPanel statePanel = new JPanel();
    statePanel.setLayout(new BoxLayout(statePanel, BoxLayout.Y_AXIS));
    statePanel.add(Box.createVerticalGlue());
    enabled5 = new JCheckBox("enabled");
    enabled5.setSelected(true);
    enabled5.addActionListener(this);
    enabled5.setActionCommand("ENABLED5");
    statePanel.add(enabled5);
    editable5 = new JCheckBox("editable");
    editable5.setSelected(true);
    editable5.addActionListener(this);
    editable5.setActionCommand("EDITABLE5");
    statePanel.add(editable5);
    statePanel.add(Box.createVerticalGlue());
    panel.add(statePanel, BorderLayout.EAST);

    return panel;
  }

  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("CLOSE"))
      {
        System.exit(0);
      }
    else if (e.getActionCommand().equals("ENABLED1"))
      {
        boolean enabled = enabled1.isSelected();
        textfield1.setEnabled(enabled);
        textfield2.setEnabled(enabled);
        textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE1"))
      {
        boolean editable = editable1.isSelected();
        textfield1.setEditable(editable);
        textfield2.setEditable(editable);
        textfield3.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED2"))
      {
        boolean enabled = enabled2.isSelected();
        textfield4.setEnabled(enabled);
        textfield5.setEnabled(enabled);
        textfield6.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE2"))
      {
        boolean editable = editable2.isSelected();
        textfield4.setEditable(editable);
        textfield5.setEditable(editable);
        textfield6.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED3"))
      {
        boolean enabled = enabled3.isSelected();
        textfield7.setEnabled(enabled);
        textfield8.setEnabled(enabled);
        textfield9.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE3"))
      {
        boolean editable = editable3.isSelected();
        textfield7.setEditable(editable);
        textfield8.setEditable(editable);
        textfield9.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED4"))
      {
        boolean enabled = enabled4.isSelected();
        textfield10.setEnabled(enabled);
        textfield11.setEnabled(enabled);
        textfield12.setEnabled(enabled);
        textfield13.setEnabled(enabled);
        textfield14.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE4"))
      {
        boolean editable = editable4.isSelected();
        textfield10.setEditable(editable);
        textfield11.setEditable(editable);
        textfield12.setEditable(editable);
        textfield13.setEditable(editable);
        textfield14.setEditable(editable);
      }
  }

  public static void main(String[] args) 
  {
    TextFieldDemo app = new TextFieldDemo("TextField Demo");
    app.pack();
    app.setVisible(true);
  }

}
