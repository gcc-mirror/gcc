/* TextFieldDemo.java -- An example showing various textfields in Swing.
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.View;
import javax.swing.text.LayeredHighlighter.LayerPainter;

/**
 * A simple textfield demo showing various textfields in different states.
 */
public class TextFieldDemo 
  extends JPanel
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
  
  static class DemoHighlightPainter
  extends LayerPainter
  {

    static DemoHighlightPainter INSTANCE = new DemoHighlightPainter();


    static Color[] colors = { Color.BLUE, Color.CYAN, Color.GRAY, Color.GREEN,
                         Color.MAGENTA, Color.ORANGE, Color.PINK,
                         Color.ORANGE, Color.RED, Color.BLUE, Color.YELLOW };


    public DemoHighlightPainter()
    {
      super();
    }

    private void paintHighlight(Graphics g, Rectangle rect)
    {
      g.fillRect(rect.x, rect.y, rect.width, rect.height);
    }

    public void paint(Graphics g, int p0, int p1, Shape bounds, JTextComponent t)
    {
      try
        {

          for (int i = p0; i < p1; i++)
            {
              Rectangle r = t.modelToView(i);
              Point l1 = t.modelToView(i + 1).getLocation();

              g.setColor(colors[(int) (Math.random() * colors.length)]);
              g.fillOval(r.x, r.y, l1.x - r.x, r.height);
            }
        }
      catch (BadLocationException ble)
        {
        }
    }

    public Shape paintLayer(Graphics g, int p0, int p1, Shape bounds,
                            JTextComponent c, View view)
    {
      paint(g, p0, p1, bounds, c);

      return bounds;
    }
    
  }

  /**
   * The left aligned textfields and state buttons.
   */
  Compound compound1;
  
  /**
   * The right aligned textfields and state buttons.
   */
  Compound compound2;

  /**
   * The centered textfields and state buttons.
   */
  Compound compound3;

  /**
   * The custom colored textfields and state buttons.
   */
  Compound compound4;
  Compound compound5;

  /**
   * Some miscellaneous textfield demos.
   */
  Compound compound6;
  
  /**
   * Some textfields with custom borders.
   */
  Compound compound7;

  /**
   * Creates a new demo instance.
   */
  public TextFieldDemo() 
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
    JPanel panel = new JPanel(new GridLayout(7, 1));
    panel.add(createLeftAlignedPanel());
    panel.add(createRightAlignedPanel());
    panel.add(createCenteredPanel());
    panel.add(createCustomColorPanel1());
    panel.add(createCustomColorPanel2());
    panel.add(createCustomBordersPanel());
    panel.add(createMiscPanel());
    
    // Put everything in a scroll pane to make it neccessary
    // to reach the bottom inner panels if the screen is to small.
    add(new JScrollPane(panel));
  }
    
  private JPanel createLeftAlignedPanel() 
  {
    compound1 = createTextFieldCompound("Left aligned", 1);

    compound1.setupTextfields("Hello World!",
                             JTextField.LEFT,
                             new Font[] { new Font("Dialog", Font.PLAIN, 8),
                                          new Font("Dialog", Font.ITALIC, 12),
                                          new Font("Dialog", Font.BOLD, 14)
                             });
    
    return compound1.panel;
  }
  
  private Compound createTextFieldCompound(String title, int actionCommandNo)
  {
    Compound compound = new Compound();
    compound.panel = new JPanel(new BorderLayout());
    compound.panel.setBorder(BorderFactory.createTitledBorder(title));
    
    compound.textFieldPanel = new JPanel();
    compound.textFieldPanel.setLayout(new BoxLayout(compound.textFieldPanel, BoxLayout.X_AXIS));

    compound.panel.add(compound.textFieldPanel);

    JPanel statePanel = new JPanel();
    statePanel.setLayout(new BoxLayout(statePanel, BoxLayout.Y_AXIS));
    statePanel.add(Box.createVerticalGlue());
    compound.enabled = new JCheckBox("enabled");
    compound.enabled.setSelected(true);
    compound.enabled.addActionListener(this);
    compound.enabled.setActionCommand("ENABLED" + actionCommandNo);
    statePanel.add(compound.enabled);
    compound.editable = new JCheckBox("editable");
    compound.editable.setSelected(true);
    compound.editable.addActionListener(this);
    compound.editable.setActionCommand("EDITABLE" + actionCommandNo);
    statePanel.add(compound.editable);
    statePanel.add(Box.createVerticalGlue());
    compound.panel.add(statePanel, BorderLayout.EAST);

    return compound;
  }

  private JPanel createRightAlignedPanel() 
  {
    compound2 = createTextFieldCompound("Right aligned", 2);

    compound2.setupTextfields("Hello World!",
                              JTextField.RIGHT,
                              new Font[] { new Font("Dialog", Font.PLAIN, 8),
                                           new Font("Dialog", Font.ITALIC, 12),
                                           new Font("Dialog", Font.BOLD, 14)
                              });
    
    return compound2.panel;
  }

  private JPanel createCenteredPanel() 
  {
    compound3 = createTextFieldCompound("Centered", 3);

    compound3.setupTextfields("Hello World!",
                              JTextField.CENTER,
                              new Font[] { new Font("Dialog", Font.PLAIN, 8),
                                           new Font("Dialog", Font.ITALIC, 12),
                                           new Font("Dialog", Font.BOLD, 14)
                              });
        
    return compound3.panel;
  }

  private JPanel createCustomColorPanel1()
  {
    compound4 = createTextFieldCompound("Custom colors I", 4);

    compound4.textfield1 = new JTextField("custom foreground");
    compound4.textfield1.setForeground(Color.RED);
    compound4.textFieldPanel.add(compound4.textfield1);

    compound4.textfield2 = new JTextField("custom background");
    compound4.textfield2.setBackground(Color.YELLOW);
    compound4.textFieldPanel.add(compound4.textfield2);

    compound4.textfield3 = new JTextField("custom foreground and background");
    compound4.textfield3.setForeground(Color.RED);
    compound4.textfield3.setBackground(Color.YELLOW);
    compound4.textFieldPanel.add(compound4.textfield3);
    
    return compound4.panel;
    
  }
  
  private JPanel createCustomColorPanel2()
  {
    compound5 = createTextFieldCompound("Custom colors II", 5);

    compound5.textfield1 = new JTextField("custom disabled textcolor");
    compound5.textfield1.setDisabledTextColor(Color.BLUE);
    compound5.textFieldPanel.add(compound5.textfield1);

    compound5.textfield2 = new JTextField("custom selected text color");
    compound5.textfield2.setSelectedTextColor(Color.RED);
    compound5.textFieldPanel.add(compound5.textfield2);

    compound5.textfield3 = new JTextField("custom selection color");
    compound5.textfield3.setSelectionColor(Color.BLACK);
    compound5.textFieldPanel.add(compound5.textfield3);
    
    return compound5.panel;
    
  }

  private JPanel createMiscPanel() 
  {
    compound6 = createTextFieldCompound("Miscellaneous", 6);

    compound6.textfield1 = new JTextField("Custom Caret");
    compound6.textfield1.setCaret(new CornerCaret());
    compound6.textFieldPanel.add(compound6.textfield1);

    compound6.textfield2 = new JTextField("Custom Caret color");
    compound6.textfield2.setForeground(Color.LIGHT_GRAY);
    compound6.textfield2.setBackground(Color.BLACK);
    compound6.textfield2.setSelectedTextColor(Color.BLACK);
    compound6.textfield2.setCaretColor(Color.WHITE);
    compound6.textfield2.setSelectionColor(Color.DARK_GRAY);
    compound6.textFieldPanel.add(compound6.textfield2);

    compound6.textfield3 = new JTextField("Custom highlighter");
    compound6.textfield3.setCaret(new DefaultCaret()
    {
      public Highlighter.HighlightPainter getSelectionPainter()
      {
        return DemoHighlightPainter.INSTANCE;
      }
    });
    compound6.textFieldPanel.add(compound6.textfield3);

    return compound6.panel;
  }
  
  private JPanel createCustomBordersPanel() 
  {
    compound7 = createTextFieldCompound("Custom borders", 7);

    compound7.textfield1 = new JTextField("red 5 pixel lineborder");
    compound7.textfield1.setBorder(new LineBorder(Color.RED, 5));
    compound7.textFieldPanel.add(compound7.textfield1);

    compound7.textfield2 = new JTextField("complex irregular border");

    CompoundBorder innerCompound = new CompoundBorder(new EmptyBorder(5, 40, 15, 10), new LineBorder(Color.BLACK));
    CompoundBorder outerCompound = new CompoundBorder(new LineBorder(Color.BLACK), innerCompound);
    compound7.textfield2.setBorder(outerCompound);
    compound7.textFieldPanel.add(compound7.textfield2);

    compound7.textfield3 = new JTextField("a titled border", 10);
    compound7.textfield3.setBorder(new TitledBorder(null, "Freak Out Border", TitledBorder.CENTER, TitledBorder.LEFT));
    compound7.textFieldPanel.add(compound7.textfield3);

    return compound7.panel;
  }

  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("CLOSE"))
      {
        System.exit(0);
      }
    else if (e.getActionCommand().equals("ENABLED1"))
      {
        boolean enabled = compound1.enabled.isSelected();
        compound1.textfield1.setEnabled(enabled);
        compound1.textfield2.setEnabled(enabled);
        compound1.textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE1"))
      {
        boolean editable = compound1.editable.isSelected();
        compound1.textfield1.setEditable(editable);
        compound1.textfield2.setEditable(editable);
        compound1.textfield3.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED2"))
      {
        boolean enabled = compound2.enabled.isSelected();
        compound2.textfield1.setEnabled(enabled);
        compound2.textfield2.setEnabled(enabled);
        compound2.textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE2"))
      {
        boolean editable = compound2.editable.isSelected();
        compound2.textfield1.setEditable(editable);
        compound2.textfield2.setEditable(editable);
        compound2.textfield3.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED3"))
      {
        boolean enabled = compound3.enabled.isSelected();
        compound3.textfield1.setEnabled(enabled);
        compound3.textfield2.setEnabled(enabled);
        compound3.textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE3"))
      {
        boolean editable = compound3.editable.isSelected();
        compound3.textfield1.setEditable(editable);
        compound3.textfield2.setEditable(editable);
        compound3.textfield3.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED4"))
      {
        boolean enabled = compound4.enabled.isSelected();
        compound4.textfield1.setEnabled(enabled);
        compound4.textfield2.setEnabled(enabled);
        compound4.textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE4"))
      {
        boolean editable = compound4.editable.isSelected();
        compound4.textfield1.setEditable(editable);
        compound4.textfield2.setEditable(editable);
        compound4.textfield3.setEditable(editable);
      } 
    else if (e.getActionCommand().equals("ENABLED5"))
      {
        boolean enabled = compound5.enabled.isSelected();
        compound5.textfield1.setEnabled(enabled);
        compound5.textfield2.setEnabled(enabled);
        compound5.textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE5"))
      {
        boolean editable = compound5.editable.isSelected();
        compound5.textfield1.setEditable(editable);
        compound5.textfield2.setEditable(editable);
        compound5.textfield3.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED6"))
      {
        boolean enabled = compound6.enabled.isSelected();
        compound6.textfield1.setEnabled(enabled);
        compound6.textfield2.setEnabled(enabled);
        compound6.textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE6"))
      {
        boolean editable = compound6.editable.isSelected();
        compound6.textfield1.setEditable(editable);
        compound6.textfield2.setEditable(editable);
        compound6.textfield3.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED7"))
      {
        boolean enabled = compound7.enabled.isSelected();
        compound7.textfield1.setEnabled(enabled);
        compound7.textfield2.setEnabled(enabled);
        compound7.textfield3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE7"))
      {
        boolean editable = compound7.editable.isSelected();
        compound7.textfield1.setEditable(editable);
        compound7.textfield2.setEditable(editable);
        compound7.textfield3.setEditable(editable);
      }
  }

  public static void main(String[] args) 
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         TextFieldDemo app = new TextFieldDemo();
         app.initFrameContent();
         JFrame frame = new JFrame("TextField demo");
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a TextFieldDemo.
   *
   * @return a DemoFactory that creates a TextFieldDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new TextFieldDemo();
      }
    };
  }

  static class Compound
  {
    JTextField textfield1;
    JTextField textfield2;  
    JTextField textfield3;
    JCheckBox enabled;
    JCheckBox editable;
    JPanel textFieldPanel;
    JPanel panel;
    
    /** Creates and initializes the textfields with the same text and
     * alignment but with a different font.
     * 
     * @param title The text for the textfields.
     * @param align The alignment for the textfields.
     * @param fonts The fonts to be used for the textfields.
     */
    void setupTextfields(String title, int align, Font[] fonts)
    {
      textfield1 = new JTextField(title);
      textfield1.setHorizontalAlignment(align);
      textfield1.setFont(fonts[0]);
      textFieldPanel.add(textfield1);

      textfield2 = new JTextField(title);
      textfield2.setHorizontalAlignment(align);
      textfield2.setFont(fonts[1]);
      textFieldPanel.add(textfield2);
      
      textfield3 = new JTextField(title);
      textfield3.setHorizontalAlignment(align);
      textfield3.setFont(fonts[2]);
      textFieldPanel.add(textfield3);
    }
    
  }
  
}
