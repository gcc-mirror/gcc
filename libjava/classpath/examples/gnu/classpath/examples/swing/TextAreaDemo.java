/* TextAreaDemo.java -- An example showing various textareas in Swing.
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
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.View;
import javax.swing.text.LayeredHighlighter.LayerPainter;

/**
 * A simple textare demo showing various textareas in different states.
 */
public class TextAreaDemo
    extends JPanel
    implements ActionListener
{

  /**
   * A custom caret for demonstration purposes. This class is inspired by the
   * CornerCaret from the OReilly Swing book.
   * 
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class CornerCaret
      extends DefaultCaret
  {
    public CornerCaret()
    {
      super();
      setBlinkRate(500);
    }

    protected synchronized void damage(Rectangle r)
    {
      if (r == null)
        return;
      x = r.x;
      y = r.y + (r.height * 4 / 5 - 3);
      width = 5;
      height = 5;
      repaint();
    }

    public void paint(Graphics g)
    {
      JTextComponent comp = getComponent();
      if (comp == null)
        return;
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
      if (r == null)
        return;
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
   * The non wrapping text areas and state buttons.
   */
  JTextArea textarea1;

  JTextArea textarea2;

  JTextArea textarea3;

  JCheckBox enabled1;

  JCheckBox editable1;

  JPanel panel1;

  /**
   * The char wrapping textareas and state buttons.
   */
  JTextArea textarea4;

  JTextArea textarea5;

  JTextArea textarea6;

  JCheckBox enabled2;

  JCheckBox editable2;

  /**
   * The word wrapping textareas and state buttons.
   */
  JTextArea textarea7;

  JTextArea textarea8;

  JTextArea textarea9;

  JCheckBox enabled3;

  JCheckBox editable3;

  /**
   * The custom colored textareas and state buttons.
   */
  JTextArea textarea10;

  JTextArea textarea11;

  JTextArea textarea12;

  JTextArea textarea13;

  JTextArea textarea14;

  JTextArea textarea14b;

  JCheckBox enabled4;

  JCheckBox editable4;

  /**
   * Some miscellaneous textarea demos.
   */
  JTextArea textarea15;

  JTextArea textarea16;

  JTextArea textarea17;

  JCheckBox enabled5;

  JCheckBox editable5;

  /**
   * Creates a new demo instance.
   */
  public TextAreaDemo()
  {
    super();
    createContent();
  }

  /**
   * When the demo is run independently, the frame is displayed, so we should
   * initialise the content panel (including the demo content and a close
   * button). But when the demo is run as part of the Swing activity board, only
   * the demo content panel is used, the frame itself is never displayed, so we
   * can avoid this step.
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
   * Returns a panel with the demo content. The panel uses a BorderLayout(), and
   * the BorderLayout.SOUTH area is empty, to allow callers to add controls to
   * the bottom of the panel if they want to (a close button is added if this
   * demo is being run as a standalone demo).
   */
  private void createContent()
  {
    setLayout(new BorderLayout());
    JTabbedPane tabPane = new JTabbedPane();
    tabPane.addTab("Non-wrap", createNonWrapPanel());
    tabPane.addTab("Char-wrap", createCharWrapPanel());
    tabPane.addTab("Word-wrap", createWordWrapPanel());
    tabPane.addTab("Custom colors", createCustomColoredPanel());
    tabPane.addTab("Misc", createMiscPanel());
    add(tabPane);
  }

  private JPanel createNonWrapPanel()
  {
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder("Not wrapping"));

    panel1 = new JPanel(new GridLayout(2, 2));

    textarea1 = new JTextArea("Hello World!");
    textarea1.setFont(new Font("Dialog", Font.PLAIN, 8));
    panel1.add(new JScrollPane(textarea1));

    textarea2 = new JTextArea("Hello World!");
    textarea2.setFont(new Font("Dialog", Font.ITALIC, 12));
    panel1.add(new JScrollPane(textarea2));

    textarea3 = new JTextArea("Hello World!");
    textarea3.setFont(new Font("Dialog", Font.BOLD, 14));
    panel1.add(new JScrollPane(textarea3));

    panel.add(panel1);

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

  private JPanel createCharWrapPanel()
  {
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder("Wrap at char bounds"));

    JPanel innerPanel = new JPanel(new GridLayout(2, 2));

    textarea4 = new JTextArea("Hello World!");
    textarea4.setLineWrap(true);
    textarea4.setFont(new Font("Dialog", Font.PLAIN, 8));
    innerPanel.add(new JScrollPane(textarea4));

    textarea5 = new JTextArea("Hello World!");
    textarea5.setLineWrap(true);
    textarea5.setFont(new Font("Dialog", Font.ITALIC, 12));
    innerPanel.add(new JScrollPane(textarea5));

    textarea6 = new JTextArea("Hello World!");
    textarea6.setLineWrap(true);
    textarea6.setFont(new Font("Dialog", Font.BOLD, 14));
    innerPanel.add(new JScrollPane(textarea6));

    panel.add(innerPanel);

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

  private JPanel createWordWrapPanel()
  {
    JPanel panel = new JPanel(new BorderLayout());
    panel.setBorder(BorderFactory.createTitledBorder("Wrap at word bounds"));

    JPanel innerPanel = new JPanel(new GridLayout(2, 2));

    textarea7 = new JTextArea("Hello World!");
    textarea7.setWrapStyleWord(true);
    textarea7.setLineWrap(true);
    textarea7.setFont(new Font("Dialog", Font.PLAIN, 8));
    innerPanel.add(new JScrollPane(textarea7));

    textarea8 = new JTextArea("Hello World!");
    textarea8.setWrapStyleWord(true);
    textarea8.setLineWrap(true);
    textarea8.setFont(new Font("Dialog", Font.ITALIC, 12));
    innerPanel.add(new JScrollPane(textarea8));

    textarea9 = new JTextArea("Hello World!");
    textarea9.setWrapStyleWord(true);
    textarea9.setLineWrap(true);
    textarea9.setFont(new Font("Dialog", Font.BOLD, 14));
    innerPanel.add(new JScrollPane(textarea9));

    panel.add(innerPanel);

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

    JPanel innerPanel = new JPanel(new GridLayout(3, 2));
    panel.setBorder(BorderFactory.createTitledBorder("Custom colors"));

    textarea10 = new JTextArea("custom foreground", 10, 15);
    textarea10.setForeground(Color.GREEN);
    innerPanel.add(new JScrollPane(textarea10));

    textarea11 = new JTextArea("custom background", 10, 15);
    textarea11.setBackground(Color.YELLOW);
    innerPanel.add(new JScrollPane(textarea11));

    textarea12 = new JTextArea("custom disabled textcolor", 10, 15);
    textarea12.setDisabledTextColor(Color.BLUE);
    innerPanel.add(new JScrollPane(textarea12));

    textarea13 = new JTextArea("custom selected text color", 10, 15);
    textarea13.setSelectedTextColor(Color.RED);
    innerPanel.add(new JScrollPane(textarea13));

    textarea14 = new JTextArea("custom selection color", 10, 15);
    textarea14.setSelectionColor(Color.RED);
    innerPanel.add(new JScrollPane(textarea14));

    textarea14b = new JTextArea("custom selection and selected text color", 10, 15);
    textarea14b.setSelectedTextColor(Color.WHITE);
    textarea14b.setSelectionColor(Color.BLACK);
    innerPanel.add(new JScrollPane(textarea14b));

    panel.add(innerPanel);

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
    panel.setBorder(BorderFactory.createTitledBorder("Miscellaneous"));

    JPanel innerPanel = new JPanel(new GridLayout(2, 2));

    textarea15 = new JTextArea("Custom Caret");
    textarea15.setCaret(new CornerCaret());
    innerPanel.add(new JScrollPane(textarea15));

    textarea16 = new JTextArea("Custom Caret color");
    textarea16.setCaretColor(Color.MAGENTA);
    innerPanel.add(new JScrollPane(textarea16));

    textarea16 = new JTextArea("Custom Selection painter");
    textarea16.setFont(new Font("Dialog", Font.PLAIN, 24));
    textarea16.setCaret(new DefaultCaret()
    {
      public Highlighter.HighlightPainter getSelectionPainter()
      {
        return DemoHighlightPainter.INSTANCE;
      }
    });
    
    innerPanel.add(new JScrollPane(textarea16));

    panel.add(innerPanel);

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
        textarea1.setEnabled(enabled);
        textarea2.setEnabled(enabled);
        textarea3.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE1"))
      {
        boolean editable = editable1.isSelected();
        textarea1.setEditable(editable);
        textarea2.setEditable(editable);
        textarea3.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED2"))
      {
        boolean enabled = enabled2.isSelected();
        textarea4.setEnabled(enabled);
        textarea5.setEnabled(enabled);
        textarea6.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE2"))
      {
        boolean editable = editable2.isSelected();
        textarea4.setEditable(editable);
        textarea5.setEditable(editable);
        textarea6.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED3"))
      {
        boolean enabled = enabled3.isSelected();
        textarea7.setEnabled(enabled);
        textarea8.setEnabled(enabled);
        textarea9.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE3"))
      {
        boolean editable = editable3.isSelected();
        textarea7.setEditable(editable);
        textarea8.setEditable(editable);
        textarea9.setEditable(editable);
      }
    else if (e.getActionCommand().equals("ENABLED4"))
      {
        boolean enabled = enabled4.isSelected();
        textarea10.setEnabled(enabled);
        textarea11.setEnabled(enabled);
        textarea12.setEnabled(enabled);
        textarea13.setEnabled(enabled);
        textarea14.setEnabled(enabled);
        textarea14b.setEnabled(enabled);
      }
    else if (e.getActionCommand().equals("EDITABLE4"))
      {
        boolean editable = editable4.isSelected();
        textarea10.setEditable(editable);
        textarea11.setEditable(editable);
        textarea12.setEditable(editable);
        textarea13.setEditable(editable);
        textarea14.setEditable(editable);
        textarea14b.setEditable(editable);
      }
  }

  public static void main(String[] args)
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         TextAreaDemo app = new TextAreaDemo();
         app.initFrameContent();
         JFrame frame = new JFrame();
         frame.getContentPane().add(app);
         frame.pack();
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a TextAreaDemo.
   *
   * @return a DemoFactory that creates a TextAreaDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new TextAreaDemo();
      }
    };
  }
}
