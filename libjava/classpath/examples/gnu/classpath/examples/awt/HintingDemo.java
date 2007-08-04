/* Demo.java -- Shows examples of AWT components
   Copyright (C) 1998, 1999, 2002, 2004, 2006 Free Software Foundation, Inc.

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
02110-1301 USA. */

package gnu.classpath.examples.awt;

import gnu.java.awt.font.FontDelegate;
import gnu.java.awt.font.GNUGlyphVector;
import gnu.java.awt.font.opentype.OpenTypeFontFactory;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.font.FontRenderContext;
import java.io.File;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.text.StringCharacterIterator;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class HintingDemo extends JFrame {

  FontDelegate font;
  GNUGlyphVector glyph;
  GlyphPreview glyphPreview;
  HintPanel hintPanel;
  StringViewer stringViewer;
  Chooser chooser;
  char character;
  Options options;
  boolean antiAlias;
  boolean showGrid;
  boolean showOriginal;
  boolean showHinted;
  int flags;

  class StringViewer extends JPanel
  implements ActionListener
  {
    JTextField input;
    GNUGlyphVector gv;
    Viewer viewer;
    StringViewer()
    {
      setLayout(new GridLayout(0, 1));
      setBorder(new TitledBorder("Use this field to render complete strings"));
      input = new JTextField();
      input.addActionListener(this);
      add(input);
      viewer = new Viewer();
      add(viewer);
    }

    public void actionPerformed(ActionEvent event)
    {
      refresh();
    }

    void refresh()
    {
      gv = (GNUGlyphVector)
      font.createGlyphVector(new Font("Dialog", 0, 12),
      new FontRenderContext(null, false, false),
      new StringCharacterIterator(input.getText()));
      viewer.repaint();
    }

    class Viewer extends JPanel
    {
      protected void paintComponent(Graphics g)
      {
        if (gv != null && g instanceof Graphics2D)
          {
            Graphics2D g2d = (Graphics2D) g;
            if (antiAlias)
              {
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                     RenderingHints.VALUE_ANTIALIAS_ON);
              }
            else
              {
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                     RenderingHints.VALUE_ANTIALIAS_OFF);
              }
            g2d.clearRect(0, 0, getWidth(), getHeight());
            g2d.setColor(Color.BLACK);
            Shape outline = gv.getOutline(0, 0,
                                          flags | FontDelegate.FLAG_FITTED);
            g2d.translate(20, Math.floor(outline.getBounds2D().getHeight()) + 2);
            g2d.fill(outline);
          }
      }
    }
  }

  class HintPanel extends JPanel
  {

    HintPanel()
    {
      setBorder(new TitledBorder("Detailed glyph view"));
    }
    protected void paintComponent(Graphics g)
    {
      if (glyph != null && g instanceof Graphics2D)
        {
          Graphics2D g2d = (Graphics2D) g.create();
          Insets i = getInsets();
          g2d.clearRect(i.left, i.top, getWidth() - i.left - i.right,
                        getHeight() - i.top - i.bottom);
          if (showGrid)
            {
              g2d.setColor(Color.GRAY);
              for (int x = 20; x < getWidth(); x += 20)
                {
                  g2d.drawLine(x, i.top, x, getHeight() - i.top - i.bottom);
                }
              for (int y = 20; y < getHeight(); y += 20)
                {
                  g2d.drawLine(i.left, y, getWidth() - i.left - i.right, y);
                }
            }
//          g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
//                               RenderingHints.VALUE_ANTIALIAS_ON);
          g2d.translate(40, 300);
          g2d.scale(20., 20.);
          g2d.setStroke(new BasicStroke((float) (1/10.)));
          if (showOriginal)
            {
              g2d.setColor(Color.RED);
              g2d.draw(glyph.getOutline(0, 0,
                                        flags & ~FontDelegate.FLAG_FITTED));
            }
          if (showHinted)
            {
              g2d.setColor(Color.RED);
              g2d.draw(glyph.getOutline(0, 0,
                                        flags | FontDelegate.FLAG_FITTED));
            }
      }
    }

  }

  class GlyphPreview extends JPanel
  {
    protected void paintComponent(Graphics g)
    {
      if (glyph != null && g instanceof Graphics2D)
        {
          Graphics2D g2d = (Graphics2D) g;
          if (antiAlias)
            {
              g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                   RenderingHints.VALUE_ANTIALIAS_ON);
            }
          else
            {
              g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                   RenderingHints.VALUE_ANTIALIAS_OFF);
            }
          g2d.clearRect(0, 0, getWidth(), getHeight());
          g2d.setColor(Color.BLACK);
          Shape outline = glyph.getOutline(0, 0,
                                           flags | FontDelegate.FLAG_FITTED);
          g2d.translate(20, outline.getBounds2D().getHeight() + 2);
          g2d.fill(outline);
        }
    }

  }

  HintingDemo()
  {
    File file = new File("/usr/share/fonts/truetype/freefont/FreeSans.ttf");
    loadFont(file);
    setLayout(new BorderLayout());
    chooser = new Chooser();
    add(chooser, BorderLayout.NORTH);
    hintPanel = new HintPanel();
    character = 'A';
    add(hintPanel);

    options = new Options();
    add(options, BorderLayout.EAST);

    stringViewer = new StringViewer();
    add(stringViewer, BorderLayout.SOUTH);
    refresh();

    JMenuBar mb = new JMenuBar();
    setJMenuBar(mb);
    JMenu fileMenu = new JMenu("File");
    mb.add(fileMenu);
    JMenuItem loadFont = new JMenuItem("Load font");
    loadFont.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent ev)
      {
        JFileChooser fc = new JFileChooser()
        {
          public boolean accept(File f)
          {
            return f.isDirectory() || f.getName().endsWith(".ttf");
          }
        };
        int status = fc.showOpenDialog(HintingDemo.this);
        if (status == JFileChooser.APPROVE_OPTION)
          {
            File file = fc.getSelectedFile();
            loadFont(file);
          }
      }
    });
    fileMenu.add(loadFont);
  }

  void refresh()
  {
    if (chooser != null)
      chooser.refresh();
    if (glyphPreview != null)
      glyphPreview.repaint();
    if (hintPanel != null)
      hintPanel.repaint();
    if (stringViewer != null)
      stringViewer.refresh();
  }

  class Options extends JPanel
    implements ActionListener
  {
    JCheckBox antiAliasOpt;
    JCheckBox showGridOpt;
    JCheckBox showOriginalOpt;
    JCheckBox showHintedOpt;
    JCheckBox hintHorizontalOpt;
    JCheckBox hintVerticalOpt;
    JCheckBox hintEdgeOpt;
    JCheckBox hintStrongOpt;
    JCheckBox hintWeakOpt;
    Options()
    {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      setBorder(new TitledBorder("Hinting options"));
      antiAliasOpt = new JCheckBox("Antialias");
      antiAliasOpt.setSelected(true);
      antiAliasOpt.addActionListener(this);
      add(antiAliasOpt);
      showGridOpt = new JCheckBox("Show grid");
      showGridOpt.setSelected(true);
      showGridOpt.addActionListener(this);
      add(showGridOpt);
      showOriginalOpt = new JCheckBox("Show original");
      showOriginalOpt.setSelected(true);
      showOriginalOpt.addActionListener(this);
      add(showOriginalOpt);
      showHintedOpt = new JCheckBox("Show hinted");
      showHintedOpt.setSelected(true);
      showHintedOpt.addActionListener(this);
      add(showHintedOpt);
      hintHorizontalOpt = new JCheckBox("Hint horizontal");
      hintHorizontalOpt.setSelected(true);
      hintHorizontalOpt.addActionListener(this);
      add(hintHorizontalOpt);
      hintVerticalOpt = new JCheckBox("Hint vertical");
      hintVerticalOpt.setSelected(true);
      hintVerticalOpt.addActionListener(this);
      add(hintVerticalOpt);
      hintEdgeOpt = new JCheckBox("Hint edge points");
      hintEdgeOpt.setSelected(true);
      hintEdgeOpt.addActionListener(this);
      add(hintEdgeOpt);
      hintStrongOpt = new JCheckBox("Hint strong points");
      hintStrongOpt.setSelected(true);
      hintStrongOpt.addActionListener(this);
      add(hintStrongOpt);
      hintWeakOpt = new JCheckBox("Hint weak points");
      hintWeakOpt.setSelected(true);
      hintWeakOpt.addActionListener(this);
      add(hintWeakOpt);
      sync();
    }

    void sync()
    {
      antiAlias = antiAliasOpt.isSelected();
      showGrid = showGridOpt.isSelected();
      showOriginal = showOriginalOpt.isSelected();
      showHinted = showHintedOpt.isSelected();
      if (hintHorizontalOpt.isSelected())
        flags &= ~FontDelegate.FLAG_NO_HINT_HORIZONTAL;
      else
        flags |= FontDelegate.FLAG_NO_HINT_HORIZONTAL;
      if (hintVerticalOpt.isSelected())
        flags &= ~FontDelegate.FLAG_NO_HINT_VERTICAL;
      else
        flags |= FontDelegate.FLAG_NO_HINT_VERTICAL;
      if (hintEdgeOpt.isSelected())
        flags &= ~FontDelegate.FLAG_NO_HINT_EDGE_POINTS;
      else
        flags |= FontDelegate.FLAG_NO_HINT_EDGE_POINTS;
      if (hintStrongOpt.isSelected())
        flags &= ~FontDelegate.FLAG_NO_HINT_STRONG_POINTS;
      else
        flags |= FontDelegate.FLAG_NO_HINT_STRONG_POINTS;
      if (hintWeakOpt.isSelected())
        flags &= ~FontDelegate.FLAG_NO_HINT_WEAK_POINTS;
      else
        flags |= FontDelegate.FLAG_NO_HINT_WEAK_POINTS;
        
      refresh();
    }

    public void actionPerformed(ActionEvent event)
    {
      sync();
    }
  }

  class Chooser extends JPanel
  {
    JSpinner spin;
    Chooser()
    {
      setLayout(new GridLayout(1, 0));
      setBorder(new TitledBorder("Choose and preview the character to render"));
      spin = new JSpinner();
      spin.addChangeListener(new ChangeListener()
      {

        public void stateChanged(ChangeEvent event)
        {
          int val = ((Integer) spin.getValue()).intValue();
          setGlyph((char) val);
        }
      });
      add(spin);
      glyphPreview = new GlyphPreview();
      add(glyphPreview);
    }
    void refresh()
    {
      spin.setValue(new Integer(character));
      repaint();
    }
  }

  private void loadFont(File file)
  {
    try
      {
        RandomAccessFile raf = new RandomAccessFile(file, "r");
        FileChannel chan = raf.getChannel();
        ByteBuffer buf = chan.map(FileChannel.MapMode.READ_ONLY, 0, raf.length());
        FontDelegate[] fonts = OpenTypeFontFactory.createFonts(buf);
        font = fonts[0];
        setGlyph(character);
        refresh();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  void setGlyph(char ch)
  {
    character = ch;
    glyph = (GNUGlyphVector)
            font.createGlyphVector(new Font("Dialog", 0, 12),
            new FontRenderContext(null, false, false),
            new StringCharacterIterator(new String(new char[]{ch})));
    refresh();
  }

  public static void main(String[] args) {
    HintingDemo f = new HintingDemo();
    f.setSize(500, 500);
    f.setVisible(true);
  }
}
