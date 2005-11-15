/* SwingDemo.java -- An example of using the javax.swing UI.
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.border.*;

import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.OceanTheme;

import java.net.URL;

public class Demo
{
  JFrame frame;
  static Color blueGray = new Color(0xdc, 0xda, 0xd5);

  static
  {
    try
      {
        if (System.getProperty("swing.defaultlaf") == null)
          {
            StringBuffer text = new StringBuffer();
            text.append("You may change the Look and Feel of this\n");
            text.append("Demo by setting the system property\n");
            text.append("-Dswing.defaultlaf=<LAFClassName>\n");
	    text.append("\n");
            text.append("Possible values for <LAFClassName> are:\n");
	    text.append("\n");
            text.append("* javax.swing.plaf.metal.MetalLookAndFeel\n");
            text.append("  the default GNU Classpath L&F\n");
	    text.append("\n");
            text.append("* gnu.classpath.examples.swing.GNULookAndFeel\n");
            text.append("  the GNU Look and Feel\n");
            text.append("  (derived from javax.swing.plaf.basic.BasicLookAndFeel)\n");
	    text.append("\n");
            text.append("MetalLookAndFeel supports different Themes.\n");
	    text.append("DefaultMetalTheme (the default) and OceanTheme (in development)\n");

	    final String DEFAULT = "MetalLookAndFeel (default)";
	    final String OCEAN = "MetalLookAndFeel (Ocean)";
	    final String GNU = "GNULookAndFeel";
	    final String[] lafs = new String[] { DEFAULT, OCEAN, GNU };

	    int laf = JOptionPane.showOptionDialog(null, text /* textPane */,
						   "Look and Feel choice",
						   JOptionPane.OK_OPTION,
						   JOptionPane.QUESTION_MESSAGE,
						   null, lafs, DEFAULT);
        if (laf == 0)
          {
            MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
            UIManager.setLookAndFeel(new MetalLookAndFeel());
          }
	    if (laf == 1)
	      {
	        MetalLookAndFeel.setCurrentTheme(new OceanTheme());
	        UIManager.setLookAndFeel(new MetalLookAndFeel());
	      }
	    else if (laf == 2)
	      UIManager.setLookAndFeel(new GNULookAndFeel());
          }
      }
    catch (UnsupportedLookAndFeelException e)
      {
        System.err.println("Cannot install GNULookAndFeel, exiting");
        System.exit(1);
      }
  }

  static Icon stockIcon(String s)
  {
    return getIcon("/gnu/classpath/examples/icons/stock-" + s + ".png", s);
  }

  static Icon bigStockIcon(String s)
  {
    return getIcon("/gnu/classpath/examples/icons/big-" + s + ".png", s);
  }

  static Icon getIcon(String location, String name)
  {
    URL url = Demo.class.getResource(location);
    if (url == null) System.err.println("WARNING " + location + " not found.");
    return new ImageIcon(url, name);
  }

  static JMenuBar mkMenuBar()
  {
    JMenuBar bar = new JMenuBar();
    
    JMenu file = new JMenu("File");
    JMenu edit = new JMenu("Edit");
    JMenu help = new JMenu("Help");

    file.setMnemonic(KeyEvent.VK_F);
    edit.setMnemonic(KeyEvent.VK_E);
    help.setMnemonic(KeyEvent.VK_H);

    file.add(new JMenuItem("New", stockIcon("new")));
    file.add(new JMenuItem("Open", stockIcon("open")));

    JMenu recent = new JMenu("Recent Files...");
    recent.add(new JMenuItem("war-and-peace.txt"));
    recent.add(new JMenuItem("taming-of-shrew.txt"));
    recent.add(new JMenuItem("sun-also-rises.txt"));
    file.add(recent);
    file.add(new JMenuItem("Save", stockIcon("save")));
    file.add(new JMenuItem("Save as...", stockIcon("save-as")));

    JMenuItem exit = new JMenuItem("Exit", stockIcon("quit"));
    exit.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          System.exit(1);
        }
      });

    file.add(exit);

    edit.add(new JMenuItem("Cut", stockIcon("cut")));
    edit.add(new JMenuItem("Copy", stockIcon("copy")));
    edit.add(new JMenuItem("Paste", stockIcon("paste")));

    JMenu preferences = new JMenu("Preferences...");
    preferences.add(new JCheckBoxMenuItem("Microphone Active",
		    stockIcon("mic")));
    preferences.add(new JCheckBoxMenuItem("Check Spelling",
		    stockIcon("spell-check")));
    preferences.add(new JCheckBoxMenuItem("World Peace"));
    preferences.add(new JSeparator());
    preferences.add(new JRadioButtonMenuItem("Radio Button"));
    edit.add(preferences);

    JMenu examples = new JMenu("Examples");
    new PopUpAction("Buttons",
		    (new ButtonDemo("Button Demo")).createContent(),
		    examples);
    
    new PopUpAction("Toggles",
		    mkToggle("cool and refreshing"),
		    examples);

    new PopUpAction("Checkbox",
		    mkCheckbox("ice cold"),
		    examples);

    new PopUpAction("Radio",
		    mkRadio("delicious"),
		    examples);

    new PopUpAction("Slider",
		    (new SliderDemo("Slider Demo")).createContent(),
		    examples);

    new PopUpAction("ProgressBar",
                    ProgressBarDemo.createContent(),
                    examples);

    new PopUpAction("List",
		    mkListPanel(new String[] { "hello",
					       "this",
					       "is",
					       "a",
					       "list",
                                               "that",
                                               "wraps",
                                               "over"}),
		    examples);

    new PopUpAction("Scrollbar",
		    (new ScrollBarDemo("ScrollBarDemo")).createContent(),
		    examples);

    new PopUpAction("Viewport",
		    mkViewportBox(mkBigButton("View Me!")),
		    examples);

    new PopUpAction("ScrollPane",
                    mkScrollPane(mkBigButton("Scroll Me!")),
                    examples);

    new PopUpAction("TabPane",
		    mkTabs(new String[] {"happy",
					 "sad",
					 "indifferent"}),
		    examples);

    new PopUpAction("Spinner",
		    mkSpinner(),
		    examples);

    new PopUpAction("TextField",
		    (new TextFieldDemo("TextField Demo")).createContent(),
		    examples);

    new PopUpAction("FileChooser",
                    (new FileChooserDemo("FileChooser Demo")).createContent(),
                    examples);

    new PopUpAction("ColorChooser",
		    mkColorChooser(),
		    examples);

    new PopUpAction("ComboBox",
		    (new ComboBoxDemo("ComboBox Demo")).createContent(),
		    examples);

    new PopUpAction("Editor",
                    mkEditorPane(),
                    examples);
    
    new PopUpAction("Tree",
                    mkTree(),
                    examples);

    new PopUpAction("Table",
                    mkTable(),
                    examples);

    help.add(new JMenuItem("just play with the widgets"));
    help.add(new JMenuItem("and enjoy the sensation of"));
    help.add(new JMenuItem("your neural connections growing"));

    bar.add(file);
    bar.add(edit);
    bar.add(examples);
    bar.add(help);
    return bar;
  }

  static void triggerDialog(final JButton but, final String dir)
  {
    but.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          JOptionPane.showConfirmDialog(but, 
                                        "Sure you want to go " + dir + "?",
                                        "Confirm",
                                        JOptionPane.OK_CANCEL_OPTION, 
                                        JOptionPane.QUESTION_MESSAGE,
                                        bigStockIcon("warning"));
        }
      });
  }

  static JToolBar mkToolBar()
  {
    JToolBar bar = new JToolBar();

    JButton b = mkButton(stockIcon("go-back"));
    triggerDialog(b, "back");
    bar.add(b);

    b = mkButton(stockIcon("go-down"));
    triggerDialog(b, "down");
    bar.add(b);

    b = mkButton(stockIcon("go-forward"));
    triggerDialog(b, "forward");
    bar.add(b);
    return bar;
  }

  static String valign2str(int a)
  {
    switch (a)
      {
      case SwingConstants.CENTER:
        return "Center";
      case SwingConstants.TOP:
        return "Top";
      case SwingConstants.BOTTOM:
        return "Bottom";
      default:
        return "Unknown";
      }
  }

  static String halign2str(int a)
  {
    switch (a)
      {
      case SwingConstants.CENTER:
        return "Center";
      case SwingConstants.RIGHT:
        return "Right";
      case SwingConstants.LEFT:
        return "Left";
      default:
        return "Unknown";
      }
  }

  static JButton mkButton(String title, Icon icon, 
                          int hAlign, int vAlign,
                          int hPos, int vPos)
  {    
    JButton b;
    if (icon == null)
      b = new JButton(title);
    else if (title == null)
      b = new JButton(icon);
    else
      b = new JButton(title, icon);
    
    b.setToolTipText(title);
    if (hAlign != -1) b.setHorizontalAlignment(hAlign);
    if (vAlign != -1) b.setVerticalAlignment(vAlign);
    if (hPos != -1) b.setHorizontalTextPosition(hPos);
    if (vPos != -1) b.setVerticalTextPosition(vPos);      
    return b;
  }

  static JButton mkButton(String title)
  {
    return mkButton(title, null, -1, -1, -1, -1);
  }

  static JButton mkButton(Icon i)
  {
    return mkButton(null, i, -1, -1, -1, -1);
  }


  static JPanel mkButtonWorld()
  {
    Icon ii = bigStockIcon("home");
    int CENTER = SwingConstants.CENTER;
    int TOP = SwingConstants.TOP;
    int BOTTOM = SwingConstants.BOTTOM;

    int[] valigns = new int[] {SwingConstants.CENTER,
                               SwingConstants.TOP,
                               SwingConstants.BOTTOM};

    int[] haligns = new int[] {SwingConstants.CENTER,
                               SwingConstants.RIGHT,
                               SwingConstants.LEFT};

    Border[] borders = new Border[] { 
      new SoftBevelBorder(BevelBorder.RAISED),
      new SoftBevelBorder(BevelBorder.LOWERED),
      new BevelBorder(BevelBorder.RAISED),
      
      LineBorder.createBlackLineBorder(),
      new MatteBorder(2, 2, 2, 2, Color.GREEN),
      LineBorder.createGrayLineBorder(),
      
      new BevelBorder(BevelBorder.LOWERED),
      new EtchedBorder(EtchedBorder.RAISED),
      new EtchedBorder(EtchedBorder.LOWERED)      
    };

    JComponent[] comps = new JComponent[3*3];

    int q = 0;

    JPanel panel = new JPanel();
    panel.setLayout(new GridLayout(3, 3));

    for (int i = 0; i < 3; ++i)
      for (int j = 0; j < 3; ++j)
        {
          JButton b = mkButton(halign2str(haligns[i])
                               + valign2str(valigns[j]),
                               ii,
                               -1, -1, haligns[i], valigns[j]);
          b.setBorder(borders[q++]);
          JPanel tmp = new JPanel();
          tmp.setBorder(new MatteBorder(5, 5, 5, 5, blueGray));
          tmp.add(b);
          panel.add(tmp);
        }
    
    return panel;
  }

  private static class CheckCellRenderer 
    extends JCheckBox implements ListCellRenderer
  {
    public Component getListCellRendererComponent(JList list,
                                                  Object value,
                                                  int index,
                                                  boolean isSelected,
                                                  boolean cellHasFocus)
    {
      setSelected(isSelected);
      setText(value.toString());
      
      return this;
    }
  }

  private static class LabelCellRenderer 
    extends DefaultListCellRenderer
  {
    public Component getListCellRendererComponent(JList list,
                                                  Object value,
                                                  int index,
                                                  boolean isSelected,
                                                  boolean cellHasFocus)
    {
      Component c = super.getListCellRendererComponent(list, value, index, 
                                                       isSelected,
						       cellHasFocus);
      
      return c;
    }
  }

  public static JScrollPane mkScrollPane(JComponent inner)
  {
    JScrollPane jsp;
    jsp = new JScrollPane(inner,
			  JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, 
			  JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    
    return jsp;
  }

  private static JPanel mkTreeWorld()
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
    DefaultTreeSelectionModel dtsm = new DefaultTreeSelectionModel();
    dtsm.setSelectionMode(DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.setSelectionModel(dtsm);
    
    // buttons to add and delete
    JButton add = mkButton("add element");
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
                 tree.repaint();
                 break;
              }
           }
        }
      });


    JPanel p1 = new JPanel(); 
    p1.setLayout(new BorderLayout());
    
    JPanel p2 = new JPanel(); 
    p2.add(add);

    p1.add(p2, BorderLayout.NORTH);
    p1.add(mkScrollPane(tree), BorderLayout.CENTER);
    
    return p1;
  }
  
  public static JPanel mkListWorld()
  {

    String foo[] = new String[] { 
      "non alcoholic ",
      "carbonated ",
      "malted ",
      "fresh squeezed ",
      "imported ",
      "high fructose ",
      "enriched "
    };
    
    String bar[] = new String[] { 
      "orange juice",
      "ginger beer",
      "yak milk",
      "corn syrup",
      "herbal remedy"
    };

    final DefaultListModel mod = new DefaultListModel();
    final JList list1 = new JList(mod);
    final JList list2 = new JList(mod);

    list2.setSelectionModel(list1.getSelectionModel());
    for (int i = 0; i < bar.length; ++i)
      for (int j = 0; j < foo.length; ++j)
        mod.addElement(foo[j] + bar[i]);

    list1.setCellRenderer(new LabelCellRenderer());
    list2.setCellRenderer(new CheckCellRenderer());

    JButton add = mkButton("add element");
    add.addActionListener(new ActionListener()
      {
        int i = 0;
        public void actionPerformed(ActionEvent e)
        {
          mod.addElement("new element " + i);
          ++i;
        }
      });

    JButton del = mkButton("delete selected");
    del.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          for (int i = 0; i < mod.getSize(); ++i)
            if (list1.isSelectedIndex(i))
              mod.remove(i);
        }
      });


    JSplitPane splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    splitter.add(mkScrollPane(list1), JSplitPane.LEFT);
    splitter.add(mkScrollPane(list2), JSplitPane.RIGHT);

    JPanel p1 = new JPanel(); 
    p1.setLayout(new BorderLayout());

    JPanel p2 = new JPanel(); 
    p2.setLayout(new GridLayout(1, 2));
    p2.add(add);
    p2.add(del);

    p1.add(p2, BorderLayout.NORTH);
    p1.add(splitter, BorderLayout.CENTER);
    return p1;
  }


  static JPanel mkDesktopWorld()
  {
    
    final JDesktopPane desk = new JDesktopPane();
    desk.setDesktopManager(new DefaultDesktopManager());
    desk.setPreferredSize(new Dimension(300,300));
    desk.setMinimumSize(new Dimension(300,300));
    JButton but = mkButton("add frame");
    but.addActionListener(new ActionListener()
      {
        int i = 10;
        public void actionPerformed(ActionEvent e)
        {
          JInternalFrame f;
	  f = new JInternalFrame("internal", true, true, true, true);
          f.getContentPane().setLayout(new BorderLayout());
          f.getContentPane().add(mkToolBar(), BorderLayout.NORTH);
          f.getContentPane().add(mkButton(bigStockIcon("fullscreen")),
				 BorderLayout.CENTER);
          desk.add(f);
          f.setBounds(i, i, 250, 200);
	  f.setVisible(true);
          i += 30;
        }
      });
    
    JPanel panel = new JPanel();
    panel.setLayout(new BorderLayout());
    panel.add(desk, BorderLayout.CENTER);
    panel.add(but, BorderLayout.NORTH);
    but.doClick();
    but.doClick();
    JInternalFrame palette = new JInternalFrame("Palette", true, true, true, 
        true);
    palette.putClientProperty("JInternalFrame.isPalette", Boolean.TRUE);
    desk.add(palette, JDesktopPane.PALETTE_LAYER);
    JLabel label = new JLabel("This is a floating palette!");
    palette.getContentPane().add(label);
    palette.pack();
    palette.setVisible(true);
    return panel;
  }

  static JPanel mkTabWorld() 
  {
    JPanel panel = new JPanel(new GridLayout(2, 2));
    panel.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
    JTabbedPane tabs1 = new JTabbedPane(SwingConstants.TOP);
    tabs1.add("Top Item 1", new JButton("Button"));
    tabs1.add("Top Item 2", new JButton("Button"));
    JTabbedPane tabs2 = new JTabbedPane(SwingConstants.LEFT);
    tabs2.add("Left Item 1", new JButton("Button"));
    tabs2.add("Left Item 2", new JButton("Button"));
    JTabbedPane tabs3 = new JTabbedPane(SwingConstants.BOTTOM);
    tabs3.add("Bottom Item 1", new JButton("Button"));
    tabs3.add("Bottom Item 2", new JButton("Button"));
    JTabbedPane tabs4 = new JTabbedPane(SwingConstants.RIGHT);
    tabs4.add("Right Item 1", new JButton("Button"));
    tabs4.add("Right Item 2", new JButton("Button"));
    panel.add(tabs1);
    panel.add(tabs2);
    panel.add(tabs3);
    panel.add(tabs4);
    return panel;        
  }

  static JTabbedPane mkTabbedPane()
  {
    JTabbedPane tabs = new JTabbedPane();
    
    tabs.add("Button world!", mkButtonWorld());
    tabs.add("List world!", mkListWorld());
    tabs.add("Desktop world!", mkDesktopWorld());
    tabs.add("Tree world!", mkTreeWorld());
    tabs.add("Tab world!", mkTabWorld());
    return tabs;
  }

  public Demo()
  {
    frame = new JFrame("Swing Activity Board");
    frame.setJMenuBar(mkMenuBar());
    JComponent component = (JComponent) frame.getContentPane();
    component.setLayout(new BorderLayout());
    component.add(mkToolBar(), BorderLayout.NORTH);
    JPanel main = new JPanel();
    main.setLayout(new BoxLayout(main, BoxLayout.Y_AXIS));
    main.add(mkTabbedPane());
    main.add(mkButtonBar());
    component.add(main, BorderLayout.CENTER);
    frame.pack();
    frame.show();
  }

  public static class LaterMain
    implements Runnable
  {
    public void run()
    {
      Demo demo = new Demo();      
    }
  }

  public static void main(String args[])
  {
    SwingUtilities.invokeLater(new LaterMain());
  }

  public static JCheckBox mkCheckbox(String label)
  {
    JCheckBox c = new JCheckBox(label);
    c.setFont(new Font("Luxi", Font.PLAIN, 14));
    return c;
  }

  public static JPanel mkRadio(String label)
  {
    JPanel p = new JPanel();
    JRadioButton c = new JRadioButton(label);
    JRadioButton d = new JRadioButton("not " + label);
    ButtonGroup bg = new ButtonGroup();
    bg.add(c);
    bg.add(d);
    p.add(c);
    p.add(d);
    return p;
  }

  public static JList mkList(Object[] elts)
  {
    JList list = new JList(elts);
    list.setFont(new Font("Luxi", Font.PLAIN, 14));
    return list;
  }

  public static JTabbedPane mkTabs(String[] names)
  {
    JTabbedPane tabs = new JTabbedPane();
    for (int i = 0; i < names.length; ++i)
      {
        tabs.addTab(names[i], mkButton(names[i]));
      }
    return tabs;
  }

  public static JComboBox mkComboBox(String[] names)
  {
    JComboBox box = new JComboBox(names);
    return box;
  }

  public static JSpinner mkSpinner()
  {
    JSpinner spinner = new JSpinner();
    return spinner;
  }

  public static JButton mkBigButton(String title)
  {
    JButton b = new JButton(title);
    b.setMargin(new Insets(5,5,5,5));
    //b.setFont(new Font("Luxi", Font.PLAIN, 14));
    return b;
  }

  public static JToggleButton mkToggle(String title)
  {
    JToggleButton b = new JToggleButton(title);
    b.setMargin(new Insets(5,5,5,5));
    b.setFont(new Font("Luxi", Font.PLAIN, 14));
    return b;    
  }

  public static JPanel mkPanel(JComponent[] inners)
  {
    JPanel p = new JPanel();
    for (int i = 0; i < inners.length; ++i)
      {
        p.add(inners[i]);
      }
    return p;
  }

  public static JScrollBar mkScrollBar()
  {
    JScrollBar scrollbar = new JScrollBar();
    return scrollbar;
  }

  public static JPanel mkViewportBox(final JComponent inner)
  {
    final JViewport port = new JViewport();
    port.setView(inner);
    JButton left = mkBigButton("left");
    JButton right = mkBigButton("right");
    
    left.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          Point p = port.getViewPosition();
          port.setViewPosition(new Point(p.x - 10, p.y));
        }
      });

    right.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          Point p = port.getViewPosition();
          port.setViewPosition(new Point(p.x + 10, p.y));
        }
      });
 
    return mkPanel(new JComponent[] {port, left, right});
  }

  public static JPanel mkListPanel(Object[] elts)
  {
    final DefaultListModel mod = new DefaultListModel();
    final JList list1 = new JList(mod);
    list1.setLayoutOrientation(JList.VERTICAL_WRAP);
    list1.setVisibleRowCount(4);
    final JList list2 = new JList(mod);
    list2.setLayoutOrientation(JList.VERTICAL_WRAP);
    list2.setVisibleRowCount(4);

    list2.setSelectionModel(list1.getSelectionModel());
    for (int i = 0; i < elts.length; ++i)
      mod.addElement(elts[i]);
    list1.setCellRenderer(new LabelCellRenderer());
    list2.setCellRenderer(new CheckCellRenderer());

    JButton add = mkBigButton("add element");
    add.addActionListener(new ActionListener()
      {
        int i = 0;
        public void actionPerformed(ActionEvent e)
        {
          mod.addElement("new element " + i);
          ++i;
        }
      });

    JButton del = mkBigButton("delete selected");
    del.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          for (int i = 0; i < mod.getSize(); ++i)
            if (list1.isSelectedIndex(i))
              mod.remove(i);
        }
      });

    return mkPanel(new JComponent[] {list1, //mkScrollPane(list1), 
                                     list2, //mkScrollPane(list2), 
                                     mkPanel(new JComponent[] {add, del})});
  }


  public static JButton mkDisposerButton(final JFrame c)
  {
    JButton close = mkBigButton("Close");
    close.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          c.dispose();
        }
      });
    return close;
  }

  public static JColorChooser mkColorChooser()
  {
    return new JColorChooser();
  }

  private static class PopUpAction
    implements ActionListener
  {
    private JComponent inner;
    private String name;

    PopUpAction(String n, JComponent i, JMenu m)
    {
      name = n;
      inner = i;

      JMenuItem item = new JMenuItem(name);
      item.addActionListener(this);
      m.add(item);
    }

    PopUpAction(String n, JComponent i, JPanel p)
    {
      name = n;
      inner = i;

      JButton b = mkBigButton(name);
      b.addActionListener(this);
      p.add(b);
    }

    public void actionPerformed(ActionEvent e)
    {
      JFrame frame = new JFrame(name);
      frame.getContentPane().setLayout(new BorderLayout());
      frame.getContentPane().add(inner, BorderLayout.CENTER);
      frame.getContentPane().add(mkDisposerButton(frame), BorderLayout.SOUTH);
      frame.pack();
      frame.show();
    }
  }

  private static JEditorPane mkEditorPane()
  {
    JEditorPane editorPane = new JEditorPane();
    editorPane.setEditable(true);
    return editorPane;
  }
  
  private static JTree mkTree()
  {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("Root node");
    DefaultMutableTreeNode child1 = new DefaultMutableTreeNode("Child node 1");
    DefaultMutableTreeNode child11 =
      new DefaultMutableTreeNode("Child node 1.1");
    DefaultMutableTreeNode child12 =
      new DefaultMutableTreeNode("Child node 1.2");
    DefaultMutableTreeNode child13 =
      new DefaultMutableTreeNode("Child node 1.3");
    DefaultMutableTreeNode child2 = new DefaultMutableTreeNode("Child node 2");
    DefaultMutableTreeNode child21 =
      new DefaultMutableTreeNode("Child node 2.1");
    DefaultMutableTreeNode child22 =
      new DefaultMutableTreeNode("Child node 2.2");
    DefaultMutableTreeNode child23 =
      new DefaultMutableTreeNode("Child node 2.3");
    DefaultMutableTreeNode child24 =
      new DefaultMutableTreeNode("Child node 2.4");

    DefaultMutableTreeNode child3 = new DefaultMutableTreeNode("Child node 3");
    root.add(child1);
    root.add(child2);
    root.add(child3);
    child1.add(child11);
    child1.add(child12);
    child1.add(child13);
    child2.add(child21);
    child2.add(child22);
    child2.add(child23);
    child2.add(child24);

    JTree tree = new JTree(root);
    tree.setLargeModel(true);
    DefaultTreeSelectionModel dtsm = new DefaultTreeSelectionModel();
    dtsm.setSelectionMode(DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.setSelectionModel(dtsm);
    
    return tree;
  }

  private static JTable mkTable()
  {
    Object[][] tableData = new Object[][] {
      {
        "Field 1", "Field 2" , "Field 3"
      },
      {
        "Field 4", "Field 5" , "Field 6"
      },
      {
        "Field 7", "Field 8" , "Field 9"
      },
      {
        "Field 10", "Field 11" , "Field 12"
      }
    };
    Object[] columnNames = new Object[] {"Column 1", "Column 2", "Column 3"};

    JTable table = new JTable(tableData, columnNames);
    return table;
  }
  
  private JPanel mkButtonBar()
  {    
    JPanel panel = new JPanel (new GridLayout(2, 1));
    JPanel panelA = new JPanel(new FlowLayout());
    JPanel panelB = new JPanel(new FlowLayout());

    new PopUpAction("Buttons",
		    (new ButtonDemo("Button Demo")).createContent(),
		    panelA);
    
    new PopUpAction("Toggles",
		    mkToggle("cool and refreshing"),
		    panelA);

    new PopUpAction("Checkbox",
		    mkCheckbox("ice cold"),
		    panelA);

    new PopUpAction("Radio",
		    mkRadio("delicious"),
		    panelA);

    new PopUpAction("Slider",
		    (new SliderDemo("Slider Demo")).createContent(),
		    panelA);

    new PopUpAction("ProgressBar",
            ProgressBarDemo.createContent(),
             panelA);


    new PopUpAction("List",
		    mkListPanel(new String[] { "hello",
					       "this",
					       "is",
					       "a",
					       "list",
                                               "that",
                                               "wraps",
                                               "over"}),
		    panelA);

    new PopUpAction("Scrollbar",
		    (new ScrollBarDemo("ScrollBar Demo")).createContent(),
		    panelA);

    new PopUpAction("Viewport",
		    mkViewportBox(mkBigButton("View Me!")),
		    panelA);

    new PopUpAction("ScrollPane",
		    mkScrollPane(mkBigButton("Scroll Me!")),
		    panelA);

    new PopUpAction("TabPane",
		    mkTabs(new String[] {"happy",
					 "sad",
					 "indifferent"}),
		    panelB);

    new PopUpAction("Spinner",
		    mkSpinner(),
		    panelB);

    new PopUpAction("TextField",
		    (new TextFieldDemo("TextField Demo")).createContent(),
		    panelB);

    new PopUpAction("FileChooser",
                    (new FileChooserDemo("FileChooser Demo")).createContent(),
                    panelB);

    new PopUpAction("ColorChooser",
		    mkColorChooser(),
		    panelB);

    new PopUpAction("ComboBox",
		    (new ComboBoxDemo("ComboBox Demo")).createContent(),
		    panelB);

    new PopUpAction("Editor",
                    mkEditorPane(),
                    panelB);
    
    new PopUpAction("Tree",
                    mkTree(),
                    panelB);
    
    new PopUpAction("Table",
                    mkTable(),
                    panelB);
    
    JButton exitDisposer = mkDisposerButton(frame);
    panelB.add(exitDisposer);
    exitDisposer.addActionListener(new ActionListener()
      {
	public void actionPerformed(ActionEvent e)
	{
	  System.exit(1);
	}
      });
    panel.add(panelA);
    panel.add(panelB);
    return panel;
  }
}
