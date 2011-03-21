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

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Canvas;
import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.CheckboxMenuItem;
import java.awt.Choice;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.DisplayMode;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Label;
import java.awt.List;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.MenuShortcut;
import java.awt.Panel;
import java.awt.ScrollPane;
import java.awt.TextField;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceContext;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

class Demo
{
  public static void main(String args[])
  {
    MainWindow f = new MainWindow();
    f.show();
  }

  static interface SubWindow
  {
    public void init ();
  }

  static class PrettyPanel extends Panel
  {
    Insets myInsets;

    public PrettyPanel ()
    {
      myInsets = new Insets (10, 10, 10, 10);
    }
    public Insets getInsets ()
    {
      return myInsets;
    }
  }

  static abstract class PrettyFrame extends Frame
  {
    public PrettyFrame ()
    {
      ((BorderLayout) getLayout ()).setHgap (5);
      ((BorderLayout) getLayout ()).setVgap (5);
    }

    public Insets getInsets()
    {
      Insets oldInsets = super.getInsets ();
      return new Insets (oldInsets.top+10,
                         oldInsets.left+10,
                         oldInsets.bottom+10,
                         oldInsets.right+10);
    }
  }

  static abstract class SubFrame extends PrettyFrame implements SubWindow
  {
    boolean initted = false;

    public void setVisible (boolean visible)
    {
      if (!initted && visible)
        init();
      super.setVisible (visible);
    }
  }

  static class MainWindow extends PrettyFrame implements ActionListener
  {
    Button closeButton;

    Hashtable windows;
    Vector buttons;

    void addSubWindow (String name, SubWindow w)
    {
      Button b = new Button (name);
      b.addActionListener (this);

      buttons.addElement (b);
      windows.put (b, w);
    }

    MainWindow ()
    {
      MenuBar mb = new MenuBar ();
      Menu menu = new Menu ("File");
      Menu submenu = new Menu ("Testing", true);
      submenu.add (new CheckboxMenuItem ("FooBar"));
      submenu.add (new CheckboxMenuItem ("BarFoo"));
      menu.add (submenu);
      menu.add (new MenuItem("Orange"));
      MenuItem quit = new MenuItem("Quit", new MenuShortcut('Q'));
      quit.addActionListener(new ActionListener()
        {
          public void actionPerformed(ActionEvent e)
          {
            System.exit(0);
          }
        });
      menu.add(quit);
      mb.add (menu);

      menu = new Menu("Edit", true);
      menu.add(new MenuItem("Cut"));
      menu.add(new MenuItem("Copy"));
      menu.add(new MenuItem("Paste"));
      mb.add (menu);

      Menu helpMenu = new Menu("Help");
      helpMenu.add(new MenuItem("About"));
      mb.add(helpMenu);
      mb.setHelpMenu(helpMenu);

      setMenuBar (mb);

      String version = System.getProperty("gnu.classpath.version");
      add (new Label ("GNU Classpath " + version), "North");

      closeButton = new Button ("Close");
      closeButton.addActionListener (this);
      closeButton.setFont (new Font ("Serif", Font.BOLD | Font.ITALIC, 18));
      add (closeButton, "South");

      windows = new Hashtable ();
      buttons = new Vector ();

      addSubWindow ("Buttons", new ButtonsWindow ());
      addSubWindow ("Cursors", new CursorsWindow ());
      addSubWindow ("Dialog", new DialogWindow (this));
      addSubWindow ("File", new FileWindow (this));
      addSubWindow ("Labels", new LabelWindow ());
      addSubWindow ("List", new ListWindow ());
      addSubWindow ("Radio Buttons", new RadioWindow ());
      addSubWindow ("TextField", new TextFieldWindow ());
      addSubWindow ("RandomTests", new TestWindow (this));
      addSubWindow ("RoundRect", new RoundRectWindow ());
      addSubWindow ("Animation", new AnimationWindow ());
      addSubWindow ("Resolution", new ResolutionWindow ());
      addSubWindow ("Fullscreen", new FullscreenWindow ());
      addSubWindow ("Drag n' Drop", new DragDropWindow ());

      Panel sp = new Panel();
      PrettyPanel p = new PrettyPanel();
      p.setLayout (new GridLayout (windows.size(), 1));

      for (Enumeration e = buttons.elements (); e.hasMoreElements (); )
        {
          p.add ((Button) e.nextElement ());
        }

      sp.add (p);
      add (sp, "Center");

      setTitle ("AWT Demo");
      pack();
    }

    public void actionPerformed (ActionEvent evt)
    {
      Button source = (Button) evt.getSource ();

      if (source==closeButton)
        {
          dispose();
          System.exit (0);
        }

      Window w = (Window) windows.get (source);
      if (w.isVisible ())
        w.dispose ();
      else
        {
          if (w instanceof Dialog)
            {
              w.show();
            }
          else
            {
              w.setVisible (true);
            }
        }
    }
  }

  static class ButtonsWindow extends SubFrame implements ActionListener
  {
    Button b[] = new Button [9];

    public void init ()
    {
      initted = true;
      Panel p = new Panel ();
      p.setLayout (new GridLayout (0, 3, 5, 5));

      for (int i=0; i<9; i++)
        {
          b[i]=new Button ("button" + (i+1));
          b[i].addActionListener (this);
        }

      p.add (b[0]);
      p.add (b[6]);
      p.add (b[4]);
      p.add (b[8]);
      p.add (b[1]);
      p.add (b[7]);
      p.add (b[3]);
      p.add (b[5]);
      p.add (b[2]);

      add (p, "North");

      Button cb = new Button ("close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e) {
            dispose();
          }
        });
      add (cb, "South");
      setTitle ("Buttons");
      pack();
    }

    public void actionPerformed (ActionEvent evt)
    {
      Button source = (Button) evt.getSource ();

      for (int i = 0; i < 9; i++)
        {
          if (source == b[i])
            {
              int i2 = ((i + 1) == 9) ? 0 : (i + 1);
              if (b[i2].isVisible())
                b[i2].setVisible(false);
              else
                b[i2].setVisible(true);
            }
        }
    }
  }


  static class DialogWindow extends Dialog implements SubWindow
  {
    Label text;
    Frame parent;
    boolean initted = false;

    public DialogWindow (Frame f)
    {
      super (f, true);

      this.parent = f;

      addWindowListener (new WindowAdapter ()
        {
          public void windowClosing (WindowEvent e)
          {
            text.setVisible (false);
            hide ();
          }
        });
    }

    public void setVisible (boolean visible)
    {
      if (!initted && visible)
        init();
      super.setVisible (visible);
    }

    public void show ()
    {
      if (!initted)
        init();
      super.show ();
    }

    public void init ()
    {
      text = new Label ("Dialog Test");
      text.setAlignment (Label.CENTER);

      add (text, "North");
      text.setVisible (false);

      Panel p = new PrettyPanel();

      Button cb = new Button ("OK");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e)
          {
            text.setVisible (false);
            hide();
          }
        });

      p.setLayout (new GridLayout (1, 3));
      ((GridLayout) p.getLayout ()).setHgap (5);
      ((GridLayout) p.getLayout ()).setVgap (5);
      p.add (cb);

      Button toggle = new Button ("Toggle");
      p.add (toggle);

      toggle.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e)
          {
            if (text.isVisible ())
              text.setVisible (false);
            else
              text.setVisible (true);
            doLayout();
          }
        });

      Button subdlg = new Button ("SubDialog");
      p.add (subdlg);

      subdlg.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e)
          {
            DialogWindow sw = new DialogWindow (parent);
            sw.show ();
          }
        });

      add (p, "South");
      setTitle ("Dialog");
      pack();
    }
  }

  static class CursorsWindow extends SubFrame implements ItemListener
  {
    Choice cursorChoice;
    Canvas cursorCanvas;

    public void init ()
    {
      cursorChoice = new Choice();
      cursorChoice.add ("Default");
      cursorChoice.add ("Crosshair");
      cursorChoice.add ("Text");
      cursorChoice.add ("Wait");
      cursorChoice.add ("Southwest Resize");
      cursorChoice.add ("Southeast Resize");
      cursorChoice.add ("Northwest Resize");
      cursorChoice.add ("Northeast Resize");
      cursorChoice.add ("North Resize");
      cursorChoice.add ("South Resize");
      cursorChoice.add ("West Resize");
      cursorChoice.add ("East Resize");
      cursorChoice.add ("Hand");
      cursorChoice.add ("Move");

      cursorChoice.addItemListener(this);

      add (cursorChoice, "North");

      cursorCanvas = new Canvas ()
        {
          public void paint (Graphics g)
          {
            Dimension d = this.getSize();
            g.setColor(Color.white);
            g.fillRect(0, 0, d.width, d.height/2);
            g.setColor(Color.black);
            g.fillRect(0, d.height/2, d.width, d.height/2);
            g.setColor(this.getBackground());
            g.fillRect(d.width/3, d.height/3, d.width/3,
                        d.height/3);
          }
        };

      cursorCanvas.setSize (80,80);

      add (cursorCanvas, "Center");

      Button cb = new Button ("Close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e) {
            dispose();
          }
        });

      add (cb, "South");
      setTitle ("Cursors");
      pack();
    }

    public void itemStateChanged (ItemEvent e)
    {
      int index = cursorChoice.getSelectedIndex();
      cursorCanvas.setCursor(Cursor.getPredefinedCursor(index));
    }
  }

  static class TextFieldWindow extends SubFrame implements ItemListener
  {
    Checkbox editable, visible, sensitive;
    TextField text;

    public void init ()
    {
      initted = true;
      text = new TextField ("hello world");
      add (text, "North");

      Panel p = new Panel();
      p.setLayout (new GridLayout (3, 1));
      ((GridLayout) p.getLayout ()).setHgap (5);
      ((GridLayout) p.getLayout ()).setVgap (5);

      editable = new Checkbox("Editable", true);
      p.add (editable);
      editable.addItemListener (this);

      visible = new Checkbox("Visible", true);
      p.add (visible);
      visible.addItemListener (this);

      sensitive = new Checkbox("Sensitive", true);
      p.add (sensitive);
      sensitive.addItemListener (this);

      add (p, "Center");

      Button cb = new Button ("Close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e) {
            dispose();
          }
        });

      add (cb, "South");
      setTitle ("TextField");
      pack();
    }

    public void itemStateChanged (ItemEvent e)
    {
      boolean on=true;

      if (e.getStateChange () == ItemEvent.DESELECTED)
        on=false;
      if (e.getSource() == editable)
        text.setEditable (on);
      if (e.getSource() == visible)
        if (on)
          text.setEchoChar ((char) 0);
        else
          text.setEchoChar ('*');
      if (e.getSource() == sensitive)
        text.setEnabled (on);

    }
  }

  static class FileWindow extends FileDialog implements SubWindow
  {
    boolean initted = false;

    public FileWindow (MainWindow mw)
    {
      super (mw);
    }

    public void setVisible (boolean visible)
    {
      if (!initted && visible)
        init();
      super.setVisible (visible);
    }

    public void init()
    {
      initted = true;
    }
  }

  static class LabelWindow extends SubFrame
  {
    public void init ()
    {
      initted = true;

      Panel p = new Panel();
      p.setLayout (new GridLayout (3, 1));
      ((GridLayout) p.getLayout ()).setHgap (5);
      ((GridLayout) p.getLayout ()).setVgap (5);

      p.add (new Label ("left justified label", Label.LEFT));
      p.add (new Label ("center justified label", Label.CENTER));
      p.add (new Label ("right justified label", Label.RIGHT));

      add (p, "Center");

      Button cb = new Button ("Close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e) {
            dispose();
          }
        });

      add (cb, "South");
      setTitle ("Labels");
      pack();
    }
  }

  static class ListWindow extends SubFrame
  {
    public void init ()
    {
      initted = true;

      Panel p = new Panel ();
      p.setLayout (new GridLayout (3, 1));

      List l = new List (5, true);
      for (int i = 0; i < 10; i++)
        l.add ("List item " + i);

      p.add (l);

      add (p, "Center");

      Button cb = new Button ("Close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e) {
            dispose();
          }
        });

      add (cb, "South");
      setTitle ("List");
      pack();
    }
  }


  static class RadioWindow extends SubFrame
  {
    public void init ()
    {
      initted = true;

      Panel p = new Panel();
      p.setLayout (new GridLayout (3, 1));
      ((GridLayout) p.getLayout ()).setHgap (5);
      ((GridLayout) p.getLayout ()).setVgap (5);

      final CheckboxGroup cg = new CheckboxGroup();
      final Checkbox[] boxes = new Checkbox[3];
      for (int i = 0; i < 3; ++i)
        {
          boxes[i] = new Checkbox("button" + i, cg, i == 0);
          p.add(boxes[i]);
        }

      add (p, "North");

      p = new Panel();
      p.setLayout (new GridLayout (1, 3));
      ((GridLayout) p.getLayout ()).setHgap (5);
      ((GridLayout) p.getLayout ()).setVgap (5);

      for (int i = 0; i < 3; ++i)
        {
          final int val = i;
          Button tweak = new Button ("Set " + i);
          tweak.addActionListener(new ActionListener ()
            {
              public void actionPerformed (ActionEvent e)
              {
                cg.setSelectedCheckbox(boxes[val]);
              }
            });
          p.add(tweak);
        }

      add (p, "Center");

      Button cb = new Button ("Close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e) {
            dispose();
          }
        });

      add (cb, "South");
      setTitle ("Radio Buttons");
      pack();
    }
  }

  static class TestWindow extends SubFrame
  {
    static int xs = 5, ys = 5;
    final Frame parent;

    public TestWindow(Frame f)
    {
      parent = f;
    }

    public void init()
    {
      initted = true;

      addWindowListener (new WindowAdapter ()
        {
          public void windowClosing (WindowEvent e)
          {
            hide ();
          }
        });

      Panel pan = new Panel();

      final Label l = new Label ("Pithy Message:");
      l.setCursor (Cursor.getPredefinedCursor (Cursor.WAIT_CURSOR));
      pan.add (l);

      TextField tf = new TextField("Hello world!");
      pan.add(tf);
      add(pan,"North");

      final Image img;
      URL imageurl;
      imageurl = this.getClass()
        .getResource("/gnu/classpath/examples/icons/big-warning.png");
      img = Toolkit.getDefaultToolkit().createImage(imageurl);

      final Canvas ch = new Canvas()
        {
          public void paint (Graphics g)
          {
            g.drawImage(img, xs + 25, ys + 25, this);

            Font font = new Font ("Serif", Font.PLAIN, 18);
            g.setFont (font);
            g.setXORMode (Color.red);

            g.drawString("Hi Red!", xs + 15, ys + 10);
            g.setColor (Color.blue);
            g.drawLine (xs, ys, xs + 100, ys + 100);

          }
        };
      ch.setSize(150, 150);
      add(ch, "Center");

      final ScrollPane sp = new ScrollPane(ScrollPane.SCROLLBARS_ALWAYS);
      final Panel p = new Panel();
      p.add(new Button("Stop"));
      p.add(new Button("evil"));
      p.add(new Button("hoarders"));
      p.add(new Button("use"));
      p.add(new Button("GNU!"));

      sp.add(p);
      add(sp, "South");

      Panel east_panel = new Panel();
      east_panel.setLayout(new GridLayout (0,1));

      CheckboxGroup group = new CheckboxGroup();
      Checkbox cb = new Checkbox("one", group, true);
      east_panel.add(cb);
      cb = new Checkbox("two", group, false);
      east_panel.add(cb);

      add(east_panel,"East");

      final Button wb = new Button();
      wb.setLabel("Hello World!");
      wb.addActionListener(new ActionListener()
        {
          public void actionPerformed (ActionEvent e)
          {
            l.setText ("Hello World!");

            final Dialog d = new Dialog(parent);
            d.setLayout(new FlowLayout());
            d.setModal(true);
            Button b = new Button("foobar");
            b.addMouseListener(new MouseAdapter()
              {
                public void mousePressed (MouseEvent me)
                {
                  d.hide ();
                }
              });
            d.add (b);

            List ch = new List();
            ch.add("Ding");
            ch.add("September");
            ch.add("Red");
            ch.add("Quassia");
            ch.add("Pterodactyl");
            d.add(ch);

            d.pack ();
            d.show ();
          }
        });

      wb.addMouseListener(new MouseAdapter()
        {
          public void mousePressed(MouseEvent e) {
            xs++;
            ys++;
            ch.repaint ();
          }
        });

      add(wb,"West");

      pack();
      show();

      sp.setScrollPosition (10,0);

      Toolkit t = Toolkit.getDefaultToolkit();
      t.beep();
    }
  }

  static class ResolutionWindow extends SubFrame
  {
    GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();

    public void init ()
    {
      initted = true;

      setTitle("Change Screen Resolution");
      final List list = new List();
      DisplayMode[] modes = gd.getDisplayModes();

      for (int i=0;i<modes.length;i++ )
        list.add(modes[i].getWidth()  + "x"
                 + modes[i].getHeight()
                 + ((modes[i].getBitDepth() != DisplayMode.BIT_DEPTH_MULTI)
                   ? "x" + modes[i].getBitDepth() + "bpp"
                   : "")
                 + ((modes[i].getRefreshRate() != DisplayMode.REFRESH_RATE_UNKNOWN)
                   ? "@" + modes[i].getRefreshRate() + "Hz"
                   : ""));

      ActionListener al = new ActionListener()
      {
        public void actionPerformed(ActionEvent ae)
        {
          int i = list.getSelectedIndex();
          gd.setDisplayMode(gd.getDisplayModes()[i]);
        }
      };

      Button b = new Button("Switch");
      Button c = new Button("Close");

      list.addActionListener(al);
      b.addActionListener(al);

      c.addActionListener(new ActionListener () {
        public void actionPerformed (ActionEvent e) {
          dispose();
        }
      });

      setLayout(new GridLayout(3, 1, 5, 5));
      add(list);
      add(b);
      add(c);

      pack();
    }
  }

  static class DragDropWindow
      extends SubFrame
      implements ActionListener, DropTargetListener
  {
    DragLabel source = new DragLabel("Drag and drop me to the following Button",
                                     Label.CENTER);

    Button target = new Button();

    public void init()
    {
      source.setForeground(Color.red);
      add(source, BorderLayout.NORTH);

      target.addActionListener(this);
      add(target, BorderLayout.SOUTH);

      new DropTarget(target, DnDConstants.ACTION_COPY_OR_MOVE, this);

      setSize(205, 100);

      pack();
    }

    public void actionPerformed(ActionEvent e)
    {
      Button b = (Button) e.getSource();
      b.setLabel("");
    }

    public void dragEnter(DropTargetDragEvent e)
    {
    }

    public void dragExit(DropTargetEvent e)
    {
    }

    public void dragOver(DropTargetDragEvent e)
    {
    }

    public void drop(DropTargetDropEvent e)
    {
      try
        {
          Transferable t = e.getTransferable();

          if (e.isDataFlavorSupported(DataFlavor.stringFlavor))
            {
              e.acceptDrop(e.getDropAction());

              String s;
              s = (String) t.getTransferData(DataFlavor.stringFlavor);

              target.setLabel(s);

              e.dropComplete(true);
            }
          else
            e.rejectDrop();
        }
      catch (java.io.IOException e2)
        {
        }
      catch (UnsupportedFlavorException e2)
        {
        }
    }

    public void dropActionChanged(DropTargetDragEvent e)
    {
    }

    class DragLabel
        extends Label
        implements DragGestureListener, DragSourceListener
    {
      private DragSource ds = DragSource.getDefaultDragSource();

      public DragLabel(String s, int alignment)
      {
        super(s, alignment);
        int action = DnDConstants.ACTION_COPY_OR_MOVE;
        ds.createDefaultDragGestureRecognizer(this, action, this);
      }

      public void dragGestureRecognized(DragGestureEvent e)
      {
        try
          {
            Transferable t = new StringSelection(getText());
            e.startDrag(DragSource.DefaultCopyNoDrop, t, this);
          }
        catch (InvalidDnDOperationException e2)
          {
            System.out.println(e2);
          }
      }

      public void dragDropEnd(DragSourceDropEvent e)
      {
        if (e.getDropSuccess() == false)
          return;

        int action = e.getDropAction();
        if ((action & DnDConstants.ACTION_MOVE) != 0)
          setText("");
      }

      public void dragEnter(DragSourceDragEvent e)
      {
        DragSourceContext ctx = e.getDragSourceContext();

        int action = e.getDropAction();
        if ((action & DnDConstants.ACTION_COPY) != 0)
          ctx.setCursor(DragSource.DefaultCopyDrop);
        else
          ctx.setCursor(DragSource.DefaultCopyNoDrop);
      }

      public void dragExit(DragSourceEvent e)
      {
      }

      public void dragOver(DragSourceDragEvent e)
      {
      }

      public void dropActionChanged(DragSourceDragEvent e)
      {
      }
    }
  }

  static class FullscreenWindow extends SubFrame
  {
    GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();

    public void init ()
    {
      initted = true;

      setTitle("Fullscreen Exclusive Mode");

      ActionListener al = new ActionListener()
      {
        public void actionPerformed(ActionEvent ae)
        {
          if (gd.getFullScreenWindow() == FullscreenWindow.this)
            gd.setFullScreenWindow(null);
          else
            gd.setFullScreenWindow(FullscreenWindow.this);
        }
      };

      Button b = new Button("Toggle Fullscreen");
      Button c = new Button("Close");

      b.addActionListener(al);

      c.addActionListener(new ActionListener () {
        public void actionPerformed (ActionEvent e) {
          gd.setFullScreenWindow(null);
          dispose();
        }
      });

      setLayout(new GridLayout(3, 1, 5, 5));
      add(b);
      add(c);

      pack();
    }
  }

  static class RoundRectWindow extends SubFrame
  {
    public void init ()
    {
      initted = true;
      setTitle("RoundRect");
      setLayout(new BorderLayout());
      add(new DrawRoundRect(), "West");
      Button cb = new Button ("Close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e) {
            dispose();
          }
        });
      add(cb, "Center");
      add(new FillRoundRect(), "East");
      pack();
    }

    static class DrawRoundRect extends Panel
    {

      public Dimension getPreferredSize()
      {
        return new Dimension(500, 500);
      }

      public void paint( Graphics g )
      {
        // left side

        // rectangles should be identical
        g.setColor(Color.red);
        g.drawRect(50, 50, 300, 100);
        g.setColor(Color.black);
        g.drawRoundRect(50, 50, 300, 100, 0, 0);

        // small round corners
        g.setColor(Color.red);
        g.drawRect(50, 200, 300, 100);
        g.setColor(Color.black);
        g.drawRoundRect(50, 200, 300, 100, 25, 25);

        // round ends
        g.setColor(Color.red);
        g.drawRect(50, 350, 300, 100);
        g.setColor(Color.black);
        g.drawRoundRect(50, 350, 300, 100, 25, 100);

        // right side

        // circle only
        g.setColor(Color.blue);
        g.drawOval(375, 50, 100, 100);

        // round rectangle should exactly cover circle
        g.setColor(Color.blue);
        g.drawOval(375, 200, 100, 100);
        g.setColor(Color.black);
        g.drawRoundRect(375, 200, 100, 100, 100, 100);

        // round rectangle should look like a circle
        g.setColor(Color.red);
        g.drawRect(375, 350, 100, 100);
        g.setColor(Color.black);
        g.drawRoundRect(375, 350, 100, 100, 100, 100);
      }
    }

    static class FillRoundRect extends Panel
    {

      public Dimension getPreferredSize()
      {
        return new Dimension(500, 500);
      }

      public void paint( Graphics g )
      {
        // left side

        // rectangles should be identical
        g.setColor(Color.red);
        g.fillRect(50, 50, 300, 100);
        g.setColor(Color.black);
        g.fillRoundRect(50, 50, 300, 100, 0, 0);

        // small round corners
        g.setColor(Color.red);
        g.fillRect(50, 200, 300, 100);
        g.setColor(Color.black);
        g.fillRoundRect(50, 200, 300, 100, 25, 25);

        // round ends
        g.setColor(Color.red);
        g.fillRect(50, 350, 300, 100);
        g.setColor(Color.black);
        g.fillRoundRect(50, 350, 300, 100, 25, 100);

        // right side

        // circle only
        g.setColor(Color.blue);
        g.fillOval(375, 50, 100, 100);

        // round rectangle should exactly cover circle
        g.setColor(Color.blue);
        g.fillOval(375, 200, 100, 100);
        g.setColor(Color.black);
        g.fillRoundRect(375, 200, 100, 100, 100, 100);

        // round rectangle should look like a circle
        g.setColor(Color.red);
        g.fillRect(375, 350, 100, 100);
        g.setColor(Color.black);
        g.fillRoundRect(375, 350, 100, 100, 100, 100);
      }
    }
  }

  static class AnimationWindow extends SubFrame
  {
    AnimationApplet a;
    public void init ()
    {
      initted = true;
      setTitle("Animation");
      Button cb = new Button ("Close");
      cb.addActionListener(new ActionListener () {
          public void actionPerformed (ActionEvent e)
          {
            if (a != null)
              {
                a.destroy();
                dispose();
              }
          }
        });
      a = new AnimationApplet();
      add(a, "Center");
      add(cb, "South");
      pack();
    }

    public void show()
    {
      super.show();
      a.init();
      a.run();
    }
  }
}
