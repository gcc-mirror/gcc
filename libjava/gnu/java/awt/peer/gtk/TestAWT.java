/* TestAWT.java -- Tests the AWT like testgtk
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

This file is part of GNU Classpath.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

import java.awt.List;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.peer.*;

class TestAWT
{
  public static void main(String args[])
  {
    if (args.length==0)
      {
	Properties prop = System.getProperties ();
	prop.put ("awt.toolkit", "gnu.java.awt.peer.gtk.GtkToolkit");
      }
    MainWindow f = new MainWindow();
    System.out.println(f.isDisplayable());
    f.show();
    System.out.println(f.isDisplayable());
  }
}

interface SubWindow
{
  public void init ();
}

class PrettyPanel extends Panel
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

abstract class PrettyFrame extends Frame
{
  public PrettyFrame ()
  {
    ((BorderLayout) getLayout ()).setHgap (5);
    ((BorderLayout) getLayout ()).setVgap (5);
  }

//    public Insets getInsets()
//    {
//      Insets oldInsets = super.getInsets ();
//      return new Insets (oldInsets.top+10,
//  		       oldInsets.left+10,
//  		       oldInsets.bottom+10,
//  		       oldInsets.right+10);
//    }
}

abstract class SubFrame extends PrettyFrame implements SubWindow
{
  boolean initted = false;

  public void setVisible (boolean visible)
  {
    if (!initted && visible)
      init();
    super.setVisible (visible);
  } 
}

class MainWindow extends PrettyFrame implements ActionListener 
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
    Menu submenu = new Menu ("Testing");
    submenu.add (new CheckboxMenuItem ("Foobar"));
    menu.add (submenu);
    mb.add (menu);

    setMenuBar (mb);

    add (new Label ("Classpath v0.0.0"), "North");
      
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

    Panel sp = new Panel();
    PrettyPanel p = new PrettyPanel();
    p.setLayout (new GridLayout (windows.size(), 1));

    for (Enumeration e = buttons.elements (); e.hasMoreElements (); )
      {
	p.add ((Button) e.nextElement ());
      }

    sp.add (p);
    add (sp, "Center");

    setSize (200, 86 + (windows.size ()*22));
    setTitle ("TestAWT");
  }

  public void actionPerformed (ActionEvent evt)
  {
    Button source = (Button) evt.getSource ();
      
    if (source==closeButton)
      {
	System.getProperties ().list (System.out);
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
            System.out.println ("Will 'show'");
	    w.show();
            System.out.println ("Has shown");
          }
        else
          {
	    w.setVisible (true);
          }
      }
  }
}

class ButtonsWindow extends SubFrame implements ActionListener
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
  }

  public void actionPerformed (ActionEvent evt)
  {
    Button source = (Button) evt.getSource ();
      
    for (int i=0; i<9; i++)
      {
	if (source == b[i])
	  {
	    int i2=((i+1)==9)?0:(i+1);
	    if (b[i2].isVisible())
	      b[i2].setVisible(false);
	    else 
	      b[i2].setVisible(true);
	  }
      }
  }
}


class DialogWindow extends Dialog implements SubWindow
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
          System.out.println ("Window Closing");
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
	  dispose();
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
            System.out.println ("Will show modal sub dialog");
            sw.show ();
            System.out.println ("Has shown modal sub dialog");
	}
    });
    
    add (p, "South");
    setTitle ("Dialog");
    setSize (240, 120);
  }
}

class CursorsWindow extends SubFrame implements ItemListener
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
	g.setColor (Color.white);
	g.fillRect (0, 0, d.width, d.height/2);
	g.setColor (Color.black);
	g.fillRect (0, d.height/2, d.width, d.height/2);
	g.setColor (this.getBackground());
	g.fillRect (d.width/3, d.height/3, d.width/3,
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
    setSize (160, 180);
  }

  public void itemStateChanged (ItemEvent e)
  {
    cursorCanvas.setCursor (Cursor.getPredefinedCursor (cursorChoice.getSelectedIndex()));
  }
}

class TextFieldWindow extends SubFrame implements ItemListener
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
    setSize (160, 180);
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

class FileWindow extends FileDialog implements SubWindow
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

class LabelWindow extends SubFrame
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
    setSize (160, 180);
  }
}

class ListWindow extends SubFrame
{
  public void init ()
  {
    initted = true;

    Panel p = new Panel ();
    p.setLayout (new GridLayout (3, 1));
    
    List l = new List (5, true);
    for (int i = 0; i < 10; i++)
      l.add ("added item " + i);

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
    setSize (85, 167);
  }
}    


class RadioWindow extends SubFrame
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
    setSize (85, 167);
  }
}
