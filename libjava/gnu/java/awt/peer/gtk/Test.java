/* Test.java -- Tests the GTK Toolkit
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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


import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.peer.*;
import java.awt.datatransfer.*;
import gnu.java.awt.image.*;
import java.io.*;

class Test
{
  static int xs = 5, ys = 5;

  public static void main(String args[])
    {
      if (args.length == 0)
	{ 
	  Properties prop=System.getProperties ();
	  prop.put ("awt.toolkit","gnu.java.awt.peer.gtk.GtkToolkit");
	}

      final Frame f=new Frame();
      f.setTitle ("Red Hat Classpath");
      
//        f.addComponentListener (new ComponentAdapter() {
//  	public void componentMoved (ComponentEvent e) {
//  	  System.out.println("component moved");
//  	}
//  	public void componentResized (ComponentEvent e) {
//  	  System.out.println("component resized");
//  	}
//        });
      f.setSize(200,200);

      Panel pan=new Panel();

      final Label l = new Label ("Pithy Message:");
      l.setCursor (Cursor.getPredefinedCursor (Cursor.WAIT_CURSOR));
      pan.add (l);

      TextField tf = new TextField("Hello world!");
      pan.add(tf);

      final Image img;
      img = Toolkit.getDefaultToolkit ().createImage (new XBMDecoder ("fvwm.xbm"));


      final Canvas ch = new Canvas () { 

	  public void update (Graphics g)
	  {
	    System.out.println ("update called");
	    super.update (g);
	  }
	public void paint (Graphics g) {
	  g.drawString("Hello world!", xs+5, ys+10);
	  g.setColor (Color.blue);
	  g.drawLine (xs,ys,xs+20,ys+20);

//  	  System.out.println (TextArea.SCROLLBARS_BOTH);
//  	  System.out.println (TextArea.SCROLLBARS_HORIZONTAL_ONLY);
//  	  System.out.println (TextArea.SCROLLBARS_VERTICAL_ONLY);

//  	  Font f1 = new Font ("TimesRoman", Font.PLAIN, 10);
//  	  System.out.println (f1.getName ());
//  	  System.out.println (f1.getFamily ());

//  	  Font font = new Font ("Serif", Font.PLAIN, 18); 
//  	  g.setFont (font);
//  	  g.setXORMode (Color.red);


//  	  System.out.println (g.getFontMetrics (font).stringWidth ("foobar"));

//        System.out.println (g.drawImage (img, 0, 0, this));
	}
      };

      ch.setSize(60, 60);
//        List ch=new List();
//        ch.add("Ding");
//        ch.add("September");
//        ch.add("Red");
//        ch.add("Quassia");
//        ch.add("Pterodactyl");

//        ch.addMouseListener(new MouseAdapter() {
//  	public void mousePressed(MouseEvent e) {
//  	  System.out.println("mouse pressed ch");
//  	  System.out.println("shift = " + e.isShiftDown());
//  	  System.out.println("meta = " + e.isMetaDown());
//  	  System.out.println("alt = " + e.isAltDown());
//  	  System.out.println("ctrl = " + e.isControlDown());
//  	  System.out.println("x = " + e.getX());
//  	  System.out.println("y = " + e.getY());
//  	  System.out.println("clickcount = " + e.getClickCount());
//  	  System.out.println("when = " + e.getWhen());
//  	  System.out.println();
//  	}
//  	public void mouseReleased(MouseEvent e) {
//  	  System.out.println("mouse released ch");
//  	}
//  	public void mouseClicked(MouseEvent e) {
//  	  System.out.println("mouse clicked ch");
//  	}
//        });

      pan.add(ch);
      f.add(pan,"North");

      final ScrollPane sp=new ScrollPane(ScrollPane.SCROLLBARS_ALWAYS);
      System.out.println ("ALWAYS HERE: " + ScrollPane.SCROLLBARS_ALWAYS);
      System.out.println ("ALWAYS" + ScrollPane.SCROLLBARS_ALWAYS);
      System.out.println ("NEEDED" + ScrollPane.SCROLLBARS_AS_NEEDED);
      System.out.println ("NEVER " + ScrollPane.SCROLLBARS_NEVER);


      final Panel p=new Panel();
      System.out.println ("PREFERED: " + p.getPreferredSize ());
      p.add(new Button("Stop"));
      System.out.println ("PREFERED: " + p.getPreferredSize ());
      p.add(new Button("evil"));
      System.out.println ("PREFERED: " + p.getPreferredSize ());
      p.add(new Button("hoarders"));
      p.add(new Button("use"));
      p.add(new Button("GNU"));
      p.add(new Scrollbar(Scrollbar.HORIZONTAL));
      System.out.println ("PREFERED: " + p.getPreferredSize ());

      sp.add(p);
      f.add(sp,"South");

      Panel east_panel = new Panel();
      east_panel.setLayout(new GridLayout (0,1));

//        CheckboxGroup group = new CheckboxGroup();

//        Checkbox cb=new Checkbox("one", group, true);
//        east_panel.add(cb);
//        cb=new Checkbox("two", group, false);
//        east_panel.add(cb);

//        cb.addMouseListener(new MouseAdapter() {
//  	  public void mousePressed(MouseEvent e) {
//  	  System.out.println("mouse pressed cb");
//  	  System.out.println("shift = " + e.isShiftDown());
//  	  System.out.println("meta = " + e.isMetaDown());
//  	  System.out.println("alt = " + e.isAltDown());
//  	  System.out.println("ctrl = " + e.isControlDown());
//  	  System.out.println("x = " + e.getX());
//  	  System.out.println("y = " + e.getY());
//  	  System.out.println("clickcount = " + e.getClickCount());
//  	  System.out.println("when = " + e.getWhen());
//  	  System.out.println();
//  	}
//  	public void mouseReleased(MouseEvent e) {
//  	  System.out.println("mouse released cb");
//  	}
//  	public void mouseClicked(MouseEvent e) {
//  	  System.out.println("mouse clicked cb");
//  	}
//  	public void mouseEntered(MouseEvent e) {
//  	  System.out.println("mouse entered cb");
//  	}
//  	public void mouseExited(MouseEvent e) {
//  	  System.out.println("mouse exited cb");
//  	}
//        });

      f.add(east_panel,"East");

      final Button wb=new Button();
      wb.setLabel("Destroy Frame on Click");
      wb.addActionListener (new ActionListener () {
	public void actionPerformed (ActionEvent e) {
	  ScrollPanePeer peer = (ScrollPanePeer)sp.getPeer ();
	  if (peer != null)
	    {
	      System.out.println (peer.getHScrollbarHeight ());
	      System.out.println (peer.getVScrollbarWidth ());
	    }

	  l.setText ("Hello World!");
	  System.out.println ("PREFERED: " + p.getPreferredSize ());

	  final Dialog d = new Dialog (f);
	  d.setModal (true);
	  Button b = new Button ("foobar");
	  b.addMouseListener (new MouseAdapter () {
	      public void mousePressed (MouseEvent me) {
		System.out.println ("I got called");
		d.hide ();

//  		System.out.println (ScrollPane.SCROLLBARS_ALWAYS);
//  		System.out.println (ScrollPane.SCROLLBARS_AS_NEEDED);
//  		System.out.println (ScrollPane.SCROLLBARS_NEVER);
	      }
	    });
	  d.add (b);
	  d.pack ();
	  d.show ();
	  System.out.println ("hello world");
//  	  System.out.println ("action listener on wb called");
//  	  Clipboard clip = Toolkit.getDefaultToolkit ().getSystemClipboard ();
//  	  StringSelection ss = new StringSelection("123456789");
//  	  clip.setContents (ss, ss);
//  	  Transferable t = clip.getContents (this);
//  	  try {
//  	    System.out.println (t.getTransferData (DataFlavor.stringFlavor));
//  	  } catch (Exception ex) {
//  	    ex.printStackTrace ();
//  	  }
//  	  System.exit (0);
	}
      });

      wb.addMouseListener(new MouseAdapter() {
	public void mousePressed(MouseEvent e) {
	  System.out.println("mouse pressed wb");
	  xs++;
	  ys++;
	  ch.repaint ();
	}
	public void mouseReleased(MouseEvent e) {
	  System.out.println("mouse released wb");
	}
	public void mouseClicked(MouseEvent e) {
	  System.out.println("mouse clicked wb");
	}
	public void mouseEntered(MouseEvent e) {
	  System.out.println("mouse entered wb");
	}
	public void mouseExited(MouseEvent e) {
	  System.out.println("mouse exited wb");
	}
      });

      f.add(wb,"West");
      
      f.pack();
      f.show();

      sp.setScrollPosition (10,0);

      Toolkit t = Toolkit.getDefaultToolkit();
      /* t.beep(); */
      System.out.println("screen size: " + t.getScreenSize());
      System.out.println("resolution : " + t.getScreenResolution());
//        try {
//  	Thread.sleep (5000);
//        } catch (InterruptedException e) {}
//        f.setSize(500,500);

      System.out.println ("background of canvas: " + ch.getBackground ());
      System.out.println ("foreground of canvas: " + ch.getForeground ());

      System.out.println("done");
    }
}



