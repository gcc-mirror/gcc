/* JOptionPane.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.*;
import javax.swing.plaf.*;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;

public class JOptionPane extends JComponent 
{
    public static final int DEFAULT_OPTION        = 0;
    public static final int YES_NO_OPTION         = 1;
    public static final int YES_NO_CANCEL_OPTION  = 2;
    public static final int OK_CANCEL_OPTION      = 3;
    public static final int YES_OPTION            = 4;
    public static final int NO_OPTION             = 5;
    public static final int CANCEL_OPTION         = 6;
    public static final int OK_OPTION             = 7;
    public static final int CLOSED_OPTION         = 8;

    public static final int ERROR_MESSAGE         = 0;
    public static final int INFORMATION_MESSAGE   = 1;
    public static final int WARNING_MESSAGE       = 2;
    public static final int QUESTION_MESSAGE      = 3;
    public static final int PLAIN_MESSAGE         = 4;

    final static String VALUE_PROPERTY = "value_prop";
    final static String INPUT_VALUE_PROPERTY = "input_value_prop";
    
    final static String UNINITIALIZED_VALUE = "uninit";

    // Ronald: shouldnt by public ?
    public Object msg;
    public int mtype;
    public int otype;
    public Icon icon;
    public Object []args;
    public Object init;

    public JDialog dialog;

    /*****************************************************************************
     *
     *
     *  joptionpanels
     *
     *
     ***********************************/

    JOptionPane()
    {
	this("mess");
    }
    
    JOptionPane(Object m)
    {
	this(m, PLAIN_MESSAGE);
    }
    
    JOptionPane(Object m,
		 int mtype)
    {
	this(m, mtype, DEFAULT_OPTION);
    }

    JOptionPane(Object m,
		int mtype,
		int otype)
    {
	this(m, mtype, otype, null);
    }
 
    JOptionPane(Object m,
		 int mtype,
		 int otype,
		 Icon icon)
    {
	this(m, mtype, otype, icon, null);
    }

    JOptionPane(Object m,
		 int mtype,
		 int otype,
		 Icon icon,
		 Object []args)
    {
	this(m, mtype, otype, icon, args, null);
    }

    JOptionPane(Object msg,
		int mtype,
		int otype,
		Icon icon,
		Object []args,
		Object init)
    {
	//	this(m, mtype, otype, icon, args, init);
	this.msg   = msg;
	this.mtype = mtype;
	this.otype = otype;
	this.icon  = icon;
	this.args  = args;
	this.init  = init;
	
	updateUI();
    }


    /*****************************************************************************
     *
     *
     *
     *
     *
     ***********************************/

    Object val;
    public void setValue(Object v)  
    {   val = v;       }
    public Object getValue()
    {	return val;    }

    public String getUIClassID()
    {	return "JOptionPane";    }


    public void setUI(OptionPaneUI ui) {
        super.setUI(ui);
    }
    
    public OptionPaneUI getUI() {
        return (OptionPaneUI)ui;
    }
    
    public void updateUI() {
	setUI((OptionPaneUI)UIManager.getUI(this));
    }


    public AccessibleContext getAccessibleContext()
    {
	return null;
    }
    
    protected  String paramString()
    {
	return "JOptionPane";
    }
    
    public static void showMessageDialog(Component frame,
				  String msg,
				  String title,
				  int bla)
    {
	DoShowOptionDialog(frame,
			  msg,
			  title,
			  bla,
			  0,
			  null,
			  null,
			  null);
    }

    public static void showMessageDialog(Component frame,
				 String msg,
				 String title,
				 int bla,
				 Icon icon)
    {
	DoShowOptionDialog(frame,
				 msg,
				 title,
				 bla,
				 0,
				 icon,
				 null,
				 null);
    }

    public static void showMessageDialog(Component frame,
				  String msg)
    {
	showMessageDialog(frame,
			  msg,
			  null);
    }
    

    public static void showMessageDialog(Component frame,
				  String msg,
				  Icon icon)
    {	
	//System.out.println("++++++++++++++++++creating message dialog:"+msg + ", frame="+frame);
         DoShowOptionDialog(frame, 
				msg, 
				"Message",				
				DEFAULT_OPTION, 
				PLAIN_MESSAGE,
				icon,
				null,
				null);
    }

    public static int showConfirmDialog(JFrame frame,
				 String yes,
				 String no, 
				 int bla)
    {
	return 0;
    }

    public static String showInputDialog(JFrame frame,
			     String msg, 
			     String title, 
			     int opt_type, 
			     int msg_type,
			     Icon icon, 
			     Object[] opts, 
			     Object init)
    {
	return (String) DoShowOptionDialog(frame,
				msg, 
				title, 
				opt_type, 
				msg_type,
				icon, 
				opts, 
				init);
    }

    public static Object showInputDialog(JFrame frame,
			     String msg, 
			     String title, 
			     int opt_type, 
			     Icon icon, 
			     Object[] opts, 
			     Object init)
    {
	return DoShowOptionDialog(frame,
				msg, 
				title, 
				opt_type, 
				0, //msg_type,
				icon, 
				opts, 
				init);
    }


    // everybody comes here eventually
    public static int showOptionDialog(Component frame,
				String msg, 
				String title, 
				int opt_type, 
				int msg_type,
				Icon icon, 
				Object[] opts, 
				Object init)
    {
	Integer a = (Integer) DoShowOptionDialog(frame,
						 msg, 
						 title, 
						 opt_type, 
						 msg_type,
						 icon, 
						 opts, 
						 init);
	if (a == null)
	    return -1;
	return a.intValue();
    }
    
    public static Object DoShowOptionDialog(Component frame,
				   String msg, 
				   String title, 
				   int opt_type, 
				   int msg_type,
				   Icon icon, 
				   Object[] opts, 
				   Object init)
    {
	
	JOptionPane p = new JOptionPane(msg,
					msg_type,
					opt_type,
					icon,
					opts,
					init);
	System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ " + p.msg);

	
	JDialog a;

	if (frame == null)
	    {
		a = new JDialog((Frame)frame,
				title,
				true);
	    }
	else if (frame instanceof Dialog)
	    {
		a = new JDialog((Dialog) frame,
				title,
				true);
	    }
	else if (frame instanceof Frame)
	    {
		a = new JDialog((Frame) frame,
				title,
				true);
	    }
	else
	    {
		System.out.println("HUUUUHHH, not a frame or dialog !");
		
		a = new JDialog((Frame)null,
				title,
				true);
	    }

	p.dialog = a;
	
	a.getContentPane().setLayout(new BorderLayout());
	a.getContentPane().add(p,
			       BorderLayout.CENTER);
	// package the deal
	a.pack();
	
	a.setVisible(true);
	
	Object s = p.getValue();

	System.out.println("RESULT FROM DIALOG = " + s);

	if (s == null)
	    return null;
	
	return s;
    }

}

















