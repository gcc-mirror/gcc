package javax.swing.plaf.basic;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.plaf.*;
import javax.accessibility.*;

public class BasicOptionPaneUI extends OptionPaneUI
{
    JOptionPane pane;

    BasicOptionPaneUI()
    {
    }

    public static ComponentUI createUI(JComponent x) 
    {
        return new BasicOptionPaneUI();
    }

    public void installUI(JComponent c)
    {
	super.installUI(c);
	pane = (JOptionPane)c;

	System.out.println("     -------------: " + pane);

	JLabel  message   = pane.msg != null ? new JLabel((String)pane.msg) : null;
	JButton ok_button = new JButton("Ok");	

	ok_button.addActionListener(new ActionListener()
	    {
		public void actionPerformed(ActionEvent a)
		{
		    System.out.println("ACTION ---> " + a);
		    //		    pane.dialog.dispose();

		    if (pane.dialog.isModal())
			{
			    System.out.println("modal dialog !!");
			    pane.dialog.setModal(false);
			}
		    pane.dialog.setVisible(false);
		}
	    });

	if (pane.args != null)
	    {
		for (int i=0; i<pane.args.length; i++)
		    {
			Object o = pane.args[i];
			if (o != null)
			    {
				if (o instanceof String)
				    {
					String s = (String) o;
					JLabel m = new JLabel(s);
					pane.add(m);
				    }
				else if (o instanceof Component)
				    {
					Component com = (Component) o;
					pane.add(com);
				    }
				else
				    {
					System.out.println("UNRECOGNIZED ARG: " + o);
				    }
			    }
		    }
	    }

	pane.add(message);
	pane.add(ok_button);
    }

    Dimension getMinimumOptionPaneSize()
    {
	return new Dimension(300,100);
    }

    public Dimension getPreferredSize(JComponent c)
    {
	if (c == null)
	    return getMinimumOptionPaneSize();

	if (c != pane)
	    return null;

	LayoutManager l  = c.getLayout();
	if (l == null)
	    return getMinimumOptionPaneSize();

	Dimension d1 = l.preferredLayoutSize(c);
	Dimension d2 = getMinimumOptionPaneSize();
	
	d1.width = Math.max(d1.width, d2.width);
	d1.height = Math.max(d1.height, d2.height);

	return d2;
    }
}
