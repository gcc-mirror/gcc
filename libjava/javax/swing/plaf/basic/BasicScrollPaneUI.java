package javax.swing.plaf.basic;


import javax.swing.plaf.*;
import javax.swing.*;
import java.awt.*;



public class BasicScrollPaneUI extends ScrollPaneUI
{
    int min_w = 50;
    int min_h = 50;

    public static ComponentUI createUI(final JComponent c) 
    {
	return new BasicScrollPaneUI();
    }

    
    public void installUI(final JComponent c) 
    {
	super.installUI(c);
    }
    

    public Dimension getPreferredSize(JComponent c) 
    {
	JScrollPane p = (JScrollPane ) c;
	
	Dimension d = new Dimension(min_w,
				    min_h);
	
	Dimension a = p.getViewport().getPreferredSize();

	if (a != null)
	    {
		d.width = Math.max(d.width, a.width);
		d.height = Math.max(d.height, a.height);
	    }
			   

	System.out.println("BasicScrollPaneUI->preff->"+d);
	return d;
    }

    public void paint(Graphics g, JComponent c)
    {      
	System.out.println("BasicScrollPaneUI->paint()->"+c);

	JScrollPane p = (JScrollPane ) c;
	p.getViewport().paint(g);
    }
}












