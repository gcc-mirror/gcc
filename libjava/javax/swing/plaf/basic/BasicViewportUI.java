package javax.swing.plaf.basic;

import javax.swing.plaf.*;
import javax.swing.*;
import java.awt.*;

public class BasicViewportUI extends ViewportUI 
{

    public static ComponentUI createUI(final JComponent c)
    {
	return new BasicViewportUI();
    }

    
    public void installUI(final JComponent c) 
    {
	super.installUI(c);
    }
    

    public Dimension getPreferredSize(JComponent c) 
    {
	Dimension d = new Dimension(100,100);
	System.out.println("BasicViewportUI->preff->"+d);
	return d;
    }

    public void paint(Graphics g, JComponent c)
    {      
	System.out.println("BasicViewportUI->paint->"+c);
    }
}
