package javax.swing.plaf.basic;

import javax.swing.*;
import java.awt.*;
import javax.swing.plaf.*;

public class BasicTabbedPaneUI  extends TabbedPaneUI 
{
    public static ComponentUI createUI(final JComponent c) 
    {
	return new BasicTabbedPaneUI();
    }
    
    public void installUI(final JComponent c) 
    {
	super.installUI(c);
    }
    
    public Dimension getPreferredSize(JComponent c) 
    {
	JTabbedPane p = (JTabbedPane) c;

	Dimension d = new Dimension(50,50);
	
	for (int i=0;i<p.getTabCount();i++)
	    {
		Component comp = p.getComponentAt(i);
		
		Dimension pr = comp.getPreferredSize();

		d.width = Math.max(d.width, comp.getWidth());
		d.height = Math.max(d.height, comp.getHeight());
	    }
	
	Insets i = p.getInsets();
	
	d.width  += i.left + i.right;
	d.height += i.top + i.bottom;

	int height_of_tabs = 25;

	d.height += height_of_tabs;

	// FIXME: should be max of panes in p
	return d;
    }
    

    public Rectangle getTabBounds(JTabbedPane pane, int index)
    {
	return null;
    }

    public int getTabRunCount(JTabbedPane pane)
    {
	return 0;
    }

    public int tabForCoordinate(JTabbedPane pane, int x, int y)
    {
	return 0;
    }
}
                       
