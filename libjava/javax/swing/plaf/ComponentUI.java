package javax.swing.plaf;

import java.awt.*;
import javax.swing.border.*;
import javax.swing.*;

import javax.accessibility.*;

public abstract class ComponentUI 
    implements UIResource // ??
{
    boolean contains(JComponent c, int x, int y)
    {
	return c.inside(x,y);
    }

    // this SHOULD thow an error:
    public static ComponentUI createUI(JComponent c)
    {
	Exception e = new Exception("createUI from ComponentUI should never be called");
	e.printStackTrace();
	System.exit(1);
	return null;
    }

    public Accessible getAccessibleChild(JComponent c, int i)
    {
	//Return the nth Accessible child of the object. 
	return null;
    }
  
    public int getAccessibleChildrenCount(JComponent c)
    {
	//Returns the number of accessible children in the object. 
	return 0;
    }
  
    public Dimension getMaximumSize(JComponent c)
    {
	return getPreferredSize(c);
    }

    public Dimension getMinimumSize(JComponent c)
    {
	return getPreferredSize(c);
    }

    public Dimension getPreferredSize(JComponent c)
    {
	return null;
    }

    public void installUI(JComponent c)
    {
	String id = c.getUIClassID() + ".border";

	Border s = UIManager.getBorder( id );
	
	if (s != null)
	    {
		c.setBorder( s );
		//System.out.println("OK-INSTALL: " + this + ", ID=" + id + ",B="+s);
	    }
	else
	    {
		///System.out.println("FAIL-INSTALL: " + this + ", " + id);
	    }	
    }

    public void paint(Graphics g, JComponent c)
    {
	//  System.out.println("UI-COMPONENT-> unimplemented paint: " + c + ", UI="+this);
    }

    public void uninstallUI(JComponent c)
    {	
    }

    public void update(Graphics g, JComponent c) {
        if (c.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(0, 0, c.getWidth(),c.getHeight());
        }
        paint(g, c);
    }
         
}

