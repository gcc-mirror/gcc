package javax.swing.plaf.basic;

import javax.swing.plaf.*;
import javax.swing.*;
import java.awt.*;


public class BasicListUI extends ListUI
{
    int gap_between_cells;
    Color textColor, disabledTextColor, pressedBackgroundColor, normalBackgroundColor;
    

    public static ComponentUI createUI(final JComponent c) 
    {
	return new BasicButtonUI();
    }

    
    public void installUI(final JComponent c) 
    {
	super.installUI(c);

	textColor                = new Color(0,0,0);
	disabledTextColor        = new Color(130, 130, 130);
	pressedBackgroundColor   = new Color(150,150,150);
	normalBackgroundColor    = new Color(192,192,192);
    }

    public Dimension getPreferredSize(JComponent c) 
    {
	JList l = (JList) c;

	System.out.println("XXXXXXXXXXXXXXXxx   getPreferredSize------------> " + l);

	
	int rows = l.getVisibleRowCount();

	ListCellRenderer render = l.getCellRenderer();
	
	int width  = 200;
	int height = rows * 16; 
	
	if (l.getModel().getSize() == 0)
	    {
		return new Dimension(width, height);
	    }

	System.out.println("BASIC_LIST_UI ====-> " + l.getModel().getElementAt(0));

	Component elt = render.getListCellRendererComponent(l,
							    l.getModel().getElementAt(0),
							    0,            
							    false,
							    false);
	Dimension a = elt.getPreferredSize();
	if (a == null)
	    {
		return new Dimension(width, height);
	    }

	return new Dimension(a.width,
			     a.height * rows);
    }

    public void paintBackground(Graphics g,
			 JComponent c)
    {
	Dimension size = getPreferredSize(c);

	g.setColor(normalBackgroundColor);
	g.fillRect(0,0,size.width, size.height);  
    }

    public void paint(Graphics g, 
		      JComponent c)
    {      
	JList l = (JList) c;

	int rows = l.getVisibleRowCount();

	ListCellRenderer render = l.getCellRenderer();

	System.out.println("RENDER-JLIST: " + rows + ", " + l.getModel().getSize());

	paintBackground(g, c);

	if (l.getModel().getSize() == 0)
	    return;

	// use element 0 to figure out how big we are:
	Component elt = render.getListCellRendererComponent(l,
							    l.getModel().getElementAt(0),
							    0,       
							    false,
							    false);
	Dimension dim = elt.getPreferredSize();
	
	Rectangle a = new Rectangle(0,
				    0,
				    dim.width,
				    dim.height);

	for (int i=0;i<l.getModel().getSize();i++)
	    {
		boolean is_sel = false;
		boolean has_focus = false;

		Component comp = render.getListCellRendererComponent(l,
								     l.getModel().getElementAt(i),
								     i,            
								     is_sel,
								     has_focus);

		//System.out.println("AAAAA=> " + a + ", " + comp + ", index = " + i);

		comp.setBounds(a);

		comp.paint(g);

		a.y += dim.height + gap_between_cells;
	    }
    }
}








