package javax.swing.plaf.basic;

import javax.swing.*;
import javax.swing.plaf.*;
import java.awt.*;


public class BasicLabelUI extends LabelUI
{
    int gap = 3;
    
    Color foreground;

    
    public static ComponentUI createUI(final JComponent c)  {
	return new BasicLabelUI();
    }

    
    public void installUI(final JComponent c)  {
	super.installUI(c);
	
	foreground = new Color(0,0,250);
    }
    

    public Dimension getPreferredSize(JComponent c) 
    {
	JLabel b = (JLabel)c;
	Dimension d = BasicGraphicsUtils.getPreferredSize(b, 
							  gap,
							  b.getText(),
							  b.getIcon(),
							  b.getVerticalAlignment(),
							  b.getHorizontalAlignment(),
							  b.getHorizontalTextPosition(),
							  b.getVerticalTextPosition());
	System.out.println("JLABEL->^^^^^^^^^^^^^^^^^^^^^^   BASIC-PREF="+d + ",T="+b.getText());
	return d;
    }
    

    public void paint(Graphics g, JComponent c)
    {      
	JLabel b = (JLabel) c;

	Rectangle tr = new Rectangle();
	Rectangle ir = new Rectangle();
	Rectangle vr = new Rectangle();

        Font f = c.getFont();

        g.setFont(f);

        FontMetrics fm = SwingUtilities.getFontMetrics(f);

        Insets i = c.getInsets();

	Rectangle bound = c.getBounds();
	
	System.out.println("BOUND=" + bound + ", insets = " + i + ", " + b.getText());
	
	if (bound == null)
	    {
		vr.x      = i.left;
		vr.y      = i.top;
		vr.width  = b.getWidth() - (i.right  + i.left);
		vr.height = b.getHeight() - (i.bottom + i.top);
	    }
	else
	    {
		vr.x      = bound.x + i.left;
		vr.y      = bound.y + i.top;
		vr.width  = bound.width - (i.right  + i.left);
		vr.height = bound.height - (i.bottom + i.top);
	    }

	System.out.println("             VIEW-RECT-JLABEL="+vr+", insets="+i+", FONTM="+fm);

	String text = SwingUtilities.layoutCompoundLabel(c,
							 fm, 
							 b.getText(),
							 b.getIcon(),
							 b.getVerticalAlignment(), 
							 b.getHorizontalAlignment(),
							 b.getVerticalTextPosition(), 
							 b.getHorizontalTextPosition(),
							 vr,
							 ir,
							 tr,
							 gap);

	paintIcon(g, c, ir);
	paintText(g, c, tr, b.getText());
	paintFocus(g, c, vr, tr, ir);
    }


    protected void paintFocus(Graphics g, 
			      JComponent c,
			      Rectangle vr,
			      Rectangle tr,
			      Rectangle ir)
    {
    }

    protected void paintIcon(Graphics g, 
			     JComponent c, 
			     Rectangle iconRect)
    {
	JLabel b = (JLabel) c;
	if (b.getIcon() != null)
	    {
		int x = iconRect.x;
		int y = iconRect.y;

		System.out.println("WE HAVE AN ICON: " + b.getIcon());
 
		b.getIcon().paintIcon(c, g, x, y);
	    }
	else
	    {
		//System.out.println("NO ICON FOR BUTTON:" + b.text);
	    }
    }

    
    protected void paintText(Graphics g,
			     JComponent c,
			     Rectangle textRect,
			     String text) 
    {
	//        AbstractLabel b = (AbstractLabel) c;
	
	System.out.println("JLabel: drawing string: " + text + ", at:" + textRect);
	
	g.setColor(foreground);
	//g.setBackColor(new Color(190,190,190));

	g.drawLine(0,0,100,100);
	
	BasicGraphicsUtils.drawString(g,
				      text, 
				      0,	
				      0,//textRect.x, 
				      0);//textRect.y);
    }
}










