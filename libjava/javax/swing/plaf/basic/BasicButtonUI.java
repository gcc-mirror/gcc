package javax.swing.plaf.basic;

import javax.swing.*;
import javax.swing.plaf.*;
import java.awt.*;


public class BasicButtonUI extends ButtonUI
{
    int gap = 3;
    //    int y_text_space = 2, x_text_space + 5;

    Color textColor, disabledTextColor;
    Color pressedBackgroundColor;
    Color normalBackgroundColor;


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
	pressedBackgroundColor   = new Color(150,150,150);
	normalBackgroundColor    = new Color(192,192,192);
    }
    

    public Dimension getPreferredSize(JComponent c) 
    {
	AbstractButton b = (AbstractButton)c;
	Dimension d = BasicGraphicsUtils.getPreferredSize(b, 
							  gap,
							  b.getText(),
							  b.getIcon(),
							  b.getVerticalAlignment(),
							  b.getHorizontalAlignment(),
							  b.getHorizontalTextPosition(),
							  b.getVerticalTextPosition());
	//	System.out.println("^^^^^^^^^^^^^^^^^^^^^^   BASIC-PREF="+d + ",T="+b.text);
	return d;
    }
    

    public void paint(Graphics g, JComponent c)
    {      
	AbstractButton b = (AbstractButton) c;

	Rectangle tr = new Rectangle();
	Rectangle ir = new Rectangle();
	Rectangle vr = new Rectangle();

        Font f = c.getFont();

        g.setFont(f);

        FontMetrics fm = SwingUtilities.getFontMetrics(f);

        Insets i = c.getInsets();

        vr.x      = i.left;
        vr.y      = i.top;
        vr.width  = b.getWidth()  - (i.right  + vr.x);
        vr.height = b.getHeight() - (i.bottom + vr.y);
	
	//System.out.println("             VIEW-RECT-BUTTON="+vr+", insets="+i+", FONTM="+fm);
	
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

        if (b.getModel().isPressed() ||
	    b.getModel().isSelected())
	    {
	      //System.out.println("paint pressed");
		paintButtonPressed(g, c);
	    }
	else
	    {
	      //System.out.println("paint normal");
		paintButtonNormal(g, c);
	    }
	
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
	AbstractButton b = (AbstractButton) c;
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

    protected void paintButtonPressed(Graphics g,
				      JComponent b)
    {
	Dimension size = b.getSize();
	
	g.setColor(pressedBackgroundColor);
	g.fillRect(1,1,size.width-2, size.height-2);                

    }
    
    protected void paintButtonNormal(Graphics g,
				     JComponent b)
    {
	Dimension size = b.getSize();
	
	g.setColor(normalBackgroundColor);
	g.fillRect(1,1,size.width-2, size.height-2);                

    }
    
    protected void paintText(Graphics g,
			     JComponent c,
			     Rectangle textRect,
			     String text) 
    {	
	Font f = c.getFont();

        g.setFont(f);

        FontMetrics fm = SwingUtilities.getFontMetrics(f);

	g.setColor(c.isEnabled() ? textColor : disabledTextColor);

	BasicGraphicsUtils.drawString(g,
				      text, 
				      0,
				      textRect.x, 
				      textRect.y + fm.getAscent()/2);
    } 
}




