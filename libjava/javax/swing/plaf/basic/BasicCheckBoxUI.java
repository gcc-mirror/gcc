package javax.swing.plaf.basic;

import javax.swing.*;
import javax.swing.plaf.*;
import java.awt.*;


public class BasicCheckBoxUI extends BasicRadioButtonUI
{  
    public static ComponentUI createUI(final JComponent c)  {
	return new BasicCheckBoxUI();
    }

    
    public void installUI(final JComponent c)  {
	super.installUI(c);
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
							  
	//System.out.println("^^^^^^^^^^^^^^^^^^^^^^   BASIC-PREF="+d + ",T="+b.text);
	return d;
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
	//        AbstractButton b = (AbstractButton) c;
	
	//	System.out.println("drawing string: " + text + ", at:" + textRect);
	
	g.setColor(textColor);
	BasicGraphicsUtils.drawString(g,
				      text, 
				      0,
				      textRect.x, 
				      textRect.y);
    } 
}




