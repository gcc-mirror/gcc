package javax.swing.plaf.basic;

import javax.swing.*;
import javax.swing.plaf.*;
import java.awt.*;


public class BasicRadioButtonUI extends BasicToggleButtonUI
{
  int large_circle_width = 20;
  int circle_width = large_circle_width - 8; // FIXME: sun == ?
  
    public static ComponentUI createUI(final JComponent c)  {
	return new BasicRadioButtonUI();
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
	
	// and add a little something for the circles:

	d.width += large_circle_width;
	d.height = Math.max(large_circle_width, d.height);
	
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
	
	paintButtonNormal(g, b);

	int x = gap;
	int y = gap;

	int diffp = 2;
	int diff = 3;
	
	g.setColor(textColor);
	g.fillArc(x+diffp, y+diffp, 
		  circle_width-diff, circle_width-diff,
		  0, 360);
    }
    
    protected void paintButtonNormal(Graphics g,
				     JComponent c)
    {
	AbstractButton b = (AbstractButton) c;
	
	Dimension size = b.getSize();
	
	g.setColor(normalBackgroundColor);
	g.fillRect(1,1,size.width-2, size.height-2);  
	
	int x = gap;
	int y = gap;
	
	g.setColor(pressedBackgroundColor);
	g.drawArc(x, y,
		  circle_width, circle_width,
		  0, 360);

	g.setColor(new Color(255,255,255));
	g.drawArc(x, y,
		  circle_width+1, circle_width+1,
		  145, 160);
    }
    
    protected void paintText(Graphics g,
			     JComponent c,
			     Rectangle textRect,
			     String text) 
    {
	//        AbstractButton b = (AbstractButton) c;
	
	//System.out.println("drawing string: " + text + ", " + c.isEnabled());
	
	g.setColor(c.isEnabled() ? textColor : disabledTextColor);
	
	BasicGraphicsUtils.drawString(g,
				      text, 
				      0,
				      textRect.x + circle_width + gap, 
				      textRect.y);
    } 
}










