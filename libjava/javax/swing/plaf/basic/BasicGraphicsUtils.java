package javax.swing.plaf.basic;


import java.awt.*;
import javax.swing.*;


public class BasicGraphicsUtils
{  
    public static Dimension getPreferredSize(JComponent b, 
					     int gap,
					     String text,
					     Icon icon,
					     int va,
					     int ha,
					     int htp,
					     int vtp)
    {
	JComponent c = b;
	// this is a staight copy from BasicButtonUI.paint()
	// 
	Rectangle tr = new Rectangle();
	Rectangle ir = new Rectangle();
	Rectangle vr = new Rectangle();

	Font f = c.getFont();

        FontMetrics fm = SwingUtilities.getFontMetrics(f);

        Insets i = c.getInsets();

        vr.x      = i.left;
        vr.y      = i.top;
        vr.width  = b.getWidth()  - (i.right  +  i.left);
        vr.height = b.getHeight() - (i.bottom +  i.top);

	//	System.out.println("              VIEW-RECT-BUTTON="+vr+", insets="+i);

	String tt = SwingUtilities.layoutCompoundLabel(b,
						       fm, 
						       text,
						       icon,
						       va,
						       ha,
						       vtp,
						       htp,
						       vr,
						       ir,
						       tr,
						       gap);
	
        Rectangle r = ir.union(tr);
	
        Insets insets = b.getInsets();
        r.width  += insets.left + insets.right;
        r.height += insets.top  + insets.bottom;

	//	System.out.println("COMPUTED SIZE FOR PREF_SIZE="+r);

	return r.getSize();
    }

    public static void drawString(Graphics g,
				  String text,
				  int underlinedChar,
				  int x,
				  int y)
    {
	g.drawString(text, x, y);
    }
}






