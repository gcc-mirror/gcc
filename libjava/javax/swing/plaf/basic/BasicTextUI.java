package javax.swing.plaf.basic;

import javax.swing.text.*;
import javax.swing.plaf.*;
import java.awt.*;
import javax.swing.*;

public class BasicTextUI extends TextUI
{
    int gap = 3;
    View view = new RootView();
    Color textColor, disabledTextColor, normalBackgroundColor;
    EditorKit kit = new DefaultEditorKit();
    
    class RootView extends View
    {
	RootView()
	{
	    super(null);
	}
        public void paint(Graphics g, Shape s)
	{
	    if (view != null)
		{
		    Rectangle r = s.getBounds();

		    view.setSize((int)r.getWidth(),
				 (int)r.getHeight());
		    view.paint(g, s);
		}
        }
    }

    public BasicTextUI()
    {
    }

    public static ComponentUI createUI(final JComponent c) 
    {
	return new BasicTextUI();
    }

    
    public void installUI(final JComponent c) 
    {
	super.installUI(c);

	textColor                = new Color(0,0,0);
	disabledTextColor        = new Color(130, 130, 130);
	normalBackgroundColor    = new Color(192,192,192);
    }
    
    public Dimension getPreferredSize(JComponent c) 
    {
	JTextComponent b = (JTextComponent) c;

	View v = getRootView(b);

	float w = v.getPreferredSpan(View.X_AXIS);
	float h = v.getPreferredSpan(View.Y_AXIS);

	return new Dimension((int)w, (int) h);
    }
    

    public void paint(Graphics g, JComponent c)
    {      
	//	view.paint(
    }

    public void damageRange(JTextComponent t, int p0, int p1)
    {
	damageRange(t, p0, p1, null, null);
    }    

    public void damageRange(JTextComponent t, 
		     int p0, int p1, 
		     Position.Bias firstBias,
		     Position.Bias secondBias)
    {
    }

    public EditorKit getEditorKit(JTextComponent t)
    {
	return kit;
    }
    
    public int getNextVisualPositionFrom(JTextComponent t, 
				  int pos,
				  Position.Bias b, 
				  int direction,
				  Position.Bias[] biasRet)
    {
	return 0;
    }
    
    public View getRootView(JTextComponent t)
    {
	return view;
    }
    
    public Rectangle modelToView(JTextComponent t, int pos)
    {
	return modelToView(t, pos, null);
    }
    
    public Rectangle modelToView(JTextComponent t, int pos, Position.Bias bias)
    {
	return null;
    }
    
    public int viewToModel(JTextComponent t, Point pt)
    {
	return viewToModel(t, pt, null);
    }
    
    public int viewToModel(JTextComponent t, Point pt, Position.Bias[] biasReturn)
    {
	return 0;
    } 
}





