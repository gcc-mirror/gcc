package javax.swing.plaf.basic;

import javax.swing.*;
import java.awt.*;
import javax.swing.border.*;

class BasicBorder extends MatteBorder
{
    static Color BtnPointClr = new Color( 180, 180, 180);

	BasicBorder()
	{
	    super(5,5,5,5, null);
	}
	
	public void paintBorder(Component  c,
				Graphics  g, 
				int  x, 
				int  y, 
				int  width, 
				int  height)
	{
	    //    System.out.println("PAINT-------------------------------------------BORDER");

	    if (g != null)
		{
		    g.setColor( BtnPointClr);
		    g.draw3DRect( 0, 0, width-1, height-1, true);
		}
	}
 }

class PanelBorder extends MatteBorder
{
    PanelBorder()
    {
	super(5,5,5,5, null);
	}
	
	public void paintBorder(Component  c,
				Graphics  g, 
				int  x, 
				int  y, 
				int  width, 
				int  height)
	{
	    //    System.out.println("PAINT-------------------------------------------BORDER");
	    super.paintBorder(c, g, x, y, width, height);
	}
 }

public class BasicDefaults extends UIDefaults
{
    public BasicDefaults()
    {
	//	System.out.println("BasicDefaults !!!!!!!!!!!!!!!!!!!!!!!!!");
	put("JButton", new BasicButtonUI());
	put("JLabel",  new BasicLabelUI());
	
	put("JPanel",  new BasicPanelUI());
	put("JCheckBox",  new BasicCheckBoxUI());
	put("JRadioButton",  new BasicRadioButtonUI());
	put("JToggleButton",  new BasicToggleButtonUI());
	put("JOptionPane",  new BasicOptionPaneUI());
	put("JList",  new BasicListUI());
	put("JTree",  new BasicTreeUI());
	put("JTextComponent",  new BasicTextUI());
	put("JTabbedPane",  new BasicTabbedPaneUI());
	put("JScrollPane", new BasicScrollPaneUI());
	put("JViewport",   new BasicViewportUI());

	put("JButton.border",      new BasicBorder());
	put("JPanel.border",       new PanelBorder());

	put("JToggleButton.border", new PanelBorder());
	put("JCheckBox.border", new PanelBorder());
	put("JRadioButton.border", new PanelBorder());
    }
    
}


