package javax.swing.plaf.basic;

import javax.swing.*;
import javax.swing.plaf.*;
import java.awt.*;


public class BasicPanelUI extends PanelUI
{
    int gap = 3;

    public static ComponentUI createUI(JComponent x) 
    {
        return new BasicPanelUI();
    }

    public void installUI(JComponent c)
    {
	super.installUI(c);
    }
}
