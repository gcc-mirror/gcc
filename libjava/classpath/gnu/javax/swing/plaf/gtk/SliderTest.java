import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import gnu.javax.swing.plaf.gtk.*;

public class SliderTest extends JFrame
{
    public SliderTest() 
    { 
	super("JSlider Test");
	Container c = getContentPane();
	c.setLayout(new BorderLayout());
	this.addWindowListener(new WindowAdapter() {
	    public void windowClosing(WindowEvent e) { System.exit(0); } 
	});

	JSlider s = new JSlider();
	s.createStandardLabels(10);
	s.setMinorTickSpacing(10);
	s.setMajorTickSpacing(20);
	s.setPaintTicks(true);
	s.setPaintTrack(true);
	s.setPaintLabels(true);
	s.setRequestFocusEnabled(true);

	// turning off double buffering in repaint manager 
	// in order to use debug graphics
	RepaintManager repaintManager = RepaintManager.currentManager(s);
	repaintManager.setDoubleBufferingEnabled(false); 

	s.setDebugGraphicsOptions(DebugGraphics.BUFFERED_OPTION | DebugGraphics.FLASH_OPTION);
	DebugGraphics.setFlashColor(Color.red);  // color of flash
	DebugGraphics.setFlashTime(4);  // time delay of drawing operation flashing
	DebugGraphics.setFlashCount(3); // number of time to draw

	this.setSize(250, 100);
	c.add(new JLabel("Default Slider"), "North");
	c.add(s, "Center");

	try {
	    UIManager.setLookAndFeel("gnu.javax.swing.plaf.gtk.GtkLookAndFeel");
	    SwingUtilities.updateComponentTreeUI(this);
	} catch (Exception e) {
	    e.printStackTrace();
	    System.exit(0);
	}

	center();
    }

    public void actionPerformed(ActionEvent e) { 
	System.exit(0);
    }
    
    public void center()
    {
	// Centering the frame 
	Toolkit t = this.getToolkit();
	Dimension framesize = this.getSize();
	Dimension screensize = t.getScreenSize();
	
	// Calculate point for frame (main)
	Point pframe = new Point();
	pframe.x = (screensize.width - framesize.width) / 2;
	pframe.y = (screensize.height - framesize.height) / 2;
	
	// Set the location of each to be centered
	this.setLocation(pframe);
    }

    public static void main(String [] argv)
    {
	SliderTest t = new SliderTest();
	t.show();
    }

}




