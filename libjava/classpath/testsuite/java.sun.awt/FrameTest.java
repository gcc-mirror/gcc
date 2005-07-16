import java.awt.*;
import java.awt.event.*;

public class FrameTest
  extends Frame
{
  public static void main(String args[])
    {
      FrameTest frame = new FrameTest();
      frame.pack();
      frame.show();
    }
  public FrameTest()
    {
      super("FrameTest");
      Button done = new Button("Press to continue");
      add(done, "Center");
      addWindowListener(new WindowAdapter() {
	public void windowClosing(WindowEvent event)
	  {
	    dispose();
	    System.exit(0);
	  }
      });
      done.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent ev) {
	  dispose();
	  System.exit(0);
        }});

    }
}
