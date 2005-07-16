import java.awt.*;
import java.awt.event.*;

public class FrameMenuTest
  extends Frame
{
  public static void main(String args[])
    {
      FrameMenuTest frame = new FrameMenuTest();
      frame.pack();
      frame.show();
    }
  public FrameMenuTest()
    {
      super("FrameMenuTest");
      addWindowListener(new WindowAdapter() {
	public void windowClosing(WindowEvent event)
	  {
	    dispose();
	    System.exit(0);
	  }
      });

      MenuBar mbar = new MenuBar();
      Menu menu = new Menu("File");
      MenuItem exit = new MenuItem("Exit");
      menu.add(exit);
      mbar.add(menu);
      setMenuBar(mbar);

      exit.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent ev) {
	  dispose();
	  System.exit(0);
        }});

      Label message = new Label("Choose File->Exit to continue");
      add(message, "Center");
    }
}
