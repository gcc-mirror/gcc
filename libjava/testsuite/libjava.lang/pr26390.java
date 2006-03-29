public class pr26390
{
  public interface ComponentPeer {
    public void setBounds();
  }

  public interface ContainerPeer extends ComponentPeer {
  }

  public interface WindowPeer extends ContainerPeer {
  }

  public interface FramePeer extends WindowPeer {
  }

  public static class SwingComponentPeer implements ComponentPeer {
    public void setBounds() {
    }
  }

  public static class SwingContainerPeer
    extends SwingComponentPeer implements ContainerPeer
  {
  }

  public static class SwingWindowPeer
    extends SwingContainerPeer implements WindowPeer
  {
  }

  public static class SwingFramePeer
    extends SwingWindowPeer implements FramePeer
  {
    public void setBounds() {
      super.setBounds();
    }
  }

  public static void main(String[] args)
  {
    SwingFramePeer s = new SwingFramePeer();
    s.setBounds();
  }
}

