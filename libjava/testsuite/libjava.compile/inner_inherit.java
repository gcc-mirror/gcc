// Test to insure that we can refer to methods inherited through an
// inner class.

public class inner_inherit
{
    private class Agent extends Thread {
    }

    public void f ()
    {
	Agent a = new Agent();
	a.setDaemon(true);
    }
}
