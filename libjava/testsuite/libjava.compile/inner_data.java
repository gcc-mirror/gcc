// Test referencing protected data from parent of inner class.

import java.util.Random;

public class inner_data
{
    private class Randomer extends Super {
	public long xxx ()
	{
	    return seed;
	}
    }
}

class Super
{
  protected long seed;
}
