// Test for thread-local allocation problems.

import java.util.HashMap;
import java.util.Observable;
import java.util.Observer;

class PR18699 extends Observable implements Runnable, Observer {

  public static void main(String[] args) throws InterruptedException {
    PR18699 PR18699_1 = new PR18699();
    PR18699 PR18699_2 = new PR18699();
    PR18699_1.addObserver(PR18699_2);
    PR18699_2.addObserver(PR18699_1);
    new Thread(PR18699_1).start();
    new Thread(PR18699_2).start();
  }

  public void run() {
    int c = 0;
    String s = "";
    while (++c < 50) {
      this.setChanged();
      s = "";
      for (int i = 0; i < 200; i++)
        s += String.valueOf(i);
      this.notifyObservers(s);
    }
  }

  HashMap map = new HashMap();
  
  public void update(Observable o, Object obj) 
  {
    map.put(o, obj);
  }
}
