import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import java.util.Hashtable;

public class PropertyChangeSupportTest implements Runnable {
	class Source {
		PropertyChangeSupport support = new PropertyChangeSupport(this);
		public void addPropertyChangeListener(PropertyChangeListener l) {
			support.addPropertyChangeListener(l);
		}
		public void addPropertyChangeListener(String s, PropertyChangeListener l) {
			support.addPropertyChangeListener(s,l);
		}
		public void removePropertyChangeListener(PropertyChangeListener l) {
			support.removePropertyChangeListener(l);
		}
		public void removePropertyChangeListener(String s, PropertyChangeListener l) {
			support.removePropertyChangeListener(s,l);
		}

		void changeProperty(String name) {
			support.firePropertyChange(name,"old","new");
			
		}
	}

	class Listener implements PropertyChangeListener {
		Hashtable numEventsReceived = new Hashtable();
		int getNumEvents(String propertyName) {
			Integer i = (Integer)numEventsReceived.get(propertyName);
			try {
				return i.intValue();
			} catch(NullPointerException e) {
				return 0;
			}
		}

		public void propertyChange(PropertyChangeEvent e) {
			Integer i = (Integer)numEventsReceived.get(e.getPropertyName());
			try {
				int newI = i.intValue() + 1;
				numEventsReceived.put(e.getPropertyName(), new Integer(newI));
			} catch(NullPointerException exc) {
				numEventsReceived.put(e.getPropertyName(), new Integer(1));
			}
		}

		public void reset() {
			numEventsReceived = new Hashtable();
		}
	}

	public void setProperties(Source s) {
		s.changeProperty("foo");
		s.changeProperty("foo");
		s.changeProperty("foo");
		s.changeProperty("bar");
		s.changeProperty("bar");
		s.changeProperty("foobar");
	}

	public void shouldEqual(Listener l, int foo, int bar, int foobar, String testName) {
		String whatsWrong = "";
		if(l.getNumEvents("foo") != foo) {
			whatsWrong += ("foo(" + l.getNumEvents("foo") + ") != " + foo);
		}
		if(l.getNumEvents("bar") != bar) {
			whatsWrong += (" bar(" + l.getNumEvents("bar") + ") != " + bar);
		}
		if(l.getNumEvents("foobar") != foobar) {
			whatsWrong += (" foobar(" + l.getNumEvents("foobar") + ") != " + foobar);
		}

		if(!whatsWrong.equals("")) {
			System.out.println("FAILURE: " + testName + ": " + whatsWrong);
		} else {
			System.out.println("Success: " + testName);
		}
	}

	public void run() {
		Source s = new Source();

		/* Test: single multi-property adds */
		Listener l = new Listener();
		s.addPropertyChangeListener(l);
		setProperties(s);
		shouldEqual(l, 3, 2, 1, "single multi-property adds");

		/* Test: multiple listeners */
		Listener l2 = new Listener();
		s.addPropertyChangeListener(l2);
		setProperties(s);
		shouldEqual(l, 6, 4, 2, "multiple listeners-l");
		shouldEqual(l2, 3, 2, 1, "multiple listeners-l2");

		/* Test: multiple multi-property adds */
		s.addPropertyChangeListener(l);
		setProperties(s);
		shouldEqual(l, 12, 8, 4, "multiple multi-property adds-l");
		shouldEqual(l2, 6, 4, 2, "multiple multi-property adds-l2");

		/* Test: remove multi-property add */
		s.removePropertyChangeListener(l);
		setProperties(s);
		shouldEqual(l, 15, 10, 5, "remove multi-property add-l");
		shouldEqual(l2, 9, 6, 3, "remove multi-property add-l2");


		s.removePropertyChangeListener(l);
		s.removePropertyChangeListener(l2);
		l.reset();
		l2.reset();

		/* ENABLE THIS IF YOU THINK RESET ISN'T HAPPENING
		 shouldEqual(l, 0, 0, 0, "RESET-l");
		 shouldEqual(l2, 0, 0, 0, "RESET-l2");
		 setProperties(s);
		 shouldEqual(l, 0, 0, 0, "RESET_AGAIN-l");
		 shouldEqual(l2, 0, 0, 0, "RESET_AGAIN-l2");
		*/


		/* Test: single property listener */
		s.addPropertyChangeListener("foo", l);
		setProperties(s);
		shouldEqual(l, 3, 0, 0, "single property listener");

		/* Test: multiple different properties */
		s.addPropertyChangeListener("bar", l);
		setProperties(s);
		shouldEqual(l, 6, 2, 0, "multiple different properties");

		/* Test: multiple of same property */
		s.addPropertyChangeListener("foo", l);
		setProperties(s);
		shouldEqual(l, 12, 4, 0, "multiple of same property");

		/* Test: multiple single-property listeners */
		s.addPropertyChangeListener("foo", l2);
		setProperties(s);
		shouldEqual(l, 18, 6, 0, "multiple single-property listeners-l");
		shouldEqual(l2, 3, 0, 0, "multiple single-property listeners-l2");

		/* Test: remove single-property add */
		s.removePropertyChangeListener("foo", l);
		setProperties(s);
		shouldEqual(l, 21, 8, 0, "remove single-property add-l");
		shouldEqual(l2, 6, 0, 0, "remove single-property add-l2");


		s.removePropertyChangeListener("foo", l);
		s.removePropertyChangeListener("bar", l);
		s.removePropertyChangeListener("foo", l2);
		l.reset();
		l2.reset();

		/* ENABLE THIS IF YOU THINK RESET ISN'T HAPPENING
		 shouldEqual(l, 0, 0, 0, "RESET-l");
		 shouldEqual(l2, 0, 0, 0, "RESET-l2");
		 setProperties(s);
		 shouldEqual(l, 0, 0, 0, "RESET_AGAIN-l");
		 shouldEqual(l2, 0, 0, 0, "RESET_AGAIN-l2");
		*/

		/* Test: multiple- and single-property interaction */
		s.addPropertyChangeListener(l);
		s.addPropertyChangeListener("foo", l);
		setProperties(s);
		shouldEqual(l, 6, 2, 1, "multiple- and single-property interaction");

		/* Test: multiple- and single-property interaction: multiple-listener removal */
		s.removePropertyChangeListener(l);
		setProperties(s);
		shouldEqual(l, 9, 2, 1, "multiple- and single-property interaction: multiple-listener removal");

		/* Test: hasListeners() with multiple cases */
		if(s.support.hasListeners("foo")) {
			System.out.println("Success: hasListeners() returning true");
		} else {
			System.out.println("FAILURE: hasListeners() returning true");
		}
		if(s.support.hasListeners("bar")) {
			System.out.println("FAILURE: hasListeners() returning false");
		} else {
			System.out.println("Success: hasListeners() returning false");
		}
		s.addPropertyChangeListener(l);
		if(s.support.hasListeners("bar")) {
			System.out.println("Success: hasListeners() with all-event listeners");
		} else {
			System.out.println("FAILURE: hasListeners() with all-event listeners");
		}
	}

	public static void main(String[] args) {
		new PropertyChangeSupportTest().run();
	}
}