import java.beans.*;

public class IntrospectorTest {
	public static void main(String[] args) {
		try {
			BeanInfo b = Introspector.getBeanInfo(java.awt.Component.class);
			if(b.getPropertyDescriptors().length == 6
			   && b.getEventSetDescriptors().length == 5
			   && b.getMethodDescriptors().length == 128) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.awt.Component.class)");
			} else {
				System.out.println("FAILED: Introspector.getBeanInfo(java.awt.Component.class)");
			}
			b = Introspector.getBeanInfo(java.util.BitSet.class);
			if(b.getPropertyDescriptors().length == 2
			   && b.getEventSetDescriptors().length == 0
			   && b.getMethodDescriptors().length == 17) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.util.BitSet.class)");
			} else {
				System.out.println("FAILED: Introspector.getBeanInfo(java.util.BitSet.class)");
			}
			b = Introspector.getBeanInfo(java.lang.Object.class);
			if(b.getPropertyDescriptors().length == 1
			   && b.getEventSetDescriptors().length == 0
			   && b.getMethodDescriptors().length == 9) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.lang.Object.class)");
			} else {
				System.out.println("FAILED: Introspector.getBeanInfo(java.lang.Object.class)");
			}
			b = Introspector.getBeanInfo(java.applet.Applet.class);
			if(b.getPropertyDescriptors().length == 24
			   && b.getEventSetDescriptors().length == 6
			   && b.getMethodDescriptors().length == 168) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.applet.Applet.class)");
			} else {
				System.out.println("FAILED: Introspector.getBeanInfo(java.applet.Applet.class)");
			}
			b = Introspector.getBeanInfo(java.awt.Button.class);
			if(b.getPropertyDescriptors().length == 8
			   && b.getEventSetDescriptors().length == 6
			   && b.getMethodDescriptors().length == 134) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.awt.Button.class)");
			} else {
				System.out.println("FAILED: Introspector.getBeanInfo(java.awt.Button.class)");
			}
			b = Introspector.getBeanInfo(java.applet.Applet.class,java.awt.Panel.class);
			if(b.getPropertyDescriptors().length == 8
			   && b.getEventSetDescriptors().length == 0
			   && b.getMethodDescriptors().length == 22) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.applet.Applet.class,java.awt.Panel.class)");
			} else {
				System.out.println(b.getPropertyDescriptors().length + " " + b.getEventSetDescriptors().length + " " + b.getMethodDescriptors().length);
				System.out.println("FAILED: Introspector.getBeanInfo(java.applet.Applet.class,java.awt.Panel.class)");
			}
			b = Introspector.getBeanInfo(java.applet.Applet.class,java.awt.Component.class);
			if(b.getPropertyDescriptors().length == 18
			   && b.getEventSetDescriptors().length == 1
			   && b.getMethodDescriptors().length == 65) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.applet.Applet.class,java.awt.Component.class)");
			} else {
				System.out.println(b.getPropertyDescriptors().length + " " + b.getEventSetDescriptors().length + " " + b.getMethodDescriptors().length);
				System.out.println("FAILED: Introspector.getBeanInfo(java.applet.Applet.class,java.awt.Component.class)");
			}
			b = Introspector.getBeanInfo(java.applet.Applet.class,java.lang.Object.class);
			if(b.getPropertyDescriptors().length == 24
			   && b.getEventSetDescriptors().length == 6
			   && b.getMethodDescriptors().length == 160) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.applet.Applet.class,java.lang.Object.class)");
			} else {
				System.out.println(b.getPropertyDescriptors().length + " " + b.getEventSetDescriptors().length + " " + b.getMethodDescriptors().length);
				System.out.println("FAILED: Introspector.getBeanInfo(java.applet.Applet.class,java.lang.Object.class)");
			}

			b = Introspector.getBeanInfo(java.applet.Applet.class,null);
			if(b.getPropertyDescriptors().length == 24
			   && b.getEventSetDescriptors().length == 6
			   && b.getMethodDescriptors().length == 168) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.applet.Applet.class,java.lang.Object.class)");
			} else {
				System.out.println(b.getPropertyDescriptors().length + " " + b.getEventSetDescriptors().length + " " + b.getMethodDescriptors().length);
				System.out.println("FAILED: Introspector.getBeanInfo(java.applet.Applet.class,null)");
			}

			b = Introspector.getBeanInfo(java.applet.Applet.class);
			if(b.getPropertyDescriptors().length == 24
			   && b.getEventSetDescriptors().length == 6
			   && b.getMethodDescriptors().length == 168) {
				System.out.println("PASSED: Introspector.getBeanInfo(java.applet.Applet.class) 2nd time");
			} else {
				System.out.println("FAILED: Introspector.getBeanInfo(java.applet.Applet.class) 2nd time");
			}
		} catch(IntrospectionException e) {
			System.out.println("FAILED: IntrospectionException");
			e.printStackTrace();
		}
	}
}
