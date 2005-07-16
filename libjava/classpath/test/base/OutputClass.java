import java.io.*;
import gnu.java.lang.*;
import java.lang.reflect.*;

public class OutputClass {
	public static void main(String[] args) {
		for(int i=1;i<args.length;i++) {
			if(args[i].endsWith(".class")) {
				args[i] = args[i].substring(0,args[i].length()-6);
			}
			args[i] = args[i].replace('/','.');
			try {
			try {
				OutputClass.outputClass(Class.forName(args[i]),System.out);
			} catch(ClassNotFoundException E) {
				new PrintStream(new FileOutputStream(args[i]+".out-"+args[0])).println(args[i] + ": class missing.");
			}
			} catch(IOException e) {
			}
		}
	}

	static void outputClass(Class c, PrintStream out) {
		out.println(c.getName() + ":class:" + Modifier.toString(sanitizeModifier(c.getModifiers())));
		// Put implemented interfaces here
		Field[] f = ClassHelper.getAllFields(c);
		for(int i=0;i<f.length;i++) {
			if(Modifier.isPublic(f[i].getModifiers()) || Modifier.isProtected(f[i].getModifiers())) {
				out.println(
				  c.getName()
				  + ":field:"
				  + Modifier.toString(sanitizeModifier(f[i].getModifiers()))
				  + " "
				  + f[i].getType().getName()
				  + " "
				  + ClassHelper.getTruncatedName(f[i].getName()));
			}
		}

		Method[] m = ClassHelper.getAllMethods(c);
		for(int i=0;i<m.length;i++) {
			if(Modifier.isPublic(m[i].getModifiers()) || Modifier.isProtected(m[i].getModifiers())) {
				out.println(
				  c.getName()
				  + ":method:"
				  + Modifier.toString(sanitizeModifier(m[i].getModifiers()))
				  + " "
				  + m[i].getReturnType().getName()
				  + " "
				  + ClassHelper.getTruncatedName(m[i].getName())
				  + "("
				  + printArgs(m[i].getParameterTypes())
				  + ")"
				  + printExceptions(m[i].getExceptionTypes()));
			}
		}

		Constructor[] cl = c.getDeclaredConstructors();
		for(int i=0;i<cl.length;i++) {
			if(Modifier.isPublic(cl[i].getModifiers()) || Modifier.isProtected(cl[i].getModifiers())) {
				out.println(
				  c.getName()
				  + ":constructor:"
				  + Modifier.toString(sanitizeModifier(cl[i].getModifiers()))
				  + " "
				  + ClassHelper.getTruncatedName(cl[i].getName())
				  + "("
				  + printArgs(cl[i].getParameterTypes())
				  + ")"
				  + printExceptions(m[i].getExceptionTypes()));
			}
		}
	}

	static String printArgs(Class[] args) {
		StringBuffer sb = new StringBuffer();
		for(int i=0;i<args.length;i++) {
			sb.append(args[i]);
			if(i!=args.length-1) {
				sb.append(",");
			}
		}
		return sb.toString();
	}

	static String printExceptions(Class[] exceptions) {
		StringBuffer sb = new StringBuffer();
		for(int i=0;i<exceptions.length;i++) {
			sb.append(" ");
			sb.append(exceptions[i].getName());
		}
		return sb.toString();
	}

	static int sanitizeModifier(int modifier) {
		return modifier & ~(Modifier.SYNCHRONIZED | Modifier.TRANSIENT | Modifier.VOLATILE | Modifier.NATIVE);
	}
}
