// PR 133

// System.exit(0) appears to hang the program.

public class pr133 {
    public static void main(String argv[])
    {
        String name;

        if (argv.length == 0) {
            name = "Rocket J. Squirrel";
        } else {
            name = argv[0];
        }

        new pr133(name).identifySelf();
        System.out.println("goodbye");

        System.exit(0);
    }

    public pr133 (String name)
    {
        v_name = name;
    }
    
    public void identifySelf()
    {
        System.out.println("This is a pr133 instance named " + v_name);
        System.out.println("Leaving identifySelf()...");
    }

    private String v_name;
}
