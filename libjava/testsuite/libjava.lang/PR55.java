public class PR55 {

    // This demonstrates a bug in gcj

    public static void main(String[] argv){
        int i = 666;
        System.out.println("The number "+i+" is "+ (""+i).length() +" digits wide");
    }
}
