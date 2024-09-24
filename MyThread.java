
 
  public class MyThread extends Thread {

    public static void main(String[] args){
      MyThread myThread = new MyThread();
      myThread.start();
}

    public void run(){
       System.out.println("MyThread running");
    }
  }



