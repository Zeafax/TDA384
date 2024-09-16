import java.util.HashMap;
import java.util.concurrent.Semaphore;

import TSim.*;

public class Lab1 {

  public Lab1(int speed1, int speed2) {
    SemaphoreManager SemMan = new SemaphoreManager();
    TSimInterface tsi = TSimInterface.getInstance();
    
    TrainBrain tb1 = new TrainBrain(SemMan,tsi,1,speed1);
    TrainBrain tb2 = new TrainBrain(SemMan,tsi,2,speed2);

    Thread t1 = new Thread(tb1);
    Thread t2 = new Thread(tb2);
    t1.start();
    t2.start();
    
  }
  private class TrainBrain implements Runnable {
    int tId, trainSpeed;
    TSimInterface tsi;
    SemaphoreManager semMan;
    
    HashMap<String, String> swtich = new HashMap<String, String>();

    TrainBrain(SemaphoreManager semMan, TSimInterface tsi, int tId, int trainSpeed){
      this.semMan = semMan;
      this.tsi = tsi;
      this.tId = tId;
      this.trainSpeed = trainSpeed;
    }

    public void run(){
      //Initial information--------------------------------//
        System.out.println("tId: "+this.tId);
        System.out.println(Thread.currentThread().getName()
                         + ", executing run() method!");
      //---------------------------------------------------//
    switch (tId){
      case 1: 
        try {
          tsi.setSpeed(tId,trainSpeed);
          semMan.semA.acquire();
        }
        catch (Exception e) {
          e.printStackTrace();    // or only e.getMessage() for the error
          System.exit(1);
        }
        break;
      case 2: 
        try {
          tsi.setSpeed(tId,trainSpeed);
          semMan.semH.acquire();
        }
        catch (Exception e) {
          e.printStackTrace();    // or only e.getMessage() for the error
          System.exit(1);
        }
        break;
    }

    while (true){
      SensorEvent sEvent = null;
      try {
        sEvent = tsi.getSensor(tId);
      } catch (Exception e) {
        // TODO: handle exception
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
      }

      if (sEvent.getXpos() == 14 && sEvent.getYpos() == 3 && sEvent.getStatus() == 0x01){ // Train at sensor A
        System.out.println("Sensor: A, Train: "+tId);
        continue;
      }

      if (sEvent.getXpos() == 14 && sEvent.getYpos() == 3 && sEvent.getStatus() == 0x01){ // Train at sensor B
        System.out.println("Sensor: A, Train: "+tId);
        continue;
      }
      

      



    }


    }

    
}
   
}
class SemaphoreManager {
  public Semaphore semA, semB, semC, semD, semE, semF, semG, semH;
  public SemaphoreManager(){
    this.semA = new Semaphore(1);
    this.semB = new Semaphore(1);
    this.semC = new Semaphore(1);
    this.semD = new Semaphore(1);
    this.semE = new Semaphore(1);
    this.semF = new Semaphore(1);
    this.semG = new Semaphore(1);
    this.semH = new Semaphore(1);
  }
}

class Swiches{
  static class Switch {
    private int x;
    private int y;  
  

  public Switch(int x, int y){
    this.x = x;
    this.y = y;
    }
  
  public int getX(){
    return x;
  }
  
  public int getY(){
    return y;
  }
  
  }
  public static final Switch A = new Switch(17, 7);
  public static final Switch B = new Switch(15, 9);
  public static final Switch C = new Switch(4, 9);
  public static final Switch D = new Switch(3, 11);
}


