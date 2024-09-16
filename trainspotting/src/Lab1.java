import java.util.HashMap;
import java.util.concurrent.Semaphore;

import TSim.*;

public class Lab1 {

  public Lab1(int speed1, int speed2) {
    SemaphoreManager SemMan = new SemaphoreManager();
    TSimInterface tsi = TSimInterface.getInstance();
    
    TrainBrain tb1 = new TrainBrain(SemMan,tsi,1,speed1,"NORTH");
    TrainBrain tb2 = new TrainBrain(SemMan,tsi,2,speed2, "SOUTH");

    Thread t1 = new Thread(tb1);
    Thread t2 = new Thread(tb2);
    t1.start();
    t2.start();
    
  }
  private class TrainBrain implements Runnable {
    int tId, trainSpeed;
    
    TSimInterface tsi;
    SemaphoreManager semMan;
    String lastStation;

    static final String[] sensors = {"16,3","8,6","16,5","7,7","8,8","9,7","16,7","17,8","18,7","3,9","4,10","5,9","14,9","15,10","16,9","2,11","4,1","16,11","3,12","16,13"};
    static final int ACTIVE = 0x01;
	  static final int INACTIVE = 0x02; 

    TrainBrain(SemaphoreManager semMan, TSimInterface tsi, int tId, int trainSpeed, String lastStation){
      this.semMan = semMan;
      this.tsi = tsi;
      this.tId = tId;
      this.trainSpeed = trainSpeed;
      this.lastStation = lastStation;
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
          semMan.semA.acquire();
        }
        catch (Exception e) {
          e.printStackTrace();    // or only e.getMessage() for the error
          System.exit(1);
        }
        break;
      case 2: 
        try {
          semMan.semF.acquire();
        }
        catch (Exception e) {
          e.printStackTrace();    // or only e.getMessage() for the error
          System.exit(1);
        }
        break;
    }
    try {
      tsi.setSpeed(tId,trainSpeed);
    }
    catch (Exception e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
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
      if (sEvent.getStatus() == INACTIVE) continue;
      String sensor_string = sEvent.getXpos()+","+sEvent.getYpos();
      int sensor_pos;
      for (int i = 0; i <sensors.length; i++){
        if (sensors[i] == sensor_string){sensor_pos = i;}
       }

      switch (sensor_pos) {
        case 0:
          
          break;
      
        default:
          break;
      }

      if ( sensor_pos == sensors[0]&& sEvent.getStatus() == ACTIVE){ // Train at sensor A
        switch (this.lastStation) {
          case "NORTH":
            break;
          case "SOUTH":
          try {
            tsi.setSpeed(tId, 0);
            wait(1000 + 20*Math.abs(this.trainSpeed));
            this.lastStation = "NORTH" ;
            this.trainSpeed = -this.trainSpeed;
            tsi.setSpeed(tId, trainSpeed);
          } catch (Exception e) {
            // TODO: handle exception
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
          }
            break;
          default:
            System.err.println("NO LAST STATION");
            System.exit(1);
        }
      }

      if (sEvent.getXpos() == 14 && sEvent.getYpos() == 3 && sEvent.getStatus() == ACTIVE){ // Train at sensor B
        System.out.println("Sensor: A, Train: "+tId);
        continue;
      }
    }
    }
    private void waitStation(){
      try {
        tsi.setSpeed(tId, 0);
        wait(1000 + 20*Math.abs(this.trainSpeed));
        this.lastStation = (this.lastStation =="NORTH") ? "SOUTH":"NORTH";
        this.trainSpeed = -this.trainSpeed; // TODO: Mellanvariable?
        tsi.setSpeed(tId, trainSpeed);
      } catch (Exception e) {
        // TODO: handle exception
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
      }
    }
}
   
}
class SemaphoreManager {
  public Semaphore semA, semB, semC, semD, semE, semF;
  public SemaphoreManager(){
    this.semA = new Semaphore(1);
    this.semB = new Semaphore(1);
    this.semC = new Semaphore(1);
    this.semD = new Semaphore(1);
    this.semE = new Semaphore(1);
    this.semF = new Semaphore(1);
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


