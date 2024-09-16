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

    static final String[] sensors = {"a"};
    static final int ACTIVE = 0x01;
	  static final int INACTIVE = 0x02; 

    TrainBrain(SemaphoreManager semMan, TSimInterface tsi, int tId, int trainSpeed, String lastStation){
      this.semMan = semMan;
      this.tsi = tsi;
      this.tId = tId;
      this.trainSpeed = trainSpeed;
      this.lastStation = lastStation; 

      //sens array
      /* this.sensors = new int[20][2];
      this.sensors[0][0] = 16; this.sensors[0][1] = 3;
      this.sensors[1][0] = 8; this.sensors[1][1] = 6;
      this.sensors[2][0] = 16; this.sensors[2][1] = 5;
      this.sensors[3][0] = 7; this.sensors[3][1] = 7;
      this.sensors[4][0] = 8; this.sensors[4][1] = 8;
      this.sensors[5][0] = 9; this.sensors[5][1] = 7;
      this.sensors[6][0] = 16; this.sensors[6][1] = 7;
      this.sensors[7][0] = 17; this.sensors[7][1] = 8;
      this.sensors[8][0] = 18; this.sensors[8][1] = 7;
      this.sensors[9][0] = 3; this.sensors[9][1] = 9;
      this.sensors[10][0] = 4; this.sensors[10][1] = 10;
      this.sensors[11][0] = 5; this.sensors[11][1] = 9;
      this.sensors[12][0] = 14; this.sensors[12][1] = 9;
      this.sensors[13][0] = 15; this.sensors[13][1] = 10;
      this.sensors[14][0] = 16; this.sensors[14][1] = 9;
      this.sensors[15][0] = 2; this.sensors[15][1] = 11;
      this.sensors[16][0] = 4; this.sensors[16][1] = 1;
      this.sensors[17][0] = 16; this.sensors[17][1] = 11;
      this.sensors[18][0] = 3; this.sensors[18][1] = 12;
      this.sensors[19][0] = 16; this.sensors[19][1] = 13; */
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

      if (sEvent.getXpos() == sensors[0][0] && sEvent.getYpos() == sensors[0][1] && sEvent.getStatus() == ACTIVE){ // Train at sensor A
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
  public Semaphore semA, semB, semC, semD, semE, semF, semG, semH;
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


