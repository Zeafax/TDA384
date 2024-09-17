import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.function.Consumer;

public class Lab1 {
  Semaphore semA = new Semaphore(1);
  Semaphore semB = new Semaphore(1);
  Semaphore semC = new Semaphore(1);
  Semaphore semD = new Semaphore(1);
  Semaphore semE = new Semaphore(1);
  Semaphore semF = new Semaphore(1);

  TSimInterface tsi = TSimInterface.getInstance();
  class Swiches{
    static class Switch {
      private int x;
      private int y;
      private int primaryDir;  
    
  
    public Switch(int x, int y, int primaryDir){
      this.x = x;
      this.y = y;
      }
    
    public int getX(){
      return x;
    }
    
    public int getY(){
      return y;
    }
  
    public Boolean isPrimary(){
    }
    
    }
    public static final Switch A = new Switch(17, 7, TSimInterface.SWITCH_RIGHT);
    public static final Switch B = new Switch(15, 9,TSimInterface.SWITCH_RIGHT);
    public static final Switch C = new Switch(4, 9,TSimInterface.SWITCH_RIGHT);
    public static final Switch D = new Switch(3, 11,TSimInterface.SWITCH_RIGHT);
  }
  class SensorObj {
    Swiches.Switch targetSwitch;
    Semaphore nextSem, prevSem;
    int altRoute;
    int switchDir;
    String pickupDir;
    Consumer<String> action;

    SensorObj(Swiches.Switch targetSwitch,Semaphore nextSem,Semaphore prevSem,int altRoute,int switchDir,String pickupDir,Consumer<String> action){
      this.targetSwitch = targetSwitch;
      this.nextSem = nextSem;
      this.prevSem = prevSem;
      this.altRoute = altRoute;
      this.switchDir = switchDir;
      this.pickupDir = pickupDir;
    }

    public void handle4way(String tDir){
      if (this.pickupDir.equals(tDir)){
      }
    }

    public void handleStation(String tDir){
      
    }

    public void handlejunction(String tDir){
      
    }

    public void PickupSem() throws Exception{
      if(nextSem.tryAcquire()){
        tsi.setSwitch(targetSwitch.getX(),targetSwitch.getY(),switchDir);
        return;
      }
      if(altRoute != 0){
        tsi.setSwitch(targetSwitch.getX(),targetSwitch.getY(),altRoute);
        return;
      }
      nextSem.acquire();
      tsi.setSwitch(targetSwitch.getX(),targetSwitch.getY(),switchDir);
    }


  }
  public Lab1(int speed1, int speed2) {
    
    
    TrainBrain tb1 = new TrainBrain(1,speed1);
    TrainBrain tb2 = new TrainBrain(2,speed2);

    Thread t1 = new Thread(tb1);
    Thread t2 = new Thread(tb2);
    t1.start();
    t2.start();
    
  }
  private class TrainBrain implements Runnable {
    int tId, trainSpeed;
    
    TSimInterface tsi;
    String lastStation;
    Boolean onAltRoute;

    static final String[] sensors = {"16,3","8,6","16,5","7,7","8,8","9,7","16,7","17,8","18,7","3,9","4,10","5,9","14,9","15,10","16,9","2,11","4,11","16,11","3,12","16,13"};

    TrainBrain(int tId, int trainSpeed){
      this.tId = tId;
      this.trainSpeed = trainSpeed;
    }

    public void run(){
      try {
        //Initial information--------------------------------//
          System.out.println("tId: "+this.tId);
          System.out.println(Thread.currentThread().getName()
                          + ", executing run() method!");
        //---------------------------------------------------//
        switch (tId){
          case 1 -> {
              semA.acquire();
              System.out.println(semA);
              lastStation = "NORTH";
              }
          case 2 -> {
              semF.acquire();
              lastStation = "SOUTH";
              System.out.println(semF);
              }
        }
        tsi.setSpeed(tId,trainSpeed);

        while (true){
          SensorEvent sEvent;
          sEvent = tsi.getSensor(tId);
          if (sEvent.getStatus() == SensorEvent.INACTIVE) {continue;}
          String sensor_string = sEvent.getXpos()+","+sEvent.getYpos();
          int sensor_pos = -1;
          System.out.println("Sensor string: "+sensor_string);
          for (int i = 0; i <sensors.length; i++){
            if (sensors[i].equals(sensor_string)){sensor_pos = i;}
          }
          System.out.println("sensor_pos: "+sensor_pos);
          switch (sensor_pos) {
            case 0, 2 -> this.waitStation();
            case 1, 3, 4, 5 -> this.handleJunction(semB,semB,0,0,0,"NORTH",false);
            case 6, 7 -> this.handleJunction(semC,semA,Swiches.A.getX(),Swiches.A.getY(),TSimInterface.SWITCH_RIGHT,"NORTH",false);
            case 8 -> this.handleJunction(semA,semC,Swiches.A.getX(),Swiches.A.getY(),TSimInterface.SWITCH_RIGHT,"SOUTH",true);
            case 9 -> this.handleJunction(semD,semE,Swiches.C.getX(),Swiches.C.getY(),TSimInterface.SWITCH_LEFT,"SOUTH",true);
            case 10, 11 -> this.handleJunction(semE,semD,Swiches.C.getX(),Swiches.C.getY(),TSimInterface.SWITCH_LEFT,"NORTH",false);
            case 12, 13 -> this.handleJunction(semC,semD,Swiches.B.getX(),Swiches.B.getY(),TSimInterface.SWITCH_RIGHT,"SOUTH",false);
            case 14 -> this.handleJunction(semD,semC,Swiches.B.getX(),Swiches.B.getY(),TSimInterface.SWITCH_RIGHT,"NORTH",true);
            case 15 -> this.handleJunction(semF,semE,Swiches.D.getX(),Swiches.D.getY(),TSimInterface.SWITCH_LEFT,"NORTH",true);
            case 16, 18 -> this.handleJunction(semE,semF,Swiches.D.getX(),Swiches.D.getY(),TSimInterface.SWITCH_LEFT,"SOUTH",false);
            case 17, 19 -> this.waitStation();
            default -> System.out.println("Sensor: "+sensor_pos+" not detected");
          }
        }
      }catch (Exception e) {
      // TODO: handle exception
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
      }
    
 
  }

  private void waitStation() throws Exception{
    tsi.setSpeed(tId, 0);
    wait(1000 + 20*Math.abs(this.trainSpeed));
    int newspeed = -this.trainSpeed;
    this.trainSpeed = newspeed; // TODO: Mellanvariable?
    tsi.setSpeed(tId, trainSpeed);
  }
  private void handleJunction(Semaphore newSem, Semaphore prevSem,int switchX, int switchY, int primaryTrack, String primaryDirection, Boolean altRoute) throws Exception{


    System.out.println("tId: "+tId+" primaryDirection: "+primaryDirection+" lastStation: "+lastStation+lastStation.equals(primaryDirection)+ "switch"+switchX+","+switchY);
    System.out.println(newSem);

    
    if (lastStation.equals(primaryDirection)){//Pickup mode
      if (!newSem.tryAcquire()){
        //fails
        if (altRoute){
          tsi.setSwitch(switchX, switchY,(primaryTrack+3)%3);
          System.out.println("!!!!ALT!!!");
          return;
        }
        else{
          tsi.setSpeed(tId, 0);
          newSem.acquire();
          tsi.setSpeed(tId, trainSpeed);
        }
      }
      // priamry track = ends with 1
      tsi.setSwitch(switchX, switchY,(false) ? (primaryTrack+3)%3:primaryTrack); //TODO: Fix this magic
      System.out.println("!!PRimary");

    }
    else{
      System.out.println("Semaphore Releasaed!!: "+prevSem.equals(prevSem));
      prevSem.release();
      System.out.println("Semaphore Releasaed: "+prevSem);
    }
    //Release mode
    
    
    }
  }
   
}



