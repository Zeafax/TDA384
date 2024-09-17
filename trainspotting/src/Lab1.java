import TSim.*;
import java.util.concurrent.Semaphore;

public class Lab1 {
  Semaphore semA = new Semaphore(1);
  Semaphore semB = new Semaphore(1);
  Semaphore semC = new Semaphore(1);
  Semaphore semD = new Semaphore(1);
  Semaphore semE = new Semaphore(1);
  Semaphore semF = new Semaphore(1);
  Semaphore semNorth = new Semaphore(1);
  Semaphore semSouth = new Semaphore(1);

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();

    TrainBrain tb1 = new TrainBrain(tsi, 1, speed1);
    TrainBrain tb2 = new TrainBrain(tsi, 2, speed2);

    Thread t1 = new Thread(tb1);
    Thread t2 = new Thread(tb2);
    t1.start();
    t2.start();

  }

  private class TrainBrain implements Runnable {
    int tId, trainSpeed;

    TSimInterface tsi;
    String lastStation;

    static final String[] sensors = { "16,3", "16,5", "8,5", "6,7", "9,8", "10,7", "15,7", "16,8", "19,7", "17,9",
        "14,10", "13,9", "6,9", "5,10", "2,9", "1,11", "3,13", "5,11", "16,11", "16,13" };
    static final String[] alt_sensors = { "9,8", "16,8", "14,10", "5,10", "3,13" };
    static final int ACTIVE = 0x01;
    static final int INACTIVE = 0x02;

    TrainBrain(TSimInterface tsi, int tId, int trainSpeed) {
      this.tsi = tsi;
      this.tId = tId;
      this.trainSpeed = trainSpeed;
    }

    public void run() {
      try {
        // Initial information--------------------------------//
        System.out.println("tId: " + this.tId);
        System.out.println(Thread.currentThread().getName()
            + ", executing run() method!");
        // ---------------------------------------------------//
        switch (tId) {
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
        tsi.setSpeed(tId, trainSpeed);

        while (true) {
          SensorEvent sEvent;
          sEvent = tsi.getSensor(tId);
          String sensor_string = sEvent.getXpos() + "," + sEvent.getYpos();
          int sensor_pos = -1;
          System.out.println("Sensor string: " + sensor_string);
          for (int i = 0; i < sensors.length; i++) {
            if (sensors[i].equals(sensor_string)) {
              sensor_pos = i;
            }
          }
          System.out.println("sensor_pos: " + sensor_pos);
          // Switch statement to handle the different sensors
          switch (sensor_pos) {
            case 0, 1 -> this.waitStation(sEvent.getStatus());
            case 2, 3 -> this.handleJunction(semB, Swiches.X, 0, "NORTH", false, sensor_string, sEvent.getStatus());
            case 4, 5 -> this.handleJunction(semB, Swiches.X, 0, "SOUTH", false, sensor_string, sEvent.getStatus());
            case 6, 7 ->
              this.handleJunction(semC, Swiches.A, TSimInterface.SWITCH_RIGHT, "NORTH", false, sensor_string,
                  sEvent.getStatus());
            case 8 ->
              this.handleJunction(semA, Swiches.A, TSimInterface.SWITCH_RIGHT, "SOUTH", true, sensor_string,
                  sEvent.getStatus());
            case 9 ->
              this.handleJunction(semD, Swiches.B, TSimInterface.SWITCH_RIGHT, "NORTH", true, sensor_string,
                  sEvent.getStatus());
            case 10, 11 ->
              this.handleJunction(semC, Swiches.B, TSimInterface.SWITCH_RIGHT, "SOUTH", false, sensor_string,
                  sEvent.getStatus());
            case 12, 13 ->
              this.handleJunction(semE, Swiches.C, TSimInterface.SWITCH_LEFT, "NORTH", false, sensor_string,
                  sEvent.getStatus());
            case 14 ->
              this.handleJunction(semD, Swiches.C, TSimInterface.SWITCH_LEFT, "SOUTH", true, sensor_string,
                  sEvent.getStatus());
            case 15 ->
              this.handleJunction(semF, Swiches.D, TSimInterface.SWITCH_LEFT, "NORTH", true, sensor_string,
                  sEvent.getStatus());
            case 16, 17 ->
              this.handleJunction(semE, Swiches.D, TSimInterface.SWITCH_LEFT, "SOUTH", false, sensor_string,
                  sEvent.getStatus());
            case 18, 19 -> this.waitStation(sEvent.getStatus());
            default -> System.out.println("Sensor: " + sensor_pos + " not detected");
          }
        }
      } catch (Exception e) {
        // TODO: handle exception
        e.printStackTrace(); // or only e.getMessage() for the error
        // System.exit(1);
      }

    }

    private void waitStation(int SensorStatus) throws Exception {
      if (SensorStatus == INACTIVE) {
        return;
      }
      tsi.setSpeed(tId, 0);
      int newspeed = -trainSpeed;
      trainSpeed = newspeed;
      tsi.setSpeed(tId, trainSpeed);
      lastStation = lastStation.equals("NORTH") ? "SOUTH" : "NORTH";
    }

    private void handleJunction(Semaphore Sem, Swiches.Switch s, int primaryTrack,
        String primaryDirection, Boolean altRoute, String sensor, int SensorStatus) throws Exception {

      System.out.println("SENSOR: " + sensor + " ACTIVE");
      System.out.println("NEW SEMAPHORE: " + Sem);
      int switchX = s.getX();
      int switchY = s.getY();

      // Determines if the train is in pickup mode or release mode
      if (SensorStatus == ACTIVE) {
        // Pickup mode, which means the train hit a sensor on the way in to the junction
        // and picks up the semaphore
        if (lastStation.equals(primaryDirection)) {// Pickup mode
          System.out.println("PICKUP MODE");
          if (!Sem.tryAcquire()) {
            System.out.println("SEMAPHORE LOCKED");
            // fails
            // If the semaphore is locked and the train has an
            // alternative route, the train will take the alternative route
            if (altRoute && switchX != 0 && switchY != 0) {
              tsi.setSwitch(switchX, switchY, (primaryTrack == 1) ? 2 : 1);
              System.out.println("ALTROUTE ACTIVATED!! " + (primaryTrack + 3) % 3);
              return;
              // else the train will wait until the semaphore is released
            } else {
              tsi.setSpeed(tId, 0);
              Sem.acquire();
              tsi.setSpeed(tId, trainSpeed);
            }
          }

          System.out.println("SEMAPHORE: " + Sem + " ACQUIRED");

          if (switchX == 0 && switchY == 0) { // Intersection without a switch, train always waits
            return;
          }

          boolean alt = false;
          for (int i = 0; i < alt_sensors.length; i++) {
            if (alt_sensors[i].equals(sensor)) {
              alt = true;
            }
          }

          System.out.println("ALTROUTE: " + alt);

          int altTrack = (primaryTrack == 1) ? 2 : 1;

          tsi.setSwitch(switchX, switchY, alt ? altTrack : primaryTrack);

        } else { // Release mode, which means the train hit a sensor on the way out of the
                 // junction and releases the semaphore
          if (Sem.availablePermits() == 0) {
            System.out.println("RELEASE MODE");
            // System.out.println("Semaphore Released!!: " + prevSem.equals(prevSem));
            Sem.release();
            System.out.println("Semaphore Released: " + Sem);
          }

        }
      }
    }
  }
}

class Swiches {
  static class Switch {
    private int x;
    private int y;

    public Switch(int x, int y) {
      this.x = x;
      this.y = y;
    }

    public int getX() {
      return x;
    }

    public int getY() {
      return y;
    }

  }

  public static final Switch A = new Switch(17, 7);
  public static final Switch B = new Switch(15, 9);
  public static final Switch C = new Switch(4, 9);
  public static final Switch D = new Switch(3, 11);
  public static final Switch X = new Switch(0, 0);
}