import TSim.*;
import java.util.concurrent.Semaphore;

public class Lab1 {
  Semaphore semA = new Semaphore(1);
  Semaphore semB = new Semaphore(1);
  Semaphore semC = new Semaphore(1);
  Semaphore semD = new Semaphore(1);
  Semaphore semE = new Semaphore(1);
  Semaphore semF = new Semaphore(1);

  TSimInterface tsi = TSimInterface.getInstance();

  public Lab1(int speed1, int speed2) {

    TrainBrain tb1 = new TrainBrain(1, speed1);
    TrainBrain tb2 = new TrainBrain(2, speed2);

    Thread t1 = new Thread(tb1);
    Thread t2 = new Thread(tb2);

    t1.start();
    t2.start();

  }

  private class TrainBrain implements Runnable {
    int tId, trainSpeed;
    String lastStation;
    boolean onAlternateRoute;

    static final String[] sensors = { "15,3", "15,5", "8,5", "6,7", "10,8", "10,7", "14,7", "15,8", "19,7", "17,9",
        "13,10", "13,9", "6,9", "6,10", "2,9", "1,11", "3,13", "5,11", "15,11", "15,13" };

    TrainBrain(int tId, int trainSpeed) {
      this.tId = tId;
      this.trainSpeed = trainSpeed;
      this.onAlternateRoute = false;
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

          for (int i = 0; i < sensors.length; i++) {
            if (sensors[i].equals(sensor_string)) {
              sensor_pos = i;
            }
          }

          switch (sensor_pos) {
            case 0, 1 -> this.waitStation(sEvent);
            case 2, 3 -> this.handle4way(semB, "NORTH", sEvent);
            case 4, 5 -> this.handle4way(semB, "SOUTH", sEvent);
            case 6, 7 ->
              this.handleJunction(semC, Swiches.A, TSimInterface.SWITCH_RIGHT, "NORTH", false, sEvent);
            case 8 ->
              this.handleJunction(semA, Swiches.A, TSimInterface.SWITCH_RIGHT, "SOUTH", true, sEvent);
            case 9 ->
              this.handleJunction(semD, Swiches.B, TSimInterface.SWITCH_RIGHT, "NORTH", true, sEvent);
            case 10, 11 ->
              this.handleJunction(semC, Swiches.B, TSimInterface.SWITCH_RIGHT, "SOUTH", false, sEvent);
            case 12, 13 ->
              this.handleJunction(semE, Swiches.C, TSimInterface.SWITCH_LEFT, "NORTH", false, sEvent);
            case 14 ->
              this.handleJunction(semD, Swiches.C, TSimInterface.SWITCH_LEFT, "SOUTH", true, sEvent);
            case 15 ->
              this.handleJunction(semF, Swiches.D, TSimInterface.SWITCH_LEFT, "NORTH", true, sEvent);
            case 16, 17 ->
              this.handleJunction(semE, Swiches.D, TSimInterface.SWITCH_LEFT, "SOUTH", false, sEvent);
            case 18, 19 -> this.waitStation(sEvent);
            default -> System.err.println("Sensor: " + sensor_pos + " not detected");
          }
        }

      } catch (Exception e) {
        e.printStackTrace(); // or only e.getMessage() for the error
        System.exit(1);
      }

    }

    private void waitStation(SensorEvent sEvent) throws Exception {
      if (sEvent.getStatus() == SensorEvent.INACTIVE)
        return;
      tsi.setSpeed(tId, 0);
      Thread.sleep(1000 + 20 * Math.abs(this.trainSpeed));
      int newspeed = -this.trainSpeed;
      this.trainSpeed = newspeed;
      tsi.setSpeed(tId, trainSpeed);
      lastStation = lastStation.equals("NORTH") ? "SOUTH" : "NORTH";
    }

    private void handleJunction(Semaphore sem, Swiches.Switch target_Switch, int primaryDir, String pickupDir,
        Boolean altRoute, SensorEvent sEvent) throws Exception {
      // Makes sure pickup is dont on an active sensor event
      if (lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.ACTIVE)
        Pickup(sem, target_Switch, altRoute, primaryDir);
      // Makes sure release is done on an inactive sensor event
      if (!lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.INACTIVE)
        Release(sem, altRoute);
    }

    private void Pickup(Semaphore sem, Swiches.Switch target_Switch, Boolean altRoute, int primaryDir)
        throws Exception {
      if (!sem.tryAcquire()) {
        if (altRoute) { // alt route
          tsi.setSwitch(target_Switch.getX(), target_Switch.getY(), (primaryDir == 1) ? 2 : 1);
          onAlternateRoute = true;
          return;
        }
        // no alt route
        tsi.setSpeed(tId, 0);
        sem.acquire();
        tsi.setSpeed(tId, trainSpeed);
      }
      int alternateDir = (primaryDir == 1) ? 2 : 1;
      tsi.setSwitch(target_Switch.getX(), target_Switch.getY(), onAlternateRoute ? alternateDir : primaryDir);
    }

    private void Release(Semaphore sem, Boolean altRoute) throws Exception {
      if (onAlternateRoute && altRoute) { // makes sure release is not done on alternate route
        onAlternateRoute = false;
        return;
      }
      sem.release();

    }

    private void handle4way(Semaphore sem, String pickupDir, SensorEvent sEvent) throws Exception { // semB
      if (lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.ACTIVE) { // pickup
        if (!sem.tryAcquire()) {
          tsi.setSpeed(tId, 0);
          sem.acquire();
          tsi.setSpeed(tId, trainSpeed);
        }
      }
      if (!lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.INACTIVE) { // release
        sem.release();
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
  }
}
