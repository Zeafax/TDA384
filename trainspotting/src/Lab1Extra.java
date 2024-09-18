import TSim.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Lab1Extra {
  Monitor monitorA = new Monitor();
  Monitor monitorB = new Monitor();
  Monitor monitorC = new Monitor();
  Monitor monitorD = new Monitor();
  Monitor monitorE = new Monitor();
  Monitor monitorF = new Monitor();

  TSimInterface tsi = TSimInterface.getInstance();

  public Lab1Extra(int speed1, int speed2) {

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
            monitorA.enter();
            System.out.println(monitorA);
            lastStation = "NORTH";
          }
          case 2 -> {
            monitorF.enter();
            lastStation = "SOUTH";
            System.out.println(monitorA);
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
          switch (sensor_pos) {
            case 0, 1 -> this.waitStation(sEvent);
            case 2, 3 -> this.handle4way(monitorB, "NORTH", sEvent);
            case 4, 5 -> this.handle4way(monitorB, "SOUTH", sEvent);
            case 6, 7 ->
              this.handleJunction(monitorC, Swiches.A, TSimInterface.SWITCH_RIGHT, "NORTH", false, sEvent);
            case 8 ->
              this.handleJunction(monitorA, Swiches.A, TSimInterface.SWITCH_RIGHT, "SOUTH", true, sEvent);
            case 9 ->
              this.handleJunction(monitorD, Swiches.B, TSimInterface.SWITCH_RIGHT, "NORTH", true, sEvent);
            case 10, 11 ->
              this.handleJunction(monitorC, Swiches.B, TSimInterface.SWITCH_RIGHT, "SOUTH", false, sEvent);
            case 12, 13 ->
              this.handleJunction(monitorE, Swiches.C, TSimInterface.SWITCH_LEFT, "NORTH", false, sEvent);
            case 14 ->
              this.handleJunction(monitorD, Swiches.C, TSimInterface.SWITCH_LEFT, "SOUTH", true, sEvent);
            case 15 ->
              this.handleJunction(monitorF, Swiches.D, TSimInterface.SWITCH_LEFT, "NORTH", true, sEvent);
            case 16, 17 ->
              this.handleJunction(monitorE, Swiches.D, TSimInterface.SWITCH_LEFT, "SOUTH", false, sEvent);
            case 18, 19 -> this.waitStation(sEvent);
            default -> System.out.println("Sensor: " + sensor_pos + " not detected");
          }
        }
      } catch (Exception e) {
        // TODO: handle exception
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

    private void handleJunction(Monitor monitor, Swiches.Switch s, int primaryDir, String pickupDir, Boolean altRoute,
        SensorEvent sEvent) throws Exception {
      if (lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.ACTIVE)
        // Pickup(monitor, s, altRoute, primaryDir);
        Pickup(monitor, s, altRoute, primaryDir);

      if (!lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.INACTIVE)
        // Release(monitor, altRoute);
        Release(monitor, altRoute);
    }

    private void Pickup(Monitor monitor, Swiches.Switch s, Boolean altRoute, int primaryDir) throws Exception {
      if (!monitor.tryEnter()) {
        if (altRoute) { // alt route
          tsi.setSwitch(s.getX(), s.getY(), (primaryDir == 1) ? 2 : 1);
          onAlternateRoute = true;
          return;
        }
        // no alt route

        tsi.setSpeed(tId, 0);
        monitor.enter();
        tsi.setSpeed(tId, trainSpeed);
      }
      tsi.setSwitch(s.getX(), s.getY(), onAlternateRoute ? (primaryDir == 1) ? 2 : 1 : primaryDir);
    }

    private void Release(Monitor monitor, Boolean altRoute) throws Exception {
      if (onAlternateRoute && altRoute) {
        onAlternateRoute = false;
        return;
      }
      monitor.leave();

    }

    private void handle4way(Monitor monitor, String pickupDir, SensorEvent sEvent) throws Exception { // semB
      if (lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.ACTIVE) { // pickup
        if (monitor.tryEnter()) {
          tsi.setSpeed(tId, 0);
          monitor.enter();
          tsi.setSpeed(tId, trainSpeed);
        }
      }
      if (!lastStation.equals(pickupDir) && sEvent.getStatus() == SensorEvent.INACTIVE) { // release
        monitor.leave();
      }
    }
  }

  private class Monitor {
    private final Lock lock = new ReentrantLock();
    private final Condition condition = lock.newCondition();
    private boolean trackFree = true;

    public void enter() throws InterruptedException {
      lock.lock();
      while (!trackFree) {
        lock.unlock();
        System.out.println("BEFORE AWAIT");
        condition.await();
        System.out.println("AFTER AWAIT");
        lock.lock();
      }
      trackFree = false;
      lock.unlock();
    }

    public void leave() {
      lock.lock();
      try {
        trackFree = true;
        condition.signal();
      } finally {
        lock.unlock();
      }
    }

    public boolean tryEnter() {
      lock.lock();
      if (trackFree) {
        trackFree = false;
        lock.unlock();
        return true;
      } else {
        lock.unlock();
        return false;
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
}