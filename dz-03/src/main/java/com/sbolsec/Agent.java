package com.sbolsec;

import java.util.concurrent.ThreadLocalRandom;

public class Agent implements Runnable {

    private final int id;

    private final LocationServer locationServer;

    public Agent(int id, LocationServer locationServer) {
        this.id = id;
        this.locationServer = locationServer;
    }

    @Override
    public void run() {
        System.out.format("Agent %d: start initializing agent to map %n", id);
        locationServer.setAgentToRandomPoint(id);

        while (true) {
            locationServer.moveOnePlaceRandomly(id);

            try {
                Thread.sleep(ThreadLocalRandom.current().nextLong(3, 5) * 1000);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

}
