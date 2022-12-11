package com.sbolsec;

import java.util.concurrent.ThreadLocalRandom;

public class Agent implements Runnable {

    public static final int NUMBER_OF_MOVES = 10;

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
        System.out.format("Agent %d: finished initializing agent to map %n", id);

        for (int i = 0; i < NUMBER_OF_MOVES; i++) {
            locationServer.moveOnePlaceRandomly(id);

            try {
                Thread.sleep(ThreadLocalRandom.current().nextLong(2, 5) * 1000);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

}
