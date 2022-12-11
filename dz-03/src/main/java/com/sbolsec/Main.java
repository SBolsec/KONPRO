package com.sbolsec;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class Main {

    public static final int MAP_SIZE = 5;

    public static void main(String[] args) throws InterruptedException {
        final LocationServer locationServer = new LocationServer(MAP_SIZE, MAP_SIZE);

        final Agent[] agents = {
            new Agent(1, locationServer), new Agent(2, locationServer),
            new Agent(3, locationServer), new Agent(4, locationServer)
        };

        final ExecutorService poll = Executors.newFixedThreadPool(4);

        for (Agent agent : agents) {
            poll.submit(agent);
        }

        poll.awaitTermination(60, TimeUnit.SECONDS);
        System.out.println("Main: done");
    }

}
