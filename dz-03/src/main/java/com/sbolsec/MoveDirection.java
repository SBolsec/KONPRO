package com.sbolsec;

import java.util.Random;

public enum MoveDirection {
    UP, DOWN, LEFT, RIGHT;

    private static final Random random = new Random();

    public static MoveDirection randomDirection() {
        final MoveDirection[] directions = values();

        return directions[random.nextInt(directions.length)];
    }
}
