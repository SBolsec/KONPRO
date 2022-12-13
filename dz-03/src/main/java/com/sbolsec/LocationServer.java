package com.sbolsec;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.Semaphore;

public class LocationServer {

    private final Point mapSize;

    private final Map<Point, Integer> agentMap = new HashMap<>();

    private final Semaphore[][] semaphores;

    private static final Random random = new Random();

    public LocationServer(int mapWidth, int mapHeight) {
        this.mapSize = new Point(mapWidth, mapHeight);
        this.semaphores = new Semaphore[mapHeight][mapWidth];

        for (int height = 0; height < mapHeight; height++) {
            for (int width = 0; width < mapWidth; width++) {
                semaphores[height][width] = new Semaphore(1);
            }
        }
    }

    public void setAgentToRandomPoint(int agentId) {
        System.out.format("LocationServer: Initializing agent %d to random starting point %n", agentId);

        try {
            while (true) {
                int x = random.nextInt(0, mapSize.getX());
                int y = random.nextInt(0, mapSize.getY());
                final Point point = new Point(x, y);

                final Semaphore semaphore = semaphores[y][x];
                semaphore.acquire();

                System.out.format("LocationServer: Setting agent %d to point %s %n", agentId, point);

                agentMap.put(point, agentId);
                printMap();
                break;
            }
        } catch (InterruptedException ignored) {
        }
    }

    public void moveOnePlaceRandomly(int agentId) {
        final Point currentLocation = agentMap.entrySet().stream()
            .filter(e -> e.getValue().equals(agentId))
            .findFirst()
            .get()
            .getKey();

        final Point newLocation = moveInRandomDirectionToValidLocation(currentLocation, agentId);

        final Semaphore oldSemaphore = semaphores[currentLocation.getY()][currentLocation.getX()];
        final Semaphore newSemaphore = semaphores[newLocation.getY()][newLocation.getX()];

        try {
            newSemaphore.acquire();
            agentMap.put(newLocation, agentId);
            agentMap.remove(currentLocation);
            printMap();
            oldSemaphore.release();
        } catch (InterruptedException ignored) {

        }
    }

    public void printMap() {
        final StringBuilder sb = new StringBuilder();
        sb.append("┌").append("─┬".repeat(mapSize.getX() - 1)).append("─┐").append("\n");

        for (int height = 0; height < mapSize.getY(); height++) {
            sb.append("│");
            for (int width = 0; width < mapSize.getX(); width++) {
                final Integer agent = agentMap.get(new Point(width, height));
                sb.append(String.format("%s│", agent == null ? " " : agent));
            }
            sb.append("\n");
            if (height != mapSize.getY() - 1) {
                sb.append("├").append("─┼".repeat(mapSize.getX() - 1)).append("─┤").append("\n");
            }
        }
        sb.append("└").append("─┴".repeat(mapSize.getX() - 1)).append("─┘").append("\n");

        System.out.println(sb);
    }

    private Point moveInRandomDirectionToValidLocation(Point startingPoint, int agentId) {
        while (true) {
            final MoveDirection randomMoveDirection = MoveDirection.randomDirection();

            if (isValidMove(startingPoint, randomMoveDirection)) {
                System.out.format("LocationServer: Move agent %d in direction: %s %n", agentId, randomMoveDirection);
                return startingPoint.move(randomMoveDirection);
            }
        }
    }

    private boolean isValidMove(Point startingPoint, MoveDirection moveDirection) {
        boolean canMoveUp = moveDirection == MoveDirection.UP && startingPoint.getY() != 0;
        boolean canMoveDown = moveDirection == MoveDirection.DOWN && startingPoint.getY() != mapSize.getY() - 1;
        boolean canMoveLeft = moveDirection == MoveDirection.LEFT && startingPoint.getX() != 0;
        boolean canMoveRight = moveDirection == MoveDirection.RIGHT && startingPoint.getX() != mapSize.getX() - 1;

        return canMoveUp || canMoveDown || canMoveLeft || canMoveRight;
    }

}
