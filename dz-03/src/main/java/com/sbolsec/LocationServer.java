package com.sbolsec;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class LocationServer {

    private final Point mapSize;

    private final Map<Point, Integer> map = new HashMap<>();

    private static final Random random = new Random();

    public LocationServer(int mapWidth, int mapHeight) {
        this.mapSize = new Point(mapWidth, mapHeight);
    }

    public synchronized Point setAgentToRandomPoint(int agentId) {
        System.out.format("LocationServer: Initializing agent %d to random starting point %n", agentId);

        while (true) {
            int x = random.nextInt(0, mapSize.getX());
            int y = random.nextInt(0, mapSize.getY());
            final Point point = new Point(x, y);

            if (!map.containsKey(point)) {
                System.out.format("LocationServer: Setting agent %d to point %s %n", agentId, point);

                map.put(point, agentId);
                printMap();
                return point;
            }
        }
    }

    public synchronized void moveOnePlaceRandomly(int agentId) {
        final Point currentLocation = map.entrySet().stream()
            .filter(e -> e.getValue().equals(agentId))
            .findFirst()
            .get()
            .getKey();
        map.remove(currentLocation);

        final Point newLocation = randomMoveToValidLocation(currentLocation);
        map.put(newLocation, agentId);

        System.out.format("Moved agent %d %n", agentId);
        printMap();
    }

    public void printMap() {
        final StringBuilder sb = new StringBuilder();
        sb.append("┌").append("─┬".repeat(mapSize.getX()-1)).append("─┐").append("\n");

        for (int height = 0; height < mapSize.getY(); height++) {
            sb.append("│");
            for (int width = 0; width < mapSize.getX(); width++) {
                final Integer agent = map.get(new Point(width, height));
                sb.append(String.format("%s│", agent == null ? " " : agent));
            }
            sb.append("\n");
            if (height != mapSize.getY() - 1) {
                sb.append("├").append("─┼".repeat(mapSize.getX()-1)).append("─┤").append("\n");
            }
        }
        sb.append("└").append("─┴".repeat(mapSize.getX()-1)).append("─┘").append("\n");

        System.out.println(sb);
    }

    private Point randomMoveToValidLocation(Point startingPoint) {
        while (true) {
            final MoveDirection randomMoveDirection = MoveDirection.randomDirection();

            if (isValidMove(startingPoint, randomMoveDirection)) {
                System.out.println("LocationServer: Move point in direction: " + randomMoveDirection);
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
