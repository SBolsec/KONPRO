package com.sbolsec;

import java.util.Objects;

public class Point {

    private final int x;

    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public Point moveUp() {
        return new Point(x, y - 1);
    }

    public Point moveDown() {
        return new Point(x, y + 1);
    }

    public Point moveLeft() {
        return new Point(x - 1, y);
    }

    public Point moveRight() {
        return new Point(x + 1, y);
    }

    public Point move(MoveDirection direction) {
        return switch (direction) {
            case UP -> moveUp();
            case DOWN -> moveDown();
            case LEFT -> moveLeft();
            case RIGHT -> moveRight();
        };
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Point point = (Point) o;
        return x == point.x && y == point.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    @Override
    public String toString() {
        return "(" + "x=" + x + ", y=" + y + ')';
    }

}
