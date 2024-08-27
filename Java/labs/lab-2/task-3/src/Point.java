import java.util.*;

class Point {
    private float x, y;

    // TODO: Add constructor.
    public Point(float x, float y) {
        this.x = x;
        this.y = y;
    }

    // TODO: Add changeCoords.
    void changeCoords(float x, float y) {
        this.x = x;
        this.y = y;
    }

    public String toString() {
        return ("(" + this.x + ", " + this.y + ")");
    }

    // TODO: Add showPoint.
    void showPoint() {
        System.out.println(this.toString());
    }
}
