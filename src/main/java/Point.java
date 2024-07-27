public class Point {
    private double x, y;

    public Point(final double x, final double y) {
        this.x = x;
        this.y = y;
    }

    public Point(
            double x, double y,
            boolean addToGrid
    ) {
        this(x, y);
    }

    public Point() {
        this(0.0, 0.0);
    }

    private void addToGrid(Point p) {
        x += p.x;
        y += p.y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

}