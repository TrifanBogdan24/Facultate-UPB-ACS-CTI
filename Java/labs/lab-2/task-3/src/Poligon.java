class Polygon {

    private Point[] pcts;

    // TODO: Add constructors.

    // va construi N puncte, initializate cu 0
    public Polygon(int n) {
        this.pcts = new Point[n];
    }

    // va construi puncte care vor lua [toate] valorile primite
    public Polygon(float[] coord) {
        this(coord.length / 2);
        for (int i = 0; i < coord.length / 2; i++) {
            this.pcts[i] = new Point(coord[2 * i], coord[2 * i + 1]);
        }
    }

    // TODO: Add showPolygon.
    void showPolygon() {
        int nr_pct = this.pcts.length;
        for (int i = 0; i < nr_pct; i++) {
            this.pcts[i].showPoint();
        }
    }

}
