class Instrument {
    public void play() {}

    static void tune(Instrument i) {
        i.play();
    }
}

// Obiectele Wind sunt instrumente
// deoarece au ca și clasa-parinte clasa Instrument
public class Wind extends Instrument {
    public static void main(String[] args) {
        Wind flute = new Wind();
        Instrument.tune(flute); // !! Upcasting automat pentru că metoda primește
        // un obiect de tip Instrument, nu un obiect de tip Wind
        // Deci ar fi redundant să faci un cast explicit cum ar fi:
        // Instrument.tune((Instrument) flute)
    }
}
