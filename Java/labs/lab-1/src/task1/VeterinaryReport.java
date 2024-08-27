package task1;// task 1 : de copiat codul din sectiune

// subpunctul 3 : observ aceasta linie de cod : package task1
// iar, la rulare, apare : // subpunctul 3 : observ aceasta linie de cod : package task1
public class VeterinaryReport {
    int dogs;
    int cats;

    public int getAnimalsCount() {
        return dogs + cats;
    }

    public void displayStatistics() {
        System.out.println("Total number of animals is " + getAnimalsCount());
    }
}
