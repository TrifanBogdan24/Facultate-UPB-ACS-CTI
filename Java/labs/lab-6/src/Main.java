import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;

import java.time.*;

public class Main {
    public static Random rand = new Random(20);
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int taskNum = scanner.nextInt();

        switch(taskNum) {
            case 1:
                // TODO: uncomment for Task1
                 Car mercedes1 = new Car(20000, Car.CarType.MERCEDES, 2019);
                 System.out.println(mercedes1);

                 Car fiat1 = new Car(7000, Car.CarType.FIAT, 2020);
                 System.out.println(fiat1);

                 Car skoda1 = new Car(12000, Car.CarType.SKODA, 2022);
                 System.out.println(skoda1);

                 Dealership dealership1 = new Dealership();
                 System.out.println(dealership1);

                break;
            case 2:
                // TODO: uncomment for Task2
                 List<Car> cars2 = new ArrayList<>();

                 cars2.add(new Car(20000, Car.CarType.MERCEDES, 2011));
                 cars2.add(new Car(35000, Car.CarType.MERCEDES, 2016));
                 cars2.add(new Car(3500, Car.CarType.FIAT, 2009));
                 cars2.add(new Car(7000, Car.CarType.FIAT, 2011));
                 cars2.add(new Car(12000, Car.CarType.SKODA, 2016));
                 cars2.add(new Car(25000, Car.CarType.SKODA, 2022));

                 Dealership dealership2 = new Dealership();

                 for (Car car : cars2) {
                     System.out.println("The price for " + car + " after applying discounts: " + dealership2.getDiscountedPrice(car) + "\n");
                 }

                break;
            case 3:
                // TODO: uncomment for Task3
                 Car mercedes3 = new Car(20000, Car.CarType.MERCEDES, 2020);
                 Dealership dealership3 = new Dealership();

                 System.out.println("Final price for " + mercedes3 + " " + dealership3.getFinalPrice(mercedes3));

                break;
            case 4:
                // TODO: uncomment for Task4
                 List<Car> cars4 = new ArrayList<>();

                // TODO: Add cars here
                cars4.add(new Car(30000, Car.CarType.MERCEDES, 2020));
                cars4.add(new Car(50000, Car.CarType.MERCEDES, 2022));
                cars4.add(new Car(10000, Car.CarType.FIAT, 2019));
                cars4.add(new Car(20000, Car.CarType.SKODA, 2020));

                 System.out.println("Before filtering");
                 for (Car car : cars4) {
                     System.out.println(car);
                 }

                // TODO: Remove expensive cars here
                // task 4
                cars4.removeIf((v) -> v.getPrice() > 25000);
                // expresia lambda din laborator : values.removeIf((v) -> v % 2 == 0);

                 System.out.println("After filtering");
                 for (Car car : cars4) {
                     System.out.println(car);
                 }

                break;
        }
    }
}




// TODO: Add your classes here
// TODO : task 1

class Car {

    // TODO : tipurile de masini
    static enum CarType {
        MERCEDES,
        FIAT,
        SKODA
    }



    private Integer price;
    private Integer year;

    private CarType type;

    public Car(Integer price, CarType type, Integer year) {
        this.price = price;
        this.year = year;
        this.type = type;
    }

    public void setPrice(Integer price) {
        this.price = price;
    }
    public Integer getPrice() {
        return this.price;
    }

    public Integer getYear() {
        return this.year;
    }

    public CarType getType() {
        return this.type;
    }

    @Override
    public String toString() {
        return ("Car{price=" + this.price + ", carType=" + this.type + ", year=" + this.year +"}");
    }
}

// TODO : task 1

interface Offer {
    Integer getDiscount(Car car);
}

class Dealership {

    private final static Random rand = new Random(20);

    public Dealership() {
    }

    @Override
    public String toString() {
        return "Dealership";
    }

    // TODO : task 2
    private class BrandOffer implements Offer {
        public Integer getDiscount(Car car){

            if (car.getType() == Car.CarType.MERCEDES) {
                return (5 * car.getPrice() / 100);
            } else if (car.getType() == Car.CarType.FIAT) {
                return (10 * car.getPrice() / 100);
            } else if (car.getType() == Car.CarType.SKODA) {
                return  (15 * car.getPrice() / 100);
            }

            return 0;
        }
    }

    // TODO : task 2
    private class DealerOffer implements Offer {

        public Integer getDiscount(Car car) {

            Integer anulCurent = Year.now().getValue();

            if (car.getYear() >= anulCurent) {
                return 0;
            }

            if (car.getType() == Car.CarType.MERCEDES) {
                return 300 * (anulCurent - car.getYear());
            } else if (car.getType() == Car.CarType.FIAT) {
                return 100 * (anulCurent - car.getYear());
            } else if (car.getType() == Car.CarType.SKODA) {
                return 150 * (anulCurent - car.getYear());
            }

            return 0;
        }
    }

    // TODO : task 2
    private class SpecialOffer implements Offer {
        public Integer getDiscount(Car car) {

            if (car.getPrice() > 1000) {
                return rand.nextInt(1000);
            }
            return 0;
        }
    }

    // TODO : task 2
    public Integer getDiscountedPrice(Car car) {
        Integer valoare = 0, suma = 0;

        valoare = new BrandOffer().getDiscount(car);
        System.out.println("Applying Brand discount: " + valoare + " euros");
        suma = suma + valoare;

        valoare = new DealerOffer().getDiscount(car);
        System.out.println("Applying Dealer discount: " + valoare + " euros");
        suma = suma + valoare;

        valoare = new SpecialOffer().getDiscount(car);
        System.out.println("Applying Special discount: " + valoare + " euros");
        suma = suma + valoare;

        return car.getPrice() - suma;

    }



    // TODO : task 3
    class OfertaAnonima implements Offer {
        public Integer getDiscount(Car car) {
            return (5 * car.getPrice() / 100);
        }
    }

    // TODO : task 3
    public Integer getFinalPrice(Car car) {
        Integer pretRedusDefault = this.getDiscountedPrice(car);
        Integer negociere = new OfertaAnonima().getDiscount(car);

        System.out.println("Applying Client discount: " + negociere + " euros");
        return pretRedusDefault - negociere;
    }

    // task3 ends here
}
