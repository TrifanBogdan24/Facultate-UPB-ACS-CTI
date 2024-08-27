class Animal {
    public void eat() {
        System.out.println("Animal eating");
    }
}

class Wolf extends Animal {
    public void howl() {
        System.out.println("Wolf howling");
    }

    public void eat() {
        System.out.println("Wolf eating");
    }
}

class Snake extends Animal {
    public void bite() {
        System.out.println("Snake biting");
    }
}

public class Test {
    public static void main(String[] args) {
        Animal[] animals = new Animal[2];

        animals[0] = new Wolf();    // Upcasting automat
        animals[1] = new Snake();   // Upcasting automat

        for (int i = 0; i < animals.length; i++) {
            animals[i].eat(); // 1

            if (animals[i] instanceof Wolf) {
                ((Wolf)animals[i]).howl(); // 2
            }

            if (animals[i] instanceof Snake) {
                ((Snake)animals[i]).bite(); // 3
            }
        }
    }
}

