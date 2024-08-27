import java.util.*;
import java.lang.String;

interface Task {
    /**
     * Executes the action characteristic of the task.
     */
    void execute();
}

interface Container {
    /**
     * Removes a Task from the Container.
     *
     * @return the removed Task, if the Container is not empty;
     *         null, otherwise
     */
    Task pop();

    /**
     * Inserts a Task in the Container.
     *
     * @param task the inserted Task
     */
    void push(Task task);

    /**
     * Returns the number of elements from the Container.
     *
     * @return The number of elements in this container.
     */
    int size();

    /**
     * Verifies if the Container is empty or not.
     *
     * @return true,  if the Container is empty
     *         false, otherwise
     */
    boolean isEmpty();

    /**
     * Transfers all the elements that exist in a Container in this Container.
     *
     * @param container the Container from which we should transfer elements.
     *                  After the transfer, container.size() == 0
     */
    void transferFrom(Container container);

    /**
     * Return all the tasks stored in the Container.
     *
     * @return The list of the tasks stored in the Container.
     */
    ArrayList<Task> getTasks();
}


// TODO : 3

// implementarea Stivei
class Stack implements Container {
    private ArrayList<Task> stiva;

    public Stack() {
        // constructor fara parametri
        this.stiva = new ArrayList<Task>();
    }

    @Override
    public ArrayList<Task> getTasks() {
        return this.stiva;
    }

    @Override
    public Task pop() {
        int len = this.stiva.size();

        if (len == 0)
            return null;

        Task ans = this.stiva.get(len - 1);
        this.stiva.remove(len - 1);
        return ans;
    }
    @Override
    public void push(Task task) {
        int len = this.stiva.size();
        this.stiva.add(len, task);
    }

    @Override
    public int size() {
        return this.stiva.size();
    }

    @Override
    public boolean isEmpty() {
        return this.stiva.isEmpty();
    }

    @Override
    public void transferFrom(Container container){
        while (container.isEmpty() == false) {
            this.push(container.pop());
        }
    }
}


// implementarea Cozii
class Queue implements  Container {
    private ArrayList<Task> coada;

    public Queue() {
        // constructor fara parametri
        this.coada = new ArrayList<Task>();
    }


    @Override
    public ArrayList<Task> getTasks() {
        return this.coada;
    }

    @Override
    public Task pop() {
        int len = this.coada.size();

        if (len == 0)
            return null;

        Task ans = this.coada.get(0);
        this.coada.remove(0);
        return ans;
    }

    @Override
    public void push(Task task) {
        int len = this.coada.size();
        this.coada.add(len, task);
    }

    @Override
    public int size() {
        return this.coada.size();
    }

    @Override
    public boolean isEmpty() {
        return this.coada.isEmpty();
    }

    @Override
    public void transferFrom(Container container){
        while (container.isEmpty() == false) {
            this.push(container.pop());
        }
    }
}

class OutTask implements Task {
    // TODO 1.1.1: Create a field to store the message
    private String message;


    // TODO 1.1.2: Implement a constructor
    public OutTask(String message) {
        this.message = message;
    }

    // TODO 1.1.3: Implement the execute() method
    @Override
    public void execute() {
        System.out.println(this.message);
    }
}

// TODO 1.2: Implement RandomOutTask
class RandomOutTask implements Task {

    // TODO 1.2.1: Create a global Random instance which uses 12345 as seed
    //  HINT: use final static to store this
    private final static Random rand = new Random(12345);

    private final int nr;

    // TODO 1.2.2: Generate a random number in constructor
    //  HINT: use a final field
    public RandomOutTask() {
        this.nr = rand.nextInt();
    }

    @Override
    public void execute() {
        System.out.println(this.nr);
    }
}



// TODO 1.3: Implement CounterOutTask
class CounterOutTask implements Task {
    // TODO 1.3.1: Add a global counter
    private static int global_counter = 0;

    public CounterOutTask() {
    }

    @Override
    public void execute() {
        this.global_counter = this.global_counter + 1;
        System.out.println(this.global_counter);
    }
}


// TODO 2: Implement Stack and Queue
//  (transferFrom should move all the elements from
//  source container into the current one)


// TODO 3: Create the following interfaces: Minus, Plus, Mult, Div
interface Minus {
    void minus(float value);
}

interface Plus {
    void plus(float value);
}

interface Mult {
    void mult(float value);
}

interface Div {
    void div(float value);
}

//  and the Operation class which implements them
class Operation implements Minus, Plus, Mult, Div {

    private float nr = 0;

    public Operation() {
        // constructor fara parametri
    }

    public Operation(float nr) {
        // constructor cu un parametru
        this.nr = nr;
    }

    public void setNumber(float nr) {
        this.nr = nr;
    }

    public float getNumber() {
        return this.nr;
    }


    @Override
    public void minus(float value) {
        this.nr = this.nr - value;
    }

    @Override
    public void plus(float value) {
        this.nr = this.nr + value;
    }

    @Override
    public void mult(float value) {
        this.nr = this.nr * value;
    }

    @Override
    public void div(float value) {
        if (value != 0.0) {
            this.nr = this.nr / value;
        } else {
            System.out.println("Division by 0 is not possible");
        }
    }

}


// TODO 4.1: Create Song as a concrete class and
class Song {
    private String name;
    private int id;
    private String composer;

    public  Song() {

    }
    public Song(String name, int id, String composer) {
        this.name = name;
        this.id = id;
        this.composer = composer;
    }

    void setName(String name) {
        this.name = name;
    }

    String getName() {
        return this.name;
    }

    void setId(int id) {
        this.id = id;
    }

    int getId() {
        return this.id;
    }

    void setComposer(String composer) {
        this.composer = composer;
    }

    String getComposer() {
        return this.composer;
    }

    public String toString() {
        String ret = new String("");
        ret = ret + "Song{name='" + this.name + "', id=" + this.id;
        ret = ret + ", composer='" + this.composer +"'}";
        return ret;
    }

}


// TODO 4.1 : Album as an abstract class
abstract class Album {
    public ArrayList<Song> cantece;

    public Album() {
        // constructor fara parametri
        this.cantece = new ArrayList<>();
    }

    public abstract void addSong(Song song);

    public abstract void removeSong(Song song);

    public String toString() {
        // cantece va folosi metoda 'toString' din clasa Song
        return  "Album{songs=" + cantece + "}";
    }

    // metoda va fi folosita in claselee copil DangerousAlbum si ThrillerAlbum
    boolean is_prim(int nr) {
        if (nr < 2) {
            return false;
        }

        if (nr == 2) {
            return  true;
        }

        for (int i = 3; i * i <= nr; i = i + 2) {
            if (nr % i == 0) {
                return false;
            }
        }

        return true;
    }
}

// TODO 4.2: Implement DangerousAlbum, ThrillerAlbum and BadAlbum


// TODO 4.2 : Implement DangerousAlbum
class DangerousAlbum extends Album {

    public DangerousAlbum() {
        // constructor fara parametri
    }


    @Override
    public void addSong(Song song) {
        if (super.is_prim(song.getId()) == true ) {
            cantece.add(song);
        }
    }

    @Override
    public void removeSong(Song song) {
        if (cantece.contains(song)) {
            cantece.remove(song);
        }
    }
}


// TODO 4.2 : implement ThrillerAlbum
class ThrillerAlbum extends Album {

    public ThrillerAlbum() {
        // constructor fara parametri
    }
    @Override
    public void addSong(Song song) {
        if (song.getComposer().equals("Michael Jackson") == true && super.is_prim(song.getId()) == true) {
            cantece.add(song);
        }
    }

    @Override
    public void removeSong(Song song) {
        if (cantece.contains(song)) {
            cantece.remove(song);
        }
    }
}


// TOOD 4.2 : implement BadAlbum
class BadAlbum extends Album {

    public BadAlbum (){
        // constructor fara parametri
    }
    @Override
    public void addSong(Song song) {
        String nume = new String(song.getName());

        if (nume.length() != 3)
            return;

        // verifcam daca contine numai litere
        for (int i = 0; i < 3; i++) {
            char c = nume.charAt(i);
            if ('a' <= c && c <= 'z')
                continue;
            if ('A' <= c && c <= 'Z')
                continue;
            return;
        }

        if (palindrom(song.getId()) == false)
            return;

        cantece.add(song);
    }

    @Override
    public void removeSong(Song song) {
        if (cantece.contains(song)) {
            cantece.remove(song);
        }
    }


    // verificam daca un numar este palindrom
    boolean palindrom(int nr) {
        if (nr < 0) {
            return  false;
        }

        int aux = nr, inv = 0;
        while (aux != 0) {
            inv = inv * 10 + aux % 10;
            aux = aux / 10;
        }
        return (nr == inv);
    }
}


public class Main {
    private static List<Task> taskList = new ArrayList<>();

    private static void loadTasks() {
        // TODO: uncomment the lines below

        if (taskList.isEmpty()) {
            taskList.add(new OutTask("First message task"));
            taskList.add(new RandomOutTask());
            taskList.add(new CounterOutTask());
            taskList.add(new OutTask("Second message task"));
            taskList.add(new CounterOutTask());
            taskList.add(new RandomOutTask());
        }
    }

    private static void test1() {
        for (Task task : taskList) {
            task.execute();
        }
    }

    private static void test2() {
        // TODO: uncomment the lines below

        System.out.println("----> Queue");
        Queue q = new Queue();
        for(Task task : taskList) {
            q.push(task);
        }
        q.pop();
        q.pop();
        for (Task task : q.getTasks()) {
            task.execute();
        }

        System.out.println("----> Stack");
        Stack s = new Stack();
        for(Task task : taskList) {
            s.push(task);
        }
        s.pop();
        s.pop();
        for (Task task : s.getTasks()) {
            task.execute();
        }

        System.out.println("----> Testing transferFrom");
        q.transferFrom(s);

        for (Task task : q.getTasks()) {
            task.execute();
        }

        // This should print true
        System.out.println(s.isEmpty());
    }

    static private void test3() {
        // TODO: uncomment the lines below

        Operation op = new Operation(13.5f);
        op.div(0.f);
        op.div(1.f);
        System.out.println(op.getNumber());  // 13.5
        op.mult(2.f);
        System.out.println(op.getNumber());  // 27
        op.minus(3.f);
        System.out.println(op.getNumber());  // 24
        op.plus(7.f);
        System.out.println(op.getNumber());  // 31
    }


    private static void test4() {
        // TODO: uncomment the lines below

        Song song1 = new Song("Bad", 101, "Michael Jackson");
        Song song2 = new Song("Dangerous", 19, "Michael Jackson");
        Song song3 = new Song("Heal the world", 53, "Composer");
        Song song4 = new Song("Thriller", 82, "Michael Jackson" );
        Song song5 = new Song("Beat it", 83, "Michel Jakson");
        Song song6 = new Song("Smooth Criminal", 77, "Composer");

        DangerousAlbum dangerous = new DangerousAlbum();
        dangerous.addSong(song2);
        dangerous.addSong(song3);
        dangerous.addSong(song6);
        System.out.println(dangerous);

        ThrillerAlbum thriller = new ThrillerAlbum();
        thriller.addSong(song4);
        thriller.addSong(song6);
        thriller.addSong(song5);
        System.out.println(thriller);

        BadAlbum bad = new BadAlbum();
        bad.addSong(song1);
        bad.addSong(song6);
        System.out.println(bad);
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int task = scanner.nextInt();

        loadTasks();

        if (task == 1) {
            test1();
        } else if (task == 2) {
            test2();
        } else if (task == 3) {
            test3();
        } else {
            test4();
        }
    }
}
