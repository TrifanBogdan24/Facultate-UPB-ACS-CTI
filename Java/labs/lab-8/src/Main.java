import java.util.*;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int taskId = scanner.nextInt();
        scanner.close();

        /* Do not modify */
        Student s1 = new Student("Maria", "Popescu", 3, 8.5);
        Student s2 = new Student("Ion", "Grigorescu", 2, 8);
        Student s3 = new Student("Ana", "Enescu", 7, 7);
        Student s4 = new Student("Mihai", "Eminovici", 1, 4.45);
        Student s5 = new Student("Andrei", "Radu", 12, 2);

        List<Student> students = new ArrayList<>(List.of(s1, s2, s3, s4, s5));
        List<Student> copyStudents = new ArrayList<>(students);
        List<Student> anotherCopyStudents = new ArrayList<>(students);

        List<Integer> numbers = List.of(10, 20, 5, 243, 5556, 312, 566, 245, 122, 5556, 5, 10, 20, 122);
        ArrayList<String> subjects = new ArrayList<>(List.of("PP", "PA", "PCOM", "IOCLA", "AA",
                "SO", "CPL", "EP", "RL", "LFA"));
        Random random = new Random(12);
        /* End of unmodifiable zone */

        switch (taskId) {

        /* ------------------------- Task 1 ------------------------- */
        /* --------- Sort using Comparable<Student> interface ------- */
            case 1:
                Collections.sort(students, Student::compareTo);
                System.out.println(students);
                break;

        /* ------------------------- Task 2 ------------------------- */
        /* -------------- Sort using a lambda expression ------------ */
            case 2:

                Collections.sort(copyStudents, (o1, o2) -> {
                    double avg1 = o1.getAverageGrade();
                    double avg2 = o2.getAverageGrade();

                    if (avg1 != avg2) {
                        return Double.compare(avg1, avg2);
                    }

                    /*
                     * surname = nume de familie
                     * name    = prenume
                     */

                    String surname1 = o1.getSurname();
                    String surnmae2 = o2.getSurname();

                    if (!surname1.equals(surnmae2)) {
                        return Student.strcmp(surname1, surnmae2);
                    }

                    String name1 = o1.getName();
                    String name2 = o2.getName();
                    return Student.strcmp(surname1, surnmae2);
                });
                System.out.println(copyStudents);

                break;
            /* ------------------------- Task 3 ------------------------- */
            /* ----------- Implement your priority queue here ----------- */
            /* --------------- Use Comparator.comparing() --------------- */
            case 3:
                PriorityQueue<Student> priorityQueue = new PriorityQueue<>(Comparator.comparingLong(Student::getId));
                priorityQueue.addAll(students);


                System.out.println(priorityQueue);
                break;
            /* ------------------------- Task 4 ------------------------- */
            case 4:
                Map<Student, LinkedList<String>> studentMap = new HashMap<>();
                students.forEach(s -> studentMap.putIfAbsent(s, new LinkedList<>()));
                /*----Add 4 random elements from subjects array in each LinkedList ----*/
                /*
                 * As index use the previously declared random object and use subjects.size() as
                 * your
                 * bound. Use addFirst() method to add elements in the LinkedList
                 */

                studentMap.forEach((student, subjectsList) -> {
                    for (int i = 0; i < 4; i++) {
                        int randomIndex = random.nextInt(subjects.size());
                        subjectsList.addFirst(subjects.get(randomIndex));
                    }
                });


                System.out.println(studentMap);
                break;
            /* ------------------------- Task 5 ------------------------- */
            /* ------------- No need to add or modify here -------------- */
            case 5:
                System.out.println(numbers);
                LinkedEvenSet linked = new LinkedEvenSet();
                linked.addAll(numbers);

                EvenSet set = new EvenSet();
                set.addAll(numbers);

                TreeEvenSet tree = new TreeEvenSet();
                tree.addAll(numbers);

                System.out.println(linked);
                System.out.println(set);
                System.out.println(tree);
            default:
                break;
        }

    }
}

class Student implements Comparable<Student> {
    /* ------------------------- Task 1 ------------------------- */
    /* Add student properties */
    String name;
    String surname;
    long id;
    double averageGrade;

    static int strcmp(String str1, String str2) {
        int len1 = str1.length();
        int len2 = str2.length();

        for (int i = 0; i < len1 && i < len2; i++) {
            int ascii1 = (int)str1.charAt(i);
            int ascii2 = (int)str2.charAt(i);

            if (ascii1 != ascii2)
                return (ascii1 - ascii2);
        }

        if (len1 == len2)
            return 0;
        return (len1 - len2);
    }

    @Override
    public int compareTo(Student s1) {

        double avg1 = this.averageGrade;
        double avg2 = s1.averageGrade;

        if (avg1 != avg2) {
            return Double.compare(avg1, avg2);
        }

        /*
        * surname = nume de familie
        * name    = prenume
         */

        if (this.surname.equals(s1.getSurname()) != true) {
            return strcmp(this.name, s1.getName());
        }

        return strcmp(this.name, s1.getName());
    }


    /* Generate getters and setters */

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public long getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public double getAverageGrade() {
        return averageGrade;
    }

    public void setAverageGrade(Integer averageGrade) {
        this.averageGrade = averageGrade;
    }


    // constructor
    public Student(String name, String surname, Integer id, double averageGrade) {
        this.name = name;
        this.surname = surname;
        this.id = id;
        this.averageGrade = averageGrade;
    }

    @Override
    public String toString() {
        return "Student {name=" + name + ", surname=" + surname + ", id=" + id + ", averageGrade=" + averageGrade + "}";
    }




    /* ------------------------- Task 4 ------------------------- */
    /* Override `equals` and `hashCode` methods */

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, surname, id, averageGrade);
    }
}

class EvenSet extends HashSet<Integer> {
    /* Task 5 - Make it that it only accepts even Integers */
    /* even number = numa par                              */
    @Override
    public boolean add(Integer element) {
        if (element % 2 != 0)
            return false;
        return super.add(element);
    }
}

class LinkedEvenSet extends LinkedHashSet<Integer> {
    /* Task 5 - Make it that it only accepts even Integers */
    /* even number = numa par                              */
    @Override
    public boolean add(Integer element) {
        if (element % 2 != 0)
            return false;
        return super.add(element);
    }
}

class TreeEvenSet extends TreeSet<Integer> {
    /* Task 5 - Make it that it only accepts even Integers */
    /* even number = numa par                              */
    @Override
    public boolean add(Integer element) {
        if (element % 2 != 0)
            return false;
        return super.add(element);
    }
}
