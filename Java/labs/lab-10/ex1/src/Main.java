import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Supplier;


/*


           ECHIPA OOP vă urează un Crăciun fericit și un AN NOU fără restanțe!
                                                        *
    void *                                                           void *
                                 void *                  *        .--.
      \/ \/  \/  \/                                        ./   /=*
        \/     \/       void *      *                ...  (_____)
         \ ^ ^/                                       \ \_((^o^))-.       void *
         (o)(O)--)--------\.                           \   (   ) \  \._.
         |    |  ||================((~~~~~~~~~~~~~~~~~))|   ( )   |     \
          \__/             ,|        \. * * * * * * ./  (~~~~~~~~~~~)    \
   *        ||^||\.____./|| |          \___________/     ~||~~~~|~'\____/ *
            || ||     || || A            ||    ||          ||    |
 void *     <> <>     <> <>          (___||____||_____)   ((~~~~~|   *

*/

// TODO: Implement the following class
class MultiMapValue<K, V> {

    private Map<K, List<V>> multiMap;
    public MultiMapValue() {
        this.multiMap = new HashMap<>();
    }
    public void add(K key, V value) {
        List<V> values = multiMap.get(key);

        if (values == null) {
            values = new ArrayList<>();
            multiMap.put(key, values);
        }

        values.add(value);
    }

    public void addAll(K key, List<V> values) {
        List<V> existingValues = multiMap.computeIfAbsent(key, k -> new ArrayList<>());
        existingValues.addAll(values);
    }

    public void addAll(MultiMapValue<K, V> map) {
        for (Map.Entry<K, List<V>> entry : map.multiMap.entrySet()) {
            addAll(entry.getKey(), entry.getValue());
        }
    }

    public V getFirst(K key) {
        List<V> values = multiMap.get(key);

        if (values == null || values.isEmpty()) {
            return null;
        }

        return values.get(0);
    }

    public List<V> getValues(K key) {
        List<V> values = multiMap.get(key);

        if (values == null) {
            return null;
        }

        return values;
    }

    public boolean containsKey(K key) {
        return multiMap.containsKey(key);
    }

    public boolean isEmpty() {
        return multiMap.isEmpty();
    }

    public List<V> remove(K key) {
        return multiMap.remove(key);
    }

    public int size() {
        return multiMap.size();
    }

}

enum UniversityCode {
    POLITEHNICA_BUCHAREST
}

class Student {

    private final String fistName;
    private final String lastName;
    private final Integer age;
    private final Float grade;
    private final UUID teacherId;

    public Student(String fistName, String lastName, Integer age, Float grade, UUID teacherId) {
        this.fistName = fistName;
        this.lastName = lastName;
        this.age = age;
        this.grade = grade;
        this.teacherId = teacherId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Student student = (Student) o;
        return Objects.equals(fistName, student.fistName) && Objects.equals(lastName, student.lastName) && Objects.equals(age, student.age) && Objects.equals(grade, student.grade) && Objects.equals(teacherId, student.teacherId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fistName, lastName, age, grade, teacherId);
    }

    @Override
    public String toString() {
        return "Student{" +
                "fistName='" + fistName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", age=" + age +
                ", grade=" + grade +
                '}';
    }
}

class Teacher {

    private static final UniversityCode UNIVERSITY_CODE = UniversityCode.POLITEHNICA_BUCHAREST;
    private final String fistName;
    private final String lastName;
    private final List<String> subjects;
    private final UUID teacherId;

    public Teacher(String fistName, String lastName, List<String> subjects, UUID teacherId) {
        this.fistName = fistName;
        this.lastName = lastName;
        this.subjects = subjects;
        this.teacherId = teacherId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Teacher teacher = (Teacher) o;
        return Objects.equals(fistName, teacher.fistName) && Objects.equals(lastName, teacher.lastName) && Objects.equals(subjects, teacher.subjects) && Objects.equals(teacherId, teacher.teacherId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fistName, lastName, subjects, teacherId);
    }

    @Override
    public String toString() {
        return "Teacher{" +
                "fistName='" + fistName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", subjects=" + subjects +
                '}';
    }
}

public class Main {

    private static final UUID coolTeacherId = UUID.randomUUID();
    private static final UUID notCoolTeacherId = UUID.randomUUID();
    private static Integer studentId = 0;
    private static Integer testNr = 0;

    private static final Supplier<Student> happyStudentSupplier = () ->
            createGenericStudent(studentId, "Happy", coolTeacherId, 10f);

    private static final Supplier<Student> sadStudentSupplier = () ->
            createGenericStudent(studentId, "Sad", notCoolTeacherId, 4.5f);

    public static void main(String[] args) {

        final Teacher coolTeacher = new Teacher(
                "Alex",
                "Amazing Teacher",
                Collections.singletonList("OOP"),
                coolTeacherId);

        final Teacher notSoCoolTeacher = new Teacher(
                "P",
                "GPL",
                Collections.singletonList("Good luck!"),
                notCoolTeacherId
        );

        final Teacher notCoolTeacher = new Teacher(
                "G.",
                "IUUU",
                Collections.singletonList("Good luck, you're going to need it!"),
                notCoolTeacherId
        );

        final List<Student> happyStudents = new ArrayList<>();
        final List<Student> sadStudents = new ArrayList<>();

        for (int i = 0; i < 10; ++i, ++studentId) {
            happyStudents.add(happyStudentSupplier.get());
            sadStudents.add(sadStudentSupplier.get());
        }

        /* TEST PART, DO NOT MODIFY IT*/

        final MultiMapValue<Teacher, Student> multiMapValue = new MultiMapValue<>();

        printTestNr(); // 1
        System.out.println(multiMapValue.isEmpty());
        System.out.println(multiMapValue.size());
        System.out.println();

        final Student firstHappyStudent = happyStudentSupplier.get();
        multiMapValue.add(coolTeacher, firstHappyStudent);
        printTestNr(); // 2
        System.out.println(multiMapValue.isEmpty());
        System.out.println(multiMapValue.size());
        System.out.println();

        final Student secondHappyStudent = happyStudentSupplier.get();
        multiMapValue.add(coolTeacher, secondHappyStudent);
        printTestNr(); // 3
        System.out.println(multiMapValue.size());
        System.out.println();

        final Student firstSadStudentNotCoolTeacher = sadStudentSupplier.get();
        multiMapValue.add(notCoolTeacher, firstSadStudentNotCoolTeacher);
        final Student firstStudentNotSoCool = sadStudentSupplier.get();
        multiMapValue.add(notSoCoolTeacher, firstStudentNotSoCool);
        printTestNr(); // 4
        System.out.println(multiMapValue.size());
        System.out.println();

        printTestNr(); // 5
        System.out.println(multiMapValue.getValues(coolTeacher).size());
        System.out.println(multiMapValue.getValues(notCoolTeacher).size());
        System.out.println(multiMapValue.getValues(notSoCoolTeacher).size());
        System.out.println();

        multiMapValue.add(coolTeacher, happyStudentSupplier.get());
        multiMapValue.add(notCoolTeacher, sadStudentSupplier.get());
        multiMapValue.add(notSoCoolTeacher, sadStudentSupplier.get());
        multiMapValue.add(coolTeacher, happyStudentSupplier.get());
        printTestNr(); // 6
        System.out.println(multiMapValue.getValues(coolTeacher).size());
        System.out.println(multiMapValue.getValues(notCoolTeacher).size());
        System.out.println(multiMapValue.getValues(notSoCoolTeacher).size());
        System.out.println();

        printTestNr(); // 7
        System.out.println(multiMapValue.containsKey(coolTeacher));
        System.out.println(multiMapValue.containsKey(notCoolTeacher));
        System.out.println(multiMapValue.containsKey(notSoCoolTeacher));
        System.out.println();

        printTestNr(); // 8
        System.out.println(multiMapValue.getFirst(coolTeacher).equals(firstHappyStudent));
        System.out.println(multiMapValue.getFirst(notCoolTeacher).equals(firstSadStudentNotCoolTeacher));
        System.out.println(multiMapValue.getFirst(notSoCoolTeacher).equals(firstStudentNotSoCool));
        System.out.println();

        printTestNr(); // 9
        final List<Student> removedStudents = multiMapValue.remove(notSoCoolTeacher);
        removedStudents.forEach(student -> System.out.println(sadStudents.contains(student)));
        System.out.println(multiMapValue.size());
        System.out.println(multiMapValue.getFirst(notSoCoolTeacher));
        System.out.println();

        printTestNr(); // 10
        System.out.println(multiMapValue.getValues(coolTeacher).size());
        multiMapValue.addAll(coolTeacher, happyStudents);
        System.out.println(multiMapValue.getValues(coolTeacher).size());
        System.out.println(multiMapValue.getValues(notCoolTeacher).size());
        System.out.println();

        printTestNr(); // 11
        final MultiMapValue<Teacher, Student> addAllMultiMapValue1 = new MultiMapValue<>();
        addAllMultiMapValue1.addAll(notSoCoolTeacher, sadStudents);
        multiMapValue.addAll(addAllMultiMapValue1);
        System.out.println(multiMapValue.size());
        System.out.println(multiMapValue.getValues(coolTeacher).size());
        System.out.println(multiMapValue.getValues(notSoCoolTeacher).size());
        System.out.println(multiMapValue.getValues(notCoolTeacher).size());
        System.out.println();

        printTestNr(); // 12
        final List<Student> notSoCoolTeacherStudentsList = multiMapValue.getValues(notSoCoolTeacher);
        System.out.println(notSoCoolTeacherStudentsList.size());
        System.out.println(notSoCoolTeacherStudentsList.size() == sadStudents.size());
        System.out.println(notSoCoolTeacherStudentsList.containsAll(sadStudents));
        System.out.println();

    }

    private static void printTestNr() {
        System.out.println("TEST " + ++testNr + " result:");
    }

    private static Student createGenericStudent(final Integer studentNr, final String firstName,
                                                final UUID teacherId, final Float grade) {
        return new Student(
                firstName + " " + studentNr,
                "Human Being",
                18,
                grade,
                teacherId);
    }
}
