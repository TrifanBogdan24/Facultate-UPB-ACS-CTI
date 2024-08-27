import java.util.*;
import java.util.stream.Collectors;

class Business {
    private final Set<Employee> employees = new HashSet<>();
    private final List<Project> projects = new ArrayList<>();

    public void addEmployee(final Employee employee) throws ExistsException {
        if (employees.contains(employee)) {
            throw new ExistsException("Employee already exists into the business.");
        }

        employees.add(employee);
    }

    public void addProject(final Project project) throws ExistsException {
        if (projects.contains(project)) {
            throw new ExistsException("Project already exists into the business.");
        }

        projects.add(project);
    }

    public Set<Employee> getEmployees() {
        // TODO: get an immutable set of data
        return Collections.unmodifiableSet(this.employees);
    }

    public List<Project> getProjects() {
        // TODO: get an immutable list of data
        return Collections.unmodifiableList(this.projects);
    }
}

class Bank {
    private final Set<Business> clients = new HashSet<>();

    public void addClient(final Business client) throws ExistsException {
        if (clients.contains(client)) {
            throw new ExistsException("Client already exists into the bank");
        }

        clients.add(client);
    }

    public Set<Business> getClients() {
        // TODO: get an immutable set of data
        return Collections.unmodifiableSet(this.clients);
    }
}

class Employee {
    private final String name;
    private final Integer age;
    private final String city;
    private Gender gender;
    private Disability disability;
    private Religion religion;
    private final Set<Account> accounts = new HashSet<>();
    private final List<Project> projects = new ArrayList<>();

    public Employee(String name, Integer age, String city) {
        this.name = name;
        this.age = age;
        this.city = city;
    }

    public Employee(String name, Integer age, String city, Gender gender, Disability disability, Religion religion) {
        this(name, age, city);
        this.gender = gender;
        this.disability = disability;
        this.religion = religion;
    }

    public void addAccount(Account account) {
        accounts.add(account);
    }

    public void addProject(Project project) {
        projects.add(project);
    }

    public String getName() {
        return name;
    }

    public String getCity() {
        return city;
    }

    public Integer getAge() {
        return age;
    }

    public Gender getGender() {
        return gender;
    }

    public Disability getDisability() {
        return disability;
    }

    public Religion getReligion() {
        return religion;
    }

    public Set<Account> getAccounts() {
        // TODO: get an immutable set of data
        return Collections.unmodifiableSet(this.accounts);
    }

    public List<Project> getProjects() {
        // TODO: get an immutable list of data
        return Collections.unmodifiableList(this.projects);
    }

    @Override
    public String toString() {
        StringBuilder res = new StringBuilder();
        res.append("Name ")
                .append(getName())
                .append(", city ")
                .append(getCity())
                .append(", age ")
                .append(getAge())
                .append(", gender ")
                .append(getGender())
                .append(", religion ")
                .append(getReligion())
                .append("\n");

        return res.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Employee employee = (Employee) o;
        return getName().equals(employee.getName()) &&
                getGender() == employee.getGender() &&
                getCity().equals(employee.getCity()) &&
                getAge() == employee.getAge();
    }

    @Override
    public int hashCode() {
        return Objects.hash(getName(), getGender(), getCity(), getAge());
    }
}

class BankReport {
    /* Customer = Employee of the Business
       Business = a client of the Bank
       Customers of the Bank = all the Employees that work for the Businesses that are clients of the Bank */

    public static int getNumberOfCustomers(Bank bank) {
        // All the customers that have accounts at the bank
        // A customer is an Employee that works for a Business (client of the bank)

        int nr = 0;

        for (Business client: bank.getClients()) {
            nr = nr + client.getEmployees().size();
        }
        return nr;
    }

    public static int getNumberOfAccounts(Bank bank) {
        // Accounts of all the customers of the bank

        int nr = 0;

        for (Business client: bank.getClients()) {
            for (Employee customer: client.getEmployees()) {
                nr = nr + customer.getAccounts().size();
            }
        }

        return nr;
    }

    public static SortedSet<Employee> getCustomersSorted(Bank bank) {
        // Return the set of customers in alphabetical order
        SortedSet<Employee> sortedCustomers = new TreeSet<>(Comparator.comparing(Employee::getName));

        for (Business client : bank.getClients()) {
            sortedCustomers.addAll(client.getEmployees());
        }

        return sortedCustomers;

    }

    public static double getTotalSumInAccounts(Bank bank) {
        // Sum of all customers' accounts balances

        double sum = 0.0d;

        for (Business client: bank.getClients()) {
            for (Employee customer: client.getEmployees()) {
                for (Account account: customer.getAccounts()) {
                    sum = sum + account.getBalance();
                }
            }
        }

        return sum;
    }

    public static SortedSet<Account> getAccountsSortedBySum(Bank bank) {
        // The set of all accounts, ordered by current account balance
        // and if the balance is equal, sort by id

        SortedSet<Account> sortedAccounts = new TreeSet<>(
                Comparator.comparing(Account::getBalance)
                        .thenComparing(Account::getId)
        );

        for (Business client : bank.getClients()) {
            for (Employee customer : client.getEmployees()) {
                sortedAccounts.addAll(customer.getAccounts());
            }
        }

        return sortedAccounts;
    }

    public static Map<Employee, Collection<Account>> getCustomerAccounts(Bank bank) {
        // Return a map of all the customers with their respective accounts

        Map<Employee, Collection<Account>> allAccounts = new HashMap<>();

        for (Business client: bank.getClients()) {
            for (Employee customer: client.getEmployees()) {
                Set<Account> accounts = new HashSet<>(customer.getAccounts());
                allAccounts.put(customer, accounts);    // adding in the Map
            }
        }

        return allAccounts;
    }

    public static Map<String, List<Employee>> getCustomersByCity(Bank bank) {
        // Map all the customers to their respective cities

        Map<String, List<Employee>> citiesAndCustomers = new HashMap<>();

        for (Business client: bank.getClients()) {
            for (Employee customer: client.getEmployees()) {

                String city = customer.getCity();

                if (citiesAndCustomers.containsKey(city)) {
                    List<Employee> customers = citiesAndCustomers.get(city);
                    customers.add(customer);

                    citiesAndCustomers.put(city, customers);
                } else {
                    List<Employee> customers = new ArrayList<>();
                    customers.add(customer);
                    citiesAndCustomers.put(city, customers);
                }
            }
        }

        return citiesAndCustomers;
    }
}

class BusinessReport {

    public static Map<Disability, List<Employee>> getEmployeesOnSameDisability(Business business) {
        // Get employees and map them on the type of disability they possess

        Map<Disability, List<Employee>> disabilitiesAndEmployees = new HashMap<>();

        for (Employee customer: business.getEmployees()) {

            Disability disability = customer.getDisability();

            if (disabilitiesAndEmployees.containsKey(disability)) {
                List<Employee> customers = disabilitiesAndEmployees.get(disability);
                customers.add(customer);

                disabilitiesAndEmployees.put(disability, customers);
            } else {
                List<Employee> customers = new ArrayList<>();
                customers.add(customer);
                disabilitiesAndEmployees.put(disability, customers);
            }
        }

        return disabilitiesAndEmployees;
    }

    public static long getNumberOfDifferentProjectsWorkedByCurrentFemaleEmployees(Business business) {
        // Get employees, filter by gender, get their projects without duplicates, count

        return business.getEmployees()
                .stream()
                .filter(employee -> employee.getGender().equals(Gender.FEMALE))
                .flatMap(employee -> employee.getProjects().stream())
                .distinct()
                .count();
    }

    public static Map<Religion, Map<Gender, List<Employee>>> getEmployeesOnSameReligionAndGender(Business business) {
        /* Map the employees by religion (Map<Religion, List<Employee>>) then map each of the lists by gender
           Map -> key (Religion)
               -> value (Map -> key -> (Gender)
                               -> value -> List<Employee>)
           Hint: use Collectors.groupingBy(Function, Collector) */

        return business.getEmployees()
                .stream()
                .collect(Collectors.groupingBy(Employee::getReligion,
                        Collectors.groupingBy(Employee::getGender)));
    }
}

class Pair<K, V> {
    protected K key;
    protected V value;

    protected Pair(K k, V v) {
        key = k;
        value = v;
    }

    @Override
    public String toString() {
        return "Pair [key=" + key + ", value=" + value + "]";
    }
}

enum Gender {
    MALE, FEMALE;
}
enum Disability {
    DISABLED, HEALTHY, ANONYMOUS
}

enum Religion {
    BUDDHISM, CHRISTIANITY, CALVINISM, DAOISM, HINDUISM, ISLAM
}

class NotEnoughFundsException extends Exception {
    private final int id;
    private final double balance;
    private final double amount;

    public NotEnoughFundsException(int id, double balance, double amount, String message) {
        super(message);
        this.id = id;
        this.balance = balance;
        this.amount = Math.round(amount * 100) / 100d;
    }

    public int getId() {
        return id;
    }

    public double getBalance() {
        return balance;
    }

    public double getAmount() {
        return amount;
    }
}

class ExistsException extends Exception {
    public ExistsException(String message) {
        super(message);
    }
}

class Project {
    private final String name;
    private final int workers;

    public Project(String name, int workers) {
        this.name = name;
        this.workers = workers;
    }

    public String getName() {
        return name;
    }

    public int getWorkers() {
        return workers;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + workers;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Project other = (Project) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (workers != other.workers)
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "Project [name=" + name + ", workers=" + workers + "]";
    }
}

class Account {
    private final int id;
    private double balance;
    private final double maximumAmountToWithdraw;

    public Account(int id, double balance, double maximumAmountToWithdraw) {
        this.id = id;
        this.balance = balance;
        this.maximumAmountToWithdraw = maximumAmountToWithdraw;
    }

    public void deposit(final double amount) {
        if (amount < 0) {
            throw new IllegalArgumentException("Cannot deposit a negative amount");
        }
        this.balance += amount;
    }

    public void withdraw(final double amount) throws NotEnoughFundsException {
        if (amount < 0) {
            throw new IllegalArgumentException("Cannot withdraw a negative amount");
        }

        if (amount > getMaximumAmountToWithdraw()) {
            throw new NotEnoughFundsException(id, balance, amount, "Requested amount exceeds the maximum amount to withdraw");
        }

        this.balance -= amount;
    }

    public int getId() {
        return id;
    }

    public double getBalance() {
        return balance;
    }

    public double getMaximumAmountToWithdraw() {
        return maximumAmountToWithdraw;
    }

    public int decimalValue() {
        return (int) Math.round(balance);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Account that = (Account) o;
        return getId() == that.getId() &&
                Double.compare(that.getBalance(), getBalance()) == 0 &&
                Double.compare(that.getMaximumAmountToWithdraw(), getMaximumAmountToWithdraw()) == 0;
    }

    @Override
    public String toString() {
        return "Account{" +
                "id=" + id +
                ", balance=" + balance +
                ", maximumAmountToWithdraw=" + maximumAmountToWithdraw +
                "}\n";
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId(), getBalance(), getMaximumAmountToWithdraw());
    }
}

public class Main {
    /* TEST PART, DO NOT MODIFY IT */
    static Bank bank;
    static Business business1;
    static Business business2;

    private static void setup() throws ExistsException {
        bank = new Bank();
        business1 = new Business();
        business2 = new Business();

        modifyBank(bank, business1, business2);
    }

    private static void clean() {
        bank = null;
        business1 = null;
        business2 = null;
    }

    private static void modifyBank(Bank bank, Business business1, Business business2) throws ExistsException {
        // Businesses
        bank.addClient(business1);
        bank.addClient(business2);

        // Employees
        Employee e1 = new Employee("Rajesh", 29, "Jaipur", Gender.MALE, Disability.DISABLED, Religion.BUDDHISM);
        Employee e2 = new Employee("Abdul", 22, "Mumbai", Gender.MALE, Disability.HEALTHY, Religion.BUDDHISM);
        Employee e3 = new Employee("Arjun", 20, "Ahmedabad", Gender.FEMALE, Disability.HEALTHY, Religion.BUDDHISM);
        Employee e4 = new Employee("Brijesh", 39, "Mumbai", Gender.MALE, Disability.DISABLED, Religion.BUDDHISM);
        Employee e5 = new Employee("Darsh", 49, "Delhi", Gender.MALE, Disability.DISABLED, Religion.CHRISTIANITY);
        Employee e6 = new Employee("Ekaraj", 32, "Mumbai", Gender.FEMALE, Disability.HEALTHY, Religion.CHRISTIANITY);
        Employee e7 = new Employee("Fariq", 25, "Delhi", Gender.MALE, Disability.DISABLED, Religion.CHRISTIANITY);
        Employee e8 = new Employee("Girik", 50, "Delhi", Gender.FEMALE, Disability.HEALTHY, Religion.CHRISTIANITY);
        Employee e9 = new Employee("Gunbir", 56, "Jaipur", Gender.FEMALE, Disability.DISABLED, Religion.CHRISTIANITY);
        Employee e10 = new Employee("Hitesh", 23, "Mumbai", Gender.MALE, Disability.HEALTHY, Religion.CALVINISM);
        Employee e11 = new Employee("Jagdish", 46, "Bangalore", Gender.MALE, Disability.HEALTHY, Religion.CALVINISM);
        Employee e12 = new Employee("Jairaj", 58, "Mumbai", Gender.FEMALE, Disability.DISABLED, Religion.DAOISM);
        Employee e13 = new Employee("Kabir", 42, "Delhi", Gender.MALE, Disability.HEALTHY, Religion.DAOISM);
        Employee e14 = new Employee("Krishna", 34, "Ahmedabad", Gender.FEMALE, Disability.DISABLED, Religion.DAOISM);
        Employee e15 = new Employee("Mitesh", 18, "Mumbai", Gender.MALE, Disability.ANONYMOUS, Religion.HINDUISM);
        Employee e16 = new Employee("Naveen", 27, "Bangalore", Gender.FEMALE, Disability.HEALTHY, Religion.HINDUISM);
        Employee e17 = new Employee("Onkar", 37, "Ahmedabad", Gender.FEMALE, Disability.ANONYMOUS, Religion.ISLAM);
        Employee e18 = new Employee("Praneel", 49, "Bangalore", Gender.MALE, Disability.ANONYMOUS, Religion.ISLAM);
        Employee e19 = new Employee("Ranveer", 52, "Mumbai", Gender.MALE, Disability.DISABLED, Religion.ISLAM);

        // Projects
        Project p1 = new Project("Bender Project", 6);
        Project p2 = new Project("Hornets", 10);
        Project p3 = new Project("Westerners", 12);
        Project p4 = new Project("Whistler", 24);
        Project p5 = new Project("Coding Region", 3);
        Project p6 = new Project("The Blue Bird", 7);
        Project p7 = new Project("Fast Ball", 14);
        Project p8 = new Project("Mercury", 60);
        Project p9 = new Project("Limitless Horizons", 8);
        Project p10 = new Project("Strive Training", 8);
        Project p11 = new Project("Yaeger", 8);
        Project p12 = new Project("Yoda", 20);
        Project p13 = new Project("Meetup for the God", 26);
        Project p14 = new Project("Next Gala", 36);
        Project p15 = new Project("Sirius", 6);
        Project p16 = new Project("Sputnik", 6);
        Project p17 = new Project("Stratos", 8);
        Project p18 = new Project("Revolution", 10);
        Project p19 = new Project("Red Butter", 3);
        Project p20 = new Project("Rhinestone", 7);
        Project p21 = new Project("Linkage", 14);
        Project p22 = new Project("Excalibur", 6);
        Project p23 = new Project("Wombat", 6);
        Project p24 = new Project("Indie", 12);
        Project p25 = new Project("The Art of Codes", 26);
        Project p26 = new Project("Annual Award Show", 36);
        Project p27 = new Project("Robust Routine", 6);
        Project p28 = new Project("Wide Stringer", 6);
        Project p29 = new Project("Static Startup", 6);

        // Accounts
        Account a1 = new Account(1, 200, 10);
        Account a2 = new Account(2, 900, 10);
        Account a3 = new Account(3, 900, 10);
        Account a4 = new Account(4, 1600, 10);
        Account a5 = new Account(5, 1600, 10);
        Account a6 = new Account(6, 1600, 10);
        Account a7 = new Account(7, 2000, 10);
        Account a8 = new Account(8, 2000, 10);
        Account a9 = new Account(9, 2000, 10);
        Account a10 = new Account(10, 3000, 20);
        Account a11 = new Account(11, 4000, 20);
        Account a12 = new Account(12, 4000, 20);
        Account a13 = new Account(13, 4000, 20);
        Account a14 = new Account(14, 5000, 20);
        Account a15 = new Account(15, 5000, 20);
        Account a16 = new Account(16, 5000, 20);
        Account a17 = new Account(17, 7000, 30);
        Account a18 = new Account(18, 7000, 30);
        Account a19 = new Account(19, 9000, 30);
        Account a20 = new Account(20, 9000, 30);
        Account a21 = new Account(21, 9000, 30);
        Account a22 = new Account(22, 9400, 30);
        Account a23 = new Account(23, 9400, 30);
        Account a24 = new Account(24, 9400, 50);
        Account a25 = new Account(25, 9400, 50);
        Account a26 = new Account(26, 9400, 50);
        Account a27 = new Account(27, 10000, 50);
        Account a28 = new Account(28, 20000, 100);
        Account a29 = new Account(29, 20500, 100);
        Account a30 = new Account(30, 30000, 300);

        // Pair employees - accounts
        e1.addAccount(a1);
        e2.addAccount(a2);
        e3.addAccount(a3);
        e4.addAccount(a4);
        e5.addAccount(a5);
        e6.addAccount(a6);
        e7.addAccount(a7);
        e8.addAccount(a8);
        e9.addAccount(a9);
        e10.addAccount(a10);
        e11.addAccount(a11);
        e12.addAccount(a12);
        e13.addAccount(a13);
        e14.addAccount(a14);
        e15.addAccount(a15);
        e16.addAccount(a16);
        e17.addAccount(a17);
        e18.addAccount(a18);
        e19.addAccount(a19);
        e10.addAccount(a20);
        e1.addAccount(a21);
        e12.addAccount(a22);
        e3.addAccount(a23);
        e4.addAccount(a24);
        e15.addAccount(a25);
        e6.addAccount(a26);
        e7.addAccount(a27);
        e8.addAccount(a28);
        e19.addAccount(a29);
        e10.addAccount(a30);

        // Pair employees - projects
        e1.addProject(p1);
        e2.addProject(p2);
        e3.addProject(p3);
        e4.addProject(p4);
        e5.addProject(p5);
        e6.addProject(p6);
        e7.addProject(p7);
        e8.addProject(p8);
        e9.addProject(p9);
        e10.addProject(p10);
        e11.addProject(p11);
        e12.addProject(p12);
        e13.addProject(p13);
        e14.addProject(p14);
        e15.addProject(p15);
        e16.addProject(p16);
        e17.addProject(p17);
        e18.addProject(p18);
        e19.addProject(p19);
        e10.addProject(p20);
        e1.addProject(p21);
        e12.addProject(p22);
        e3.addProject(p23);
        e4.addProject(p24);
        e15.addProject(p25);
        e6.addProject(p26);
        e7.addProject(p27);
        e8.addProject(p28);
        e19.addProject(p29);
        e10.addProject(p29);
        e10.addProject(p10);
        e11.addProject(p11);
        e12.addProject(p2);
        e13.addProject(p3);
        e14.addProject(p4);
        e15.addProject(p5);
        e16.addProject(p6);
        e17.addProject(p7);
        e18.addProject(p8);
        e19.addProject(p9);
        e10.addProject(p20);
        e1.addProject(p21);
        e12.addProject(p22);
        e3.addProject(p13);
        e4.addProject(p14);
        e15.addProject(p15);
        e6.addProject(p16);
        e7.addProject(p17);
        e8.addProject(p18);
        e19.addProject(p19);
        e10.addProject(p20);
        e1.addProject(p11);
        e2.addProject(p21);
        e3.addProject(p13);
        e4.addProject(p14);
        e5.addProject(p15);
        e6.addProject(p16);
        e7.addProject(p17);
        e8.addProject(p18);
        e9.addProject(p19);
        e10.addProject(p12);
        e11.addProject(p12);
        e12.addProject(p17);
        e13.addProject(p12);
        e14.addProject(p12);
        e15.addProject(p12);
        e16.addProject(p12);
        e17.addProject(p12);
        e18.addProject(p12);
        e19.addProject(p12);
        e10.addProject(p2);
        e1.addProject(p14);
        e12.addProject(p26);

        // Pair Businesses - projects
        business1.addProject(p1);
        business1.addProject(p2);
        business1.addProject(p3);
        business1.addProject(p4);
        business1.addProject(p5);
        business1.addProject(p6);
        business1.addProject(p7);
        business1.addProject(p8);
        business1.addProject(p9);
        business1.addProject(p10);
        business1.addProject(p11);
        business1.addProject(p12);
        business1.addProject(p13);
        business1.addProject(p14);
        business1.addProject(p15);
        business1.addProject(p16);
        business1.addProject(p17);
        business1.addProject(p18);
        business2.addProject(p19);
        business2.addProject(p20);
        business2.addProject(p21);
        business2.addProject(p22);
        business2.addProject(p23);
        business2.addProject(p24);
        business2.addProject(p25);
        business2.addProject(p26);
        business2.addProject(p27);
        business2.addProject(p28);
        business2.addProject(p29);

        // Pair Businesses - employees
        business1.addEmployee(e1);
        business1.addEmployee(e2);
        business1.addEmployee(e3);
        business1.addEmployee(e4);
        business1.addEmployee(e5);
        business1.addEmployee(e6);
        business1.addEmployee(e7);
        business1.addEmployee(e8);
        business1.addEmployee(e9);
        business1.addEmployee(e10);
        business1.addEmployee(e11);
        business1.addEmployee(e12);
        business2.addEmployee(e13);
        business2.addEmployee(e14);
        business2.addEmployee(e15);
        business2.addEmployee(e16);
        business2.addEmployee(e17);
        business2.addEmployee(e18);
        business2.addEmployee(e19);
    }

    static void displayTest(int numberTest) throws ExistsException {
        /* TEST PART, DO NOT MODIFY IT */
        setup();

        System.out.println("Test" + numberTest + " result:");

        switch (numberTest) {
            case 1: {
                System.out.println(BankReport.getNumberOfCustomers(bank));
                break;
            }

            case 2: {
                System.out.println(BankReport.getNumberOfAccounts(bank));
                break;
            }

            case 3: {
                System.out.println(BankReport.getCustomersSorted(bank));
                break;
            }

            case 4: {
                System.out.println(BankReport.getTotalSumInAccounts(bank));
                break;
            }

            case 5: {
                System.out.print(BankReport.getAccountsSortedBySum(bank).first());
                System.out.print(BankReport.getAccountsSortedBySum(bank).last());
                System.out.println(BankReport.getAccountsSortedBySum(bank).size());
                break;
            }

            case 6: {
                System.out.println(BankReport.getCustomerAccounts(bank).size());
                break;
            }

            case 7: {
                System.out.println(BankReport.getCustomersByCity(bank).size());
                break;
            }

            case 8: {
                var employees = BusinessReport.getEmployeesOnSameDisability(business2);
                System.out.println(employees.size());
                System.out.println(employees.get(Disability.ANONYMOUS).size());
                System.out.println(employees.get(Disability.HEALTHY).size());
                System.out.println(employees.get(Disability.DISABLED).size());
                break;
            }

            case 9: {
                System.out.println(BusinessReport
                        .getNumberOfDifferentProjectsWorkedByCurrentFemaleEmployees(business1));
                break;
            }

            case 10: {
                var employees = BusinessReport.getEmployeesOnSameReligionAndGender(business1);
                System.out.println(employees.size());
                System.out.println(employees.get(Religion.BUDDHISM).size());
                System.out.println(employees.get(Religion.CHRISTIANITY).size());
                System.out.println(employees.get(Religion.DAOISM).size());
                System.out.println(employees.get(Religion.CALVINISM).size());
                System.out.println(employees.get(Religion.ISLAM));
                System.out.println(employees.get(Religion.HINDUISM));
                break;
            }
        }
        clean();
    }

    public static void main(String[] args) throws ExistsException {
        /* TEST PART, DO NOT MODIFY IT */
        Scanner scanner = new Scanner(System.in);
        int taskNumber = scanner.nextInt();
        displayTest(taskNumber);
    }
}
