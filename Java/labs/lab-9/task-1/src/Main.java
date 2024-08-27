import java.util.ArrayList;
import java.util.List;
import java.util.Random;

interface TransactionPublisher {
    void createTransaction(Person person);
}

interface BalanceObserver {
    void update();
}

interface PaymentStrategy {
    void pay(Integer amount);
}

class Shop implements TransactionPublisher {
    private final Random priceGenerator = new Random(56);
    private final RandomPaymentFactory paymentFactory = new RandomPaymentFactory();
    private final ArrayList<Person> customers = new ArrayList<>();

    public void addCustomers(List<Person> customers) {
        this.customers.addAll(customers);
    }

    public ArrayList<Person> getCustomers() {
        return customers;
    }

    @Override
    public void createTransaction(Person person) {
        // TODO - use price generator bounded to 100!

        PaymentStrategy strategy = paymentFactory.getPaymentMethod(person);

        if (person.payBy(strategy, priceGenerator.nextInt(100))) {
            customers.add(person);
        }
    }
}
class Person implements BalanceObserver {
    private String IBAN;
    private String cardNumber;
    private String name;
    private String surname;
    private Integer balance;

    private Integer limit;

    // TODO - All args constructor, getters, setters
    public Person() {

    }

    public Person(final String IBAN, final String cardNumber, final String name,
                  final String surname, final Integer balance, final Integer limit) {
        this.IBAN = IBAN;
        this.cardNumber = cardNumber;
        this.name = name;
        this.surname = surname;
        this.balance = balance;
        this.limit = limit;
    }

    public String getIBAN() {
        return IBAN;
    }

    public void setIBAN(String IBAN) {
        this.IBAN = IBAN;
    }

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

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

    public Integer getBalance() {
        return balance;
    }

    public void setBalance(Integer balance) {
        this.balance = balance;
    }

    public Integer getLimit() {
        return limit;
    }

    public void setLimit(Integer limit) {
        this.limit = limit;
    }

    @Override
    public void update() {
        // TODO
        if (balance < limit) {
            System.out.println(name + " " + surname + ", ramai fara bani, saracule!");
        }
    }

    public boolean payBy(PaymentStrategy strategy, Integer amount) {
        // TODO
        if (strategy != null) {
            // the payment can be successfully done
            strategy.pay(amount);
            return true;
        }

        // the payment cannot be done
        return false;
    }
}


class CardPayment implements PaymentStrategy {
    private final Person person;

    public CardPayment(Person person) {
        this.person = person;
    }

    @Override
    public void pay(Integer amount) {
        // TODO
        if (person.getBalance() >= amount) {
            person.setBalance(person.getBalance() - amount);
            System.out.println(person.getName() + " " + person.getSurname()
                    + " a platit cu cardul " + person.getCardNumber()
                    + " suma de " + amount + " RON");
            person.update();
        } else {
            System.out.println(person.getName() + " " + person.getSurname()
                    + ", pleaca de aici, saracule!");
        }
    }
}
class VoucherPayment implements PaymentStrategy {
    private final Person person;
    public VoucherPayment(Person person){
        this.person = person;
    }
    @Override
    public void pay(Integer amount){
        // TODO
        if (person.getBalance() >= amount) {
            person.setBalance(person.getBalance() - amount);
            System.out.println(person.getName() + " " + person.getSurname()
                    + " a platit folosind un voucher, suma de " + amount + " RON");
            person.update();
        } else {
            System.out.println(person.getName() + " " + person.getSurname()
                    + ", pleaca de aici, saracule!");
        }
    }
}

class BankAccountPayment implements PaymentStrategy {
    private final Person person;

    public BankAccountPayment(Person person) {
        this.person = person;
    }

    @Override
    public void pay(Integer amount) {
        // TODO
        if (person.getBalance() >= amount) {
            person.setBalance(person.getBalance() - amount);
            System.out.println(person.getName() + " " + person.getSurname()
                    + " a platit prin transfer bancar din contul " + person.getIBAN()
                    + ", suma de " + amount + " RON");
            person.update();
        } else {
            System.out.println(person.getName() + " " + person.getSurname()
                    + ", pleaca de aici, saracule!");
        }
    }
}

class RandomPaymentFactory {
    private enum PaymentType { CARD, IBAN, VOUCHER }
    private static final Random random = new Random(1234565);

    public PaymentStrategy getPaymentMethod(Person person) {

        switch (PaymentType.values()[random.nextInt(3)]) {
            case CARD: return new CardPayment(person); // TODO - replace with constructor call
            case IBAN: return new BankAccountPayment(person); // TODO - replace with constructor call
            case VOUCHER: return new VoucherPayment(person); //TODO - replace with constructor call
            default: return null;
        }
    }
}


public class Main {

    // DO NOT MODIFY
    private static void test(Person... people) {
        Shop shop = new Shop();
        Random random = new Random(2023);
        shop.addCustomers(List.of(people));

        for (int i = 0; i < 20; i++) {
            int idx = random.nextInt(4);
            shop.createTransaction(shop.getCustomers().get(idx));
        }
    }

    public static void main(String[] args) {
//        TODO - uncomment when done for testing
        Person p1 = new Person("RORNCB221", "523123222", "Gheorghe", "Iliescu", 1234, 287);
        Person p2 = new Person("RORNCB222", "523123223", "Ion", "Popescu", 823, 150);
        Person p3 = new Person("RORNCB223", "523123224", "Giani", "Orlando", 99, 480);
        Person p4 = new Person("RORNCB224", "523123225", "Florin", "Andone", 606, 500);
        test(p1, p2, p3, p4);
    }
}
