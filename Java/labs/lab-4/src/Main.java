import java.util.*;

class Helpers {
    public static final String TOYOTA = "Toyota";
    public static final String HONDA = "Honda";
    public static final String FORD = "Ford";
    public static final String CHEVROLET = "Chevrolet";
    public static final String BMW = "BMW";
    public static final String MERCEDES_BENZ = "Mercedes-Benz";
    public static final String AUDI = "Audi";
    public static final String VOLKSWAGEN = "Volkswagen";
    public static final String NISSAN = "Nissan";
    public static final String HYUNDAI = "Hyundai";
    public static final String KIA = "Kia";
    public static final String MAZDA = "Mazda";
    public static final String SUBARU = "Subaru";
    public static final String JEEP = "Jeep";
    public static final String RAM = "Ram";
    public static final String GMC = "GMC";
    public static final String LEXUS = "Lexus";
    public static final String ACURA = "Acura";
    public static final String INFINITI = "Infiniti";

    public static void addSellers() {
        Database.getDatabase().addSellers(Arrays.asList(
                new Seller("AutoRomânia", Arrays.asList(TOYOTA, HONDA, FORD)),
                new Seller("AutoLux", Arrays.asList(CHEVROLET, BMW, MERCEDES_BENZ)),
                new Seller("Mașini de Vis", Arrays.asList(AUDI, VOLKSWAGEN, NISSAN)),
                new Seller("AutoCity", Arrays.asList(HYUNDAI, KIA, MAZDA)),
                new Seller("Drumuri Off-Road", Arrays.asList(SUBARU, JEEP, RAM)),
                new Seller("Grup AutoElite", Arrays.asList(GMC, LEXUS, ACURA)),
                new Seller("AutoInfinit", Arrays.asList(INFINITI, TOYOTA, HONDA)),
                new Seller("AutoViteză", Arrays.asList(FORD, CHEVROLET, BMW)),
                new Seller("Mașini de Lux", Arrays.asList(MERCEDES_BENZ, AUDI, VOLKSWAGEN)),
                new Seller("Vânzări AutoExpress", Arrays.asList(NISSAN, HYUNDAI, KIA))
        ));
    }

    public static void addDealerships() {
        Database.getDatabase().addDealerships(Arrays.asList(
                new Dealership("Dealer AutoRomânia", Map.of(
                        HONDA, 22000,
                        CHEVROLET, 18000,
                        BMW, 31000,
                        MERCEDES_BENZ, 36000,
                        AUDI, 16000
                )),
                new Dealership("Dealer AutoLux", Map.of (
                        VOLKSWAGEN, 24000,
                        NISSAN, 19000,
                        HYUNDAI, 33000,
                        KIA, 37000,
                        JEEP, 16500
                )),
                new Dealership("Dealer AutoVision", Map.of (
                        LEXUS, 23000,
                        ACURA, 19500,
                        INFINITI, 32000,
                        GMC, 37000,
                        RAM, 16000
                )),
                new Dealership("Dealer AutoElite", Map.of (
                        TOYOTA, 26000,
                        FORD, 20500,
                        NISSAN, 35000,
                        KIA, 38000,
                        SUBARU, 17500
                )),
                new Dealership("Dealer AutoVitesse", Map.of (
                        VOLKSWAGEN, 27000,
                        HONDA, 21500,
                        AUDI, 36000,
                        MERCEDES_BENZ, 39000,
                        CHEVROLET, 18000
                )),
                new Dealership("Dealer AutoFast", Map.of (
                        LEXUS, 28000,
                        NISSAN, 22500,
                        ACURA, 37000,
                        BMW, 40000,
                        INFINITI, 18500
                )),
                new Dealership("Dealer AutoSpeed", Map.of (
                        GMC, 29000,
                        RAM, 23500,
                        JEEP, 38000,
                        CHEVROLET, 41000,
                        TOYOTA, 19000
                )),
                new Dealership("Dealer AutoPower", Map.of (
                        SUBARU, 30000,
                        FORD, 24500,
                        TOYOTA, 39000,
                        NISSAN, 42000,
                        KIA, 19500
                )),
                new Dealership("Dealer AutoRapid", Map.of (
                        ACURA, 31000,
                        INFINITI, 25500,
                        LEXUS, 40000,
                        VOLKSWAGEN, 43000,
                        HONDA, 20000
                ))
        ));
    }
}

class Dealership {
    private String name;
    private Map<String, Integer> cars;

    public Dealership(Dealership dealership) {
        // TODO
        // copy constructorul clasei
        this.name = dealership.name;
        this.cars = dealership.cars;
    }

    public Dealership(String name, Map<String, Integer> cars) {
        this.name = name;
        this.cars = cars;
    }

    public double averagePrice() {
        // TODO : bI

        double sum = 0;
        double N = (double) (this.cars.size());

        for (String cheie : this.cars.keySet()) {
            sum = sum + (double)(this.cars.get(cheie));
        }

        return (sum / N);
    }

    public int getPriceForBrand(String brand) {
        // TODO
        if (!this.cars.containsKey(brand)) {
            return 0;
        }
        return this.cars.get(brand);
    }

    public List<Seller> getAllSellers() {
        // TODO : fI
        List<Seller> referinta = Database.getDatabase().getAllSellers();

        // deep-copy
        List<Seller> ans = new ArrayList<>();
        for (Seller ref: referinta) {
            ans.add(ref);
        }

        return Collections.unmodifiableList(ans);
    }

    public List<Dealership> getAllDealerships() {
        // TODO : fII
        List<Dealership> referinta = Database.getDatabase().getAllDealerships();

        // deep-copy
        List<Dealership> ans = new ArrayList<>();
        for (Dealership ref: referinta) {
            ans.add(ref);
        }

        return Collections.unmodifiableList(ans);
    }

    public List<Dealership> getDealershipsByBrand(String brand) {
        // TODO : fIII
        List<Dealership> referinta = Database.getDatabase().getDealershipsByBrand(brand);

        // deep-copy
        List<Dealership> ans = new ArrayList<>();
        for (Dealership ref: referinta) {
            ans.add(ref);
        }

        return Collections.unmodifiableList(ans);
    }

    public List<Seller> getSellersByBrand(String brand) {
        // TODO : fIV
        List<Seller> referinta = Database.getDatabase().getSellersByBrand(brand);

        // deep-copy
        List<Seller> ans = new ArrayList<>();
        for (Seller ref: referinta) {
            ans.add(ref);
        }

        return Collections.unmodifiableList(ans);
    }

    public List<Dealership> getDealershipsByAveragePrice() {
        // TODO : fV
        List<Dealership> referinta = Database.getDatabase().getDealershipsByAveragePrice();

        // deep-copy
        List<Dealership> ans = new ArrayList<>();
        for (Dealership ref: referinta) {
            ans.add(ref);
        }

        return Collections.unmodifiableList(ans);
    }

    public List<Dealership> getDealershipsByPriceForBrand(String brand) {
        // TODO : fVI
        List<Dealership> referinta = Database.getDatabase().getDealershipsByPriceForBrand(brand);
        // deep-copy
        List<Dealership> ans = new ArrayList<>();
        for (Dealership ref: referinta) {
            ans.add(ref);
        }

        return Collections.unmodifiableList(ans);
    }

    @Override
    public String toString() {
        return "Dealership{" +
                "name='" + name + '\'' +
                ", cars=" + cars +
                '}';
    }

    public String getName() {
        return this.name;
    }

    public Map<String, Integer> getCars() {
        return this.cars;
    }

    public void setName(String name) {
        this.name = name;
    }
}

class Seller {
    private String name;
    private List<String> carBrands = new ArrayList<>();

    public Seller(Seller seller) {
        this.name = seller.getName();
        this.carBrands = new ArrayList<>(seller.getCarBrands());
    }

    public Seller(String name, List<String> carBrands) {
        this.name = name;
        this.carBrands = carBrands;
    }

    public List<Seller> getAllSellers() {
        // TODO : e1
        return Database.getDatabase().getAllSellers();
    }

    public List<Dealership> getAllDealerships() {
        // TODO : eII
        return Database.getDatabase().getAllDealerships();
    }

    public List<Dealership> getDealershipsByBrand(String brand) {
        // TODO : eIII
        return Database.getDatabase().getDealershipsByPriceForBrand(brand);
    }

    public List<Seller> getSellersByBrand(String brand) {
        // TODO : eIV
        return Database.getDatabase().getSellersByBrand(brand);
    }

    public List<Dealership> getDealershipsByAveragePrice() {
        // TODO : eV
        return Database.getDatabase().getDealershipsByAveragePrice();
    }

    public List<Dealership> getDealershipsByPriceForBrand(String brand) {
        // TODO : eVI
        return Database.getDatabase().getDealershipsByPriceForBrand(brand);
    }

    public String getName() {
        return this.name;
    }

    public List<String> getCarBrands() {
        return this. carBrands;
    }

    @Override
    public String toString() {
        return "Seller{" +
                "name='" + name + '\'' +
                ", carBrands=" + carBrands +
                '}';
    }
}

class Database {
    private List<Seller> sellers = new ArrayList<>();
    private List<Dealership> dealerships = new ArrayList<>();

    private static int numberOfInstances = 0;

    private static Database instance = null;

    private Database() {
    }

    // TODO: make it Singleton
    public static Database getDatabase() {
        // TODO : make it Singleton
        if (instance == null) {
            instance = new Database();
        }
        return instance;
    }



    public static int getNumberOfInstances() {
        // TODO : g
        if (instance == null) {
            return 0;
        }
        return 1;
    }

    public List<Seller> getAllSellers() {
        return this.sellers;
    }

    public List<Dealership> getAllDealerships() {
        return this.dealerships;
    }

    public List<Dealership> getDealershipsByBrand(String brand) {
        // TODO : dIII
        // primește ca parametru numele unui brand
        // întoarce lista cu dealership-urile care detin brand-ul respectiv

        List<Dealership> res = new ArrayList<>();

        for (Dealership reprezentanta: this.dealerships) {
            Map<String, Integer> masini = reprezentanta.getCars();

            // verifcam apartenenta in dictionar
            if (masini.containsKey(brand)) {
                // ar fi mers si : reprezentanta.getCars().containsKey(brand)
                res.add(new Dealership(reprezentanta));     // deep-copy
            }
        }

        return  res;
    }

    public List<Seller> getSellersByBrand(String brand) {
        // TODO : dIV
        // primeste ca parametru numele unui brand
        // intoarce lista cu sellerii care vand brandul respectiv


        List<Seller> res = new ArrayList<>();

        for (Seller comerciant: this.sellers) {
            List<String> masini = comerciant.getCarBrands();

            // verificam apartenenta in vector
            if (masini.contains(brand)) {
                // ar fi mers si : comerciant.getCarBrands().contains(brand)
                res.add(new Seller(comerciant));     // deep-copy
            }
        }

        return res;
    }

    public List<Dealership> getDealershipsByAveragePrice() {
        // TODO : dV
        // întoarce lista cu dealership-uri
        // sortate crescător în funcție de pretul lor mediu

        // deep copy
        List<Dealership> res = new ArrayList<>();
        for (Dealership reprezentanta:  this.getAllDealerships()) {
            res.add(new Dealership(reprezentanta));
        }

        // ordonam crescator lista schimband referintele acolo unde este cazul
        Collections.sort(res, new Comparator<Dealership>() {
            @Override
            public int compare(Dealership d1, Dealership d2) {
                double average1 = d1.averagePrice();
                double average2 = d2.averagePrice();
                return Double.compare(average1, average2);
            }
        });

        return  res;
    }

    public List<Dealership> getDealershipsByPriceForBrand(String brand) {
        // TODO : dVI
        // primește ca parametru numele unui brand
        // întoarce lista cu dealership-urile care detin acel brand
        // sortate după pretul acestuia în ordine crescătoare

        // secventa new ArrayList<>(original-list)      va copia lista la o alta zona de memorie
        List<Dealership> res = new ArrayList<>(this.getDealershipsByBrand(brand));

        // ordonam crescator lista schimband referintele acolo unde este cazul
        Collections.sort(res, new Comparator<Dealership>() {
            @Override
            public int compare(Dealership d1, Dealership d2) {
                double price1 = d1.getPriceForBrand(brand);
                double price2 = d2.getPriceForBrand(brand);
                return Double.compare(price1, price2);
            }
        });

        return  res;
    }

    public void addSellers(List<Seller> sellers) {
        this.sellers = sellers;
    }

    public void addDealerships(List<Dealership> dealerships) {
        this.dealerships = dealerships;
    }

    @Override
    public String toString() {
        return "Database{" +
                "sellers=" + sellers +
                ", dealerships=" + dealerships +
                '}';
    }
}



public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int task = scanner.nextInt();

        // adding entities to database
        Helpers.addDealerships();
        Helpers.addSellers();

        // tests
        Dealership testDealership = Database.getDatabase().getAllDealerships().get(0);
        Seller testSeller = Database.getDatabase().getAllSellers().get(0);

        switch(task) {
            case 1:
                System.out.println(testDealership.getName());
                System.out.println(testSeller.getName());
                break;

            case 2:
                System.out.println(testDealership.averagePrice());
                System.out.println(testDealership.getPriceForBrand("Chevrolet"));
                break;

            case 3:
                Seller testSellerCopy = new Seller(testSeller);
                System.out.println(testSellerCopy);
                break;

            case 4:
                for (Dealership dealership : Database.getDatabase().getAllDealerships()) {
                    System.out.println(dealership.getName());
                }
                System.out.println("------------------------------");
                for (Seller seller : Database.getDatabase().getAllSellers()) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Dealership dealership : Database.getDatabase().getDealershipsByBrand("Honda")) {
                    System.out.println(dealership.getName());
                }
                System.out.println("------------------------------");
                for (Seller seller : Database.getDatabase().getSellersByBrand("Toyota")) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Dealership dealership : Database.getDatabase().getDealershipsByAveragePrice()) {
                    System.out.println(dealership.getName());
                }
                System.out.println("------------------------------");
                for (Dealership dealership : Database.getDatabase().getDealershipsByPriceForBrand("Lexus")) {
                    System.out.println(dealership.getName());
                }
                break;

            case 5:
                System.out.println("TESTING SELLER FUNCTIONALITIES:\n");

                for (Seller seller : testSeller.getAllSellers()) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Seller seller : testSeller.getSellersByBrand("Toyota")) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Seller seller : testSeller.getSellersByBrand("Mercedes-Benz")) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Dealership dealership : testSeller.getAllDealerships()) {
                    System.out.println(dealership.getName());
                }

                System.out.println("\nTESTING MUTABILITY FOR SELLERS:\n");

                testSeller.getAllDealerships().get(0).setName("New Dealership");
                for (Dealership dealership : testSeller.getAllDealerships()) {
                    System.out.println(dealership.getName());
                }
                System.out.println("------------------------------");
                for (Dealership dealership : testSeller.getDealershipsByBrand("Nissan")) {
                    System.out.println(dealership.getName());
                }
                for (Dealership dealership : testSeller.getDealershipsByAveragePrice()) {
                    System.out.println(dealership.getName());
                }
                System.out.println("------------------------------");
                for (Dealership dealership : testSeller.getDealershipsByPriceForBrand("Kia")) {
                    System.out.println(dealership.getName());
                }
                break;

            case 6:
                System.out.println("TESTING DEALERSHIP FUNCTIONALITIES:\n");

                for (Seller seller : testDealership.getAllSellers()) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Seller seller : testDealership.getSellersByBrand("Nissan")) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Seller seller : testDealership.getSellersByBrand("Kia")) {
                    System.out.println(seller.getName());
                }
                System.out.println("------------------------------");
                for (Dealership dealership : testDealership.getAllDealerships()) {
                    System.out.println(dealership.getName());
                }
                break;

            case 7:
                try {
                    // testing immutability for Dealership
                    testDealership.getAllDealerships().get(0).setName("New Dealership");

                    // it should throw an error if getAllDealerships() from Dealership is completely immutable
                    testDealership.getAllDealerships().add(new Dealership(testDealership));
                }
                catch(Exception e) {
                    System.out.println("Dealership immutability test passed!");
                }
                break;

            case 8:
                System.out.println(Database.getNumberOfInstances());
                break;
        }

    }
}