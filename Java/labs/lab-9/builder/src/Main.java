import java.util.ArrayList;

class House {

    // TODO: write optional and mandatory facilities to have in a house
    private String location;
    private int numFloors;
    private int numRooms;
    private boolean pool;
    private String securityCompany;
    private boolean appliances;
    private boolean solarPanels;


    // TODO: complete the private constructor
    private House(HouseBuilder builder) {
        this.location = builder.location;
        this.numFloors = builder.numFloors;
        this.numRooms = builder.numRooms;
        this.pool = builder.pool;
        this.securityCompany = builder.securityCompany;
        this.appliances = builder.appliances;
        this.solarPanels = builder.solarPanels;
    }


    // TODO: generate getters
    public String getLocation() {
        return location;
    }

    public int getNumFloors() {
        return numFloors;
    }

    public int getNumRooms() {
        return numRooms;
    }

    public boolean isPool() {
        return pool;
    }

    public String getSecurityCompany() {
        return securityCompany;
    }

    public boolean isAppliances() {
        return appliances;
    }

    public boolean isSolarPanels() {
        return solarPanels;
    }


    // TODO: override toString method
    @Override
    public String toString() {
        return "House{" +
                "location='" + location + '\'' +
                ", numFloors=" + numFloors +
                ", numRooms=" + numRooms +
                ", pool=" + pool +
                ", securityCompany='" + securityCompany + '\'' +
                ", appliances=" + appliances +
                ", solarPanels=" + solarPanels +
                '}';
    }

    static class HouseBuilder {

        // TODO: write same facilities
        private String location;
        private int numFloors;
        private int numRooms;
        private boolean pool = false;
        private String securityCompany = "";
        private boolean appliances = false;
        private boolean solarPanels = false;


        // TODO: complete the house builder constructor only with the mandatory facilities
        public HouseBuilder(String location, int numFloors, int numRooms) {
            this.location = location;
            this.numFloors = numFloors;
            this.numRooms = numRooms;
        }
        public HouseBuilder() {

        }

        // TODO: add the optional facilities in a builder design
        public HouseBuilder pool(boolean pool) {
            this.pool = pool;
            return this;
        }

        public HouseBuilder securityCompany(String securityCompany) {
            this.securityCompany = securityCompany;
            return this;
        }

        public HouseBuilder appliances(boolean appliances) {
            this.appliances = appliances;
            return this;
        }

        public HouseBuilder solarPanels(boolean solarPanels) {
            this.solarPanels = solarPanels;
            return this;
        }

        // TODO: complete the final build method

        // TODO: test functionality in a Main class
        public House build() {
            return new House(this);
        }
    }
}

class Main {
    private static String spacerSymbols = new String(new char[40]).replace("\0", "-");

    public static void main(String[] args) {
        String spacerSymbols = new String(new char[40]).replace("\0", "-");

        House house = new House.HouseBuilder("Piata Unirii", 3, 10)
                .pool(true)
                .securityCompany("POO_Security")
                .build();

        printOutputSpacerFor("testHouse");
        testHouse(house);
    }

    private static void printOutputSpacerFor(String test) {
        System.out.println(spacerSymbols + test + spacerSymbols);
    }

    private static void testHouse(House house) {
        System.out.println(house.getLocation());
        System.out.println(house.getNumFloors());
        System.out.println(house.getNumRooms());
        System.out.println(house.isPool());
        System.out.println(house.getSecurityCompany());
        System.out.println(house.isAppliances());
        System.out.println(house.isSolarPanels());
    }
}
