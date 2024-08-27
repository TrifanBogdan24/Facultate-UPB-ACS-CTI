package app;

import lombok.Data;

@Data
public class Event {
    private String artistName;
    private String name;
    private String decription;
    private String date;
    private int day;
    private int month;
    private int year;


    private static final int ZERO = 0;
    private static final int ONE = 1;
    private static final int TWO = 2;
    private static final int THREE = 3;
    private static final int FOUR = 4;
    private static final int FIVE = 5;
    private static final int SIX = 6;
    private static final int SEVEN = 7;
    private static final int EIGHT = 8;
    private static final int NINE = 9;
    private static final int TEN = 10;
    private static final int ELEVEN = 11;
    private static final int TWELVE = 12;

    private static final int TWENTY_EIGHT = 28;
    private static final int TWENTY_NINE = 29;
    private static final int THIRTY = 30;
    private static final int THIRTY_ONE = 30;


    private static final int MIN_VALID_YEAR = 1900;
    private static final int MAX_VALID_YEAR = 2023;
    private static final int ONE_HUNDRED = 100;
    private static final int FOUR_HUNDREDS = 400;



    public Event() {

    }

    /**
     *
     * @param artistName        the artist, the, the owner of the event
     * @param name              the name of the album
     * @param decription        the description of the album
     * @param date              the date of the album : 03-05-2022
     */
    public Event(final String artistName, final String name,
                 final String decription, final String date) {
        this.artistName = artistName;
        this.name = name;
        this.decription = decription;
        this.setDate(date);
    }

    /**
     * sets all date related fields (String, day, month, year)
     *
     * @param date      the String date : 04-03-2022
     */
    public void setDate(final String date) {

        this.date = date;
        this.day = validNumberInString(date, ZERO, ONE);
        this.month = validNumberInString(date, THREE, FOUR);
        this.year = validNumberInString(date, SIX, NINE);
    }

    /**
     *
     * @param date      String representing the date: 03-04-2022
     * @return          true if the String stores a valid date
     *                  false if the String doesn't store a valid date
     */
    public static boolean validDate(final String date) {
        // valid date : dd-mm-yyyy
        if (date == null || date.length() != TEN) {
            return false;
        }

        if (date.charAt(TWO) != '-' || date.charAt(FIVE) != '-') {
            return false;
        }

        int verifyDay = validNumberInString(date, ZERO, ONE);
        int verifyMonth = validNumberInString(date, THREE, FOUR);
        int verifyYear = validNumberInString(date, SIX, NINE);

        if (verifyDay <= ZERO || verifyMonth <= ZERO || verifyYear <= ZERO) {
            return false;
        }

        if (verifyYear < MIN_VALID_YEAR || verifyYear > MAX_VALID_YEAR) {
            return false;
        }

        /**
         * verifyMonths with 31 verifyDays : 1st, 3rd, 5th, 7th, 8th, 10th, 12th
         * verifyMonths with 30 verifyDay : 4th, 6th, 9th, 11th
         * 2nd verifyMonth has
         *      28 verifyDays for non-bisect verifyYear
         *      29 verifyDays for bisect verifyYear
         * bisect verifyYear : https://ro.wikipedia.org/wiki/An_bisect
         */

        switch (verifyMonth) {
            case ONE: return (verifyDay <= THIRTY_ONE);       // January
            case THREE: return (verifyDay <= THIRTY_ONE);     // March
            case FOUR: return (verifyDay <= THIRTY);          // April
            case FIVE: return (verifyDay <= THIRTY_ONE);      // May
            case SIX: return (verifyDay <= THIRTY);           // June
            case SEVEN: return (verifyDay <= THIRTY_ONE);     // July
            case EIGHT: return (verifyDay <= THIRTY_ONE);     // August
            case NINE: return (verifyDay <= THIRTY);          // September
            case TEN: return (verifyDay <= THIRTY_ONE);       // October
            case ELEVEN: return (verifyDay <= THIRTY);        // November
            case TWELVE: return (verifyDay <= THIRTY_ONE);    // December
            case TWO:                         // February
                if (verifyDay < TWENTY_EIGHT || verifyDay > TWENTY_NINE) {
                    return false;
                }
                if (verifyDay == TWENTY_EIGHT) {
                    return true;
                }

                // verifyDay == 29 (bisect verifyYear logic)
                return ((verifyYear % FOUR == 0 && verifyYear % ONE_HUNDRED != 0)
                        || verifyYear % FOUR_HUNDREDS == 0);
            default: return false;          // invalid verifyMonth number
        }
    }

    /**
     *
     * @param str       String representing index
     * @param idx1      the first index
     * @param idx2      the second inex
     * @return          the int number that we assume the String stores between
     *                  the first and the second index
     */
    public static int validNumberInString(final String str, final int idx1, final int idx2) {
        if (str == null || str.length() < idx1 || str.length() < idx2 || idx1 > idx2) {
            return -1;
        }

        int nr = 0;
        char c;

        for (int i = idx1; i <= idx2; i++) {
            c = str.charAt(i);
            if (!isDigit(c)) {
                return -1;
            }
            nr = nr * TEN + c - '0';
        }
        return nr;
    }

    /**
     *
     * @param c     a character
     * @return      true if c is a digit
     *              and false otherwise
     */
    public static boolean isDigit(final char c) {
        if (c < '0' || c > '9') {
            return false;
        }
        return true;
    }
}
