package fileio.input;

import lombok.Data;
import main.Playlist;

import java.util.ArrayList;

@Data
public final class UserInput {
    private String username;
    private int age;
    private String city;

    private ArrayList<Playlist> playlists;      // grupam playlist-urile pt fiecre user in parte
    private ArrayList<SongInput> likedSongs;

    public UserInput() {
        this.playlists = new ArrayList<Playlist>();
        this.likedSongs = new ArrayList<SongInput>();
    }

    /**
     * metoda cauta un utilizator in functie de nume
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param username numele utilizatorului pe care il cautam
     * @return o instanta de tip UserInput
     */
    public static UserInput getUserByName(final LibraryInput library, final String username) {

        if (library == null || library.getUsers() == null) {
            return null;
        }

        for (UserInput user: library.getUsers()) {
            if (user.getUsername().equals(username)) {
                return user;
            }
        }

        return null;
    }

}
