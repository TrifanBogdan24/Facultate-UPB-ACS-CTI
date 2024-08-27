package fileio.input;

import java.util.ArrayList;
import lombok.Data;

@Data
public final class SongInput {
    private String name;
    private Integer duration;
    private String album;
    private ArrayList<String> tags;
    private String lyrics;
    private String genre;
    private Integer releaseYear;
    private String artist;

    private ArrayList<UserInput> usersWhoLikedIt;

    public SongInput() {
        this.usersWhoLikedIt = new ArrayList<UserInput>();
    }

    /**
     * metoda cauta o melodie in functie de numele primit ca parametru
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param songName primeste numele meldoiei pe care dorim sa o cautam
     * @return o instanta SongInput daca am gasit melodia dupa nume, alfel null
     */
    public static SongInput getSongByName(final LibraryInput library, final String songName) {

        if (library == null) {
            return null;
        }

        for (SongInput song: library.getSongs()) {
            if (song.getName().equals(songName)) {
                return song;
            }
        }

        return null;
    }

}
