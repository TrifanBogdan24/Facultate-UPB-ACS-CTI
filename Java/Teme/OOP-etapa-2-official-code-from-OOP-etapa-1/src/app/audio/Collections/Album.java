package app.audio.Collections;

import app.audio.Files.AudioFile;
import app.audio.Files.Song;
import app.user.User;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;

@Getter
public class Album extends AudioCollection {
    private final Integer releaseYear;
    private final String description;
    private final ArrayList<Song> songs;

    @Setter
    private ArrayList<User> usersWhoLoadIt;      // list of the user who loaded the album

    @Setter
    private Integer likes;



    /**
     *
     * @param owner         artist who created the album
     * @param name          name of the album
     * @param releaseYear   release year of the album
     * @param description   description of album
     * @param songs         album's songs
     */
    public Album(final String owner, final String name, final Integer releaseYear,
                 final String description, final ArrayList<Song> songs) {
        super(name, owner);
        this.releaseYear = releaseYear;
        this.description = description;
        this.songs = songs;
        this.usersWhoLoadIt = new ArrayList<>();
        this.likes = 0;
    }

    /**
     *
     * @return      the number of album's songs
     */
    @Override
    public int getNumberOfTracks() {
        return songs.size();
    }

    /**
     *
     * @param index     a song index
     * @return          the current playing song of the album
     */
    @Override
    public AudioFile getTrackByIndex(final int index) {
        return songs.get(index);
    }

    /**
     *
     * @return      the names of all album's songs
     */
    public ArrayList<String> getSongsToString() {
        if (this == null || this.songs == null) {
            return null;
        }

        ArrayList<String> names = new ArrayList<String>();

        for (Song song: songs) {
            names.add(song.getName());
        }
        return names;
    }
}
