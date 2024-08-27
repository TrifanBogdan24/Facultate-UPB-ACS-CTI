package main;

import fileio.input.LibraryInput;
import fileio.input.SongInput;
import fileio.input.UserInput;

import lombok.Data;

import java.util.ArrayList;

@Data
public class Playlist {

    private String name;
    private String owner;
    private String visibility;

    private int playlistId;
    private int createTime;

    private ArrayList<SongInput> songs;
    private ArrayList<UserInput> followers;

    /**
     * owner => compunere : un paylist nu poate exista fara un utilizator
     * songs => numele melodiilor
     * followers => numele utilizatorilor
     * visibility => "private" sau "public"
     */

    public Playlist() {
        this.songs = new ArrayList<SongInput>();
        this.followers = new ArrayList<UserInput>();
        this.visibility = new String("public");
    }

    /**
     * Cautam un playlist, stiind doar numele acestuia si al detinatorului (user)
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                (playlist-urile apartin utilizatorilor)
     * @param userName primeste numele utilizatorului care detine playlistul
     * @param playlistName numele playlistului pe care il cautam
     * @return playlistul cautat sau null daca nu il gasim
     */
    public static Playlist getPlaylistByUserAndName(final LibraryInput library,
                                                    final String userName,
                                                    final String playlistName) {

        UserInput owner = UserInput.getUserByName(library, userName);

        if (owner == null) {
            // nu exista niciun utilizator cu numele specificat
            return null;
        }

        for (Playlist playlist: owner.getPlaylists()) {
            if (playlist.getName().equals(playlistName)) {
                return playlist;
            }
        }

        return null;
    }



    /**
     * calculam cat timp s-a scurs din playlist-ul incarcat
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                (playlist-urile apartin utilizatorilor)
     * @return timpul total redat din playlist pana la acest moment de timp
     */
    public int getPlaylistDuration(final LibraryInput library) {

        if (this == null) {
            return 0;
        }

        int duration = 0;

        for (SongInput song : this.getSongs()) {
            if (song != null) {
                duration += song.getDuration();
            }
        }

        return duration;
    }

    /**
     * calculam cat timp a mai ramas din playlist-ul incarcat
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                (playlist-urile apartin utilizatorilor)
     * @return timpul total ramss din playlist
     */
    public int getPlaylistPlayedTime(final LibraryInput library) {

        SingletonInformation info = SingletonInformation.getInstance();

        int playedTime = info.getTimeCurrentPlaylistSong();

        for (int i = 0; i <= info.getIdxCurrentPlaylistSong() - 2; i++) {

            SongInput song = this.getSongs().get(i);

            if (song != null) {
                playedTime += song.getDuration();
            }
        }

        return playedTime;
    }

}
