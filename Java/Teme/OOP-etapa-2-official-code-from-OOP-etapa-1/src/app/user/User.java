package app.user;

import app.audio.Playlist;
import app.audio.Files.AudioFile;
import app.audio.Files.Song;
import app.player.Player;
import app.player.PlayerStats;
import app.searchBar.SearchBar;
import app.utils.Enums;
import lombok.Data;

import java.util.ArrayList;

/**
 * The type User.
 */

@Data
public class User {
    private String username;
    private int age;
    private String city;
    private ArrayList<Playlist> playlists;
    private ArrayList<Song> likedSongs;
    private ArrayList<Playlist> followedPlaylists;
    private Player player;
    private SearchBar searchBar;
    private boolean lastSearched;
    private Integer timestampLastPlayedFile;

    private boolean online;

    private Enums.PageType pageType;
    private Artist artistForPage;
    private Host hostForPage;

    // name of users who have an audio file belonging to him/her
    private ArrayList<User> userInteractions;


    public User() {
        this.userInteractions = new ArrayList<>();
        this.online = true;
        this.pageType = Enums.PageType.HOME;
    }

    /**
     * Instantiates a new User.
     *
     * @param username the username
     * @param age      the age
     * @param city     the city
     */
    public User(final String username, final int age, final String city) {
        this.username = username;
        this.age = age;
        this.city = city;
        this.playlists = new ArrayList<>();
        this.likedSongs = new ArrayList<>();
        this.followedPlaylists = new ArrayList<>();
        this.player = new Player();
        this.searchBar = new SearchBar(username);
        this.lastSearched = false;
        this.online = true;

        this.pageType = Enums.PageType.HOME;
        this.userInteractions = new ArrayList<>();
    }



    /**
     * Gets player stats.
     *
     * @return the player stats
     */
    public PlayerStats getPlayerStats() {
        return player.getStats();
    }

    /**
     * Show preferred songs array list.
     *
     * @return the array list
     */
    public ArrayList<String> showPreferredSongs() {
        ArrayList<String> results = new ArrayList<>();
        for (AudioFile audioFile : likedSongs) {
            results.add(audioFile.getName());
        }

        return results;
    }

    /**
     * Gets preferred genre.
     *
     * @return the preferred genre
     */
    public String getPreferredGenre() {
        String[] genres = {"pop", "rock", "rap"};
        int[] counts = new int[genres.length];
        int mostLikedIndex = -1;
        int mostLikedCount = 0;

        for (Song song : likedSongs) {
            for (int i = 0; i < genres.length; i++) {
                if (song.getGenre().equals(genres[i])) {
                    counts[i]++;
                    if (counts[i] > mostLikedCount) {
                        mostLikedCount = counts[i];
                        mostLikedIndex = i;
                    }
                    break;
                }
            }
        }

        String preferredGenre = mostLikedIndex != -1 ? genres[mostLikedIndex] : "unknown";
        return "This user's preferred genre is %s.".formatted(preferredGenre);
    }

    /**
     * Simulate time.
     *
     * @param time the time
     */
    public void simulateTime(final int time) {
        if (this.online) {
            player.simulatePlayer(time);
        }
    }

    /**
     * calling this function will remove the current source that the user was playing
     * removing it from the player
     */
    public void anulatesUserLoad() {
        this.searchBar.unloadSource();
    }
}
